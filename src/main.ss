;;;; .title Compiler Core (main.ss)

;;;; This contains the core compiler entry points. 
;;;; Loaded by both the Chez and PLT versions.
;;;; (Expects to be loaded from the directory that contains it.)


;======================================  
;(display "Loading main compiler module.  RegionStreams Demo.")
;(newline)

(define-regiment-parameter regiment-version "0.87.2")

(define-regiment-parameter svn-revision
  (and (zero? (system "which svn > /dev/null"))
       (read (open-input-string (system-to-str "svn info | grep Revision | sed s/Revision://")))))

;; This is the global parameter that determines which transformations
;; (passes) the compiler applies and in what order.  We set it here.
(pass-list
  (list
    ;; (1) Type checking comes first, but that happens before these passes
    ;; are run.  Maybe should make it one of the "passes".
    
    ;; (2) Next we verify our input language.
    verify-regiment

    ;; (3) Then we do a little massaging/normalization.
    eta-primitives
    rename-vars
    remove-unquoted-constant
    retypecheck

    ;; (4) Then -- here comes the metaprogramming -- we evaluate as much
    ;; of the program as can be evaluated.  The residual had better follow our
    ;; restrictions on implementable Regiment programs.
    static-elaborate
    verify-elaborated
    retypecheck

    ;; (5) Now we normalize the residual in a number of ways to
    ;; produce the core query language, then we verify that core.
    reduce-primitives    
    remove-complex-constant  
    retypecheck

    uncover-free             

    lift-letrec              
    lift-letrec-body         
    remove-complex-opera*
    verify-core
    retypecheck

    ;; (6) Analysis: these passes analyze the query circuit and
    ;; annotate it with various information which may be used in
    ;; "deglobalize" further down the road.  Currently, most of these
    ;; analyses are underdeveloped; more work is warranted.
    classify-names ;; Remove this pass..
    add-heartbeats
    add-control-flow
    add-data-flow
    resolve-fold-trees
    add-places      ;; UNNECESSARY CURRENTLY
;    add-routing
    analyze-places

    ;; (7) Finally, the core of the Regiment compiler.  Convert a
    ;; Regiment query into a (albeit high level) node-level
    ;; Token Machine program.
    deglobalize

    ;; (8) There's a large gap from the high-level (human readable) TM
    ;; language and the low-level (actually implemented) TM language.
    ;; Cleanup-token-machine does a lot of the work of desugaring.
    cleanup-token-machine 
    desugar-macros		
;    cleanup-token-machine   ;; TEMP: FIXME

    ;; (9) The next major step is desugaring the gradient
    ;; communication constructs used in TML.  
    find-emittoks
    desugar-gradients
    cleanup-token-machine   ;; Rerun to expand out some stuff.
    ;    analyze-tokmac-recursion    

    ;; (10) Then we desugar another construct: "let-stored".
    desugar-let-stored
    rename-stored

    ;; (11) CPS: we CPS the program to get rid of all non-tail calls.

    ;; This is because Token handlers may only schedule more tokens,
    ;; not wait for "child" handlers to return!  This is also the
    ;; trick we use to implement synchronous sensor reading, which is
    ;; really split-phased in the underlying NesC.
;    cps-tokmac
;    sever-cont-state 
;    closure-convert
;    cleanup-token-machine ;; Trying this.. [2005.09.27]

    ;; (12) Optimization: (unfinished) there's a bunch of low-hanging
    ;; fruit optimizations on Token Machines that we should be doing.
;    inline-tokens

    ;; (*) OLD: The compiler back-end used to be in Haskell.  Thus we
    ;; would export the intermediate language to a haskell readable
    ;; form for the back-end to work with it.
    ;haskellize-tokmac 
    
    ;; (13) This is the current back-end.  
    ;; Uncomment to make the compiler generate NesC code.
;    flatten-tokmac
;    emit-nesc
    ))

; ==================================================================
;; Functions for input/output to filesystem and for invoking compiler.

(define (dump-tokenmachine-to-file prog fn)
  (match prog
    [(haskellize-tokmac-language ,str)
     (with-output-to-file fn
       (lambda () (display str) (newline))
       'replace)]
    ;; If it's an earlier file, pretty print it:
    [(,lang ,prog ,rest ...)
     (guard (list? prog))
     (with-output-to-file fn
       (lambda () (pretty-print `(,lang ,prog)))
       'replace)]
    [,other (error 'dump-tokenmachine-to-file "invalid input: ~S" prog)]))

;; This dumps to file only when provided the optional string filename argument:
;; The symbolic options are:  'barely-tokens 'almost-tokens 'almost-haskell 'haskell-tokens
;; Also: use 'verbose to print the output of every pass.
(define (run-compiler p . args )                              ;; Entrypoint.
  ;(disp "RUN COMP:" p)
  (let ([filename #f]
	[passes (pass-list)]
	[verbose #f]
	;; If there are still values that need to be evaluated and filled in in the TM, do it.
	[p (match p
	     [(quasiquote ,_) (eval p)]
	     [,_ p])])
    (for-each (lambda (arg)
		(cond
                  [(string? arg) ;; It's an output filename.
                   (set! filename arg)]
		  [(eq? arg 'verbose) (set! verbose #t)]
		  ;; The pass-list may have already been restricted to be just the TML passes:
                  [(eq? arg 'barely-tokens)
                   (set! passes (list-remove-first cleanup-token-machine
				  (list-remove-after cleanup-token-machine (pass-list))))]
                  [(eq? arg 'almost-tokens)
                   (set! passes (list-remove-first deglobalize ;; <- might fizzle
				  (list-remove-first cleanup-token-machine
                                     (list-remove-after cleanup-token-machine (pass-list)))))]
                  [(eq? arg 'almost-haskell)
                   (set! passes (remq haskellize-tokmac (pass-list)))]
                  [(eq? arg 'haskell-tokens) (void)]
		  ;; Otherwise... do nothing.
		  ;[else (warning 'run-compiler "ignored flag: ~s" arg)]
		  ))
	      args)
    (when verbose
	  (printf "Running compiler with pass-list: \n")
	  (pretty-print passes))
    (let ((funs (map eval passes)))
      (let loop ([p p] [funs funs] [names passes])
        (if (null? funs) 
            (begin (if filename (dump-tokenmachine-to-file p filename)
                       p))
	    (begin 
	      (if verbose
		  (begin
		    (printf ";===============================================================================\n")
		    (printf "~a:\n\n" (regiment-pass->name (car names)))))
	      (let ((result ((car funs) p)))
		(when verbose
		  (pretty-print result) (newline))
		(loop result (cdr funs) (cdr names)))))))))



;; [2006.08.27] This version executes the alternate WaveScript compiler.
;; It takes it from (parsed) source down as far as the WaveScript commpiler 
;; can go right now.  But it does not invoke the simulator or the c_generator.
(define (run-ws-compiler p)                                   ;; Entrypoint.
  (define optional-stop 
    (lambda (x)      
      (if (regiment-verbose)
	  (IFDEBUG
	   (begin (parameterize ([pretty-line-length 150]
				 [print-length 20]
				 [print-level 20])
		    (newline)
		    (pretty-print x))
		  (printf "================================================================================\n\n")
		  (read-line)
		  x)
	   x)
	  x)))

  (set! p (optional-stop (pass_desugar-pattern-matching p)))
  (set! p (optional-stop (verify-regiment p)))
  (printf "Program verified.\n")
  
  (set! p (optional-stop (rename-vars p)))
  (set! p (optional-stop (eta-primitives p)))
  (set! p (optional-stop (remove-unquoted-constant p)))
  (set! p (optional-stop (static-elaborate p)))

  (set! p (optional-stop (reduce-primitives p)))

  (set! p (optional-stop (merge-iterates p)))
  ;(set! p (optional-stop (nominalize-types p)))

  p)


;; The WaveScript "interpreter".  (Really a wavescript embedding.)
;; It loads, compiles, and evaluates a wavescript query.
;; .param x - can be an input port, a filename, or a wavescript AST (list)
(define (wsint x)                                             ;; Entrypoint.  
  (define prog
    (strip-types ;; <- TODO REMOVE!!!
     (cond  [(input-port? x) (printf "WSINT: Loading WS source from port: ~s\n" x) 
	     ;; We assume this is parsed but not post-processed:
	     (ws-postprocess (read x))]
	    [(string? x) (printf "WSINT: Loading WS source from file: ~s\n" x)
	     (read-wavescript-source-file x)]
	    [(list? x)   (printf "WSINT: Evaluating WS source.\n" x)  x]
	    [else (error 'wsint "bad input: ~s" x)])))

  (define _ (begin (printf "Evaluating program: (original program stored in .__inputprog.ss)\n\n") 
		   (parameterize ([pretty-line-length 180]
				  [pretty-maximum-lines 1000]
				  [print-level 20]
				  [print-length 10])
		     (pretty-print prog))
		   (with-output-to-file ".__inputprog.txt"
		     (lambda () 
		       (parameterize ([pretty-line-length 200]
				      [pretty-maximum-lines #f]
				      [print-level #f]
				      [print-length #f]
				      [print-graph #f])
			 (pretty-print prog))
		       (flush-output-port))
		     'replace)
		   ))

  (define typed (run-ws-compiler prog))

  (define __ (printf "Program verified, type-checked. (Also dumped to \".__parsed.txt\".)"))

  ;(define ___ (inspect typed))
  
  ;; TEMP
  ;;(printf "doing eta-prims: \n")
  ;;(set! typed (eta-primitives typed))
  ;;(pretty-print typed)
  ;; other desugaring...

  (define stream (delay (wavescript-language 
			 (match (strip-types typed)
			   [(,lang '(program ,body ,_ ...)) body]))))
;  (define stream (wavescript-language desugared))

;  (inspect typed)
  
  (printf "\nProgram types: (also dumped to \".__types.txt\")\n\n")
  (print-var-types typed)(flush-output-port)
  (with-output-to-file ".__types.txt"
    (lambda () (print-var-types typed)(flush-output-port))
    'replace)
  (parameterize ([pretty-line-length 150]
		 [pretty-one-line-limit 100])
    (with-output-to-file ".__parsed.txt"
      (lambda () (pretty-print prog)(flush-output-port))
      'replace))
  stream)



;; This one just stops after deglobalize:
(define (compile-to-tokens p . args)                          ;; Entrypoint.
  (apply run-compiler p 'barely-tokens args))
(define (compile-almost-to-tokens p . args)                   ;; Entrypoint.
  (apply run-compiler p 'almost-tokens args))

;; This finishes off the compilation of scheme-format token machine.
;; It's just a front-end to run-compiler that restricts the passes we run over.
(define (assemble-tokmac tm . args)                           ;; Entrypoint.
  (printf "assem tokmac...\n" )
  (let ([starting-place 
	 (match tm
	   [(,lang ,prog ...)
	    (case lang
	      [(add-places-language) analyze-places] ;; Not strictly correct.
	      [(deglobalize-lang) deglobalize]
	      [(cleanup-token-machine-lang) cleanup-token-machine]
	      [(cps-tokmac-lang) cps-tokmac]
	      
					;[(haskellize-tokmac-lang) (error...
	      [else deglobalize])]
	   [,else deglobalize])
	 ])
  (let ((passes (cdr (list-remove-before starting-place (pass-list)))))
    (disp "Assembling tokmac with passes: " passes)
;    (lambda (tm)
      (parameterize ([pass-list passes])
	(apply run-compiler tm args)))))



; =============================================================
;;; Shorthands.  
;;; These are just for my convenient usage in interactive invocation of the compiler.

(define r  ;; shorthand
  (letrec ((loop
	    (case-lambda 
	     [(pass x)
	      (let ((prog  x))
		(parameterize ((pass-list (list-remove-after (eval pass) (pass-list)))
			       ;(tracer #t)
			       )
		  (test-one prog #f #f)))]
	     [(x) (loop (rac (pass-list)) x)])))
    loop))

(define at assemble-tokmac) ;; shorthand

(define rc run-compiler) ;; shorthand

(define ct compile-to-tokens) ;; shorthand

(define ra run-simulator-alpha) ;; shorthand

;; Token and later compiler:
(define (tr x)  ;; shorthand
  (let ((prog  x))
    (parameterize (;(tracer #t)
		   (game-eval (lambda args 'unspecified))
		   (host-eval (lambda args 'unspecified)))
      (parameterize ((pass-list (cdr (list-remove-before 'deglobalize (pass-list)))))
	(test-one prog)))))

; temp:
(define (rr) (r '(circle 50 (anchor-at '(30 40))))) ;; shorthand
; temp:
(define (doit x) ;; shorthand
  (cleanse-world)
  (run-simulation (build-simulation (compile-simulate-nought x))		  
		  20.0))


;; These are some temporary diagnostic functions:
(define (all-incoming) ;; shorthand
  (filter (lambda (ls) (not (null? ls)))
          (map simobject-incoming all-objs)))
(define (all-homepage) ;; shorthand
  (filter (lambda (ls) (not (null? ls)))
          (map simobject-homepage all-objs)))

(define (sim) (build-simulation  ;; shorthand
	     (compile-simulate-nought 
	      (cadadr (run-compiler '(anchor-at '(30 40)))))))

; =============================================================
;;; Temporary junk:


;; HOW TO RUN:
;; ----------------------------------------
;; Here I'll give some examples of how to run the system.  
;; This is for me -- because I forget how to invoke things over time.
;; (Though the unit tests give me something...)

;; simulator_nought.examples.ss -- has some example token machines.


;(define (t1) (init-world) (run-simulation        sim 2.0))
;(define (t2) (init-world) (run-simulation-stream sim 2.0))

;; Sigh, first class tokens:
;(r '(rmap (lambda (x) (rmap (lambda (y) y) world)) world)) 
  
;======================================================================

[define tm-to-list ;; This is boilerplate, many of these tests just run the following:
	 (lambda (tm . extraparams)
	   `(parameterize ([unique-name-counter 0] 
			   [simalpha-dbg-on #f]
			   ,@extraparams
			   )
		 (let ([prog (assemble-tokmac ',tm
					   ;'verbose
					   )])
		   (let ((prt (open-output-string)))
		     (display "(" prt)
		     (let ((result (run-simulator-alpha prog 
					;'timeout 10000
					;'outport prt
							)
				   ))
		     (display ")" prt)
		     (read (open-input-string (get-output-string prt)))
		     result ;; Returns the soc-returned values rather than the output list.
		     ))))
	   )]

;; TODO FIXME:  Expand some of these tests to use more passes.

;; These are some of our system tests.  They test the compiler and the simulator as a whole.
;; The rest of the system tests are in the files named tests_*.ss
;; But some of the below tests may also be miscellaneous unit tests that require more than one module.
(define these-tests 
  (let ([tm-to-list ;; This is boilerplate, many of these tests just run the following:
	 (lambda (tm . extraparams)
	   `(parameterize ([unique-name-counter 0]
			   [simalpha-dbg-on #f]
			   ,@extraparams
			   )
		 (let ([prog (assemble-tokmac ',tm
					   ;'verbose
					   )])
		   (let ((prt (open-output-string)))
		     (display "(" prt)
		     (let ((result (run-simulator-alpha prog 
					;'timeout 10000
					'outport prt
							)
				   ))
		       (display ")" prt)
		       (read (open-input-string (get-output-string prt)))
		     ))))
	   )]
	
	;; This one returns soc-return'd vals rather than the printed data.
	[tm-to-socvals
	 (lambda (tm . extraparams)
	   `(parameterize ([unique-name-counter 0] 
			   [simalpha-dbg-on #f]
			   ,@extraparams
			   )
		 (let ([prog (assemble-tokmac ',tm 
					   )])
		   ;(profile-clear) ;; Temp: profiling the simulator:
		   (let ((result (run-simulator-alpha prog 
					;'timeout 10000
						      )))
		     result ;; Returns the soc-returned values rather than the output list.
		     ))))]
	)

    ;; I put them in another file because they were simply taking up too many LOC:
    (include "generic/testing/system_tests.ss")
))

(define test-this (default-unit-tester "Main compiler units + system tests." these-tests))
(define maintests these-tests)
(define maintest test-this)



'
(parameterize ([pass-list
             '(cleanup-token-machine
                desugar-gradients
                cleanup-token-machine
                desugar-let-stored
                rename-stored
                cps-tokmac
                closure-convert
                cleanup-token-machine)])
  (let ([prog
         (run-compiler
           '(tokens
              (SOC-start () (gemit tok1))
              (tok1 (x) (printf "_ ") (grelay))))])
    (let ([prt (open-output-string)])
      (display "(" prt)
      (run-simulator-alpha prog 'outport prt)
      (display ")" prt)
      (read (open-input-string (get-output-string prt))))))




'
      (parameterize ([unique-name-counter 0] [simalpha-dbg-on #f])
      (parameterize ([pass-list
		   '(cleanup-token-machine  desugar-gradients
		     cleanup-token-machine desugar-let-stored
		     rename-stored          cps-tokmac
		     closure-convert        cleanup-token-machine
		     )])
	(let ([prog
	       (run-compiler
		'(tokens
		  (SOC-start () (call tok1 '1))
		  (catcher (v) (printf "~a" v))
		  (tok1 (reps) 
			(gemit tok2)
			(if (> reps 0)
			    (timed-call 500 tok1 (- reps 1))))
		  (tok2 () (greturn 34 ;(my-id) 
				    (to catcher)))
		  ))])
	  (let ((lst 
		 (let ([prt (open-output-string)])
		   (run-simulator-alpha prog)
		   (read (open-input-string (get-output-string prt))))))
	    lst
	    ))))


(define (test)
  (eval (caddr (list-ref (maintest 'get-tests) 55))))





;; Define a bunch of useful shorthands for interacting with the graphical simulator.
(IF_GRAPHICS
 (begin 
   (define-id-syntax ig (identifier-syntax (init-graphics))) ;; shorthand

   (define-id-syntax cg (identifier-syntax (close-graphics))) ;; shorthand

;   (define-syntax g  (identifier-syntax (simalpha-draw-world (fresh-simulation))))

   (define-id-syntax debug-grammar (identifier-syntax (analyze-grammar-failure failure-stack)))

   (define-id-syntax edges (identifier-syntax ;; shorthand
			    (list->set 
			     (apply append
				    (map (lambda (x) (map cadr (gobject-edgelist (simobject-gobj x))))
				      (simworld-all-objs (simalpha-current-simworld)))))))
   
   (define-id-syntax demos (begin (if (getenv "REGIMENTD")
				      (cd (++ "REGIMENTD")))
				  (cd "demos/regiment")
				  (printf "Regiment demos:\n")
				  (system "ls")))
				      
   ))
(begin ;; Some convenient syntax shortcuts:
   (define (node id) ;; shorthand
     (simobject-node (hashtab-get (simworld-obj-hash (simalpha-current-simworld)) id)))
   (define (nodeid->neighbors id)
     (map node-id (cdr (assq (node id) (simworld-graph (simalpha-current-simworld))))))
   (define (dist id1 id2) ;; shorthand
     (sim-locdiff (node-pos (simobject-node (node id1)))
		  (node-pos (simobject-node (node id2)))))
   (define-id-syntax world (simalpha-current-simworld)) ;; shorthand   
;   (define (
)

(define-id-syntax t1 (begin (close-graphics) b2))
;(define (t2) (parameterize ((simalpha-realtime-mode #f)) (eval (caddr (list-ref (maintest 'get) 60)))))
(define-id-syntax t2 (load-regiment "demos/regiment/nested_regions.rs"))
(define-id-syntax t3 (load-regiment "demos/regiment/simple/events.rs"))
(define-id-syntax t3b (load-regiment "demos/regiment/simple/union.rs"))

(define-id-syntax t4 (load-regiment "demos/regiment/static_elab.rs"))
(define-id-syntax t5 (load-regiment "demos/regiment/anchor_free_localization.rs"))
(define-id-syntax t5b (load-regiment "demos/token_machs/anchor_free_localization.tm"))
(define-id-syntax t6 (load-regiment "demos/regiment/tracking.rs"))

(define-id-syntax t7a (load-regiment "demos/firelightning/simple_lightup.tm"))
(define-id-syntax t7b (load-regiment "demos/firelightning/lightup_video.tm"))
;(define-id-syntax t7 (load-regiment "demos/firelightning/manual_nbrhood.tm"))
;(define-id-syntax t8 (load-regiment "demos/firelightning/nbrhood_alarm0.rs"))
;(define-id-syntax t8 (load-regiment "demos/firelightning/nbrhood_alarm1.rs"))

(define-id-syntax t7 (load-regiment "../analysis/firelightning/prog1.rs" 'verbose))
(define-id-syntax t8 (load-regiment "../analysis/firelightning/prog2.rs" 'verbose))
(define-id-syntax t9 (load-regiment "../analysis/firelightning/prog2_manual.tm" 'verbose))
(define-id-syntax t10 (load-regiment "../analysis/firelightning/prog2_manual_batchopt.tm" 'verbose))
(define-id-syntax t11 (load-regiment "../analysis/firelightning/test.tm" 'verbose))

(define-id-syntax tb (load-regiment "demos/regiment/bug.tm" ))

;(define-id-syntax t9 (load-regiment "demos/firelightning/deadsimple_alarm.rs"))
;(define-id-syntax t9b (load-regiment "demos/firelightning/deadsimple_alarm.tm"))

(define-id-syntax d1 (load-regiment "demos/regiment/simple_fold.rs"))
(define-id-syntax d2 (load-regiment "demos/regiment/average_temperature2.rs"))
(define-id-syntax d3 (load-regiment "demos/regiment/simple_events.rs"))
(define-id-syntax d4 (load-regiment "demos/regiment/smap2_two_anchors.rs"))
(define-id-syntax d5 (load-regiment "demos/regiment/nested_regions.rs"))
(define-id-syntax d6 (load-regiment "demos/regiment/static_elab.rs"))


(define-id-syntax tn (begin (simalpha-realtime-mode #t) (rerun-simulator-alpha 'use-stale-world)))

(define-id-syntax t  ;; uber shorthand
  (let ([start (begin (collect (collect-maximum-generation))
		      (statistics))]
	[startobs (oblist)])
    (and 
     (time (test-units))
     (collect (collect-maximum-generation))
     (let ([end (statistics)]
	  [endobs (oblist)])
      (let ([before (- (sstats-bytes start) (sstats-gc-bytes start))]
	    [after (- (sstats-bytes end) (sstats-gc-bytes end))])
	(printf "\nAfter a thorough collection:\n")
	(printf "   ~:d additional allocated bytes are left over (~:d before ~:d after).\n"
		(- after before) before after)
	(let ([len1 (length startobs)]
	      [len2 (length endobs)]
	      [diff (difference endobs startobs)])
	  (printf "   ~s syms in oblist before, ~s syms after, diff ~s\n" len1 len2 (- len2 len1))
	  (parameterize ([print-length 20])
	    (printf "Difference: ~s\n" diff))
	  (printf "  Difference with top-level bindings: ~s\n\n" (filter top-level-bound? diff))
	))))))


(define-id-syntax mem  ;; shorthand
  (begin (collect (collect-maximum-generation))
	 (let ([stats (statistics)])
	   (printf "Approx dynamic mem usage: ~:d\n" (- (sstats-bytes stats) (sstats-gc-bytes stats))))))


(define-id-syntax cur  ;; shorthand, current test
  (begin (cd "~/wavescript/src/demos/wavescope/")
	 (browse-stream (wsint "demo6_sync.ws"))))
