#!r6rs

;;;; .title Compiler Core (main.ss)

;;;; This contains the core compiler entry points. 
;;;; Loaded by both the Chez and PLT versions.
;;;; (Expects to be loaded from the directory that contains it.)

;======================================  
;(display "Loading main compiler module.  RegionStreams Demo.")
;(newline)

(library (main)
  (export
   main
   ws wsint wsint-early
   wscaml wsmlton wscomp wsc2
   )
  ;; We use the generated, aggregated package to supply all the bindings we need:
  (import (rnrs) 
	  (except (rnrs r5rs) force delay)
	  (main_r6rs);(except (main_r6rs) +)
	  )


;;================================================================================;;
;;                             System Initialization                              ;;
;;================================================================================;;


  
;; This is a bit weird, ultimately the global param
;; REGIMENTD is the thing to look at, but we have some
;; issues with the order of evaluation/loading/binding
;; here, so first we bind this at expand time:
(define-syntax default-regimentd
  (syntax-rules ()
    [(_) (if (getenv "REGIMENTD") (getenv "REGIMENTD") (current-directory))]))

;; A global parameter that is the equivalent of the eponymous
;; environment var.  Now that constants.ss is loaded we can set this. 
;; This uses the kinder behavior -- try the current directory.
;; (However, that's a bit irrelevent if an error was already signaled above.)
(define ___ (REGIMENTD (default-regimentd)))

;;================================================================================;;


;; STUB
(define par list)

(define-regiment-parameter regiment-version "0.9?")

;; This is the global parameter that determines which transformations
;; (passes) the compiler applies and in what order.  We set it here.
(IFWAVESCOPE (begin)
(pass-list
  (list
    ;; (1) Type checking comes first, but that happens before these passes
    ;; are run.  Maybe should make it one of the "passes".
    
    ;; (2) Next we verify our input language.
    verify-regiment
    
    ;; (3) Then we do a little massaging/normalization.
    pass_desugar-pattern-matching
    retypecheck ;; This is actually the first typecheck.

    eta-primitives
    desugar-misc
    rename-vars
    remove-unquoted-constant
    retypecheck

    ;; (4) Then -- here comes the metaprogramming -- we evaluate as much
    ;; of the program as can be evaluated.  The residual had better follow our
    ;; restrictions on implementable Regiment programs.
    static-elaborate    

    remove-complex-constant  ;; [2008.03.28] Moving this, sad to touch it at all since this backend is dead.

    degeneralize-arithmetic
    rename-vars ;; We run again after elaborator.
    retypecheck

;    verify-elaborated

    ;; (5) Now we normalize the residual in a number of ways to
    ;; produce the core query language, then we verify that core.
    reduce-primitives    
;    remove-complex-constant  
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
    ;; Alternatively:
    ;; flatten-comm deglobalize2 streams-to-tm

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

    )))

; ==================================================================
;; Functions for input/output to filesystem and for invoking compiler.



(IFWAVESCOPE
  (begin )
  (begin

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
;; The symbolic options are:  'barely-tokens 'almost-tokens 'full-tokens
;; Also: use 'verbose to print the output of every pass.
(define (run-compiler p . args )                              ;; Entrypoint.
  (parameterize ([compiler-invocation-mode 'regiment-simulator])
    (if (memq 'deglobalize2 args)
        (apply run-compiler2 p (remq 'deglobalize2 args))

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
                  [(eq? arg 'almost-tokens)
                   (set! passes (list-remove-first deglobalize ;; <- might fizzle
				  (list-remove-first cleanup-token-machine
                                     (list-remove-after cleanup-token-machine (pass-list)))))]
                  [(eq? arg 'barely-tokens)
                   (set! passes (list-remove-first cleanup-token-machine
				  (list-remove-after cleanup-token-machine (pass-list))))]
		  ;; Out dated... don't haskellize any more:
#;
                  [(eq? arg 'full-tokens)
                   (set! passes (remq haskellize-tokmac (pass-list)))
		   ]
                  ;[(eq? arg 'haskell-tokens) (void)]
		  ;; Otherwise... do nothing.
		  [else (warning 'run-compiler "ignored flag: ~s" arg)]
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
	      (let ((result (if verbose 
				(time ((car funs) p))
				((car funs) p))))
		(when verbose
		  (pretty-print result) (newline))
		(loop result (cdr funs) (cdr names))))))))
      )))


;; This is a front-end to run-compiler that reads source from a file.
;; Either returns compiled program or writes it to a file.
;; Flags:
;;  'write-file : write output to file.
;; 
;;  'to-typed
;;  'almost-tokens
;;  'barely-tokens
;;  'full-tokens 
;;  'to-simcode
(define regiment-compile-file
  (let ()
    (define compile-target-flags '(to-typed almost-tokens barely-tokens full-tokens to-simcode))
    (define (do-the-compilation fn flags)
      (let ([type (extract-file-extension fn)])
	;; Do a little sanity checking between our options and our input file:
	(cond
	 ;; [2007.05.16] symargs is unbound... clean this junk up at some point.
#|
	 [(equal? type "tm") 
	  (if (or (memq '-l0 symargs) (memq '-l1 symargs))
	      (error 'regiment_command_line_compiler
		     "Cannot use -l0/-l1 with an input that's already a TM!: ~s" fn))]
|#
	 [(equal? type "rs") (void)]
	 [(equal? type "ws") (void)]
	 [else (error 'regiment_command_line_compiler "invalid input file extension: ~s" fn)])
	;; ----------------------------------------
	(parameterize ([pass-list
			(cond
			 [(equal? type "rs") (pass-list)]
			 ;; We treat these as normal regiment files:
			 [(equal? type "ws") (pass-list)]
			 [(equal? type "tm") (list-remove-before cleanup-token-machine
								 (pass-list))]
			 [else (error 'regiment "unknown input file extension: ~s" type)]
			 )])

	  (mvlet ([(prog params)
		   (parameterize ([current-directory (if (top-level-bound? 'start-dir)
							 (top-level-value 'start-dir)
							 (current-directory))])
		     (read-regiment-source-file fn))])
	    (let ((comped 
		   (if (memq 'to-simcode flags)
		       ;; This goes all the way to a sim-file and bakes the parameters right in:
		       (let ([comped1 (apply run-compiler prog flags)])
			 (apply compile-simulate-alpha comped1 params))
		       (apply run-compiler prog flags))))
	      (values comped params)
	      )))))
    ;----------------------------------------
    (lambda (fn . flags)      
      (ASSERT (andmap symbol? flags))
      (unless (= 1 (length (intersection flags compile-target-flags)))
	(error 'regiment-compile-file "expects exactly one flag to indicate compilation target"))
      (mvlet ([(comped params) (do-the-compilation fn flags)])
	(let ([compile-target (car (intersection flags compile-target-flags))]	    )
	  (if (not (memq 'write-file flags))
	      comped
	      (let* ([extension (case compile-target 
				  [(to-typed) ".sexp0"]
				  [(almost-tokens) ".sexp"]
				  [(barely-tokens) ".tm0"]
				  [(full-tokens) ".tm"]
				  [(to-simcode) ".sim.alpha"]
				  )]
		     [out_file (string-append (remove-file-extension fn) extension)])
		;; Don't overwrite our own input file!
		(if (equal? fn out_file) (set! out_file (string-append "out." extension)))
		(printf "\n  Writing compilation-result to: ~s \n" out_file)
		(delete-file out_file)		
		(parameterize ([print-graph #t] 
			       [print-level #f] 
			       [print-length #f]
			       [pretty-maximum-lines #f])
		  (with-output-to-file out_file
		    (lambda ()
		      ;; Otherwise we need to propogate the params to the output file:
		      (if (memq 'to-simcode flags)
			  (pretty-print `(parameters ,@params)))
		      (pretty-print comped))))
		;'compiled-result-written-to-file
		;; Return the name of the output file:
		out_file
		)))
	))))

;; This is the second version of the compiler, uses deglobalize2
(define (run-compiler2 p . args)                              ;; Entrypoint.
  (parameterize ([pass-list (snoc deglobalize2 
				  (rdc (list-remove-after deglobalize (pass-list))))])
    (apply run-compiler p args)
    ))

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


(define (reg:printlog file)
  (let ((stream (reg:read-log file 'stream)))
    (let loop ((s (reg:read-log file 'stream)))
      (unless (null? s)
	(display (log-line->human-readable 0 (stream-car s) ()))
	(loop (stream-cdr s))))))

    ))



;; ====================================================================================================
;; WaveScope Compiler:

(define ws-pass-optional-stop 
  (lambda (x)
    (if (>= (regiment-verbosity) 2)
	(IFDEBUG
	 (begin (parameterize ([pretty-line-length 160]
			       [print-length 300]
			       [print-level 60])
		    (newline)
		    ;(pretty-print (strip-annotations x))
		    (pretty-print x)
		    )
		  (printf "================================================================================\n\n")
		  x)
	   x)
	x)))
(define-syntax ws-run-pass
  (syntax-rules ()
    [(_ v pass args ...)
     (parameterize ([regiment-current-pass 'pass])
       (when (>= (regiment-verbosity) 2) ;unless (<= (regiment-verbosity) 0)
	 (printf "Running Pass: ~s\n" 'pass)(flush-output-port (current-output-port)))
       (if (>= (regiment-verbosity) 3)
	   (time (set! v (ws-pass-optional-stop (pass v args ...))))
	   (set! v (ws-pass-optional-stop (pass v args ...))))
       ;; Allows multiple hooks:
       (let ([hooks (map cadr 
		      (filter (lambda (pr) (eq? (car pr) 'pass))
			(ws-compiler-hooks)))])
	 (for-each (lambda (hk) (hk v))
	   hooks)))]))

;; EXPERIMENTAL:
;; Playing around with fusing passes.
;; This is *just* for optimization.
(define-syntax ws-run-fused/disjoint
  (syntax-rules ()
    [(_ v pass* ...)
     ;; [2007.06.13] For now having problems with this, so just disabling it.
     (begin (ws-run-pass v pass*) ...)
     
#;
     ;; DEBUGGING.
     ;; Asserting that the result of the fused passes is *exactly* the
     ;; same as the passes individually.
     (let ([orig v])
       (printf "  EXPERIMENTAL: Running passes fused.")
       (time (begin (ws-run-pass v pass*) ...))
       ;; Now the fused version:
       (printf "Now running fused version: ~s\n" '(pass* ...))
       (let ([fusedver (time ((fuse-passes/disjoint pass* ...) orig))])
	 ;(ASSERT (equal? v fusedver))
	 (unless (equal? (reunique-names v) (reunique-names fusedver))
	   (diff (reunique-names v) (reunique-names fusedver))
	   ;(error 'ws-run-fused/disjoint "didn't match"))
	   (inspect (reunique-names fusedver))
	 )))]))


;; This just encapsulates the first few steps of the compiler.
;; Everything up to and including type checking the program.
(define (ws-compile-until-typed p)

  (ws-run-pass p verify-regiment)

  ;; TEMPTOGGLE:
  ;(ws-run-pass p eta-primitives)
  (ws-run-pass p pass_desugar-pattern-matching)
  (ws-run-pass p resolve-varrefs)

  ;; TODO: Insert optional PRUNE-UNUSED pass to quickly prune unused code.

  (ws-run-pass p resolve-type-aliases)
  (ws-run-pass p ws-label-mutable)

  ;; This is the initial typecheck. 
  (parameterize ([inferencer-enable-LUB     #f]
		 [inferencer-let-bound-poly #t])
    (ws-run-pass p retypecheck))
  p)

;; This will store a continuation that hops into the compiler write before generate-marshal-code.
(define before-marshal-hook     (make-parameter 'bmh-uninit))
;; We also store the program at the point before the marshal-hook:
(define before-marshal-snapshot (make-parameter 'bms-uninit))

;; ================================================================================ ;;
;; [2006.08.27] This version executes the WaveScript version of the compiler.
;; It takes it from (parsed) source down as far as WaveScript 
;; can go right now.  But it does not invoke the simulator or the c_generator.
;; ================================================================================ ;;
;; .param p               -- a program to compiler
;; .param params          -- flags to the ws compiler
;; .param disabled-passes -- passes to omit (mic)
;; .param already-typed   -- skip initial typecheck
;; .param kont            -- continuation to invoke at end of compilation
(define run-ws-compiler             ;; Entrypoint.
  ; FIXME: this case-lambda is probably a temporary construction
  (case-lambda
    [(p params)                               (run-ws-compiler p params '())]
    [(p params disabled-passes)               (run-ws-compiler p params disabled-passes #f)]
    [(p params disabled-passes already-typed) (run-ws-compiler p params disabled-passes already-typed (lambda (x) x))]
    [(p params disabled-passes already-typed kont)

  (define (do-typecheck lub poly)
    (parameterize ([inferencer-enable-LUB      lub]
		   [inferencer-let-bound-poly poly])
      (time-accum (ws-run-pass p retypecheck))))
  ;; There are currently two different typecheck configs that we use.
  ;; One for the meta language, one for the object.
  ;; The meta language has let-polymorphism, and doesn't bindings to LUB types.
  (define (do-early-typecheck) (do-typecheck #f #t))
  ;; The object language is the reverse:
  (define (do-late-typecheck)  (do-typecheck #t #f))

  (define (run-that-compiler) ;; Main compilation routine.
    (parameterize ()
      (ws-pass-optional-stop p)
      (unless already-typed (set! p (ws-compile-until-typed p)))

      ;; FIXME FIXME FIXME [2007.09.07] It seems that a repeated typecheck
      ;; here breaks something wrt "union2" and sum types... 
      ;; Perhaps the LUB typing isn't being implemented correctly.
      
      ;;(DEBUGMODE (do-early-typecheck) (void))
      ;;(do-early-typecheck)

  ;; [2007.07.06] Moving this back where it belongs... after typechecking
  ;; The only reason it was moved earlier was to accomodate using a hash table for type environments
  ;; ... which didn't work well anyway.

  (ws-run-pass p rename-vars)

;  (DEBUGMODE (do-early-typecheck) (void))
  (ws-run-pass p eta-primitives)
  (ws-run-pass p desugar-misc)
  (ws-run-pass p remove-unquoted-constant)
 
  ;; Run this twice!!!
  ;;;;(ws-run-pass p degeneralize-arithmetic)

;; FIXME Broken here too:
;  (DEBUGMODE (do-early-typecheck) (void))
  ;; This doesn't work either:
;  (do-typecheck #t #t)


;; [2008.05.02] FIXME FIXME FIXME FIXME FIXME FIXME FIXME DISABLING THIS for R6RS:
#;
  ;; <OPTIMIZATION>: REWRITE RULES
  ;; -----------------------------------------
  (when (memq 'rewrites (ws-optimizations-enabled))
    (ws-run-pass p hide-special-libfuns)
    (dump-compiler-intermediate (strip-annotations p) ".__hidden.ss")
    (parameterize ([regiment-primitives
		    (append (map (match-lambda ([,lhs ,ty ,rhs]) 
				   (match ty
				     [(,args ... -> ,res) `(,lhs ,args ,res)]))
			      (cdr (ASSERT (project-metadata 'special-libfuns p))))
			    (regiment-primitives))])
      (ws-run-pass p eta-primitives)
      (ws-run-pass p interpret-meta) (do-early-typecheck)

      (ws-run-pass p rename-vars)
      (dump-compiler-intermediate (strip-annotations p) ".__presmooshed.ss")
      (ws-run-pass p smoosh-together)      
      (dump-compiler-intermediate (strip-annotations p) ".__smooshed.ss")

      ;(inspect (match p [(,lang '(program ,bod . ,_)) bod]))
      (ws-run-pass p rewrite-rules)
      (dump-compiler-intermediate (strip-annotations p) ".__rewritten.ss")
      ;(inspect (match p [(,lang '(program ,bod . ,_)) bod]))
      (ws-run-pass p reveal-special-libfuns)))
  ;; -----------------------------------------

;  (ws-run-pass p interpret-meta) (do-early-typecheck)  ;; Testing idempotentcy 
;  (DEBUGMODE (dump-compiler-intermediate p ".__elaborated_first.ss"))


  ;; <OPTIMIZATION>: FUSE BOXES
  ;; -----------------------------------------
  ;; RRN: Will enable merge-iterates as soon as the backend can handle (app _) constructs...
  ;(unless (memq 'merge-iterates disabled-passes)
  ;  ;(pretty-print p)
  ;  (ws-run-pass p merge-iterates)
  ;  ;(pretty-print p)
  ;  ) ;; <Optimization>
  #;
  (when (or (memq 'fuse (cdr (or (assoc 'optimizations params) '(_ . ()))))
            (memq 'fuse (ws-optimizations-enabled))
            (memq 'merge-iterates (ws-optimizations-enabled)))
    (ws-run-pass p simple-merge-policy:always)
    (ws-run-pass p simple-merge-iterates)
    ;; Inline those function defs again.
    ;; This really is terrible code bloat... need to simply support
    ;; first order functions in the backends.  Might not be time for
    ;; that right this second though.
    ;(ws-run-pass p interpret-meta) (do-early-typecheck)
    )
  ;; -----------------------------------------

  ;; <METAPROGRAM-EVAL>: 
  ;; -----------------------------------------  
  (when (>= (regiment-verbosity) 1)
    (printf "Output of metaprogram evaluation:\n")
    (printf "------------------------------------------------------------\n"))
  (when (>= (regiment-verbosity) 2) (printf "  PROGSIZE: ~s\n" (count-nodes p)))
  (when (or (>= (regiment-verbosity) 5) (IFDEBUG #t #f))
    (dump-compiler-intermediate (strip-annotations p 'src-pos) ".__preelab.ss"))

  (if (>= (regiment-verbosity) 2) (time (ws-run-pass p interpret-meta)) (ws-run-pass p interpret-meta))
;  (time (ws-run-pass p static-elaborate))
  (when (>= (regiment-verbosity) 2) (printf "  PROGSIZE: ~s\n" (count-nodes p)))

  ;(pretty-print (strip-annotations p 'src-pos))

  (when (>= (regiment-verbosity) 1)
    (printf "------------------------------------------------------------\n")
    (printf "Metaprogram evaluation succeeded.\n"))
  (when (or (>= (regiment-verbosity) 3) (IFDEBUG #t #f))
    (dump-compiler-intermediate (strip-annotations p 'src-pos) ".__elaborated.ss"))

  ;------------------------------------------------------------------
  ;<<<<<<<<<<<<<<<<<<<< POST ELABORATION CLEANUP >>>>>>>>>>>>>>>>>>>>

  ;; We want to immediately get our uniqueness property back.
  (ws-run-pass p rename-vars)

  ;; NOTE: wavescript-language won't work until we've removed complex constants.
  ;; Quoted arrays work differently in WS than in Scheme.
  ;; (WS has a freshness guarantee.)
  (ws-run-pass p remove-complex-constant)

;(ASSERT (null? (deep-assq-all 'BOTTOM p))) ;; These should all be gone.

  ;; Now fill in some types that were left blank in the above:
  ;; Shouldn't need to redo LUB because the types are already restrictive???
  (IFDEBUG (do-late-typecheck) (void))

  ;; This is expensive because it lifts generic ops, and retypechecks:
  ;; (Like we later do for polymorphic constants)
  (ws-run-pass p degeneralize-arithmetic)
  ;; NOTE: SHOULD BE SAFE TO TURN OFF LET-BOUND-POLYMORPHISM HERE:
  (DEBUGMODE (do-late-typecheck)) ;; [2008.02.21] changing to debug-only

  ;; We strip out ascriptions so we don't have any polymophic hanging around in there:
  (ws-run-pass p strip-unnecessary-ascription)

  (IFDEBUG (do-late-typecheck) (void))

  ;; [2007.10.27] For MLton we might should remove at least part of this pass:
  (ws-run-pass p anihilate-higher-order)  ;; Of a kind with "reduce-primitives"

  ;; Pull constants up to the top.
  ;(ws-run-pass p lift-complex-constant)


  ;; This lift/typecheck/unlift process is inefficient, but easy:
  ;; A hack, but a pretty cool hack.
  (ws-run-pass p lift-polymorphic-constant)
  (do-late-typecheck)
  
  ;; This just fills polymorphic types with unit.  These should be
  ;; things that don't matter.  
  ;; [2007.10.11] Right now this messes up demo3f:
  (ws-run-pass p strip-irrelevant-polymorphism)
  (ws-run-pass p unlift-polymorphic-constant)   

  (ws-run-pass p split-union-types) ;; monomorphize sum types (not necessary for MLton)

  (ws-run-pass p verify-elaborated) ;; Also strips src-pos info.  

  (IFDEBUG 
   (unless (<= (regiment-verbosity) 0)
     (printf "Post elaboration types: \n")
     (print-var-types p +inf.0))
   (void))

  ;-------------------------------------------------------------
  ;<<<<<<<<<<<<<<<<<<<< MONOMORPHIC PROGRAM >>>>>>>>>>>>>>>>>>>>

  ;(inspect (strip-annotations p 'src-pos))
  
  (do-late-typecheck)  ;; Anihilate introduced type variables.

  (when (>= (regiment-verbosity) 1) (printf "Monomorphism achieved.\n"))
  (when (or (>= (regiment-verbosity) 3) (IFDEBUG #t #f))
    (dump-compiler-intermediate (strip-annotations p 'src-pos) ".__monomorphic.ss"))

  ;; <-------- NOTE: Old location for merge-iterates. [2007.11.01]

  ;(IFDEBUG (do-late-typecheck) (void))

  ;; (5) Now we normalize the residual in a number of ways to
  ;; produce the core query language, then we verify that core.

  (ws-run-pass p reduce-primitives)
 
  (IFDEBUG (do-late-typecheck) (void))
  ;(profile-clear)
  (ws-run-pass p type-annotate-misc) ;; This pass is really slow...
  ;(parameterize ([current-directory "html"]) (profile-dump-html))
  ;(printf "<<<<<<<< PROFILE DUMPED >>>>>>>>\n")
  ;;(assure-type-annotated p (lambda (x) (equal? x ''())))

  (when (wsc2-variant-mode? (compiler-invocation-mode))
    (unless (suppress-main-stream-printing)	
      (ws-run-pass p explicit-toplevel-print)))

  (ws-run-pass p optimize-print-and-show) ;; Should be optional.
  (ws-run-pass p generate-printing-code)

  (ws-run-pass p lift-immutable-constants)

  ;; To reduce the complexity of the wsc2 backend, we get rid of strings:
  (when (and (wsc2-variant-mode? (compiler-invocation-mode))
	     ;; But java needs them:
	     (not (java-mode? (compiler-invocation-mode))))
    (ws-run-pass p embed-strings-as-arrays)
    (DEBUGMODE 
     (let ([tmp (deep-assq-all 'String p)])
       (unless (null? tmp) 
	 (warning 'embed-strings-as-arrays "The symbol String occured in the output.  Here's a snippet:")
	 (inspect tmp))))
    ;(ws-run-pass p remove-complex-constant) ;; Should we leave those array constants?
    )
  
  ;; HACKISH, TEMP: Grab the continuation at this point:
  (set! p (call/cc (lambda (k) (before-marshal-hook k) 
			       (before-marshal-snapshot p) p)))

  ;; We do this after string embedding so we don't worry about marshaling strings.
  (ws-run-pass p generate-marshal-code)

  ;; wsc2 and derivatives support monomorphic backends that need a little help here:
  (when (and (wsc2-variant-mode? (compiler-invocation-mode))
	     (not (java-mode? (compiler-invocation-mode))))
    (ws-run-pass p type-annotate-misc)
    (ws-run-pass p generate-comparison-code))

  ;(DEBUGMODE (do-late-typecheck)) ;; [2008.08.05] check all that generated printing/marshaling/comparison code.

;  (ws-run-pass p uncover-free)

  ;(ws-run-pass p purify-letrec)
  ;; This is what we need to do.

  ;; Now that we're done with elaboration we should take the stream
  ;; processing spine, convert it to let.
  ;; For the time-being we don't even need letrec in the object code
  ;; because functions have all been inlined.
  (ws-run-pass p remove-letrec) ;; This is a bit redundant with interpret-meta, which already sorts the bindings.
  (IFDEBUG (do-late-typecheck) (void)) ;; Do a typecheck to make sure it works without letrec.

  (ws-run-pass p standardize-iterate) ;; no fuse

  ;; <OPTIMIZATION>: FUSE BOXES
  ;; -----------------------------------------
  ;; mic: for now, simple-merge-iterates does manual inlining
  ;; (interpret-meta doesn't work this far down in the compiler, yet)
  ;(ws-optimizations-enabled (cons 'fuse (ws-optimizations-enabled)))
  (when (or (memq 'fuse (cdr (or (assoc 'optimizations params) '(_ . ()))))
            (memq 'fuse (ws-optimizations-enabled))
            (memq 'merge-iterates (ws-optimizations-enabled)))
    (ws-run-pass p smoosh-together)
    (ws-run-pass p simple-merge-policy:always)
    (ws-run-pass p simple-merge-iterates)
    (ws-run-pass p rename-vars)
    (ws-run-pass p standardize-iterate)
    )


;  (ws-run-pass p introduce-lazy-letrec)
;  (ws-run-pass p lift-letrec)
;  (ws-run-pass p lift-letrec-body)
;  (profile-clear)

  (ws-run-pass p ws-remove-complex-opera*)

  ;; Don't do this yet!!  (At least make it debug only.)
  ;; Remove-complex-opera* added new type variables, but delay a
  ;; couple more passses before type checking.
  (IFDEBUG (do-late-typecheck) (void))
;  (with-output-to-file "./pdump_new"  (lambda () (fasl-write (profile-dump)))  'replace)

  ;; This is an example of the experimental pass-fusion mechanism.
  ;(ws-run-fused/disjoint p ws-normalize-context ws-lift-let)
  (ws-run-pass p ws-normalize-context)

  (ws-run-pass p ws-lift-let)
  
  ; --mic, <OPTIMIZATION>
  (unless (memq 'propagate-copies disabled-passes)
    (ws-run-pass p propagate-copies))

  ;(pp (blaze-path-to/assq p 'Array:make 'Array:makeUNSAFE))

  ;; Mandatory re-typecheck.  Needed to clear out some polymorphic
  ;; types that might have snuck in from lifting.
  ;(do-typecheck #f #f)
  (do-late-typecheck)

  ;; Replacing remove-complex-opera* with a simpler pass:
  ;(ws-run-pass p flatten-iterate-spine) ;; Not anymore....
  (DEBUGMODE (dump-compiler-intermediate p ".__nocomplexopera.ss"))

  (ws-run-pass p type-annotate-misc)
;(assure-type-annotated p (lambda (x) (equal? x ''()))) ;; TEMP HACK
  (ws-run-pass p reify-certain-types)

  ;; for analysis of data rates between boxes
  ;; uncomment to enable
  ;(ws-optimizations-enabled (cons 'profile ws-optimizations-enabled))
  (when (memq 'profile (ws-optimizations-enabled))
     (unless  (memq 'annotate-with-data-rates disabled-passes)
       
       ;(ws-profile-limit '(elements 4))

       (printf "============================================================\n")
       (printf "       PROFILING IN SCHEME \n")
       (printf "============================================================\n")
       (ws-run-pass p annotate-with-data-rates)
       
       ;; TEMPTOGGLE TEMP HACK FIXME:
       (define-top-level-value 'scheme-profiling-performed! #t)
       #;
       (let ([get-vtime (wavescript-language 'get-current-vtime)])
	 (printf "After profiling, virtual time was ~s\n" (get-vtime)))
       
       ))
  
  ;; Now remove IFPROFILE constructs:
  (ws-run-pass p remove-IFPROFILE)

  ;; ========================================
  ;; End passes


;(assure-type-annotated p (lambda (x) (and (pair? x) (eq? 'cons (car x)))))

  (when (>= (regiment-verbosity) 2)
    (printf "Total typechecker time used:\n")
    (time-accum-report)(newline))
;  (with-output-to-file "./pdump_new"  (lambda () (fasl-write (profile-dump)))  'replace)
;  (exit)

  ;; Here we dump it to a .dot file for graphviz.
  ;; Wasted work if we're going to apply explicit-stream-wiring again later.
  (when (dump-graphviz-output)
    (string->file (output-graphviz (explicit-stream-wiring p)) "query.dot")
    ;; If this fails, oh well:
    (system "rm -f query.png")
    (time (system "dot -Tpng query.dot -oquery.png")))
 
  p)) ;; End run-that-compiler
 
 (kont (run-that-compiler))
  ;(if (<= (regiment-verbosity) 0) (run-that-compiler) (time (run-that-compiler)))
]))
;; ================================================================================ ;;
;; ================================================================================ ;;


  
  ;; [2008.01.10] This is a need that pops up frequently.
  ;; When there's a stray datum that should be type annotated, this helps find it.
  (define (assure-type-annotated prog pred)
    (define asserts (deep-assq-all 'assert-type prog))
    (define hits    (deep-all-matches pred prog))
    (for-each (lambda (hit)
		(unless (ormap (match-lambda ((assert-type ,ty ,ob)) (eq? ob hit))
			       asserts)
		  (warning 'assure-type-annotated "object was not type annotated: ~s" hit)))
      hits))

;; ================================================================================
;; For usability, the below ws* procedures (wsint wscomp wscaml wsmlton...) can 
;; each take their input as a filename, a parsed program, or a port.
;; This is the coercion function they use to coerce their input to a parseed program.
(define (coerce-to-ws-prog x input-params)
  (let ((prog
         (cond  [(input-port? x)
                 (when (>= (regiment-verbosity) 2) (printf "WSCOMP: Loading WS source from port: ~s\n" x))
                 ;; We assume this is parsed but not post-processed:
                 (wsparse-postprocess (read x))]
                [(string? x) 
                 (when (>= (regiment-verbosity) 2) (printf "WSCOMP: Loading WS source from file: ~s\n" x))
                 (read-wavescript-source-file x)]
                [(list? x)   
                 (when (>= (regiment-verbosity) 2) (printf "WSCOMP: Evaluating WS source: \n \n"))
                 x]
                [else (error 'wsint "bad input: ~s" x)])))
    
    ;; add in the input-parameters
    (set! prog
          (match prog
            [(,lang '(program ,s* ... ,types))
             `(,lang '(program ,@s* (input-parameters ,input-params) ,types))]
            
            ;; do nothing in this case
            [,oth prog]))

    prog))


;; Dump output of intermediate stages in copmiler.
;; (This sets a variety of parameters controlling printing)
(define (dump-compiler-intermediate prog fn)
  (when (file-exists? fn) (delete-file fn))
  (with-output-to-file fn
    (lambda () 
      (parameterize ([pretty-line-length 200]
		     [pretty-maximum-lines #f]
		     [print-level #f]
		     [print-length #f]
		     [print-graph #f])
	(begin ;parameterize-IFCHEZ ([pretty-one-line-limit 120])
	 (if (>= (regiment-verbosity) 4)
	     ;; It's very expensive to actually pretty-print very large sexps:
	     (pretty-print ;(strip-annotations prog)
	      prog)
	     (write prog))))
      (flush-output-port (current-output-port)))))

(define ws-disabled-by-default '(merge-iterates ))

(define ws-default-wavescope-scheduler 'default-scheduler)


;; ================================================================================
;; The WaveScript "interpreter".  (Really a wavescript embedding.)
;; It loads, compiles, and evaluates a wavescript query.
;; .param x - can be an input port, a filename, or a wavescript AST (list)
;; .returns - A stream of results
(define-values (wsint wsint-early)
  (let ()

    (define (cleanup-compiler-tmpfiles)
      (let ([please-delete-file 
	     (lambda (f) (if (file-exists? f) (delete-file f)))])
	;; Erk, should use a better system, there's no way this will be kept up to date:
	;; Delete these files so that we don't get mixed up.		  
	(please-delete-file ".__types.txt")
	(please-delete-file ".__inputprog.ss")
	(please-delete-file ".__compiledprog.ss")
	(please-delete-file ".__elaborated.ss")
	(please-delete-file ".__nocomplexopera.ss")))

    (define (wsint-parameterize th)
      (parameterize ([compiler-invocation-mode 'wavescript-simulator]
		     ;;[regiment-compile-sums-as-tuples ]
		     ;;		 [included-var-bindings '()]
		     [regiment-primitives
		      ;; Remove those regiment-only primitives.
		      (difference (regiment-primitives) regiment-distributed-primitives)])
	(th)))

    (define (early-part x input-params . flags)
      (define prog (coerce-to-ws-prog x input-params))
      (define _ (begin (DEBUGMODE 
			(cleanup-compiler-tmpfiles)
			(dump-compiler-intermediate prog ".__inputprog.ss")
			)))
      (define typed (ws-compile-until-typed prog))
      (define __ 
	(begin 
	  (when (>= (regiment-verbosity) 1)
	    ;(printf "Program verified, type-checked. (Also dumped to \".__parsed.ss\".)")
	    ;(printf "\nProgram types as follows: (also dumped to \".__types.txt\")\n\n")
	    (printf "Program type-checked: \n")
	    (if (>= (regiment-verbosity) 4)
		(print-var-types typed +inf.0)
		(print-var-types typed 1))
	    (flush-output-port (current-output-port)))
	  (when (>= (regiment-verbosity) 2)
	   (if (file-exists? ".__types.txt") (delete-file ".__types.txt"))
	   (with-output-to-file ".__types.txt"
	     (lambda () (print-var-types typed +inf.0)(flush-output-port (current-output-port)))))))
      typed)

    (define (make-uniontype-defs x)
      (match x
	[#f '(void)]
	[(union-types ((,name* . ,_) [,fld** ,ty**] ...) ...)
	 (cons 'begin
	   (map (lambda (fld ty) 
		  `(define ,fld (lambda (val) (make-uniontype ',fld val))))
	     (apply append fld**)
	     (apply append ty**)))]))

  (define (wsint x input-params . flags)                                             ;; Entrypoint.      
    (wsint-parameterize
     (lambda ()    
       (define typed (early-part x input-params))
       (define disabled-passes (append (map cadr (find-in-flags 'disable 1 flags)) ws-disabled-by-default))
       (define compiled (time (run-ws-compiler typed input-params disabled-passes #t)))
       
       (when (>= (regiment-verbosity) 1) (printf "WaveScript compilation completed.\n"))
       (DEBUGMODE (dump-compiler-intermediate compiled ".__compiledprog.ss"))
       ;;(inspect compiled)
       (run-wavescript-sim compiled))))
  
  (define (wsint-early x input-params . flags)
    (wsint-parameterize
     (lambda ()
       (define p x)
       (time (begin 
	       (set! p (early-part p input-params))
	       (ws-run-pass p eta-primitives)
	       ;; (ws-run-pass p desugar-misc)

	       ;; Need to convert readFile to __readFile
	       ;(ws-run-pass p type-annotate-misc)
	       (ws-run-pass p strip-src-pos)
	       (ws-run-pass p reify-certain-types)
	       (ws-run-pass p strip-annotations)
	       ))
       (printf "Running program EARLY:\n")
       (run-wavescript-sim p))))

  (values wsint wsint-early)))

;; When invoking "ws" from the command line, this is the function that
;; determines where the stream goes.  I.e. to an interactive browser
;; or to a file.
(define (wsint:direct-stream strm)
  ;(IFCHEZ (import streams) (begin))
  
  (define (run-to-tuplimit) (first-value (stream-take (wsint-tuple-limit) strm)))
  (define (run) 
    (map ws-show 
      (if (wsint-time-query) (time (run-to-tuplimit)) (run-to-tuplimit))))

  (cond
   [(not (stream? strm))
    (eprintf  "\nWS query returned a non-stream value:\n  ~s\n" strm)
    
    (let ([x (delay 3)])
      (printf "Very odd ~a ~a ~a ~a\n" strm (procedure? strm) (promise? strm) (stream? strm))
      (printf "Grr ~a ~a ~a\n" x (record? x) (promise? x)))
    ]
   [(and (wsint-tuple-limit) (wsint-output-file))
    ;; This could be more efficient, but for now we just take it all
    ;; into memory and then dump it all to disk.
    (when (>= (regiment-verbosity) 1)
      (printf "Executing stream program with output to file: ~s\n" (wsint-output-file))
      (printf "------------------------------------------------------------\n"))
    (slist->file  (run)  (wsint-output-file) 'display)]
   [(wsint-tuple-limit)
    (when (>= (regiment-verbosity) 1)
      (printf "Executing stream program:\n")
      (printf "------------------------------------------------------------\n"))
    ;; TODO, use proper WS printing:
    ;(for-each pretty-print (run))
    (for-each (lambda (x) (display x) (newline)) (run))
    ]

   [(wsint-output-file)
    (when (>= (regiment-verbosity) 1)
      (eprintf "Dumping output to file: ~s\n" (wsint-output-file)))    
    (stream-dump strm (wsint-output-file))]
   [else
    ;; Otherwise, browse it interactively:
    (when (>= (regiment-verbosity) 1)
      (printf "Interactively executing stream program:\n")
      (printf "------------------------------------------------------------\n"))
    (parameterize ([print-vector-length #t])
      (if (>= (regiment-verbosity) 2)
	  (browse-stream strm)
	  (browse-stream (stream-map ws-show strm) (lambda (x) (display x) (newline)))))]))

;; ================================================================================
;; WaveScript Compiler Entrypoint:

(define (wscomp x input-params . flags)                                 ;; Entrypoint.  
  (define new-version? (or (not (null? (find-in-flags 'wsc2 0 flags)))
			   (not (null? (find-in-flags 'wstiny 0 flags)))
			   (not (null? (find-in-flags 'wsjavame 0 flags)))))
  (unless new-version? (compiler-invocation-mode 'wavescript-compiler-xstream))
  (parameterize ([regiment-primitives
		 ;; Remove those regiment-only primitives.
		 (difference (regiment-primitives) regiment-distributed-primitives)])
   (define prog (coerce-to-ws-prog x input-params))
   (define typed (ws-compile-until-typed prog))
   (define disabled-passes (append (map cadr (find-in-flags 'disable 1 flags)) ws-disabled-by-default))
   (define wavescope-scheduler (car (append (map cadr (find-in-flags 'scheduler 1 flags))
                                            `(,ws-default-wavescope-scheduler))))
   ;; [2007.11.23] Are we running the new C backend?
   
   (define outfile (if new-version? #f "./query.cpp")) ;; The new version isn't controlled by "outfile"

   (define (run-wscomp)
     
     ;;(ASSERT (andmap symbol? flags)) ;; [2007.11.06] Not true after Michael added (scheduler _) flags.
     
     (unless (<= (regiment-verbosity) 0)
       (printf "\nTypecheck complete, program types:\n\n")
       (if (>= (regiment-verbosity) 2) 
	   (print-var-types typed +inf.0)
	   (print-var-types typed 1))
       (flush-output-port (current-output-port)))
     
     ;; Run the main body of the compiler.  Call it in continuation passing style.
     (set! prog (call/cc (lambda (k) 
			   (run-ws-compiler typed input-params disabled-passes #t k))))
         
     (ws-run-pass prog convert-sums-to-tuples)
     
     (if new-version?
	  (begin 
	    (ws-run-pass prog nominalize-types)
	    (ws-run-pass prog gather-heap-types)

	    (when (or (>= (regiment-verbosity) 3) (IFDEBUG #t #f))
	      (dump-compiler-intermediate prog ".__beforeexplicitwiring.ss"))
	    (ws-run-pass prog explicit-stream-wiring)
	    (when (or (>= (regiment-verbosity) 3) (IFDEBUG #t #f))
	      (dump-compiler-intermediate prog ".__afterexplicitwiring.ss"))
	    ;(ws-run-pass prog remove-unused-streams)
	    
	    ;; Encapsulate the last-few-steps to use on different graph partitions.
	    (let ([last-few-steps
		   (lambda (prog class)
		     ;;(ws-run-pass heuristic-parallel-schedule)
		     
		     (when (>= (regiment-verbosity) 2) (printf "  PROGSIZE: ~s\n" (count-nodes prog)))

		     (ws-run-pass prog classify-emits)

                     ;;
                     ;; multi-in-multi-out
                     ;;
                     ;(pretty-print prog)
                     ;(pretty-print  (rewrite-merges-as-iterates (convert-to-multi-in-multi-out prog)))

                     ;(pretty-print
                     ; (new-simple-merge-iterates
                     ;  (new-simple-merge-policy:always
                     ;   (make-output-streams-unique
                     ;    (rewrite-merges-as-iterates (convert-to-multi-in-multi-out prog))))))


		     (unless (embedded-mode? (compiler-invocation-mode))
		       (ws-run-pass prog insert-refcounts)
		       (when (>= (regiment-verbosity) 2) (printf "  PROGSIZE: ~s\n" (count-nodes prog))))

		     (ws-run-pass prog flag-static-allocate)
		     ;;(assure-type-annotated prog (lambda (x) (and (pair? x) (eq? 'cons (car x)))))

		     ;; It's painful, but we need to typecheck again.
		     ;; HACK: Let's only retypecheck if there were any unknown result types:
		     (let ([len (length (deep-assq-all 'unknown_result_ty prog))])
		       (unless (zero? len)
			 (printf "*** GOT AN UNKNOWN RESULT TYPE, ADDITION TYPECHECK ***\n")
			 (parameterize ([inferencer-enable-LUB #t]
					[inferencer-let-bound-poly #f])
			   (time (ws-run-pass prog retypecheck)))))
		     (when 			 
			 (or (>= (regiment-verbosity) 3) (IFDEBUG #t #f)
			     ;; TEMP
			     (eq? 'wavescript-compiler-nesc (compiler-invocation-mode))
			     )
		       (dump-compiler-intermediate prog ".__after_refcounts.ss"))
		     (when (>= (regiment-verbosity) 2) (printf "  PROGSIZE: ~s\n" (count-nodes prog)))

		     (inspect prog)

		     (time (ws-run-pass prog emit-c2 class))

		     ;; Now "prog" is an alist of [file text] bindings, along with 
		     ;; a thunk to execute when the files are written.
		     (let-match ([#(((,file* ,contents*) ...) ,thunk) prog])
		       (for-each (lambda (file contents)
				   (string->file (time (text->string contents)) file))
			 file* contents*)
		       (unless (<= (regiment-verbosity) 0)
			 (printf "\nGenerated output to files ~s.\n" file*))
		       ;; And then execute the post-write thunk in the same directory:
		       (thunk)))])

	      ;; Currently we partition the program VERY late into node and server components:
	      ;; This happens after we've converted to the "explicit-stream-wiring" representation.
	      (if (not (embedded-mode? (compiler-invocation-mode)))		  

		  ;; In this case we're not in embedded mode, but we might still want to split.
		  (let ([class (match (compiler-invocation-mode)
				      ;[wavescript-compiler-c      <emitC2-timed>]
				 [wavescript-compiler-c 
				       (match (wsc2-gc-mode)
					 [refcount <emitC2>]
					 [deferred <emitC2-zct>]
					 [boehm    <emitC2-nogc>]
					 [none     <emitC2-nogc>]
					 [,oth (error 'wscomp "unsupported garbage collection mode: ~s" oth)])]
				 [wavescript-compiler-c      <emitC2-nogc>]
				 [wavescript-compiler-java   <java>])])
		    ;; In this case we do a 'normal', non-partitioned compile:
		    (eprintf " Generating code for GC = ~a\n" (wsc2-gc-mode))
		    
		    (if (memq 'split (ws-optimizations-enabled))

			;; Here we find the cutpoints, insert marshals and TCP communication, and 
			;; jump back into an earlier point in the compilation.
			(let-match ([#(,node-part ,server-part) (partition-graph-by-namespace prog)])
			  (define cutstreams '()) ;; an association list of (name . type)
			  (define (strip-cutpoints part)
			    (apply-to-partition-ops
			     (lambda (ops)
			       (filter (lambda (op) (not (eq? (car op) 'cutpoint))) ops))
			     part))

		  (printf "\n Node operators:\n\n")
		  (pretty-print (partition->opnames node-part))
		  (printf "\n Server operators:\n\n")
		  (pretty-print (partition->opnames server-part))

			  ;; Any cutpoints that are passing types other than raw bytes need marshaling.
			  (map-partition-ops
			   (lambda (op)
			     (define type (cadr (ASSERT (assq 'output-type (cdr op)))))			     
			     (when (eq? (car op) 'cutpoint)
			       (printf "   Cutpoint type: ~s\n" (cadr (ASSERT (assq 'output-type (cdr op))))))
			     (and (eq? (car op) 'cutpoint)			  
				  (not (equal? type '(Stream (Array Uint8))))
				  (not (equal? type '(Stream #()))) ;; TEMP: This has a different meaning.
				  (set! cutstreams (set-cons:list 
						    (cons (cadr (ASSERT (assq 'incoming (cdr op))))
							  (cadr (ASSERT (assq 'output-type (cdr op)))))
						    cutstreams))))

			   server-part)
			  	
			  ;; If we need to, we do the marshal conversions and jump way back in the compiler.
			  (unless (null? cutstreams)
			    ;; It would be better to use an explicit list-of-passes instead
			    ;; of using continuations to jump in and out of the compiler.
			    (printf "  <---------|\n")
			    (printf "  <REWINDING| earlier in the compiler to insert marshaling code.\n")
			    (printf "  <---------|\n")
			    ;(pretty-print (before-marshal-snapshot))
			    (let ([x (insert-marshal-and-comm (before-marshal-snapshot) cutstreams)])
			      (printf "INSERTED MARSHALING FOR CUTSTREAMS: ~s\n" cutstreams)	      
			      (pretty-print x)
			      ((before-marshal-hook) x)))
			  
			  ;; Otherwise, any cutpoints are of the right type Stream (Array Uint8).  We proceed.			  
			  ;(error wscomp "can't currently do a split (partitioned) compile for non-embedded backend")
			  (parameterize ([emitC2-output-target "query_client.c"])
			    (last-few-steps (strip-cutpoints node-part)   class))
			  (parameterize ([emitC2-output-target "query_server.c"])
			    (last-few-steps (strip-cutpoints server-part) class))
			  ;(last-few-steps node-part  class)
			  )
			(last-few-steps prog class)))

		;; EMBEDDED MODE -- might still not want split execution:
		(if (not (memq 'split (ws-optimizations-enabled)))
		    (last-few-steps prog
				    (match (compiler-invocation-mode)
				      [wavescript-compiler-nesc   <tinyos>]
				      [wavescript-compiler-javame <javaME>]))

		 ;; HERE'S THE SPLIT SERVER/EMBEDDED PATH:
		 (let-match ([#(,node-part ,server-part) (partition-graph-by-namespace prog)])
		  ;; This exports the partitioning problem as a linear-programming problem.
		  (define (DUMP-THE-LINEAR-PROGRAM merged)
		    ;; [2008.04.08] TEMP - this is for my experimentation:
		    ;; TEMPTOGGLE
		    (when #t ;;(top-level-bound? 'scheme-profiling-performed!)
		      ;;(printf "\nDumping integer linear program, using Scheme profile only.\n")
		      (let ()		    
			(printf "\nDumping integer linear program.\n")
			(string->file (emit-lp (partition-sourcesonly merged)
					       (partition-getmiddle merged)
					       (partition-baseonly merged))
				      "partition_scheme.lp")
			(printf "\n Running LP solver.\n")
			(time (system "lp_solve partition_scheme.lp > partition_assignments_scheme.txt"))
			(let ([results (file->string "partition_assignments_scheme.txt")])
			  (match (string->slist results)
			    [(Value of objective function: ,objective
				    Actual values of the variables: 
				    ,rest ...)
			     (define (everyother ls)
			       (cond
				[(null? ls) '()]
				[(null? (cdr ls)) ls]
				[else (cons (car ls) (everyother (cddr ls)))]))
			     (define names (everyother rest))
			     (define vals (everyother (cdr rest)))
					;(inspect (map list names vals))
			     
			     (define assigned (inject-assignments merged (map list names vals)))
			     
			     (string->file (output-graphviz assigned) "query_partitioned.dot")
			     (printf "Produced new graphviz output...\n")
			     (system "dot -Tpng query_partitioned.dot -oquery_partitioned.png")
			     (exit)]
			    [,else (error 'lp_solve "could not parse lp_solve output")])))))

		  ;; Tag everything that is part of the user's node partition:
		  (set! node-part (map-partition-ops (lambda (x) (tag-op '(originally-on-node) x)) node-part))
		  		  		
		  (printf "\n Node operators:\n\n")
		  (pretty-print (partition->opnames node-part))
		  (printf "\n Server operators:\n\n")
		  (pretty-print (partition->opnames server-part))
		  (newline)
		  
		  ;; TEMPTOGGLE: ;; EARLY DUMP HERE, WITH ONLY SCHEME PROFINFO:
		  ;(DUMP-THE-LINEAR-PROGRAM (merge-partitions node-part server-part))
		  		  
		  ;; PROFILING/AUTOSPLIT:
		  (when (memq 'autosplit (ws-optimizations-enabled))
		    (newline)
		    (printf "============================================================\n")		  
		    (printf "       PROFILING ON TELOS: \n")
		    (printf "============================================================\n")

		  ;; Expose the parallelism here, operating on totally separate parts:
		  (let-match ([(#(,maybe-node ,definite-node) 
				#(,maybe-server ,definite-server))
			       (par (refine-node-partition node-part) 
				    (refine-server-partition server-part))])
		   		    
		    (define max-node 
		      (merge-partitions 
		       ;; Note: Can't just reuse node-part here because we want to tag on some extra metadata:
		       (merge-partitions definite-node 
					 ;; Here we tag tho floating operators:
					 (map-partition-ops (lambda (x) (tag-op '(floating) x)) maybe-node))
		       (map-partition-ops (lambda (x) (tag-op '(floating) x)) maybe-server)))
		    (define floating-opnames
		      (append (partition->opnames maybe-node)
			      (partition->opnames maybe-server)))

		    (when (>= (regiment-verbosity) 2) 
		      (printf "Connectivity of profiled partition:\n")(print-partition max-node))
		    
		    (printf "\n Node-only operators:\n\n  ")
		    (pretty-print (partition->opnames definite-node))
		    (printf "\n Floating operators:\n\n  ")
		    (pretty-print floating-opnames)
		    (printf "\n Server-only operators:\n\n  ")
		    (pretty-print (partition->opnames definite-server))
		    (newline)
		    		    
		    (pretty-print (partition->simple-graph max-node))

		    (begin
		      (last-few-steps max-node <tinyos-timed>)		      
		      ;; Need a big printf buffer... this MUST be replaced with a smarter system at some point.
		      ;; Either flushing between every operator, or a less intrusive measurement method.
		      ;;(system "export CFLAGS += -DPRINTF_BUFFER_SIZE=1000")
		      ;;(putenv "CFLAGS" (** (or (getenv "CFLAGS") "") "-DPRINTF_BUFFER_SIZE=1000"))
		      (let ([themote (if (getenv "THEMOTE") (string-append "bsl," (getenv "THEMOTE")) "")])
			(unless (zero? (system (format "make -f Makefile.tos2 telosb ~a install 2> .TOS_build_log.txt" themote)))
			  (error 'wstiny "error when trying to build profiling code for telosb, see output in .TOS_build_log.txt"))))
		    
		    (printf "============================================================\n")
		    (printf "       Reading back profile results: \n")
		    (printf "============================================================\n")
		    ;; Here we perform a hack to prune a spurious line from the printfClient output.
		    ;(system "java PrintfClient | grep -v \"^Thread\\[\"")

		    ;; We read past TWO end markers to make sure we got a whole cycle:
		    (let* ([times 
;'()
			    (extract-time-intervals 
			     ;; FUDGE factor for tinyos:
			     ;82
			     (process-read/until-garbage-or-pred 
			     ;;"exec java PrintfClient 2> /dev/null | grep -v \"^Thread\\[\""
			     "java PrintfClient"			     
			     (lambda (exp) ;; Stop when we get to the end
			       (match exp
				 [(EndTraverse . ,_) #t]
				 [,else #f]))))]
			   [newprog (inject-times prog times 32000)])

                      (when (file-exists? "profiled_times.txt") (delete-file "profiled_times.txt"))
		      (with-output-to-file "profiled_times.txt" (lambda () (pp times)))

		      (string->file (output-graphviz newprog) "query_profiled.dot")
		      (system "dot -Tpng query_profiled.dot -oquery_profiled.png")

		      (DUMP-THE-LINEAR-PROGRAM newprog) 

		      (printf "============================================================\n")
		      (printf "       Auto Partitioning: \n")
		      (printf "============================================================\n")
		      		      
		      (let-match ([#(,new-node ,new-server)

				   ;; Old method, my limited exhaustive search:
				   #;
				   (exhaustive-partition-search max-nodepart-heuristic
								(inject-times max-node times 32000))
				   ;; New method, run a linear program solver:
				   (if (null? floating-opnames)
				       (begin (printf "SKIPPING LP because there were no floating ops...")
					      (exit 0))
				   (let ([merged (merge-partitions maybe-node maybe-server)])
				     (printf "\nDumping integer linear program.\n")
				     (string->file (emit-lp (inject-times definite-node times 32000)
							    (inject-times merged times 32000)
							    definite-server)
						   "partition.lp")
				     (printf "\n Running LP solver.\n")				     
				     (time (system "lp_solve partition.lp > partition_assignments.txt"))
				     (let-values ([(objective assignments) (read-back-lp-results "partition_assignments.txt")])
				       (define assigned (inject-assignments merged assignments))
				       (printf "Value of objective function was: ~s\n" objective)
				       (printf "Number of boxes on node/server: ~s / ~s\n"
					       (length (filter (lambda (ls) (eq? 1 (cadr ls))) assignments))
					       (length (filter (lambda (ls) (eq? 0 (cadr ls))) assignments)))
				       (string->file (output-graphviz assigned) "query_lp.dot")
				       (printf "Produced new graphviz output... query_lp.png\n")
				       (system "dot -Tpng query_lp.dot -oquery_lp.png")
				       (printf "  ... done.\n")

				       (exit)			  
				       ;(partition-based-on-lp assigned)
				       )
				     ))				   
				   ])
			(define all-server (reinsert-cutpoints (merge-partitions definite-server new-server)))
			
			(printf "\n Final Partitioning, node operators:\n\n")
			;(pretty-print (partition->opnames new-node))
			(print-partition new-node)
			(printf "\n Server operators:\n\n")			
			;(pretty-print (partition->opnames all-server))
			(print-partition all-server)
			(newline)

			(let ([merged (merge-partitions new-node all-server)])
			  (delete-file "query_partitioned.png")
			  (printf "Dumping query_partitioned.png..")
			  (string->file (output-graphviz merged) "query_partitioned.dot")
			  (when (>= (regiment-verbosity) 1)
			    (printf "Dumping profile visualization to query_partitioned.png... ")(flush-output-port (current-output-port)))
			  (system "dot -Tpng query_partitioned.dot -oquery_partitioned.png")
			  (when (>= (regiment-verbosity) 1) (printf "done.\n")))

			;; Now we need to multiplex operators that have migrated to 
			;; the server to handle many streams from different nodes.
			;(set! all-server (multiplex-migrated 'originally-on-node all-server))
			;(inspect all-server)

			(set! node-part new-node)
			(set! server-part all-server)
			
			)))) ;; End autosplit path

		  (printf "Performing code generation for PC and mote side:\n")
		  ;(printf "============================================================\n")
		  (last-few-steps node-part <tinyos>)
		  ;(printf "============================================================\n")
		  (parameterize ([compiler-invocation-mode 'wavescript-compiler-c])
		    (last-few-steps server-part <emitC2>)
		    (printf "============================================================\n"))
		  (unless (file-exists? (** (or (getenv "TOSROOT") "") "/support/sdk/c/serialpacket.h"))
		    (error 'wstiny "you need to run 'make' in ~a" 
			   (** (or (getenv "TOSROOT") "") "/support/sdk/c/")))
		  )  ;; End split-program path.
		) ;; End embedded path
	    ))) ;; End wsc2 path
       ;; Old C++ / XStream version:
       (begin 
	 (ws-run-pass prog nominalize-types)

	 (DEBUGASSERT (dump-compiler-intermediate prog ".__almostC.ss"))   
	 (string->file 
	  (text->string 
	   (wsquery->text prog wavescope-scheduler))
	  outfile)   
	 (unless (<= (regiment-verbosity) 0)
	   (printf "\nGenerated C++/XStream output to ~s.\n" outfile))
	 )))
   
   (if (>= (regiment-verbosity) 1) (time (begin (run-wscomp) (printf "Total compile time:\n"))) (run-wscomp))
   )
 ) ; End wscomp






;; ================================================================================
;; WaveScript OCAML Compiler Entrypoint:

(define (wscaml x input-params . flags)                                 ;; Entrypoint.  
  (parameterize ([compiler-invocation-mode 'wavescript-compiler-caml]
		 [regiment-primitives ;; Remove those regiment-only primitives.
		  (difference (regiment-primitives) regiment-distributed-primitives)])
    (define disabled-passes (append (map cadr (find-in-flags 'disable 1 flags)) ws-disabled-by-default))
    (define outfile "./query.ml")
    (define prog (begin (ASSERT list? x) x))

    (ASSERT (andmap symbol? flags))
    (set! prog (run-ws-compiler prog input-params disabled-passes #f))
    (set! prog (explicit-stream-wiring prog))
    (string->file (text->string (emit-caml-wsquery prog)) outfile)
    (printf "\nGenerated OCaml output to ~s.\n" outfile)
    ))

;; ================================================================================
;; WaveScript MLTON Compiler Entrypoint:

(define (wsmlton x input-params . flags)                                 ;; Entrypoint.  
  (parameterize ([compiler-invocation-mode 'wavescript-compiler-caml]
		 [regiment-primitives ;; Remove those regiment-only primitives.
		  (difference (regiment-primitives) regiment-distributed-primitives)])
    (define disabled-passes (append (map cadr (find-in-flags 'disable 1 flags)) ws-disabled-by-default))
    (define outfile "./query.sml")
    (define prog (coerce-to-ws-prog x input-params))
    
    (ASSERT (andmap symbol? flags))
    (set! prog (run-ws-compiler prog input-params disabled-passes #f))

;    (inspect prog)

    (ws-run-pass prog explicit-stream-wiring)
    
    ;(IFCHEZ (string->file (output-graphviz prog) "query.dot") (void))

;    (inspect prog)
    (printf "SIZE BEFORE MLTON CODEGEN: ~s\n" (count-nodes prog))

    (time (ws-run-pass prog emit-mlton-wsquery))

;    (inspect 'made-it-this-far)
    (string->file (text->string prog) outfile)
    (printf "\nGenerated MLton output to ~s.\n" outfile)
    ))



;===============================================================================
;;; These functions are used for command-line invocation of the whole system:

(define (print-help)
  (printf "Regiment system, version ~s (rev ~s) (loaded from ~a)\n" 
	  (regiment-version) 
	  (top-level-value 'svn-revision)
	  (top-level-value 'regiment-origin))
  (printf "Usage: regiment command [options] \n")
  (printf "\n")
  (printf "Commands: \n")
  (printf "  help          prints this message\n")
  (printf "  compile  (c)  compile Regiment source (.rs) to token machines\n")
  (printf "  simulate (s)  simulate a token machine or simulator file\n")
  (printf "  interact (i)  start up Scheme REPL with Regiment loaded\n")
  (printf "  test     (t)  run all regiment tests\n")
  (printf "  log      (l)  simulator trace manipulation mode\n")
  (printf "  wsint    (wsint)  WaveScript evaluator mode\n")
  (printf "  wscomp   (wscomp) WaveScript (C++) compiler mode\n")
  (printf "  wsc2     (wsc2)   WaveScript (C) compiler mode ver 2\n")
  (printf "  wsml     (wsml)   WaveScript compiler SML (MLton) backend\n")
  (printf "  wscaml   (wscaml) WaveScript compiler Caml backend\n")
  (printf "\n")
  (printf "General Options:  \n")
  (printf "  -v   verbose compilation/simulation, includes warnings\n")
  (printf "  -q   suppress banners and other nonessential output\n")
  (printf "\n")
  (printf "Regiment Compiler Options: \n")
  (printf "  -d2  use new compiler: deglobalize2 \n")
  (printf "  -lt  type check only, print typed program to stdout           \n")
  (printf "  -ltt type check only, print *only* top level types to stdout  \n")
  (printf "  -l0  stop compilation just before deglobalize          (.sexp)\n")
  (printf "  -l1  compile barely to tokens (just after deglobalize) (.tm0)\n")
  (printf "  -l2  compile to just tokens (maximally lowered)        (.tm)\n")
  (printf "  -l4  output generated simulator-alpha code             (.sim.alpha)\n")
  (printf "  -l5  output generated NesC/Tossim code                 (.sim.nesc) \n")
  (printf "  -debug       print extra info, inspect errors \n")
  (printf "\n")
  (printf "Simulator Options: \n")
  (printf "  -timeout <n>  timeout after n clock ticks\n")
  (printf "  -plot         when simulation finishes, gnuplot output\n")
  (printf "  -repl         when simulation finishes, run interactive REPL\n")
  (printf "\n")
  (printf "Interactive Options: \n")
  (printf "  --script  <file>    run a scheme file as a script\n")
  (printf "  -exit-error        exit process w/ nonzero error code on a scheme error\n")
  (printf "\n")
  (printf "Log-manipulation Options: \n")
  (printf "  -print    <file>    print any log-file in human readable format\n")
  (printf "  -examine  <file>    describe the chunking format of an existing logfile\n")
  (printf "  -reencode <f1> <f2> reencode a logfile in a compressed but fast-loading way\n")
  (printf "  -vw <worldfile>     (not really a log) if gui is loaded, view saved world\n")
  (printf "\n")
  ;(display (file->string (string-append (REGIMENTD) "bin/regiment_opts.txt")))
  (display (file->string (string-append (REGIMENTD) "/bin/ws_opts.txt")))
  )

(define (print-ws-prim-table)
  (define prims (difference (regiment-primitives) regiment-distributed-primitives))
  ;; These are prims that we don't want to publicize for whatever reason:
  (define secret-prims
    `(world anchor locdiff sense nodeid 
      ;HACK_O_RAMA 
      Secret:newTimebase
      tuple tupref
      static statref
      ,@(filter (lambda (n) (define str (symbol->string n))
			(or (memq #\? (string->list str))
			    (and (> (string-length str) 3)
				 (equal? (substring str 0 2) "__"))))
	  (map car prims)))
    )
  (define (pad name)
    (define namestr (format "~a" name))
    (define padincr 6)
    (define padsize (- padincr (remainder (string-length namestr) padincr)))
    (define padding (make-string (if (= padsize padincr) 0 padsize) #\space))
    (format "~a~a" name padding))
  (for-each (lambda (entry)
	      (match entry
		[(,name ,args ,retty . ,rest)
		 (unless (null? rest) (inspect rest))
		 (when (and (list? args) (not (memq name secret-prims)))
		   (printf "~a :: ~a\n" (pad name) (show-type `(,@args -> ,retty))))]
		;; Constants:
		[(,name ,ty)
		 (unless (memq name secret-prims)		   
		   (printf "~a :: ~a\n" (pad name) (show-type ty)))
		 ])
	      ) prims))



(define (print-types-and-exit prog . opts)
  (define verbose? (memq 'verbose opts))
  (printf ";; Regiment program with infered types: \n")
  ;; Run just the verify regiment pass, it will associate types:
  (parameterize ([print-vector-length #f])
  (match (verify-regiment `(lang '(program ,prog)))
    [(,lang '(program ,p ,t))
     (match p
       [(letrec ([,id* ,t* ,rhs*] ...) ,bod)
	(for-each (lambda (id t rhs)
		    (if verbose?
			(begin (pretty-print `(define ,id : ,t ,rhs))(newline))
			(printf "~a : ~a\n" (pad-width 30 id) t)))
	  id* t* rhs*)
	(if verbose? (pretty-print bod))]
       [,p (pretty-print p)])
     ;(printf "\n;; Regiment program return type: ~a\n" t)
     (printf "  : ~a\n" t)
     (exit 0)]
    [,other (error 'print-types-and-exit "bad output from verify-regiment: ~s" other)])))

(define (regiment-exit code)
  ;; In case we're building a heap, we set this before we exit.
  ;(disp "SETTING HEAP: " regiment-origin (top-level-value 'regiment-origin))
  (set-top-level-value! 'regiment-origin "saved heap")
  ;(disp "HEAP SET: " regiment-origin (top-level-value 'regiment-origin))
  (exit code))


;; *THE* Main function for the regiment/wavescript process.
;; Takes an arbitrary number of strings (flags) as arguments.
(define main 
  (lambda args    
    (define makesimcode #f)
    (define outfile #f)
    (define plot #f)
    (define simrepl #f)
    ;(disp "Main called w ARGS: " args)
    (when (null? args) (print-help) (regiment-exit 0))

    (IF_THREADS
     (begin       
       ;; No matter which arguments we're called with, let's go ahead and
       ;; initialize the thread system.  Really this should happen only
       ;; for compile-modes that may actually use it.
       (define-top-level-value 'desired-number-of-threads
	 (ASSERT (string->number (or (getenv "REGTHREADS") "1"))))
       ;; TODO: PUT IN CORRECT NUMBER OF CPUS!
       (init-par desired-number-of-threads)
)
     (void))

;    (printf "regimentc: compile regiment programs!\n")
    ;; This is a list of option flags mutated by "loop" below.
    ;; This is a bit sketchy.  The same flags are sent to run-compiler and run-simulator-alpha.
    ;; FIXME: may want to merge these two eventually; input-parameters added mostly for
    ;;        profiling information, which can have slightly more complex structure
    (let ([opts '()]
          [input-parameters '()])      
      (define (coerce-symbol x)
	(cond 
	 [(symbol? x) x]
	 [(string? x) (string->symbol x)]
	 [else (string->symbol (format "~a" x))]))
      (define (coerce-string x)
	(cond
	 [(string? x) x]
	 [else (format "~a" x)]))
      ;; This determines what mode we're in then calls "loop" to process the flags.
      (define (main)		
        ;; I keep disjoint options for the modes so I use the same option-parser for all modes ("loop")
	(let ([mode (coerce-symbol (car args))] 
	      [filenames (loop (map coerce-string (cdr args)))])

	(define (acquire-input-prog callmode ) 
	  (match filenames
	    ;; If there's no file given read from stdin
	    [() (standard-input-port)]
	    [(,fn ,rest ...) 
	     (unless (null? rest)
	       (error callmode  "bad extra filename(s) or flag(s) following filename ~s:\n ~s" fn rest))
	     ;; If it's a ws file we need to parse the file:
	     (if (equal? "ws" (extract-file-extension fn))
		 (or (read-wavescript-source-file fn)
		     (error callmode  "couldn't parse file: ~s" fn))
		 ;; Otherwise let's assume 
		 (open-input-file fn))]
	    [,else (error 'regiment "~a should take one file name as input, given: ~a" callmode else)]))

	;; AFTER, those options are processed we switch on the mode flag.
	(case mode
	  ;; Unit Test mode:
	  [(t test)
	   (define-top-level-value 'REGIMENT-BATCH-MODE #t)
	   (test-units)
	   ;(test-everything)
	   ]

	  ;; Compile mode:
	  [(c compile)
	   ;(define-top-level-value 'REGIMENT-BATCH-MODE #t)
	   (IFWAVESCOPE (void)
	     (if (null? filenames)
	       (begin
		 (printf "No input file.  Type top-level Regiment expression.\n")
		 (printf "> ")(flush-output-port (current-output-port))
		 (let* ([expr (read)])
		   (printf "\n Using default output file: out.tm...\n")		
		   (apply run-compiler expr "out.tm" opts)))
	       (begin 
		 (if (> (length filenames) 1)
		     (error 'regiment_command_line_compiler 
			    "can't handle more than one filename at a time currently: ~s" filenames))
		 ;; Otherwise we're good to go.
		 (cond 
		  [(memq 'type-only-verbose opts)
		   (print-types-and-exit 
		    (apply regiment-compile-file (car filenames) 'to-typed opts)
		    'verbose)]
		  [(memq 'type-only opts)
		   (print-types-and-exit 
		    (apply regiment-compile-file (car filenames) 'to-typed opts))]
		  [else 
		   (apply regiment-compile-file (car filenames) 'write-file opts)])
		 )))]

	  ;; Simulation mode (also may invoke compiler):
	  [(s simulate)
	   (IFWAVESCOPE (void)
	     (parameterize ([simulation-logger-level 1]
			  );; Only get send/receive, groundtruth, and newworld msgs.
	   (let ((fn (if (null? filenames)
			 "out.sim"
			 (car filenames))))
	     
	     (define-top-level-value 'go-sim
	       (lambda ()
		 (printf "Running simulation from file: ~a\n" fn)
		 (let ((result
			(apply load-regiment fn opts)
#;
			;; Be careful to watch for parameterization:	     
			(mvlet (([prog params] (read-regiment-source-file fn)))
			  (with-evaled-params params
					      (lambda () 
						(apply run-simulator-alpha prog 
						       'srand (current-time)
						       opts))))))
		   ;; Print simalpha stats:
		   (print-stats)
		   (if plot (gnuplot result))
		   (repl);(if simrepl (IFCHEZ (new-cafe) (read-eval-print-loop)))
		   result)))

	     (IF_GRAPHICS 
	      ;; This starts swl then evals the expression.
	      (bounce-to-swl '(go-sim))	      
	      (begin 
		     ((top-level-value 'go-sim))
		     ))
	     )))]

	  ;; Interactive mode.  A Scheme REPL.
	  ;; [2006.02.21] This is a better way of exposing the normal scheme startup behavior:
	  [(i interact)
	   (eprintf "Exposing Regiment through interactive read-eval-print loop:\n")
	   ;; [2006.02.28] We want to reset global bindings before loading any more source:
	   ;(eval '(import scheme))
	   ;; Can't trust new code to not mutate primitive names:
	   ;(IFCHEZ (optimize-level 1) (void))	   

	   (cond
	    [(null? filenames) 

;	     (printf "GOING INTO REPL ~s\n" main)
	     ;(eval '(require main_plt))
	     ;(eval '(require regiment_pltscript))
;	     (printf "GOT MODULE INTO TOP LEVEL ~s\n" (eval 'main))
	     (repl)	     
	     #;
	     (IFCHEZ (call-with-values new-cafe (lambda ls (apply exit ls)))
		     (read-eval-print-loop))]
	    ;; To run a script through "regiment"
	    ;; --script must be the first argument after "regiment i"
	    ;;
	    ;; This won't occur, chez grabs the --script parameter
	    ;; directly.  Code should go in the scheme-script parameter.
	    ;;
	    ;; Note, if we're doing it this way we pass the raw
	    ;; arguments, not those processed by "loop" above.
	    [(equal? (cadr args) "--script")
	     ;(printf "Using Regiment to invoke script: ~a\n" args)
	     ;(error 'regiment.ss "this shouldn't happen.")
	     ;; --script implies --exit-error: add that setting:
	     (loop '("-exit-error"))
	     (let* ([file (caddr args)]
		    [exps (file->slist file)])
	       ;; Under R6RS a script is essentially a sequence of expressions sent to "eval".
	       (for-each reg:top-level-eval
		 ;;(lambda (x) (printf "EVALUATING: ~a\n" x) (reg:top-level-eval x))
		 exps))]

	    [else 
	     ;(inspect (list->vector args))
	     (error 'main:script  "not allowed to invoke regiment i with a filename in this r6rs port yet (except through --script).")
	     #;
	     (IFCHEZ (apply orig-scheme-start (cdr args))
		     (error 'interact-mode "cannot currently run scripts through regiment in PLT Scheme")
		     )
	     ])]

	  ;; Printing SExp log files.
	  [(l log)
	   (IFWAVESCOPE (void)
	   (match (cdr args)
	     [() (if (file-exists? "__temp.log") (reg:printlog "__temp.log")
		     (if (file-exists? "__temp.log.gz") (reg:printlog "__temp.log.gz")
			 (error 'regiment:log:print "no log file supplied or found")))]
	     [("-print" ,file) 
	      (let loop ([s (reg:read-log file 'stream)])
		(unless (stream-empty? s) 
		  (printf "~a\n" (stream-car s))
		  (loop (stream-cdr s))))]
	     [("-print" ,_ ...) (error 'regiment:log:print "only can print exactly one logfile at a time: ~a" args)]
	     [("-reencode" ,in ,out)
	      ;; Do not replace output file if it's there:
	      (let ((out (open-output-file out '(compressed)))
		    (block-size 1000)  ;; 1000 lines of log chunked at a time.
		    (count 0))
		(progress-dots 
		 (lambda ()
		   (let loop ((in (reg:read-log in 'stream))
			      (n block-size) (acc '()))
		     (cond
		      [(stream-empty? in) 
		       (fasl-write (list->vector (reverse! acc)) out)
		       (close-output-port out)]
		      [(fxzero? n)
		       (fasl-write (list->vector (reverse! acc)) out)
		       (loop in block-size '())]
		      [else 
		       (set! count (add1 count))
		       (loop (stream-cdr in) (fx- n 1) (cons (stream-car in) acc))]))
		   )))]
	     [("-reencode" ,_ ...)
	      (error 'regiment:log:reencode 
		     "bad arguments for log reencoding, expects input and output file: ~a" args)]
	     [("-examine" ,file)
	      (newline)
	      (let* ([in (lambda ()
			   (if (equal? (extract-file-extension file) "gz")
			       (open-input-file file 'compressed)
			       (open-input-file file)))]
		     [first (read (in))])
		;; First classify contents.
		(cond
		 [(vector? first)
		  (printf "Log file batched into vectors, first vector is size: ~a\n" (vector-length first))]
		 [(list? first)
		  (printf "Log file contains raw, unbatched log entries.\n")]
		 [else (printf "WARNING: Log file contains the following unrecognized object: ~a" first)])
		;; Next classify fasl/plaintext.
		(let* ([in (in)]
		       [c1 (read-char in)]
		       [c2 (read-char in)])
		  ;; TODO: THIS DOESN'T WORK!!! COULD HAVE COMMENTS AT THE BEGINNING OF THE FILE.
		  (if (equal? "#@" (list->string (list c1 c2)))
		      (printf "First expression in file is FASL encoded.  (Binary fast-loading format.)\n")
		      (printf "First characters in file appears to be non-FASL plaintext (might be wrong) ~a.\n"
			      (if (equal? (extract-file-extension file) "gz")
				  " (except for being gzipped)" ""))
		      )))]
	     [("-examine" ,_ ...)
	      (error 'regiment:log:examine "-examine expects exactly one file name argument: ~a" args)]

	     [("-vw" ,worldfile)
	      (let ([file worldfile])
		(IF_GRAPHICS 			     
		 (begin 
		   (unless (file-exists? file)
		     (error 'regiment:log:view-world "this worldfile doesn't exist: ~s" file)
		     (exit -1))
		   ;; Read only the first entry, set up as global world.
		   (simalpha-current-simworld (read (open-input-file file)))
		   (animate-world! (simalpha-current-simworld))
		   (printf "Animated world from world file ~s.\n" file)
		   ;(clean-simalpha-counters!)
		   ;(inspect (simalpha-current-simworld))
		   (bounce-to-swl '(begin 
				     ;(clean-simworld! (simalpha-current-simworld))
				     (init-graphics)
				     (simalpha-draw-world (simalpha-current-simworld))
					;(printf "HMM: ~s\n" (simalpha-current-simworld))
				     ;; Run a stupidly simple query just to initialize things. 
					;(run-simulator-alpha (run-compiler '3) 'use-stale-world)
				     )
				  )
		   )
		 (begin (printf "view-world (-vw) mode doesn't really make sense without GUI loaded!")
			(exit -1))))]

	     [,other (warning 'regiment:log "unsupported options or missing arguments: ~a" other)
		     (exit -1)]
	     ))]

	  ;; Interpreting (preparsed) wavescript code.
	  [(wsint)
	   (let ()
	   (define prog (acquire-input-prog 'wsint))
	   (wsint:direct-stream (apply wsint (cons prog (cons input-parameters opts)))))]

	  ;; Same as wsint but only runs the compiler up to typechecking.
	  [(wsearly)
	   (let ()
	     (define prog (acquire-input-prog 'wsearly))
	     (wsint:direct-stream (apply wsint-early (cons prog (cons input-parameters opts)))))]
	  
	  [(wscomp)
	   ;(define-top-level-value 'REGIMENT-BATCH-MODE #t)
	   (let ()
	     (define port (acquire-input-prog 'wscomp))
	     (apply wscomp port input-parameters opts))]

	  [(wsc2)
	   ;; HACK: need to set compiler-invocation-mode EARLY for wsc2 (for parsing):
	   (parameterize ([compiler-invocation-mode 'wavescript-compiler-c])
	     (let ((port (acquire-input-prog 'wscomp)))
	       (apply wscomp port input-parameters 'wsc2 opts)))]

	  ;; NesC/TinyOS target, similar to wsc2:
	  [(wstiny)
	   (parameterize ([compiler-invocation-mode 'wavescript-compiler-nesc])
	     (let ((port (acquire-input-prog 'wscomp)))
	       (apply wscomp port input-parameters 'wstiny opts)))]

	  [(wsjava)
	   (parameterize ([compiler-invocation-mode 'wavescript-compiler-java])
	     (let ((port (acquire-input-prog 'wscomp)))
	       (apply wscomp port input-parameters 'wsjavame opts)))]

	  ;; Java ME target:
	  [(wsjavame)
	   (parameterize ([compiler-invocation-mode 'wavescript-compiler-javame])
	     (let ((port (acquire-input-prog 'wscomp)))
	       (apply wscomp port input-parameters 'wsjavame opts)))]

	  [(wscaml)
	   (let ()
	     (define exp (acquire-input-prog 'wscaml))
	     (apply wscaml exp input-parameters opts))]

	  ;; Copy/pasted from above:
	  [(wsml wsmlton)
	   (let ()
	     (define exp (acquire-input-prog 'wsmlton))
	     (apply wsmlton exp input-parameters opts))]

	  [else (error 'main "unhandled mode ~s" mode)]	  
	  )))

      ;; Loop goes through the arguments, processing them accordingly:
      ;; Anything not matched by this is presumed to be a file name.
      (define (loop args)
        (match args

          [() '()]
	  [("-v" ,num? ,rest ...)  
	   (set! opts (cons 'verbose opts))
	   (if (string->number num?)
	       (begin (regiment-verbosity (string->number num?))
		      (loop rest))
	       (begin (regiment-verbosity (add1 (regiment-verbosity)))
		      (loop (cons num? rest))))]

	  [("-n" ,limit ,rest ...)
	   (wsint-tuple-limit (ASSERT integer? (string->number limit)))
	   (loop rest)]
	  ;; Goes with -n... Time query for wsint:
	  [("-t" ,rest ...)
	   (wsint-time-query #t)
	   (loop rest)]

	  ;; This SHOULD also switch on some optimization passes with ws-optimizations-enabled:
	  [("-O3" ,rest ...) (ws-optimization-level 3) (loop rest)]
	  [("-O2" ,rest ...) (ws-optimization-level 2) (loop rest)]
	  
	  [("-o" ,outfile ,rest ...)
	   (wsint-output-file outfile)
	   (loop rest)]

	  [("-opt" ,name ,rest ...)
	   ;(unless (symbol? name) (error 'main "bad option to -opt flag: ~s" name))
	   (set! name (string->symbol name))
	   (unless (memq name '(rewrites fuse merge-iterates profile autosplit))
	     (error 'main "unsupported name for optimization passed to -opt flag: ~s" name))
	   (unless (<= (regiment-verbosity) 0) (printf "  Optimization enabled: ~s\n" name))
	   (ws-optimizations-enabled (cons name (ws-optimizations-enabled )))
	   (loop rest)]

	  ;[("-profelements") ]
	  ;[("-profvtime")  ]
	  ;[("-profrealtime") ]

	  
	  [("-gc" ,name ,rest ...)
	   (set! name (string->symbol name))
	   (printf "Setting GC mode to ~s\n" name)
	   ;; Allow some shorthands:
	   (case name
	     [(no none)                    (wsc2-gc-mode 'none)]
	     [(rc ref refcount)            (wsc2-gc-mode 'refcount)]
	     [(de def deferred)            (wsc2-gc-mode 'deferred)]
	     ;[(deferred-disjoint)          (wsc2-gc-mode )]
	     ;[(refcount-disjoint)          (wsc2-gc-mode )]
	     ;[(refcount-noshared)          (wsc2-gc-mode )]
	     ;[(rcmult refcount-multiheap)         (wsc2-gc-mode )]
	     [(bo boehm)                   (wsc2-gc-mode 'boehm)]
	     [else               (error "unsupported garbage collection mode: ~s" name)])
	   (loop rest)]
	  
	  [(,ss ,name ,rest ...) (guard (or (string=? ss "-sigseg") (string=? ss "-ss")))
	   (set! name (string->symbol name))
	   (printf "Setting Sigseg mode to ~s\n" name)
	   (case name
	     [(copy copyalways)         (wsc2-sigseg-mode 'copyalways)]
	     [(list seglist wsharing)   (wsc2-sigseg-mode 'seglist)]
	     [else               (error "unsupported sigseg implementation: ~s" name)])
	   (loop rest)]
	  
	  ;; This tells wstiny to split the program into node and server components:
	  [("-split" ,rest ...)
	   ;; Using the optimization list as a place to keep track of this:
	   (ws-optimizations-enabled (cons 'split (ws-optimizations-enabled)))
	   (loop rest)]

	  ;; This signals that we include whatever debugging info we can in the output code.
	  ;; [2007.10.26] Currently it's just used by the wsmlton script, and doees nothing here.
	  [("-dbg" ,rest ...) (loop rest)]

	  [(".h" ,rest ...) (print-help) (regiment-exit 0)]
	  
	  [("-plot" ,rest ...) (set! plot #t) (loop rest)]
	  [("-repl" ,rest ...) (set! simrepl #t) (loop rest)]

	  ;; Do not print output elements on the "main" stream automatically.
	  [("-noprint" ,rest ...) (suppress-main-stream-printing #t) (loop rest)]

	  ;; How far should the regiment compiler go:
	  [("-d2" ,rest ...)  (set! opts (cons 'deglobalize2 opts)) (loop rest)]	 
	  [("-lt" ,rest ...)  (set! opts (cons 'type-only-verbose opts)) (loop rest)]
	  [("-ltt" ,rest ...) (set! opts (cons 'type-only opts)) (loop rest)]
	  [("-l0" ,rest ...)  (set! opts (cons 'almost-tokens opts))   (loop rest)]
	  [("-l1" ,rest ...)  (set! opts (cons 'barely-tokens opts))   (loop rest)]
	  [("-l2" ,rest ...)  (set! opts (cons 'full-tokens opts))  (loop rest)]
	  [("-l4" ,rest ...) 
	   (IFWAVESCOPE (begin) 
	     (begin (set! makesimcode #t)
		    (set! opts (cons 'to-simcode opts))))
	   (loop rest)]	  
	  [("-l5" ,rest ...)
	   ;; [2006.11.11] Not handled right now:
	   (IFWAVESCOPE (begin) 
	     (begin (set! opts (cons 'to-nesc opts))
		    (pass-list 
		     (snoc emit-nesc (snoc flatten-tokmac
					   (remq flatten-tokmac (remq emit-nesc (pass-list))))))))
	   (loop rest)]

	  [("-exit-error" ,rest ...)
	   (eprintf "SETTING BATCH MODE\n")
	   (define-top-level-value 'REGIMENT-BATCH-MODE #t)
	   (loop rest)]

	  [("-dot" ,rest ...) (dump-graphviz-output #t) (loop rest)]
	  [("-ret" ,name ,rest ...) (ws-alternate-return-stream name) (loop rest)]
	  
					;		    [(--script ,rest ...) (set! opts (cons 'script opts))  (loop rest)]
	  [("-debug" ,rest ...)		     
	   (define-top-level-value 'REGIMENT-BATCH-MODE #f)
	   (regiment-emit-debug #t)
	   (loop rest)]

	  [("-quiet" ,rest ...)
	   (regiment-verbosity -1)
	   (loop rest)]
	  
	  [("-dump" ,file ,rest ...)
	   (set! outfile file)
	   (loop rest)]

	  ;; How far should the wsc compiler go:
	  [("-c0" ,rest ...) (set! opts (cons 'stop-at-c++ opts)) (loop rest)]
	  
	  [("-timeout" ,n ,rest ...)
	   (let ((n (read (open-string-input-port n))))
	     (set! opts (cons 'timeout (cons n opts))))]

	  ;; --mic
	  ;; FIXME: add to print-help (or automate print-help)
	  [("--disable-pass" ,pass-name ,rest ...)
	   (set! opts (append `(disable ,pass-name) opts))
	   (loop rest)]

	  [("--inspect" ,pass-name ,rest ...)
	   (ws-compiler-hooks
	    (cons `[,(string->symbol pass-name) ,inspect] (ws-compiler-hooks)))
	   (loop rest)]

     ;; --mic
     ;; FIXME: add to print-help (or automate print-help)
     ;; FIXME: get rid of this; put it into input-parameters
     ;; [2008.07.23] wrapped string->symbol around sched-name, because
     ;;              ikarus passes in command line args. as strings
     [("--scheduler" ,sched-name ,rest ...)
      (set! sched-name (string->symbol sched-name))
      (unless (<= (regiment-verbosity) 0) (printf "Setting scheduler: ~s\n" sched-name))
      (set! opts (append `(scheduler ,sched-name) opts))
      (loop rest)]

     ;; --mic
     ;; FIXME: add to print-help (or automate print-help)
     [("--param-file" ,param-file-name ,rest ...)
      (let ((param-file (open-input-file param-file-name)))
        (set! input-parameters (read param-file))
        (close-input-port param-file))
      (loop rest)]
	  
     ;; otherwise a file to compile that we add to the list
     [(,fn ,rest ...)
      ;;(regiment-compile-file fn)
      (cons fn (loop rest))]

     [,_ (error "Bad command line arguments to regimentc: ~a\n" args)]
     ))
      
      (main) ;; Call the entrypoint above.      
      )))

;; ENTRYPOINTS:  These mimic the shell commands but are callable from within Scheme.
(define (ws prog . args)   (apply main "wsint"  prog args))
(define (wsc2 prog . args) 
  (apply main "wsc2"   prog args)
  (system "wsc2-gcc"))

(define (wstiny prog . args) 
  (apply main "wstiny"   prog args)
  ;(system "wsc2-gcc")
  )

;(define (wsc prog . args)  (apply main "wscomp" prog args))

;===============================================================================
;;; TESTING:


(define tm-to-list ;; This is boilerplate, many of these tests just run the following:
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
	   ))

;; These are some of our system tests.  They test the compiler and the simulator as a whole.
;; The rest of the system tests are in the files named tests_*.ss
;; But some of the below tests may also be miscellaneous unit tests that require more than one module.
;;
;; NOTE: in WaveScript only mode these are mostly invalid, disabling them:
(IFWAVESCOPE 
(begin)
(begin 
  (define-testing maintests 
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
     (include "ws/testing/system_tests.ss")  
    ))
  (define-testing maintest (default-unit-tester "Main compiler units + system tests." maintests))
  ))


) ;; End module