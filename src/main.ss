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
    pass_desugar-pattern-matching
    desugar-misc
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
		(loop result (cdr funs) (cdr names))))))))
      ))

;; This is the second version of the compiler, uses deglobalize2
(define (run-compiler2 p . args)                              ;; Entrypoint.
  (parameterize ([pass-list (snoc deglobalize2 
				  (rdc(list-remove-after deglobalize (pass-list))))])
    (apply run-compiler p args)
    ))


;; This needs to be replaced with something real.
(define-pass remove-lazy-letrec
    [Expr (lambda (x fallthru)
	    (match (fallthru x)
	      [(lazy-letrec ,rest ...) `(letrec ,rest ...)]
	      [,other other]))])
(define-pass introduce-lazy-letrec
    [Expr (lambda (x fallthru)
	    (match x
	      [(free ,_ ,[e]) e]
	      [,other 
	       (match (fallthru other)
		 [(letrec ,rest ...) `(lazy-letrec ,rest ...)]
		 [,other other]) ]))])

(define-pass type-print/show
    (define (process-expr x tenv fallthru)
      (match x
	[(print ,e) 
	 `(print (assert-type ,(recover-type e tenv) ,(process-expr e tenv fallthru)))]
	[(show ,e) 
	 `(show (assert-type ,(recover-type e tenv) ,(process-expr e tenv fallthru)))]
	[,other (fallthru other tenv)]))
  [Expr/Types process-expr])

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

  (set! p (optional-stop (verify-regiment p)))
  (set! p (optional-stop (pass_desugar-pattern-matching p)))
  (printf "Program verified.\n")
  
  (set! p (optional-stop (rename-vars p)))
  (set! p (optional-stop (eta-primitives p)))
  (set! p (optional-stop (remove-unquoted-constant p)))
  (set! p (optional-stop (static-elaborate p)))

  (set! p (optional-stop (reduce-primitives p)))

  (set! p (optional-stop (merge-iterates p)))
  
  (set! p (optional-stop (verify-elaborated p)))
  (set! p (optional-stop (retypecheck p)))

  ;; (5) Now we normalize the residual in a number of ways to
  ;; produce the core query language, then we verify that core.
  (set! p (optional-stop (reduce-primitives p)))
  (set! p (optional-stop (remove-complex-constant p)))
  (set! p (optional-stop (retypecheck p)))

;  (set! p (optional-stop (uncover-free              p)))

;  (set! p (optional-stop (introduce-lazy-letrec     p)))
;  (set! p (optional-stop (lift-letrec               p)))
;  (set! p (optional-stop (lift-letrec-body          p)))
;  (set! p (optional-stop (remove-complex-opera* p)))
;  (set! p (optional-stop (remove-lazy-letrec p)))

  
;  (set! p (optional-stop (verify-core p)))
;  (set! p (optional-stop (retypecheck p)))

  (set! p (optional-stop (type-print/show p)))

  ;(set! p (optional-stop (nominalize-types p)))

  p)


;; The WaveScript "interpreter".  (Really a wavescript embedding.)
;; It loads, compiles, and evaluates a wavescript query.
;; .param x - can be an input port, a filename, or a wavescript AST (list)
(define (wsint x)                                             ;; Entrypoint.  
  (define prog
    (cond  [(input-port? x) (printf "WSINT: Loading WS source from port: ~s\n" x) 
	     ;; We assume this is parsed but not post-processed:
	     (ws-postprocess (read x))]
	    [(string? x) (printf "WSINT: Loading WS source from file: ~s\n" x)
	     (read-wavescript-source-file x)]
	    [(list? x)   (printf "WSINT: Evaluating WS source: \n ~a\n" x)  x]
	    [else (error 'wsint "bad input: ~s" x)]))

  (define _ (begin (printf "Evaluating program: (original program stored in .__inputprog.ss)\n\n") 
		   (parameterize ([pretty-line-length 180]
				  [pretty-maximum-lines 1000]
				  [print-level 20]
				  [print-length 10])
		     (void)
		     ;(pretty-print prog)
		     )
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

  (define compiled (run-ws-compiler prog))
  (define typed (pass_desugar-pattern-matching (verify-regiment prog)))

  (define __ (printf "Program verified, type-checked. (Also dumped to \".__parsed.txt\".)"))

  (define stream (delay (wavescript-language 
			 (match (strip-types compiled)
			   [(,lang '(program ,body ,_ ...)) body]))))
  
  (printf "\nProgram types: (also dumped to \".__types.txt\")\n\n")
  (print-var-types typed)
  (flush-output-port)
  (with-output-to-file ".__types.txt"
    (lambda () (print-var-types typed)(flush-output-port))
    'replace)
  (parameterize ([pretty-line-length 150]
		 [pretty-one-line-limit 100])
    (with-output-to-file ".__parsed.txt"
      (lambda () (pretty-print prog)(flush-output-port))
      'replace))
  stream)


(define (wscomp port . flags)                                 ;; Entrypoint.  
  
  (define outfile "./query.cpp")
  (define prog (ws-postprocess (read port)))
  (define typed (pass_desugar-pattern-matching (verify-regiment prog)))

  (ASSERT (andmap symbol? flags))

  (printf "Compiling program. \n\n")
  ;(pretty-print prog)
  
  (printf "\nTypecheck complete, program types:\n\n")
  (print-var-types typed)(flush-output-port)
  
  (set! prog (run-ws-compiler prog))
  (REGIMENT_DEBUG 
   (printf "================================================================================\n")
   (printf "\nNow nominalizing types.\n"))
  (set! prog (nominalize-types prog))
  (REGIMENT_DEBUG (pretty-print prog))
  (REGIMENT_DEBUG 
;   (printf "================================================================================\n")
   (printf "\nNow emitting C code:\n"))
  
  (string->file 
   (text->string 
    (wsquery->text
     prog))
   outfile)
  
  (printf "\nGenerated C++ output to ~s.\n" outfile)
  )


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
