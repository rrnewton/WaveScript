;; This is the core.  Loaded by both the Chez and PLT versions.

;;======================================  
;(display "Loading main compiler module.  RegionStreams Demo.")
;(newline)

(define-regiment-parameter regiment-version 0.87)

;; This is a global variable mutated by the node-programs in the
;; simulator_nought...  Counts total communications received.
;;(define total-messages 0)
;; Moved this *again*.  Now the simulator just defines this (via
;; eval), when it starts running.

(define pass-names
  '(verify-regiment
    eta-primitives
    rename-var
    remove-unquoted-constant                        ;;  5
    
    static-elaborate
    
    reduce-primitives    
    remove-complex-constant                         ;;  7
    uncover-free                                    ;; 14
    ;    convert-closure                            ;; 15
    lift-letrec                                     ;; 16
    lift-letrec-body                                ;; 22
    remove-complex-opera*
    verify-core
    classify-names
    add-heartbeats
    add-control-flow
    add-places
    ;    add-routing
    analyze-places
    deglobalize

    cleanup-token-machine 
    desugar-macros		
    desugar-gradients
    cleanup-token-machine   ;; Rerun to expand out some stuff.

    ;    analyze-tokmac-recursion


    desugar-let-stored
    rename-stored

;    cps-tokmac
;    sever-cont-state
 
;    closure-convert
;    cleanup-token-machine ;; Trying this.. [2005.09.27]

    ;    inline-tokmac ???

    ;haskellize-tokmac 
    
    flatten-tokmac
;    emit-nesc

    ))

;; ==================================================================
;; Functions for input/output to filesystem and for invoking compiler.

(define (dump-tokenmachine-to-file prog fn)
  (match prog
    [(haskellize-tokmac-language ,str)
     (with-output-to-file fn
       (lambda () (display str) (newline))
       'replace)]
    ;; If it's an earlier file, pretty print it:
    [(,lang ,prog)
     (with-output-to-file fn
       (lambda () (pretty-print `(,lang ,prog)))
       'replace)]
    [,other (error 'dump-tokenmachine-to-file "invalid input: ~S" prog)]))

;; This dumps to file only when provided the optional string filename argument:
;; The symbolic options are:  'barely-tokens 'almost-tokens 'almost-haskell 'haskell-tokens
(define (run-compiler p . args )
  ;(disp "RUN COMP:" p)
  (let ([filename #f]
	[passes pass-names]
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
		  ;; The pass-names may have already been restricted to be just the TML passes:
                  [(eq? arg 'barely-tokens)
                   (set! passes (list-remove-first 'cleanup-token-machine
				  (list-remove-after 'cleanup-token-machine pass-names)))]
                  [(eq? arg 'almost-tokens)
                   (set! passes (list-remove-first 'deglobalize ;; <- might fizzle
				  (list-remove-first 'cleanup-token-machine
                                     (list-remove-after 'cleanup-token-machine pass-names))))]
                  [(eq? arg 'almost-haskell)
                   (set! passes (remq 'haskellize-tokmac pass-names))]
                  [(eq? arg 'haskell-tokens) (void)]))
	      args)
    (when verbose
	  (printf "Running compiler with pass-names: \n")
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
		    (printf "~a:\n\n" (car names))))
	      (let ((result ((car funs) p)))
		(if verbose
		    (pretty-print result) (newline))
		(loop result (cdr funs) (cdr names)))))))))

;; This one just stops after deglobalize:
(define (compile-to-tokens p . args)
  (apply run-compiler p 'barely-tokens args))
(define (compile-almost-to-tokens p . args)
  (apply run-compiler p 'almost-tokens args))

(define rc run-compiler) ;; shorthand
(define ct compile-to-tokens) ;; shorthand

;; This finishes off the compilation of scheme-format token machine.
;; It's just a front-end to run-compiler that restricts the passes we run over.
(define (assemble-tokmac tm . args)
  (printf "assem tokmac...\n" )
  (let ([starting-place 
	 (match tm
	   [(,lang ,prog ...)
	    (case lang
	      [(add-places-language) 'analyze-places] ;; Not strictly correct.
	      [(deglobalize-lang) 'deglobalize]
	      [(cleanup-token-machine-lang) 'cleanup-token-machine]
	      [(cps-tokmac-lang) 'cps-tokmac]
	      
					;[(haskellize-tokmac-lang) (error...
	      [else 'deglobalize])]
	   [,else 'deglobalize])
	 ])
  (let ((passes (cdr (list-remove-before starting-place pass-names))))
    (disp "Assembling tokmac with passes: " passes)
;    (lambda (tm)
      (fluid-let ([pass-names passes])
	(apply run-compiler tm args)))))

(define at assemble-tokmac) ;; shorthand

(define test
  (lambda (set)
    (fluid-let ([tests 
		 (map (lambda (p) 
			`(base-language '(program ,p)))
		      set)])
      (test-all))))

(define r  ;; shorthand
  (letrec ((loop
	    (case-lambda 
	     [(pass x)
	      (let ((prog  x))
		(fluid-let ((pass-names (list-remove-after pass pass-names)))
		  (parameterize ((tracer #t))
				(test-one prog #f #f))))]
	     [(x) (loop (rac pass-names) x)])))
    loop))

;; Token and later compiler:
(define (tr x)  ;; shorthand
  (let ((prog  x))
    (parameterize ((tracer #t)
		   (game-eval (lambda args 'unspecified))
		   (host-eval (lambda args 'unspecified)))
      (fluid-let ((pass-names (cdr (list-remove-before 'deglobalize pass-names))))
	(test-one prog)))))

;; Temp =============================================================

(newline)

(define (rr) (r '(circle 50 (anchor-at '(30 40))))) ;; shorthand

(define (doit x) ;; shorthand
  (cleanse-world)
  (run-simulation (build-simulation (compile-simulate-nought x))		  
		  20.0))

;; This is my big target program!!
'(define theprog
  '(let* ((R (circle-at 50 '(30 40)))
	 (f (lambda (tot next)
	      (cons (+ (car tot) (sense next))
		    (+ (cdr tot) 1))))
	 (g (lambda (tot) (/ (car tot) (cdr tot))))
	 (avg (smap g (rfold f (cons 0 0) R))))
    (until (pred (lambda (x) (> x 15.3)) avg)
	   R
	   (circle-at 100 '(0 0)))))

(define prog
  '(program
     (bindings (tmp_3 (cons '40 '())) (tmp_1 (cons '30 tmp_3)))
     (socpgm (bindings) (call f_token_result_2))
     (nodepgm
       (tokens
         (f_token_result_2 () (flood token_4))
         (token_4
           ()           
           (if (< (locdiff (loc) tmp_1) 10.0)
               (begin
                 (disp "PROG: blah blah calling elect leader")
                 (elect-leader m_token_result_2))
               '#f))
         (m_token_result_2 ()
                           (disp "PROG: Bang, election finished, got result..")
                           (soc-return (list 'anch this))))
       (startup))))

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
  

(define theprog '(rfold + 0 (rmap sense world)))

;; I'm binding all these little random letter combinations!  Bah!
(define mp;;myprog ;; shorthand
;  '(rfold + 0 (rmap sense (circle-at '(30 40) 10))))
  '(rfold + 0 (rmap sense (khood-at '(30 40) 10))))

;; [2005.09.29] Moved run-simulator-alpha to simulator_alpha.ss
	    
(define ra run-simulator-alpha) ;; shorthand

;;======================================================================

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
    (include "generic/system_tests.ss")
;    ()
))

(define test-this (default-unit-tester "Main compiler units + system tests." these-tests))
(define maintests these-tests)
(define maintest test-this)


'
     (fluid-let ((pass-names
		  (list-remove-after desugar-gradients ;'cps-tokmac
				     (list-remove-before 'cleanup-token-machine pass-names))))
       (disp "PASS NAMES" pass-names)
       (game-eval (lambda args (void)))
       (let ((prog 
	      (r
	       '(tokens 
		 (SOC-start () (gemit gradient))
		 (gradient () 
			   (greturn x (to handler))
			   (grelay))
		 (handler (x) (display " ") (display x))
		 ))))
	 (disp "PROG")
	 (pp prog)
;	 (run-simulator-alpha prog)
	 ))



#;
    (fluid-let ((pass-names '(cleanup-token-machine cps-tokmac closure-convert)))
	 (let ((prog 
		(run-compiler
	       '(tokens 
		 (SOC-start () (printf "result ~a" (subcall tok1 3)))
		 (tok1 (x) (return (* x (+ (subcall tok2) (subcall tok3)))))
		 (tok2 () (return 55))
		 (tok3 () (return 45))
		 ))))
	   (let ((prt (open-output-string)))
	     (display "(" prt)       
	     (run-simulator-alpha prog 'outport prt)
	     (display ")" prt)
	     (read (open-input-string (get-output-string prt))))))



	       '(tokens 
		 (SOC-start () (printf "result ~a" (subcall tok1 3)))
		 (tok1 (x) (return (* x (+ (subcall tok2) (subcall tok3)))))
		 (tok2 () (return 55))
		 (tok3 () (return 45))
		 )


#;    [
     '(fluid-let ((pass-names
		  (list-remove-after desugar-gradients ;'cps-tokmac
				     (list-remove-before 'cleanup-token-machine pass-names))))
       (disp "PASS NAMES" pass-names)
       (let ((prog 
	      (run-compiler
	       '(tokens 
		 (SOC-start () (gemit gradient))
		 (gradient () 
			   (greturn x (to handler))
			   (grelay))
		 (handler (x) (display " ") (display x))
		 ))))
	 (disp "PROG")
	 (pp prog)
;	 (run-simulator-alpha prog)
	 ))
     
     ,(lambda a #t)
     ]





'

(fluid-let ([pass-names
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
      (fluid-let ([pass-names
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
