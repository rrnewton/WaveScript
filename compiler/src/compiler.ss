;; This is the core.  Loaded by both the Chez and PLT versions.

;;======================================  
;(display "Loading main compiler module.  RegionStreams Demo.")
;(newline)

(define-regiment-parameter regiment-version 0.58)

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
    ;    convert-closure                                 ;; 15
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

    desugar-gradients
    cleanup-token-machine   ;; Rerun to expand out some stuff.

    desugar-let-stored
    rename-stored
    
    ;    analyze-tokmac-recursion
    ;    inline-tokmac
    cps-tokmac
    closure-convert
    ;    verify-token-machine
    ;    haskellize-tokmac 
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
  (let ([filename #f]
	[passes pass-names])    
    (for-each (lambda (arg)
		(cond
                  [(string? arg) ;; It's an output filename.
                   (set! filename arg)]
                  [(eq? arg 'barely-tokens)
                   (set! passes (list-remove-after 'deglobalize pass-names))]
                  [(eq? arg 'almost-tokens)
                   (set! passes (list-remove-first 'deglobalize
                                                   (list-remove-after 'deglobalize pass-names)))]
                  [(eq? arg 'almost-haskell)
                   (set! passes (remq 'haskellize-tokmac pass-names))]
                  [(eq? arg 'haskell-tokens) (void)]))
	      args)
    (let ((funs (map eval passes)))
      (let loop ([p p] [funs funs])
        (if (null? funs) 
            (begin (if filename (dump-tokenmachine-to-file p filename)
                       p))
(loop ((car funs) p) (cdr funs)))))))

;; This one just stops after deglobalize:
(define (compile-to-tokens p . args)
  (apply run-compiler p 'barely-tokens args))
(define (compile-almost-to-tokens p . args)
  (apply run-compiler p 'almost-tokens args))

(define rc run-compiler) ;; shorthand
(define ct compile-to-tokens) ;; shorthand

;; This finishes off the compilation of scheme-format token machine.
(define (assemble-tokmac tm . args)
  (let ([starting-place 
	 (match tm 
		[(,lang ,prog)
		 (case lang
		   [(deglobalize-lang) 'deglobalize]
		   [(cleanup-token-machine-lang) 'cleanup-token-machine]
		   [(cps-tokmac-lang) 'cps-tokmac]

		   ;[(haskellize-tokmac-lang) (error...
		   [else 'deglobalize])])
	 ])
  (let ((passes (cdr (list-remove-before starting-place pass-names))))
;    (lambda (tm)
      (fluid-let ([pass-names passes])
	(apply run-compiler tm args)))))

(define at assemble-tokmac) ;; shorthand
(define assemble at) ;; shorthand

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
				(test-one prog))))]
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


;; This requires pass21_cleanup-token-machine.ss as well as helpers.ss
;; This handles writing the generated code to a file and loading it back.
;; FLAGS:
;; 'numnodes int -- Set the number of nodes in the world to int.
;; 'outport prt  -- Set the printed output of the simulation to port prt.
;; 'srand int    -- Seed the random number generator with int.
(define run-simulator-alpha
  (letrec ([run-alpha-loop
	    (lambda args
	      (match args
		     ;; This is a weak requirement... 
		     ;; We consider the first arg to represent a token machine if it is a *list*.
		     [(,tm . ,rest) (guard (list? tm))
		      (let ((cleaned tm )) ;;;(cleanup-token-machine tm)))
			(let ([comped (compile-simulate-alpha cleaned)])
			  (let ((out (open-output-file "_genned_node_code.ss" 'replace)))
;			    (printf "Ouputting token machine to file: _genned_node_code.ss~n")
			    (parameterize ([print-level #f]
					   [pretty-maximum-lines #f]
					   [print-graph #t])				    					  
			    (pretty-print comped out)
			    (newline out)
			    (newline out)
			    (display ";; Above is compiled program for this token machine: " out)
			    (newline out)
			    (display "'" out)
			    (pretty-print tm out)
			    (newline out))
			    (close-output-port out))
			  (read-params rest)
			  ))]
		     [(,rest ...) (read-params rest)]
		     ))]
	    [read-params
	     (lambda (params)	       
	       (match params
;		      [,x (guard (disp "read params" params) #f) 3]
		      [() 
		       (load "_genned_node_code.ss")
                       ;; We have to do this because of the module system:
                       (let ((node-code (eval 'node-code)))
                         ;(disp "NODE CODE:" node-code) ;" eq: " (eq? node-code (eval 'node-code)))
                         ;(printf "Node code loaded from file.~n")
                         ;(if (not node-code)  (error 'run-simulator-alpha "node-code not defined!"))
                         (start-alpha-sim node-code 10.0 'simple))]
		      [(numnodes ,n . ,rest)
		       (if (not (integer? n))
			   (error 'run-simulator-alpha
				  "'numnodes switch should be followed by an integer, not: ~a" n))
		       (parameterize ([simalpha-num-nodes n])
				     (read-params rest))]
		      [(outport ,p . ,rest)
		       (if (not (output-port? p))
			   (error 'run-simulator-alpha
				  "'outport switch should be followed by a port object, not: ~a" n))
		       (parameterize ([simalpha-output-port p])
				     (read-params rest))]
		      [(srand ,n . ,rest)
;		       (if (not (integer? n))
;			   (error 'run-simulator-alpha
;				  "'srand switch should be followed by an integer, not: ~a" n))
		       (let ([stored-state #f])
			 (dynamic-wind
			     (lambda () (set! stored-state (reg:get-random-state)))
			     (lambda () (read-params rest))
			     (lambda () (reg:set-random-state! stored-state))))
		       ]))])
	   run-alpha-loop))
	    
(define ra run-simulator-alpha) ;; shorthand

;;======================================================================

;; These are some of our system tests.  They test the compiler and the simulator as a whole.
;; The rest of the system tests are in the files named tests_*.ss
;; But some of the below tests may also be miscellaneous unit tests that require more than one module.
(define these-tests 
  `( 
    ;; Urg, this is wrong:
    ;    [(deep-assq 'startup (run-compiler '(circle-at '(30 40) 50))) (startup)]
    
    ["Verify that the trivial program produces no token bindings but the defaults"
     (filter (lambda (tokbind)
	       (not (memq (car tokbind) '(spread-global global-tree))))
	     (cdr (deep-assq 'tokens (compile-to-tokens '3))))
     ()]

    ["Now we test running the Simulator Alpha on a very simple token machine."
     (let ((prt (open-output-string)))
       (display "(" prt)
       (run-simulator-alpha 
	(cleanup-token-machine '(tokens (node-start () (display " ") (display (my-id)))))
	'outport prt)
       (display ")" prt)
       (read (open-input-string (get-output-string prt))))
     ,(lambda (ls) 	
	(set-equal? (list->set ls)
		    (list->set (cons BASE_ID (cdr (iota 30))))))]


    ;; [2005.05.29] Note tok1 should be statically called and is currently called dynamically!
    ;; Oh duh, that's because all calls go through the dyndispatch table.
     ["Run simulator on simple subcall program." 
      (fluid-let ((pass-names '(cleanup-token-machine cps-tokmac closure-convert)))
	 (let ((prog 
		(cleanup-token-machine (run-compiler
	       '(tokens 
		 (SOC-start () (printf "result ~a" (subcall tok1 3)))
		 (tok1 (x) (return (+ x 300)))
		 )))))
	   
	   (disp "CLEANED")(pretty-print prog)

	   (let ((prt (open-output-string)))
	     (display "(" prt)       
	     (run-simulator-alpha prog 'outport prt)
	     (display ")" prt)
	     (read (open-input-string (get-output-string prt))))))
     (result 303)]

     ,@(let ([commontest 
	     '(let ((prog 
		     (run-compiler
		      '(tokens 
			(SOC-start () (printf "result ~a" (+ (subcall tok1 4) (subcall tok1 3))))
			(tok1 (x) (return (+ x 1000)))
			))))
		(let ((prt (open-output-string)))
		  (display "(" prt)
		  (run-simulator-alpha prog 'outport prt)
		  (display ")" prt)
		  (read (open-input-string (get-output-string prt)))))])	 
	 `(["Add two subcalls (only through cps-tokmac)"
	    (fluid-let ((pass-names '(cleanup-token-machine cps-tokmac )))
	      ,commontest)
	    (result 2007)]
	   ["Same test but now with closure-convert"
	    (fluid-let ((pass-names '(cleanup-token-machine cps-tokmac closure-convert)))
	      ,commontest)
	    (result 2007)]))
     
     ["Testing simple combinations of passes: generate a continuation." 
      (let ((toks (cdr (deep-assq 'tokens 
		 (closure-convert (cleanup-token-machine '(tokens (tok1 () (subcall tok2)))))))))	
	(let ((x (remq 'SOC-start (remq 'node-start (map car toks)))))
	  ;; This is the continuation that was added:
	  (length x)))
      1]
    		

    ))

(define test-this (default-unit-tester "Main compiler unit." these-tests))
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
		 (SOC-start () (emit gradient))
		 (gradient () 
			   (greturn x (to handler))
			   (relay))
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
		 (SOC-start () (emit gradient))
		 (gradient () 
			   (greturn x (to handler))
			   (relay))
		 (handler (x) (display " ") (display x))
		 ))))
	 (disp "PROG")
	 (pp prog)
;	 (run-simulator-alpha prog)
	 ))
     
     ,(lambda a #t)
     ]
