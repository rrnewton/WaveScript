;; This is the core.  Loaded by both the Chez and PLT versions.

;;======================================  
(display "Loading main compiler module.  RegionStreams Demo.")
(newline)

(define regiment-version 0.5)

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

  ;    verify-token-machine


    ; analyze??
    ; ?? 
    ; inline??
    ; cps-tokmac
    haskellize-tokmac 
    ))

;;======================================================================

(define these-tests 
  `( 
;; Urg, this is wrong:
;    [(deep-assq 'startup (run-compiler '(circle-at '(30 40) 50))) (startup)]

    ["Verify that the trivial program produces no token bindings but the defaults"
     (filter (lambda (tokbind)
	       (not (memq (car tokbind) '(spread-global global-tree))))
	     (cdr (deep-assq 'tokens (compile-to-tokens '3))))
     ()]
    ))

(define test-this (default-unit-tester "Main compiler unit." these-tests))


  
;; ==================================================================
;; Functions for input/output to filesystem and for invoking compiler.

(define (dump-tokenmachine-to-file prog fn)
  (match prog
   [(haskellize-tokmac-lang ,str)
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
;; The options are:  'barely-tokens 'almost-tokens 'almost-haskell 'haskell-tokens
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

(define (r x)  ;; shorthand
  (let ((prog  x))
    (parameterize ((tracer #t))
		  (test-one prog))))
  
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


(define compilertests these-tests)
(define compilertest test-this)

;; These are some temporary diagnostic functions:
(define (all-incoming) ;; shorthand
  (filter (lambda (ls) (not (null? ls)))
          (map simobject-incoming all-objs)))
(define (all-homepage) ;; shorthand
  (filter (lambda (ls) (not (null? ls)))
          (map simobject-homepage all-objs)))

(define (g) ;; shorthand
  ;  (define prog (rc '(anchor-at '(30 40))))
  (init-world)
  (let ((res (run-simulation 
              (build-simulation
               (compile-simulate-nought prog)) 3.0)))
    (disp "EXEC FINISHED, HERE WAS PROG:")
    (pretty-print prog)
    res))


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

'(define theprog
  '(program
    (bindings
     [tmp_23 (cons '40 '())]
     [tmp_21 (cons '30 tmp_23)])
    (socpgm (bindings) (call f_token_result_22))
    (nodepgm
     (tokens
      [f_token_result_22 () (begin (disp "  nodeprog: ftok call flood")
				   (flood token_25))]
      [token_25 () (begin 
		     (disp "  nodeprog: tok25 elect leader")
		     (if (< (locdiff (loc) tmp_21) 10.0)
			 (elect-leader m_token_result_22)
			 '#f))]
      [m_token_result_22 () (begin
			      (disp "  nodeprog: mtokres lightup")
			      (light-up 0 255 255))]
      
      [m_token_result_22 () (begin 
			      (disp "  nodeprog: mtokres soc-reeturn")
			      (soc-return (list 'anch this)))])
      (startup))))

(define (t1) ;; shorthand
  (cleanse-world)
  (run-simulation 
   (build-simulation (csn theprog)) 1.0))

(define (t2) ;; shorthand
  (cleanse-world)
  (run-simulation-stream
   (build-simulation (csn (cadadr (rc '(anchor-at '(30 40)))))) 12.0))

;; I'm binding all these little random letter combinations!  Bah!
(define mp;;myprog ;; shorthand
;  '(rfold + 0 (rmap sense (circle-at '(30 40) 10))))
  '(rfold + 0 (rmap sense (khood-at '(30 40) 10))))

;; Runs a token machine simulation.
(define (run-token-machine x) 
  (run-simulation (build-simulation x) 2.0))

(define rtm run-token-machine) ;; shorthand


(define tmprog
  '(deglobalize-lang
    '(program
      (bindings
       (tmpunknpr_13 (cons '40 '()))
       (tmp_4 (cons '30 tmpunknpr_13)))
      (socpgm (bindings) (void))
      (nodepgm
       (tokens
	(f_token_tmpanch_8 () (flood constok_15))
	(constok_15
	 ()
           (if (< (locdiff (loc) tmp_4) 10.0)
               (elect-leader m_token_tmpanch_8)
               '#f))
	(f_token_tmpanch_8 () (draw-mark tmp_4 (rgb 0 100 100)))
	(m_token_tmpanch_8 () (light-up 0 255 255))
	(m_token_tmpanch_8 () (call f_token_tmpcirc_9))
	(f_token_tmpcirc_9 () 
			   (begin (disp "about to emit to mtmpcirc" this)
			   (emit m_token_tmpcirc_9 this)))
	(f_token_tmpcirc_9 () (draw-circle (loc) 20))
	(m_token_tmpcirc_9 (v) (light-up 0 100 100))
	(m_token_tmpcirc_9 (v) (if (< (dist) '10) (relay)))
	(tmpfunc_10 (a_1) (lazy-letrec () (call f_token_result_5)))
	(f_token_result_5 () (local-sense))
	(m_token_tmpcirc_9 (v) 
			   (begin (disp "bout to call tmpsig " v)
				  (call f_token_tmpsig_11 v)))
	(f_token_tmpsig_11
           (v)
;	   (disp "OK, about to call this damn plus fun")
	   (begin 
	   (disp "bout to call plus We got this arg." v)
           (call m_token_tmpsig_11 		 
		 (call tmpfunc_10 v))))
         (tmpfunc_12
	  (a_3 b_2)
	  (lazy-letrec ((result_6 (+ a_3 b_2))) (call result_6)))
         (m_token_tmpsig_11 (v) (call f_token_result_7 v))
         (f_token_result_7
	  (v)
           (return
	    v
	    (to m_token_result_7)
	    (via m_token_tmpsig_11)
	    (seed '0)
	    (aggr tmpfunc_12)))
         (m_token_result_7 (v) (soc-return v)))
       (startup f_token_tmpanch_8)))))


;; Sigh, first class tokens:
;(r '(rmap (lambda (x) (rmap (lambda (y) y) world)) world)) 
