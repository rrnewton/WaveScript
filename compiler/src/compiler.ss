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
    ;    verify-token-machine
    ;    haskellize-tokmac 
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

;; Token Run:  
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


(define compilertests these-tests)
(define compilertest test-this)

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





(define (t)
  (compile-simulate-alpha
   '(cps-tokmac-lang
  '(program
     (bindings (result_2 '3))
     (nodepgm
       (tokens
         (global-tree
           subtok_ind
           (g_parent g_origin g_hopcount g_version)
           (stored
             (storedgparent_8 #0='#f)
             (storedgorigin_7 #1='#f)
             (storedghopcount_6 #2='#f)
             (storedgversion_5 #3='#f))
           (if (if (not storedghopcount_6)
                   '#t
                   (if (= '0 g_hopcount)
                       (if (> g_version storedgversion_5)
                           (if (= g_version storedgversion_5)
                               (< g_hopcount storedghopcount_6)
                               '#f)
                           '#f)
                       '#f))
               (begin (bcast
                        (tok global-tree subtok_ind)
                        (my-id)
                        g_origin
                        (+ '1 g_hopcount)
                        g_version)
                      (if (not (= g_hopcount '0))
                          (begin (set! storedgparent_8 g_parent)
                                 (set! storedgorigin_7 g_origin)
                                 (set! storedghopcount_6 g_hopcount)
                                 (set! storedgversion_5 g_version)
                                 #4=(void))
                          (void))
                      #4#)
               (void)))
         (spread-global
           subtok_ind
           (g_parent g_origin g_hopcount g_version)
           (stored
             (storedgparent_14 #0#)
             (storedgorigin_13 #1#)
             (storedghopcount_12 #2#)
             (storedgversion_11 #3#)
             (ver_10 (void))
             (storedliftoption_9 '#f))
           (if (if (not storedghopcount_12)
                   '#t
                   (if (= '0 g_hopcount)
                       (if (> g_version storedgversion_11)
                           (if (= g_version storedgversion_11)
                               (< g_hopcount storedghopcount_12)
                               '#f)
                           '#f)
                       '#f))
               (begin (if (not storedliftoption_9)
                          (begin (set! storedliftoption_9 '#t)
                                 (set! ver_10 '0)
                                 #4#))
                      (set! ver_10 (+ '1 ver_10))
                      (bcast (tok global-tree '0) (my-id) '1 ver_10)
                      (timed-call
                        1000
                        (tok spread-global 0)
                        '#f
                        '#f
                        '0
                        '#f)
                      (if (not (= g_hopcount '0))
                          (begin (set! storedgparent_14 g_parent)
                                 (set! storedgorigin_13 g_origin)
                                 (set! storedghopcount_12 g_hopcount)
                                 (set! storedgversion_11 g_version)
                                 #4#)
                          (void))
                      #4#)
               (void)))
         (node-start subtok_ind () (stored) (void))
         (soc-start
           subtok_ind
           ()
           (stored)
           (begin (void)
                  (soc-return result_2)
                  (soc-finished)
                  'multiple-bindings-for-token
                  #4#))))))))


;; Sigh, first class tokens:
;(r '(rmap (lambda (x) (rmap (lambda (y) y) world)) world)) 
  
(define ra ;; shorthand
  (case-lambda 
   [(tm)
    (let ((cleaned (cleanup-token-machine tm)))
      (let ([comped (compile-simulate-alpha cleaned)])
	(slist->file (list comped) "_genned_node_code.ss" 'pretty)
	(ra)))]
   [()
    (load "_genned_node_code.ss")
    (disp "NODE CODE:" node-code "global: " (eval 'node-code) " eq: " (eq? node-code (eval 'node-code)))
    (if (not node-code)  (error 'ra "node-code not defined!"))
    (start-alpha-sim node-code 10.0 'simple)]
   ))

;(ra '(tokens))


(define theprog '(rfold + 0 (rmap sense world)))

;; I'm binding all these little random letter combinations!  Bah!
(define mp;;myprog ;; shorthand
;  '(rfold + 0 (rmap sense (circle-at '(30 40) 10))))
  '(rfold + 0 (rmap sense (khood-at '(30 40) 10))))
