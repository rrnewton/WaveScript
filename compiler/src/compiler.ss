
;;======================================  
(display "Loading main compiler module.  RegionStreams Demo.")
(newline)

;; This is a global variable mutated by the node-programs in the
;; simulator_nought...  Counts total communications received.
;;(define total-messages 0)
;; Moved this *again*.  Now the simulator just defines this (via
;; eval), when it starts running.
     
(define pass-names
  '(verify-regiment
    rename-var
    remove-unquoted-constant                        ;;  5
    remove-complex-constant                         ;;  7
;     uncover-symbol-references                       ;;  8

     ;;introduce-cps  -- removing for the moment
     ;    rename-k
     ;;;;;;;;;;;reduce-primitives.1

;     remove-anonymous-lambda                         ;; 13
     uncover-free                                    ;; 14
     convert-closure                                 ;; 15
     ;optimize-direct-call
     lift-letrec                                     ;; 16

;; HMM: 
;     introduce-closure-primitives                    ;; 17
;     convert-excessive-args                          ;; 18
;     reduce-primitives.1                             ;; 19
;     reduce-primitives.2                             ;; 20

;     normalize-context                               ;; 21
     lift-letrec-body                                ;; 22

;; VERIFY-CORE

;     uncover-return                                  ;; 23
;     uncover-calltype                                ;; 24
;     remove-nonunary-let                             ;; 25
;     uncover-local                                   ;; 26
;     the-return-of-set!                              ;; 27

;     flatten-if                                      ;; 29
     ;clarify-types
    ))



(define pass-names
  '(verify-regiment
    rename-var
    remove-unquoted-constant                        ;;  5
    remove-complex-constant                         ;;  7
    uncover-free                                    ;; 14
;    convert-closure                                 ;; 15
    lift-letrec                                     ;; 16
    lift-letrec-body                                ;; 22
    remove-complex-opera*
    verify-core
    classify-names
    deglobalize
    ))


(define (run-compiler p)
  (let ((funs (map eval pass-names)))
    (let loop ([p p] [funs funs])
      (if (null? funs) p
	  (loop ((car funs) p) (cdr funs))))))

(define test
  (lambda (set)
    (fluid-let ([tests 
		 (map (lambda (p) 
			`(base-language '(program ,p)))
		      set)])
      (test-all))))

(define (r x)
  (let ((prog  x #;(match x
		     [(,lang '(program ,p)) x]
		     [,p `(NOLANG '(program ,p))])))
    (parameterize ((tracer #t))
		  (test-one prog))))

;; These are all the unit-testers accumulated from all the files.
#;(define all-testers
  (list test00
	test01
	test02
	test03
	test04
	test05
	test06	
	testsim
	testgsim))
   
   

;; Temp =============================================================

'(display (list (test00)
	       (test01)
	       (test07)))
(newline)

(define rc run-compiler)

(define (rr) (r '(circle 50 (anchor-at '(30 40)))))

(define (doit x)
  (run-simulation (build-simulation (compile-simulate-nought x)) 2.0))


;; This is my big target program!!
(define theprog
  '(let* ((R (circle-at 50 '(30 40)))
	 (f (lambda (tot next)
	      (cons (+ (car tot) (sense next))
		    (+ (cdr tot) 1))))
	 (g (lambda (tot) (/ (car tot) (cdr tot))))
	 (avg (smap g (rfold f (cons 0 0) R))))
    (until (pred (lambda (x) (> x 15.3)) avg)
	   R
	   (circle-at 100 '(0 0)))))


(define these-tests 
  `( 
;; Urg, this is wrong:
;    [(deep-assq 'startup (run-compiler '(circle-at '(30 40) 50))) (startup)]

    ["Verify that the trivial program produces no token bindings"
     (deep-assq 'tokens (run-compiler '3))   (tokens)]
    ))
      
(define test-this (default-unit-tester "Main compiler unit." these-tests))

(define compilertests these-tests)
(define compilertest test-this)
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
                 (disp "blah blah calling elect leader")
                 (elect-leader m_token_result_2))
               '#f))
         (m_token_result_2 ()
                           (disp "Bang, election finished, got result..")
                           (soc-return (list 'anch this))))
       (startup))))

(define (all-incoming)
  (filter (lambda (ls) (not (null? ls)))
          (map simobject-incoming all-objs)))

(define (g) 
  ;  (define prog (rc '(anchor-at '(30 40))))
  (init-world)
  (let ((res (run-simulation 
              (build-simulation
               (compile-simulate-nought prog)) 10.0)))
    (disp "EXEC FINISHED, HERE WAS PROG:")
    (pretty-print prog)
    res))
