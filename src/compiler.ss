
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
    deglobalize
    ))

(define test
  (lambda (set)
    (fluid-let ([tests 
		 (map (lambda (p) 
			`(base-language '(program ,p)))
		      set)])
      (test-all))))

(define (t x)
  (parameterize ((tracer #t))
		(test-one x)))


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

(define (g) (t '(circle 50 (anchor-at '(30 40)))))

