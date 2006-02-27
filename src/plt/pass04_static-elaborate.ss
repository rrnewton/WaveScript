
(module pass04_static-elaborate mzscheme
  (require (lib "include.ss")
           (all-except (lib "compat.ss") flush-output-port)
           "iu-match.ss"
	   (all-except "helpers.ss" test-this these-tests)
           (all-except "regiment_helpers.ss" test-this these-tests))

#; ; This is in helpers now:  
  (define (remq x ls)
    (cond 
      [(null? ls) ls]
      [(eq? x (car ls)) (remq x (cdr ls))]
      [else (cons (car ls) (remq x (cdr ls)))]))
  
;  (define (id x) x)
  
  (include (build-path "generic" "pass04_static-elaborate.ss"))
  
  (provide (all-defined))
  ;	(provide rename-var 
  ;		 test-this these-tests test01 tests01)
  )

; (require pass04_static-elaborate)