;;; Lang 00: trivial regiment stubs
;===============================================================================

;; make-bignum assumes 2's complement numbers:
(define-language
  'regiment-stub-evaluator
  (make-begin
    `(,(base-language 'return)

      (define field 
	(make-n-list 5000 (lambda (_) (list (random 100) (random 100)))))
      
      (define make-bignum
	(let* ([pow32 (expt 2 32)]
	       [convert-to-unsigned
		(lambda (n)
		  (if (< n 0) (+ pow32 n) n))])
	  (lambda (b v)
	    ((if b - (lambda (x) x))
	     (let loop ([i 0] [p2 1])
                (if (= i (vector-length v))
                    0
                    (+ (* (convert-to-unsigned (vector-ref v i)) p2)
                       (loop (add1 i) (* p2 pow32)))))))))

       (define (circle-at pos rad)
	 )

)))



