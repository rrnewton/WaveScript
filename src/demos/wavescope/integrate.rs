


(define s1 (timer 300))

;; An ever increasing stream:
(define s2 (sintegrate (lambda (#() sum)
			 (tuple sum (+ 1 sum)))
		       0 s1))

(define s3 (lambda (x) (seg_get x 3)))

;(sintegrate (lambda ))

(tuple s2 s3)
