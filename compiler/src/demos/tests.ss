;; Ryan Newton
;; Started 2004.03.26
;; Here are some sample programs for my simple demo compiler.



;;;;;;;;; IN My restricted language.
(define prog1
  '(let* ((R (circle-at 50 '(30 40)))
         (f (lambda (tot next)
              (cons (+ (car tot) (sense next))
                    (+ (cdr tot) 1))))
         (g (lambda (tot) (/ (car tot) (cdr tot))))
         (avg (smap g (rfold f (cons 0 0) R))))
    (until (pred (lambda (x) (> x 15.3)) avg)
           R
           (circle-at 100 '(0 0)))))


;;;;;; Pre-compiled to simple form.

(define prog1_core
  '(let* ((a (anchor '(30 40)))
	  (R (circle 50 a))
	  (f (lambda (tot next)
	       (let* ((sum (car tot))
		      (cnt (cdr tot))
		      (sns (sense next))
		      (newsum (+ sum sns))
		      (newcnt (+ cnt 1))
		      (res (cons newsum newcnt)))
		 res)))
	  (g (lambda (tot) 
	       (let* ((sum (car tot))
		      (cnt (cdr tot))
		      (res (/ sum cnt)))
		 res)))	    
	  (start (cons 0 0))
	  (S (rfold f start R))
	  (avg (smap g S)))
     avg))
