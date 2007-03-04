":";exec snow -- "$0" "$@"

(package* testmatch/v0.0.0
	  (require: iu-match/v0)
 )

;;;============================================================================

(define (add1 x) (+ x 1))
(define* (test-match)
  (for-each 
      (lambda (pr)
	(display "   Test: ") (write (car pr)) (newline)
	(if (equal? (eval (car pr) (interaction-environment)) ;(scheme-report-environment 5)
		    (cadr pr))
	    (begin (display "-- Passed." ) (newline))
	    (begin (display "-- FAILED." ) (newline))
	    ))
    '(   
      ((match 3 (,x x)) 3)

      ((match '(1 2) ((,x ,y) (+ x y))) 3)
      
      ((match '(1 2) ((,x ,y ,z) (+ x x y)) ((,x ,y) (* 100 y))) 200)
      
      ((match '(1 2) ((,x ,y ,z) (+ x x y)) (,v v)) (1 2))

      ((match '(1 2) ((3 ,y) (* 1000 y)) ((1 ,y) (* 100 y))) 200)

      ((match '(1 2) ((,(x) ,(y)) (list x y)) (1 3) (2 4)) (3 4))

      ((match '(1 2) ((,(x y) ,(z w)) (list x y z w)) (1 (values 3 4)) (2 (values 5 6)))
       (3 4 5 6))

      ((match '(1 . 2) ((,x . ,y) y)) 2)

      ((match '(1 2 3) ((1 ,x* ....) x*)) (2 3))
      ((match '((a 1) (b 2) (c 3)) (((,x* ,y*) ....) (vector x* y*))) #((a b c) (1 2 3)))
      ((match '((a 1) (b 2) (c 3 4)) (((,x* ,y*) ....) (vector x* y*)) (,_ 'yay)) yay)

      ;; Redirect:
      ((match '(1 2 3) ((1 ,(add1 -> x) ,(add1 -> y)) (list x y))) (3 4))

      ;; Basic guard:
      ((match 3 (,x (guard (even? x)) 44) (,y (guard (< y 40) (odd? y)) 33)) 33)

      ;; Redirect and ellipses.
;      ((match '(1 2 3) ((1 ,(add1 -> x*) ....) x*)) (3 4))

;       ;; Make sure we keep those bindings straight.
;       ((match '((a 2 9) (b 2 99) (c 2 999))
; 	 (((,x 2 ,(y)) ....) (vector x y))
; 	 (,n (add1 n)))
;        )

      )))


(test-match)

(newline)(display "Also running not with eval: ")

(display (match 3 (5 "zeroth")(3 "first") (4 "second")))
(newline)
