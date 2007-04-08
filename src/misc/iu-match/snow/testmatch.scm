":";exec snow -- "$0" "$@"

;;;; This is a very simple script to make sure we can import the
;;;; iu-match package and run it.

(package* testmatch/v0
	  (require: iu-match/v0)
 )

;;;============================================================================

(define* (add1 x) (+ x 1))
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

      ((match '(1 2 3) ((1 ,x* _...) x*)) (2 3))
      ((match '((a 1) (b 2) (c 3)) (((,x* ,y*) _...) (vector x* y*))) #((a b c) (1 2 3)))
      ((match '((a 1) (b 2) (c 3 4)) (((,x* ,y*) _...) (vector x* y*)) (,_ 'yay)) yay)

      ;; Redirect:
      ((match '(1 2 3) ((1 ,(add1 -> x) ,(add1 -> y)) (list x y))) (3 4))

      ;; Basic guard:
      ((match 3 (,x (guard (even? x)) 44) (,y (guard (< y 40) (odd? y)) 33)) 33)

      ;; Redirect and ellipses.
;     ((match '(1 2 3) ((1 ,(add1 -> x*) _...) x*)) (3 4))

;       ;; Make sure we keep those bindings straight.
;       ((match '((a 2 9) (b 2 99) (c 2 999))
; 	 (((,x 2 ,(y)) _...) (vector x y))
; 	 (,n (add1 n)))
;        )

      )))


;; TESTS TO ADD:
;; Make sure we only evaluate guards and catas ONCE:
 '(match '(timer 2 3.0)
    [(,prim ,rand*  ...)
     (guard (printf "ACK ~s\n" prim))
     (printf "HMM: ~s\n" rand*)
     3993]
    [,other 'FAILLLLLLLLLED])

'
(let ([acc '()])
  (match '(let ([a int 1] [b float 2] [c complex 9]) bod)
    ((let ((,x* ,y* ,z*) ...) ,bod) 
     (guard (set! acc (cons (list x* y* z* bod) acc)))
     (vector x* y* z* bod acc))))

'
;; This breaks:
(let ([acc '()])
  (match '(let ([a int 1] [b float 2] [c complex 9]) bod)
    ((let ((,x* ,y* ,[z*]) ...) ,bod)
     (guard (set! acc (cons (list x* y* z* bod) acc)))
     (vector x* y* z* bod acc))
    (,oth (add1 oth))))

'
;; Ellipses in vector patterns:
(match #(1 3 4 5 99) [#(,z ... ,y) (vector z y )])


; (test-match)
; (newline)(display "Also running not with eval, should get 'first': ")
; (display (match 3 (5 "zeroth")(3 "first") (4 "second")))
; (newline)

(display "Remember to 'export SNOW_TEST=testmatch' when running this file.")(newline)
(test* 
 (expect* (equal?         3   (match 3 (,x x))))
 (expect* (equal?         3   (match '(1 2) ((,x ,y) (+ x y)))))
 (expect* (equal?       200   (match '(1 2) ((,x ,y ,z) (+ x x y)) ((,x ,y) (* 100 y)))))
 (expect* (equal?     '(1 2)  (match '(1 2) ((,x ,y ,z) (+ x x y)) (,v v))))
 (expect* (equal?       200   (match '(1 2) ((3 ,y) (* 1000 y)) ((1 ,y) (* 100 y)))))
 (expect* (equal?     '(3 4)  (match '(1 2) ((,(x) ,(y)) (list x y)) (1 3) (2 4))))

 (expect* (equal?         2   (match '(1 . 2) ((,x . ,y) y))))
 (expect* (equal?     '(2 3)  (match '(1 2 3) ((1 ,x* _...) x*))))
 (expect* (equal? '#((a b c) (1 2 3))  
                              (match '((a 1) (b 2) (c 3)) (((,x* ,y*) _...) (vector x* y*)))))
 (expect* (equal?      'yay   (match '((a 1) (b 2) (c 3 4)) (((,x* ,y*) _...) (vector x* y*)) (,_ 'yay))))
 ;; Redirect:
 (expect* (equal?     '(3 4)  (match '(1 2 3) ((1 ,(add1 -> x) ,(add1 -> y)) (list x y)))))
 ;; Basic guard:
 (expect* (equal?        33   (match 3 (,x (guard (even? x)) 44) (,y (guard (< y 40) (odd? y)) 33))))

 ;; This is the test that kills bigloo, scheme48, stklos, gauche
 ;; I think we just can't support multiple value returns:
 (expect* (equal? '(3 4 5 6)  (match '(1 2) ((,(x y) ,(z w)) (list x y z w)) (1 (values 3 4)) (2 (values 5 6)))))
 
 ;; Redirect and ellipses.
;      (expect* (equal? (match '(1 2 3) ((1 ,(add1 -> x*) _...) x*)) (3 4)))

;       ;; Make sure we keep those bindings straight.
;       ((match '((a 2 9) (b 2 99) (c 2 999))
; 	 (((,x 2 ,(y)) _...) (vector x y))
; 	 (,n (add1 n)))
;        )
 )
