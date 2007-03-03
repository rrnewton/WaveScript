":";exec snowrun -- "$0" "$@"

;; This passes tests under scm, chez, mzscheme, guile

;; bigloo -- Loads, passes some tests.  Gets some kind of call-with-values 
;;           error on the multiple value test
;; larceny -- Gets a wrong number of arguments error on the same test as bigloo.

(package* snow-match v0.0.1
 (provide:
  (define* (test-match))
  ;; Can we make a dummy export?  Not currently.
  ;(define-macro* (match e . clauses) 'matcher-undefined)
  )
 ;(require: _syntax-rules)
 )

(display "Loading pattern matcher in scheme system \"")
(display (cond-expand
	  (gambit 'gambit)
	  (bigloo 'bigloo)
	  (scm 'scm)
	  (guile 'guile)
	  (mzscheme 'mzscheme)
	  (gauche 'gauche)
	  (chez 'chez)
	  (petite 'petite)
	  (else 'unknown))) (display #\") (newline)

;(define thecode (read (open-input-file "match.r5rs")))

;; Switch into r5rs mode.
(cond-expand
 (scm  (require 'r5rs))
 (guile (use-syntax (ice-9 syncase)))
 (gambit
  ;; gambit -- ok, load "syntax-case.scm"  
  (void)
  )
 (else (begin)))


;; Now load the syntax-transformers into the top level.
(cond-expand
; (chez     (include "match.chez"))
; (mzscheme (include "match.plt"))
; ((or scm gambit) (eval thecode (interaction-environment)))
 ((or scm guile chez mzscheme mit bigloo)
  ;(include "match.r5rs")
  (load "match.r5rs")
  )
 (else 
  (error 'match "pattern matching package doesn't support this scheme platform")
  ))

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


(display "TESTING: ") (newline) (test-match)
;(display thecode)
;(eval thecode (interaction-environment))
