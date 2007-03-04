":";exec snow -- "$0" "$@"

;;;; This is a reimplementation of a subset of the functionality
;;;; provided by the IU pattern matching facility (match.ss)
;;;;  -Ryan Newton [2006/2007]

;; [2007.03.03] The module passes its simple tests under scm, chez, mzscheme, guile
;; bigloo -- Loads, passes some tests.  Gets some kind of call-with-values 
;;           error on the multiple value test
;; larceny -- Gets a wrong number of arguments error on the same test as bigloo.

(package* iu-match/v0.0.0
 (provide:
  (define* (test-match))
  ;; Can we make a dummy export?  Not currently.
  ;(define-macro* (match e . clauses) 'matcher-undefined)
  )
 ;(require: _syntax-rules)

 (maintainer: "Ryan Newton <ryan.newton at alum.mit.edu>")
 (author:     "Ryan Newton <ryan.newton at alum.mit.edu>")
 (homepage:   "http://snow.iro.umontreal.ca")
 (description: "Pattern matching against lists and vectors.")
 (keywords: pattern-matching data)
 (license: lgpl/v2.1)
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

;------------------------------------------------------------
;; Switch into r5rs mode to get define-syntax.
(cond-expand
 ;; Already there:
 ((or chez petite mzscheme bigloo) (begin))
 (scm  (require 'r5rs))
 (guile (use-syntax (ice-9 syncase)))
 (gambit
  ;; gambit -- ok, load "syntax-case.scm", but how can we reliably find that file?
  (void)
  )
 (else (snow-error "Cannot currently support R5RS macros under this implementation.")))

;------------------------------------------------------------
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
  (snow-error "pattern matching package doesn't support this scheme platform")
  ))


;------------------------------------------------------------

;(display "TESTING: ") (newline) (test-match)
;(display thecode)
;(eval thecode (interaction-environment))

;; Hmm, test* doesn't seem to work for me in snow v1.0.0

(test* 
 (define (add1 x) (+ x 1))

 (expect* (equal? 3   
		  (match 3 (,x x))))
 (expect* (equal? 3   
		  (match '(1 2) ((,x ,y) (+ x y)))))
 (expect* (equal? 200 
		  (match '(1 2) ((,x ,y ,z) (+ x x y)) ((,x ,y) (* 100 y)))))
 (expect* (equal? '(1 2) 
		  (match '(1 2) ((,x ,y ,z) (+ x x y)) (,v v))))
 (expect* (equal? 200
		  (match '(1 2) ((3 ,y) (* 1000 y)) ((1 ,y) (* 100 y)))))
 (expect* (equal? '(3 4)
		  (match '(1 2) ((,(x) ,(y)) (list x y)) (1 3) (2 4))))
 (expect* (equal? '(3 4 5 6) 
		  (match '(1 2) ((,(x y) ,(z w)) (list x y z w)) (1 (values 3 4)) (2 (values 5 6)))))
 (expect* (equal? 2
		  (match '(1 . 2) ((,x . ,y) y))))
 (expect* (equal? '(2 3) 
		  (match '(1 2 3) ((1 ,x* ....) x*))))
 (expect* (equal? #((a b c) (1 2 3)) 
		  (match '((a 1) (b 2) (c 3)) (((,x* ,y*) ....) (vector x* y*)))))
 (expect* (equal? 'yay 
		  (match '((a 1) (b 2) (c 3 4)) (((,x* ,y*) ....) (vector x* y*)) (,_ 'yay))))
 ;; Redirect:
 (expect* (equal? '(3 4) 
		  (match '(1 2 3) ((1 ,(add1 -> x) ,(add1 -> y)) (list x y)))))
 ;; Basic guard:
 (expect* (equal? 33 
		  (match 3 (,x (guard (even? x)) 44) (,y (guard (< y 40) (odd? y)) 33))))
 
 ;; Redirect and ellipses.
;      (expect* (equal? (match '(1 2 3) ((1 ,(add1 -> x*) ....) x*)) (3 4)))

;       ;; Make sure we keep those bindings straight.
;       ((match '((a 2 9) (b 2 99) (c 2 999))
; 	 (((,x 2 ,(y)) ....) (vector x y))
; 	 (,n (add1 n)))
;        )


 )
