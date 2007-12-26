

;;; EXPERIMENTAL - playing around with the idea of supporting Larceny, but not even close yet.


#|
 echo '(import (err5rs load)) (load "main_larceny.ss")' | larceny -err5rs
|#

;(require 'syntax-case)
;(require 'r6rsmode)
;(require 'err5rsmode)
;(display "Switched modes\n")


(import  
 (for (rnrs base) expand run (meta 2))
 (rnrs io simple)
 (for (rnrs syntax-case) expand run (meta 2))

 (err5rs load)
 (for (rnrs r5rs) expand run) ;; backwards compat
 )

(define-syntax test (lambda (x) (syntax-case x () [(_ hmm) #'hmm])))


;; Dummy module syntax.
(define-syntax module
  (syntax-rules (require provide chezprovide chezimports)
    [(_ name parent (require __ ...) (provide exports ...) (chezimports imp ...) exp ...)
     (begin exp ...)]
    [(_ name parent (require __ ...) (provide exports ...) (chezprovide chezexports ...) (chezimports imp ...) exp ...)
     (begin exp ...)]))

(module foo mzscheme (require) (provide) (chezimports)
   (define w 11111)
   (define xx 399)
   )

(define z 9292)


(library (ellipse-pred)
  (export symbolic-identifier=? ellipsis? delay-values 
	  extend-backquote simple-eval syntax-error
	  andmap call/1cc
	  )
  (import (for (rnrs) run expand)
	  (rnrs r5rs) (rnrs eval))

  (define symbolic-identifier=?
    (lambda (x y)
      (eq? (syntax->datum x)
	   (syntax->datum y))))
	 
  (define ellipsis?
    (lambda (x)
      (and (identifier? x) (symbolic-identifier=? x #'(... ...)))))

  (define-syntax delay-values
    (syntax-rules ()
      ((_ e)
       (let ((vals #f))
	 (lambda ()
	   (if vals (apply values vals)
	       (call-with-values (lambda () e)
		 (lambda args (set! vals args) (apply values args))))
	   )))))

  ;; A dummy binding that uses the normal backquote
  (define-syntax extend-backquote
    (lambda (x)
      (syntax-case x () ((kwd Template Exp ...) #'(begin Exp ...)))))

  ;(define (simple-eval x) (eval x (environment '(r6rs))))
  (define (simple-eval x) (eval x (environment '(rnrs))))

  (define-syntax syntax-error
    (syntax-rules ()
      [(kwd form msg) 
       (syntax-violation #f msg form)]))
  
  (define (andmap fun ls)
    (if (null? ls) #t
	(and (fun (car ls)) (andmap fun (cdr ls)))))

  (define call/1cc call/cc)

)

(import (for (ellipse-pred) run expand))


;(load "larceny/larc_match.ss") ;; this seems to work

(load "generic/util/rn-match.r5rs") ;(test-match)
(display "TEST: ")(display (match 3 [3 9] [4 2]))(newline)

;(dump-interactive-heap "larc.heap")
;(dump-heap "larc.heap" (lambda args (display "Starting from heap... YAY \n") (display xx) (newline)))

