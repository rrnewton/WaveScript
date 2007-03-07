
;;;; .title       Desugar-Misc: Desugar miscellaneous syntactic sugars.
;;;; .author      Ryan Newton
;;;; This takes away and, or, list, and some other sugars.


(module desugar-misc mzscheme
  (require "../../../plt/common.ss"
	   "eta-primitives.ss")
  (provide desugar-misc desugar-misc-grammar test-desugar-misc)
  (chezimports)

  ;; Like remove primitive but throws an error if not found.
  (define (myremove x ls)
    (cond
     [(null? ls) (error 'myremove "element not found: ~s" x)]
     [(equal? x (car ls)) (cdr ls)]
     [else (cons (car ls) (myremove x (cdr ls)))]
     ))

  (define desugar-misc-grammar 
    ;(myremove '(Prim 'dataFile)
    (myremove '(Prim 'and) 
    (myremove '(Prim 'or) 
    (myremove '(Prim 'head) 
    (myremove '(Prim 'tail) 
    (myremove '(Prim 'show-and-string-append)
	      eta-primitives-grammar))))))

  (define-pass desugar-misc
    [OutputGrammar desugar-misc-grammar]
    [Expr/Types
     (letrec ([processExpr 
	     (lambda (x tenv fallthrough)
	       (match x

	  ;; A bit of sugar.
          ;; This just expands "lists" into a bunch of cons cells.
	  [(list ,[rands] ...)
	   (processExpr (match rands
			   [() ''()]
			   [(,a . ,[b]) `(cons ,a ,b)])
			tenv
			fallthrough)]

	  ;; More sugar.
	  ;; Don't really have a syntax for this in WaveScript:
	  [(or ,[rands] ...)
	   (processExpr (match rands
			   [() ''#f]
			   [(,a . ,[b]) `(if ,a '#t ,b)])
			tenv
			 fallthrough)]
	  [(and ,[rands] ...)
	   (processExpr (match rands
			   [() ''#t]
			   [(,a . ,[b]) `(if ,a ,b '#f)])
			tenv
			fallthrough)]

	  [(head ,[x]) `(car ,x)]
	  [(tail ,[x]) `(cdr ,x)]

	  [(show-and-string-append ,[a] ,[b])
	   `(string-append (show ,a) (show ,b))]

	  ;; For now we just expand this into the forloop.
	  ;; Might want to do something else later.
	  [(sigseg_foreach ,[f] ,[s])
	   (let ([i (unique-name 'i)]
		 [tmp (unique-name 'tmp)])
	     `(letrec ([,tmp ,(recover-type s tenv) ,s])
		(for (,i 0 (- (width ,tmp) 1))
		    (app ,f (seg-get ,tmp ,i))
		  )))]

	  ;; THIS IS INVALID FOR OUR TYPE SYSTEM:
	  ;; It might work if the program were already typed, but future retype-checking will break:
	  
	  ;; TODO: should remove:
	  ;; I have mixed feelings about this (makes intermediate programs hard to read)
	  [(let ([,x ,t ,[y]] ...) ,[body])
	   `(app (lambda ,x ,t ,body) ,y ...)]
	  
	  [,other (fallthrough other tenv)]
	  ))])
     processExpr)])

(define test-desugar-misc
  (default-unit-tester " 1: Desugar-misc: remove miscellaneous syntactic sugar"
    `(
      )))

) ; End module