
;;;; .title       Desugar-Misc: Desugar miscellaneous syntactic sugars.
;;;; .author      Ryan Newton
;;;; This takes away let*, list, and some other sugars.


(module desugar-misc mzscheme
  (require "../../../plt/common.ss")
  (provide desugar-misc desugar-misc-grammar test-desugar-misc)
  (chezimports)

  (define desugar-misc-grammar initial_regiment_grammar)

  (define-pass desugar-misc
    [OutputGrammar desugar-misc-grammar]
    [Expr 
     (letrec ([processExpr 
	     (lambda (x fallthrough)
	       (match x

	  ;; A bit of sugar.
          ;; This just expands "lists" into a bunch of cons cells.
	  [(list ,[rands] ...)
	   (processExpr (match rands
			   [() ''()]
			   [(,a . ,[b]) `(cons ,a ,b)])
			 fallthrough)]

	  ;; More sugar.
	  ;; Don't really have a syntax for this in WaveScript:
	  [(or ,[rands] ...)
	   (processExpr (match rands
			   [() ''#f]
			   [(,a . ,[b]) `(if ,a '#t ,b)])
			 fallthrough)]
	  [(and ,[rands] ...)
	   (processExpr (match rands
			   [() ''#t]
			   [(,a . ,[b]) `(if ,a ,b '#f)])
			 fallthrough)]

	  ;; THIS IS INVALID FOR OUR TYPE SYSTEM:
	  ;; It might work if the program were already typed, but future retype-checking will break:
	  
	  ;; I have mixed feelings about this (makes intermediate programs hard to read)
	  [(let ([,x ,t ,[y]] ...) ,[body])
	   `(app (lambda ,x ,t ,body) ,y ...)
	   ]

	  ;; [2006.09.19] Desugar let* at this point:
	  [(let* ,binds ,expr)
	   (process-expr 
	    (match  binds
	      [() expr]
	      [(,bind . ,[rest])
	       `(letrec (,bind) ,rest)])
	    env  type-env)]
	  
	  [,other (fallthrough other)]
	  ))])
     processExpr)])

(define test-desugar-misc
  (default-unit-tester " 1: Eta-Primitives: remove non-operator usages of primitive names."
    `(
      ["Simple test of eta-primitives"
       (eta-primitives '(base-language
			 '(program
			      (rfold + 0 (rmap nodeid (khood (anchor-at 50 10) 2)))
			    (Signal Int))))
       (eta-primitives-language
	'(program
	     (rfold
	      (lambda (a b) (Int Int) (+ a b))
	      0
	      (rmap
	       (lambda (a) (Node) (nodeid a))
	       (khood (anchor-at 50 10) 2)))
	   (Signal Int)))]
      )))
)