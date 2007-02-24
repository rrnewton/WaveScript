
;; Now that the program is elaborated, we can translate generic
;; arithmetic ops (g+) into their specialized equivalents (+.).

(module degeneralize-arithmetic mzscheme
  (require "../../../plt/common.ss"
           "../normalize_source/typecheck.ss")
  (provide)
  (chezprovide (degeneralize-arithmetic
		degeneralize
		lift-generics))
  (chezimports)
  (IFCHEZ (begin) (provide degeneralize-arithmetic))

  (define genops '(g+ g- g* g/ g^ gint))

  (define-pass lift-generics
      [Expr (lambda (x fallthru)
	      (match x 
		[(,genop ,[args] ...)
		 (guard (memq genop genops))
		 (let ([tmp (unique-name 'tmp)]
		       [alpha (unique-name 'alpha)])
		   `(let ([,tmp (,genop . ,args)])
		      ,tmp))]
		[,other (fallthru other)]))])

  (define (int x)     (match x [g+ '+_] [g- '-_] [g* '*_] [g/ '/_] [g/ '/_] [g^ '^_] [abs 'absI]))
  (define (float x)   (match x [g+ '+.] [g- '-.] [g* '*.] [g/ '/.] [g/ '/.] [g^ '^.] [abs 'absF]))
  (define (complex x) (match x [g+ '+:] [g- '-:] [g* '*:] [g/ '/:] [g/ '/:] [g^ '^:] [abs 'absC]))

;; Should remove the generic ops from the grammar.
#;  
  (define degeneralize-grammar
    ...
    static-elaborate-grammar)

  (define-pass degeneralize
      [Expr (lambda (x fallthru)
	      (match x
		[(let ([,v ,t (,genop ,[args] ...)]) ,v2)
		 (guard (eq? v v2) (memq genop genops))		 
		 (if (eq? genop 'gint)		     
		     (match (list t (car args))
		       [(Int     (quote ,n))  `(quote ,n)]
		       [(Float   (quote ,n))  `(quote ,(+ n 0.0))]
		       [(Complex (quote ,n))  `(quote ,(+ n 0.0+0.0i))]
		       [,else 
			(if (memq t '(Int Float Complex))
			    (error 'degeneralize-arithmetic
				   "gint should only be used on numeric constants, not: ~s" 
				   (cons 'gint args))
			    (error 'degeneralize-arithmetic
				   "unhandled output type demanded of gint, ~s, expression: ~s"
				   t (cons 'gint args)) )])
;; NOTE: THIS WON'T WORK FOR ABS YET...
;; IT WORKS BASED ON THE RETURN TYPE, WHICH IS AMBIGUOUS FOR ABS.

		     (case t
		       [(Int)     `(,(int     genop) . ,args)]
		       [(Float)   `(,(float   genop) . ,args)]
		       [(Complex) `(,(complex genop) . ,args)]
		       [else (error 'degeneralize-arithmetic
				    "generic operation did not have concrete numeric type after elaboration: ~s"
				    (cons genop args))]
		       ))]
		[,oth (fallthru oth)]))])

  (define (degeneralize-arithmetic p)
    (degeneralize 
     (retypecheck
      (lift-generics p))))
)

