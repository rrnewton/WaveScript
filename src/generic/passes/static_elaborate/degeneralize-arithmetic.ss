
;; Now that the program is elaborated, we can translate generic
;; arithmetic ops (g+) into their specialized equivalents (+.).

(module degeneralize-arithmetic mzscheme
  (require "../../../plt/common.ss"
           "../normalize_source/typecheck.ss"
	   "../static_elaborate/static-elaborate.ss"
	   )
  (provide degeneralize-arithmetic
	   degeneralize-arithmetic-grammar
	   degeneralize
	   lift-generics)
  (chezprovide ) ;; Wait, why was this stuff provided through here, was that for hiding?
  (chezimports )
  ;(IFCHEZ (begin) (provide degeneralize-arithmetic))

  (define-pass lift-generics
      [Expr (lambda (x fallthru)
	      (match x 
		[(,genop ,[args] ...)
		 (guard (assq genop generic-arith-primitives))
		 (let ([tmp (unique-name 'tmp)]
		       [alpha (unique-name 'alpha)])
		   `(let ([,tmp ',alpha (,genop . ,args)])
		      ,tmp))]
		[,other (fallthru other)]))])

  (define (int x)     (match x [g+ '_+_] [g- '_-_] [g* '*_] [g/ '/_] [g^ '^_] [abs 'absI]))
  (define (float x)   (match x [g+ '+.] [g- '-.] [g* '*.] [g/ '/.] [g^ '^.] [abs 'absF]))
  (define (double x)  (match x [g+ '+D] [g- '-D] [g* '*D] [g/ '/D] [g^ '^D] [abs 'absD]))
  (define (complex x) (match x [g+ '+:] [g- '-:] [g* '*:] [g/ '/:] [g^ '^:] [abs 'absC]))

  (define (int16 x)   (match x [g+ '_+I16] [g- '_-I16] [g* '*I16] [g/ '/I16] [g^ '^I16] [abs 'abs16]))
  (define (int64 x)   (match x [g+ '_+I64] [g- '_-I64] [g* '*I64] [g/ '/I64] [g^ '^I64] [abs 'abs64]))
  
  (define degeneralize-arithmetic-grammar
    (filter (lambda (x)
	      (match x
		[(Prim ',p) (not (assq p generic-arith-primitives))]
		[,else #t]))
	static-elaborate-grammar))

  (define-pass degeneralize
      [Expr (lambda (x fallthru)
	      (match x
		[(let ([,v ,t (,genop ,[args] ...)]) ,v2)
		 (guard (eq? v v2) (assq genop generic-arith-primitives))		 
		 (if (eq? genop 'gint)		     
		     ;; Strip annotations so they don't throw off our pattern matching:
		     (match (list t (strip-annotations (car args)))
		       [(Int16   (quote ,n))  
			(ASSERT (constant-typeable-as? n 'Int16))
			`(assert-type Int16 (quote ,n))
			;(quote Int16 ,n)
			]

		       [(Int64   (quote ,n))  
			(ASSERT (constant-typeable-as? n 'Int64))
			`(assert-type Int64 (quote ,n))]

		       ;; [2007.07.12] Looks like these three cases aren't strictly *necessary*
		       [(Int     (quote ,n))  `(quote ,n)]
		       [(Float   (quote ,n))  `(quote ,(+ n 0.0))]
		       [(Complex (quote ,n))  `(quote ,(+ n 0.0+0.0i))]

		       [(Int     ,e)  e]
		       [(Int64   ,e)  `(intToInt64 ,e)]
		       [(Int16   ,e)  
			(error 'degeneralize-arithmetic
			       "cannot currently use gint with an arbitrary expression and output type Int16, it might overflow: ~s"
			       `(gint ,e))]

		       [(Float   ,e)  `(intToFloat ,e)]
		       [(Double  ,e)  `(intToDouble ,e)]
		       [(Complex ,e)  `(intToComplex ,e)]
		       
		       ;; DANGER: FIXME: Don't know if this is a good idea:
		       [((NUM ,_) ,e) e]

		       [,else 
			(error 'degeneralize-arithmetic
			       "unhandled output type demanded of gint, ~s, expression: ~s"
			       t (cons 'gint args))])
;; NOTE: THIS WON'T WORK FOR ABS YET...
;; IT WORKS BASED ON THE RETURN TYPE, WHICH IS AMBIGUOUS FOR ABS.

		     (case t
		       [(Int)     `(,(int     genop) . ,args)]
		       [(Int16)   `(,(int16   genop) . ,args)]
		       [(Int64)   `(,(int64   genop) . ,args)]
		       [(Float)   `(,(float   genop) . ,args)]
		       [(Double)  `(,(double  genop) . ,args)]
		       [(Complex) `(,(complex genop) . ,args)]

		       ;; Defaulting to INT: TEMPTOGGLE:
		       ;; FIXME: Haven't fully verified the safety of this yet.
		       [else      `(,(int     genop) . ,args)]
#;
		       [else (error 'degeneralize-arithmetic
				    "generic operation did not have concrete numeric type after elaboration: ~s"
				    (cons genop args))]
		       ))]
		[,oth (fallthru oth)]))])

  (define (degeneralize-arithmetic p)
    (degeneralize 
     ;; Let bound poly should be off, then we get the right types for the generics.
     (parameterize ([inferencer-enable-LUB #f]
		    [inferencer-let-bound-poly #f])
       (let ([lifted (lift-generics p)])
;	 (inspect lifted)
	 (let ([typed (retypecheck lifted)])
;	   (inspect typed)
	   typed))
))))