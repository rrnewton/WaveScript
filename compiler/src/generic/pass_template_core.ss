;; [2004.06.19]  A template for passes that operate on my core language.
;; Most stuff is simple, so very little match recursion needs to happen.

;;; <Pgm>  ::= <Let>
;;; <Let>  ::= (lazy-letrec (<Decl>*) <var>)
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= <Simple>
;;;          | (if <Simple> <Simple> <Simple>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <Simple>*)
;;; <Formalexp> ::= (<var>*)
;;; <Simple> ::= (quote <Lit>) | <Var>

;; This template itself does a lot of needless list reconstruction.

(define TEMPLATE_FOR_CORE_LANG 
  (let ()

    (define (process-let expr env)
      (match expr
	 [(lazy-letrec ([,lhs* ,[process-expr -> rhs*]] ...) ,expr)
	  `(lazy-letrec ([,lhs* ,rhs*] ...) ,expr)]))

    (define (process-primapp prim args)
      (cons prim args))

    (define process-expr
      (lambda (expr env)
        (match expr
          [(quote ,const) `(quote ,const)]	
          [,var (guard (symbol? var)) var]
          [(lambda ,formalexp ,expr)
	   (process-let expr (union formalexp env))]
          [(if ,test ,conseq ,altern)
	   `(if ,test ,conseq ,altern)]                    
          [(,prim ,rand* ...)
           (guard (regiment-primitive? prim))
	   (process-primapp prim rand*)]
          [,unmatched
	   (error 'TEMPLATE "invalid syntax ~s" unmatched)])))

    (lambda (expr)
      (match expr
        [(,input-language (quote (program ,[process-let -> body])))
	 `(,input-language (quote (program ,body)))]))))
