;; [2004.06.19] This is a template for a pass which takes place near
;; the beginning of the compiler, but after the rename pass.

(define TEMPLATE
  (let ()
    
    (define process-expr
      (lambda (expr)
        (match expr
          [(quote ,datum)  `(quote ,datum)]
          [,var (guard (symbol? var)) var]
          [(if ,[test] ,[conseq] ,[altern])
	   `(if ,test ,conseq ,altern)]
          [(lambda ,formals ,[body])
	   `(lambda ,formals ,[body])]
          [(letrec ([,lhs* ,[rhs*]] ...) ,[body])
	   `(letrec ([,lhs* ,rhs*] ...) ,body)]
          [(,prim ,[rand*] ...)
           (guard (wavescript-primitive? prim))
           (process-primapp prim rand*)]          
          [,unmatched
            (error 'TEMPLATE "invalid expression: ~s"
                   unmatched)])))

    (define process-primapp
      (lambda (prim args)
	(cons prim args)))

    (define process-method
      (lambda (meth)
        (match meth
          [(lambda ,args ,body)
           (mvlet ([(body body-b*) (process-expr body)])
             (if (null? body-b*)
                 `(lambda ,args ,body)
                 `(lambda ,args (let ,body-b* ,body))))])))

    (lambda (expr)
      (match expr
	     [(,input-language (quote (program ,[process-expr -> body])))
	      `(,input-language (quote (program ,body)))]))
    ))