;; Pass reduce-primitives 

;;; This pass simplifies the progam, basically it reduces
;;; rewrites primitive expressions into other primitive exprssions.

;;; PRIMITIVES ELIMINATED:
;;;   circle-at


(define reduce-primitives
  (let ()
    
    (define process-expr
      (lambda (expr)
        (match expr
          [(quote ,datum)  `(quote ,datum)]
          [,var (guard (symbol? var)) var]
          [(if ,[test] ,[conseq] ,[altern])
	   `(if ,test ,conseq ,altern)]
          [(lambda ,formals ,[body])
	   `(lambda ,formals ,body)]
          [(letrec ([,lhs* ,[rhs*]] ...) ,[body])
	   `(letrec ([,lhs* ,rhs*] ...) ,body)]
          [(,prim ,[rand*] ...)
           (guard (regiment-primitive? prim))
           (process-primapp prim rand*)]          
	  [(,[rator] ,[rand*] ...)
	   `(,rator ,rand* ...)]
          [,unmatched
            (error 'reduce-primitives "invalid expression: ~s"
                   unmatched)])))

    (define process-primapp
      (lambda (prim args)
;	(disp "PROCESS PRIMAP" prim args)
	(match (cons prim args)
	       [(circle-at ,loc ,rad)
		`(circle (anchor-at ,loc) ,rad)]
	       [(khood-at ,loc ,rad)
		`(khood (anchor-at ,loc) ,rad)]
	       [(k-neighborhood ,args ...)
		`(khood ,args ...)]
	       [,orig orig])))

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
	      `(,input-language (quote (program ,body)))]
	     [,otherwise
	      (error 'reduce-primitives "Invalid input program: ~s" otherwise)]))
    ))
