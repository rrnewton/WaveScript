;;; Pass 23: lift-letrec-body
;;; September 2001
;===============================================================================

;;; Output grammar:

;;; This pass, which is really not a pass at all, makes a (first-order)
;;; procedure out of the letrec body:

;;; <Input> ::= (<language-name> <Program>)
;;; <Program> ::= (program (<var>*) (<pkg>*) <class-def>* <Letrec>)
;;; <Letrec>  ::= (letrec ((<var> <>)*) (entry-point <var>))

(define lift-letrec-body
  (let ()
    (define (simple? x) 
      (match x
          [(quote ,imm) #t]
          [,var (guard (symbol? var)) #t]
	  [,otherwise #f]))

    (define process-letrec
      (lambda (expr)
;	(disp "processing letrec")
;	(pp expr)

        (match expr
          [(lazy-letrec ([,lhs* ,[process-expr -> rhs*]] ...) ,body)
;; NOW we lift it even if it is simple.
;	   (if (simple? body)
           (if (symbol? body)
	       `(lazy-letrec ([,lhs* ,rhs*] ...) ,body)
	       (let ([main (unique-name 'result)])
					;(code-name 'main)]) ;; Old version used code-name for labels...
		 `(lazy-letrec ([,lhs* ,rhs*] ...
				[,main ,body])  ;(lambda () ,body))])
			       ,main)))])))

    (define process-expr
      (lambda (expr)
        (match expr
	  [,x (guard (simple? x)) x]
          [(if ,[test] ,[conseq] ,[altern])
	   `(if ,test ,conseq ,altern)]
	  [(lambda ,formalexp ,[process-letrec -> body])
	   `(lambda ,formalexp ,body)]
          [(,prim ,[rand*] ...) (guard (regiment-primitive? prim))
	   `(,prim ,rand* ...)]
          [,unmatched
            (error 'lift-letrec "invalid expression: ~s"
                   unmatched)])))

    (lambda (prog)
      (match prog
        [(,input-language (quote (program ,[process-letrec -> body])))
	 ;; This pass uses the same language as the prior pass, lift-letrec
	 `(,input-language '(program ,body))]))))
