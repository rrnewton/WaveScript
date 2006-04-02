;;;; Pass 10: Lift Letrec Body
; ===============================================================================

;;; Output grammar:
;;;      <br>
;;; This pass, which is really not a pass at all, makes sure the body
;;; of the main letrec is simply a variable.
;;;      <br>
;;;      <br>
;;; <Input> ::= (<language-name> <Program>)                           <br>
;;; <Program> ::= (program ) <Letrec>)                                <br>
;;; <Letrec>  ::= (letrec ((<var> <Type> <Expr>)*) <var>)             <br>

(define lift-letrec-body
  (let ()
    (define (simple? x) 
      (match x
          [(quote ,imm) #t]
          [,var (guard (symbol? var)) #t]
	  [,otherwise #f]))

    (define process-letrec
      (lambda (tenv namehint)
	(lambda  (expr)
;	(disp "processing letrec")
;	(pp expr)
        (match expr
          [(lazy-letrec ([,lhs* ,type* ,rhs*] ...) ,body)
	   (let* ([newenv (tenv-extend tenv lhs* type*)]
		  [rhs* (map (process-expr newenv) rhs* lhs*)]
		  [body ((process-expr newenv) body 
			 #f ;(symbol-append 'body_of namehint)
			 )])
;; NOW we lift it even if it is simple.
;;	   (if (simple? body)
           (if (and (symbol? body) (not (regiment-constant? body)))
	       `(lazy-letrec ([,lhs* ,type* ,rhs*] ...) ,body)
	       (let* ([main (unique-name (symbol-append 'result_of_ namehint))]
		      [maintype (recover-type body newenv)])
					;(code-name 'main)]) ;; Old version used code-name for labels...
		 `(lazy-letrec ([,lhs* ,type* ,rhs*] ...
				[,main ,maintype ,body])  ;(lambda () ,body))])
			       ,main))))]))))

    (define process-expr
      (lambda (tenv)
      (lambda (expr namehint)	
        (match expr
	  [,x (guard (simple? x)) x]
          [(if ,[test] ,[conseq] ,[altern])
	   `(if ,test ,conseq ,altern)]
	  [(lambda ,formalexp ,types ,body)
	   ;; Assumes that formals is just a list.  No optional arguments allowed currently.
	   (let ((newenv (tenv-extend tenv formalexp types)))
	     `(lambda ,formalexp ,types ,((process-letrec newenv 
							  (or namehint 'anonlambda)
							  ) body)))]
          [(,prim ,[rand*] ...) (guard (regiment-primitive? prim))
	   `(,prim ,rand* ...)]
          [,unmatched
            (error 'lift-letrec-body "invalid expression: ~s"
                   unmatched)]))))

    (lambda (prog)
      (match prog
        [(,input-language (quote (program ,[(process-letrec (empty-tenv) 'toplevel) -> body] ,type)))
	 ;; This pass uses the same language as the prior pass, lift-letrec
	 `(,input-language '(program ,body ,type))]))))
