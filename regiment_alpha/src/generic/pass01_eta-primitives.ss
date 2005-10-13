
;; Pass: eta-primitives
;; [2004.06.24]

;; This simple pass eta expands all operand-context references to
;; primitives, hereafter primitive-names will only occur in operator
;; context.  This is just a simple, regularizing pass.

;; DEPENDS: list-head, get-primitive-entry, regiment-primitive?

(define eta-primitives
  (let ()
    (define process-expr*
      (lambda (expr* env)
        (map (lambda (expr) (process-expr expr env)) expr*)))
    (define process-expr
      (lambda (expr env)
        (match expr
          [,const (guard (constant? const)) const]
          [(quote ,datum)
           (guard (not (memq 'quote env)))
	   `(quote ,datum)]                                       
          [,var (guard (symbol? var) (regiment-primitive? var))
		(let* ([possible-formals '(a b c d e f g h i j)]
		       [args (cadr (get-primitive-entry var))])
		  (if (regiment-constant? var)
		      var
		      (let ([formals (list-head possible-formals (length args))])
			`(lambda ,formals (,var ,@formals)))))]
          [,var (guard (symbol? var))
		var]
          [(if ,[test] ,[conseq] ,[altern])
           (guard (not (memq 'if env)))
           `(if ,test ,conseq ,altern)]
          [(lambda ,formals ,expr)
           (guard (not (memq 'lambda env)))
	   `(lambda ,formals
	      ,(process-expr expr (append formals env)))]
          [(letrec ([,lhs* ,rhs*] ...) ,expr)
           (guard (not (memq 'letrec env)))
	   (let ([env (append lhs* env)])
	     (let ([rhs* (process-expr* rhs* env)]
		   [expr (process-expr expr env)])
	       `(letrec ([,lhs* ,rhs*] ...) ,expr)))]
          [(,prim ,[rand*] ...)
           (guard (not (memq prim env)) (regiment-primitive? prim))
           `(,prim ,rand* ...)]

	  ;; Adding normal applications because the static elaborator will get rid of them.
	  [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
          [,unmatched (error 'eta-primitives "invalid syntax ~s" unmatched)])))
    (lambda (expr)
      (match expr
	     [(,input-language (quote (program ,body)))
	      (let ([body (process-expr body '())])
		`(,input-language '(program ,body)))]))))

