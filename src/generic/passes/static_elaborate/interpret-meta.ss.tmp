








(reg:define-struct (plain val)) ;; Contains a datum: number, list, array, tuple.
(reg:define-struct (tuple fields)) ;; To distinguish tuples from vectors.
(reg:define-struct (closure formals code env))
(reg:define-struct (streapop name op code parents))

(define (apply-env env v) (cadr (assq v env)))
(define (extend-env id* val* env) (append (map list id* val*) env))

(define (Expr x env)
  (match x
    [,v (guard (symbol? v)) (apply-env env x)]

    [(tuple ,[x*] ...) (make-tuple x*)]
    [(iterate ,[f] ,[s])    (make-streamop #f 'iterate f (list s))]
    [(unionList ,[ls])      (make-streamop #f 'unionN #f (plain-val ls))]

    [(let ([,lhs* ,ty* ,[rhs*]] ...) ,bod)
     (Expr bod (extend-env lhs* rhs* env))]

    ;; letrec

    [(lambda ,formal* ,ty* ,bod) (make-closure formal* bod env)]
    [(app ,[f] ,[e*] ...)
     (Expr (closure-code f) 
	   (extend-env (closure-formals f) e*
		       (closure-env f)))]
    ))

