;;; Pass 1: verify-schem

;;; Well-formed input to this pass is in the following language:



;;; <Prog> ::= <Exp>

;;; <Exp>  ::= <constant>
;;;          | (quote <datum>)
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (lambda <Formalexp> <Exp> <Exp>*)
;;;          | (case-lambda [<Formalexp> <Exp> <Exp>*]*)
;;;          | (let (<Decl>*) <Exp> <Exp>*)
;;;          | (<primitive> <Exp>*)


;;; <Decl> ::= (<var> <Exp>)
;;; <Formalexp> ::= <var>
;;;               | (<var>*)
;;;               | (<var>* . <var>)


;;;          | (<Exp> <Exp>*)





;;; A <constant> is a boolean or integer.  A <datum> is a <constant>,
;;; list or pair of <datum>, or vector of <datum>.  A <var> is a
;;; symbol.  A <primitive> is one of the symbols in the set {-, *, +, <,
;;; <=, =, add1, boolean?, car, cdr, cons, eq?, integer?, make-vector,
;;; not, null?, pair?, procedure?, set-car!, set-cdr!, sub1, vector?,
;;; vector-length, vector-ref, vector-set!, void, zero?}.

;;; Output from this pass is in the same language.

;;; The implementation requires constant?, datum?, keyword?,
;;; scheme-primitive?, set?, formalexp?, get-formals, and the list
;;; scheme-primitives from helpers.ss.

(define convert-to-simulator
  (let ()

    (define process-expr*
      (lambda (expr* env)
        (map (lambda (expr) (process-expr expr env)) expr*)))

    (define process-expr
      (lambda (expr env)
        (match expr
          [,const (guard (constant? const)) const]
          [(quote ,datum)
           (guard (not (memq 'quote env)) (datum? datum))
           `(quote ,datum)]
          [,var (guard (symbol? var))
            var]

#|
          [(if ,[test] ,[conseq] ,[altern])
           (guard (not (memq 'if env)))
           `(if ,test ,conseq ,altern)]
          [(begin ,[first-expr] ,[rest-expr*] ...)
           (guard (not (memq 'begin env)))
           `(begin ,first-expr ,rest-expr* ...)]
          [(lambda ,formalexp ,expr ,expr* ...)
           (guard (formalexp? formalexp)
                  (not (memq 'lambda env)))					
;	   (check-lambda-clause formalexp expr expr* env)
           `(lambda ,formalexp ,expr ,expr* ...)]

          [(case-lambda [,formalexp* ,expr* ,expr** ...] ...)
           (guard (andmap formalexp? formalexp*)
                  (not (memq 'case-lambda env)))
           (for-each (lambda (f e e*)
                       (check-lambda-clause f e e* env))
                     formalexp* expr* expr**)
           `(case-lambda [,formalexp* ,expr* ,expr** ...] ...)]
          [(let ([,lhs* ,[rhs*]] ...) ,expr ,expr* ...)
           (guard (not (memq 'let env))
                  (andmap symbol? lhs*)
                  (set? lhs*))
           (check-lambda-clause lhs* expr expr* env)
           `(let ([,lhs* ,rhs*] ...) ,expr ,expr* ...)]
          [(letrec ([,lhs* ,rhs*] ...) ,expr ,expr* ...)
           (guard (not (memq 'letrec env))
                  (andmap symbol? lhs*)
                  (set? lhs*))
           (let ([env (append lhs* env)])
             (let ([rhs* (process-expr* rhs* env)]
                   [expr (process-expr expr env)]
                   [expr* (process-expr* expr* env)])
               `(letrec ([,lhs* ,rhs*] ...) ,expr ,expr* ...)))]

          [(,keyword ,form* ...)
           (guard (not (memq keyword env))
                  (keyword? keyword))
           (error 'verify-scheme "invalid syntax ~s" `(,keyword ,form* ...))]
          
          ;; Optimize level specifier, n, is optional
          [(\#primitive ,prim)
           (guard (scheme-primitive? prim))
           `(\#primitive ,(snet-optimize-level) ,prim)]
          [(\#primitive ,n ,prim)
           (guard (scheme-primitive? prim))
           `(\#primitive ,n ,prim)]
          
          [(\#primitive ,n ... ,prim)
           (error 'verify-scheme
                  "(#primitive ...) used, but ~s~a~n"
                  prim " isn't one of our primitives: " )]
          
          [((\#primitive ,n ,prim) ,[rand*] ...)
           (guard (not (memq prim env))
                  (scheme-primitive? prim))
           (check-primitive-numargs prim rand*)
           `((\#primitive ,n ,prim) ,rand* ...)]
          [(,prim ,[rand*] ...)
           (guard (>= (snet-optimize-level) 2)
                  (not (memq prim env))
                  (scheme-primitive? prim))
           (check-primitive-numargs prim rand*)
           `(,prim ,rand* ...)]
          [(,[rator] ,[rand*] ...)
           `(,rator ,rand* ...)]
|#

          [,unmatched
            (error 'verify-scheme "invalid syntax ~s" unmatched)])))

    (lambda (expr)
      (display "Running on ") (display expr) (newline)
      
      (match expr
	     ;; Doesn't change the input language... 
        [(,input-language (quote (program ,body)))

           (let ([body (process-expr body '())])
             `(,input-language '(program ,body)))]))))  
  