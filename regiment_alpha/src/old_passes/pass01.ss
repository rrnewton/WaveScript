;;; Pass 1: verify-scheme
;;; January 2001

;;; This pass verifies that the input is a well-formed program.  If so,
;;; it returns a copy of the input program.  If not, it signals a syntax
;;; error.

;;; Well-formed input to this pass is in the following language:

;;; <Prog> ::= (program <Class-Def> ... <Exp>)
;;;          | <Exp>

;;; <Exp>  ::= <constant>
;;;          | (quote <datum>)
;;;          | <var>
;;;          | (set! <var> <Exp>)
;;;          | (if <Exp> <Exp>)
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (begin <Exp> <Exp>*)
;;;          | (lambda <Formalexp> <Exp> <Exp>*)
;;;          | (case-lambda [<Formalexp> <Exp> <Exp>*]*)
;;;          | (let (<Decl>*) <Exp> <Exp>*)
;;;          | (letrec (<Decl>*) <Exp> <Exp>*)
;;;          | (<primitive> <Exp>*)
;;;          | (<Exp> <Exp>*)
;;; <Decl> ::= (<var> <Exp>)
;;; <Formalexp> ::= <var>
;;;               | (<var>*)
;;;               | (<var>* . <var>)

;;; <Class-Def> ::= Save this for later...

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

(define verify-scheme
  (let ()
    (define process-expr*
      (lambda (expr* env)
        (map (lambda (expr) (process-expr expr env)) expr*)))
    (define check-primitive-numargs
      (lambda (prim args)
        (let ([actual (length args)]
              [expected (get-primitive-numargs prim)])
          (unless
            (or (and (pair? expected)
                     (<= (car expected) actual (cdr expected)))
                (and (number? expected) (= expected actual)))
            (error 'verify-scheme
                   "~s is the wrong number of arguments for primitive ~s"
                   actual prim)))))
    (define check-lambda-clause
      (lambda (formalexp expr expr* env)
        (let ([env (append (get-formals formalexp) env)])
          (process-expr expr env)
          (process-expr* expr* env))))
    (define process-expr
      (lambda (expr env)
        (match expr
          [,const (guard (constant? const)) const]
          [(quote ,datum)
           (guard (not (memq 'quote env)) (datum? datum))
           `(quote ,datum)]
          [,var (guard (symbol? var))
            var]
          [(set! ,var ,[rhs])
           (guard (not (memq 'set! env))
                  (symbol? var))
           `(set! ,var ,rhs)]
          [(set! (object-reference ,obj ,class ,field) ,[rhs])
           (guard? (memv obj env))
           `(set! (,object-reference ,obj ,class ,field) ,rhs)]
          [(set! (static-ref ,class ,field) ,[rhs])
           `(set! (,static-ref ,class ,field) ,rhs)]
          [(set! (this-ref ,class ,field) ,[rhs])
           `(set! (,this-ref ,class ,field) ,rhs)]
          [(if ,[test] ,[conseq])
           (guard (not (memq 'if env)))
           `(if ,test ,conseq)]
          [(if ,[test] ,[conseq] ,[altern])
           (guard (not (memq 'if env)))
           `(if ,test ,conseq ,altern)]
          [(begin ,[first-expr] ,[rest-expr*] ...)
           (guard (not (memq 'begin env)))
           `(begin ,first-expr ,rest-expr* ...)]
          [(lambda ,formalexp ,expr ,expr* ...)
           (guard (formalexp? formalexp)
                  (not (memq 'lambda env)))
           (check-lambda-clause formalexp expr expr* env)
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
          [(let-class (,class-defn* ...) ,body)
           (let ([class-defn* (map (lambda (x)
                                     (process-class-defn x env))
                                   class-defn*)]
                 [body (process-expr body (map cadr class-defn*))])
             `(let-class ,class-defn* ,body))]
          [(new ,class-name ,[args*] ...)
           `(new ,class-name ,args* ...)]
          [(open-instance ,obj ,class-name ,[body])
           (guard (memv obj env))
           `(open-instance ,obj ,class-name ,body)]
          [(open-package ,pkg ,[body])
           `(open-package ,pkg ,body)]
          [(object-reference ,obj ,class ,field)
           `(object-reference ,obj ,class ,field)]
          [(invoke-method ,obj ,class ,method (,[arg*] ...))
           `(invoke-method ,obj ,class ,method (,arg* ...))]
          [(foreign-call ,name ,type-sig (,[arg*] ...))
           `(foreign-call ,name ,type-sig (,arg* ...))]
          [(static-ref ,class ,field)
           `(static-ref ,class ,field)]
          [(this-ref ,obj ,class ,var)
           `(this-ref ,obj ,class ,var)]
          [(invoke-static-method ,class ,method-name ,type-sig (,[arg*] ...))
           `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))]
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
          [,unmatched
            (error 'verify-scheme "invalid syntax ~s" unmatched)])))
    (define process-class-defn
      (lambda (cdef env)
        (match cdef
          [(define-class ,name ,base
             ,ctor
             (fields (,fmods** ... ,fname*) ...)
             (methods (,mmods** ... ,mtype*
                        (,mname* ,mbody*)) ...))
           (guard (or (eqv? base 'object)
                      (memv base env)))
           (let ([env (append mname* fname* env)])
             (let ([ctor (process-expr ctor env)]
                   [mbody* (map (lambda (x) (process-expr x env)) mbody*)])
               `(define-class
                  ,name ,base ,ctor
                  (fields (,fmods** ... ,fname*) ...)
                  (methods (,mmods** ... ,mtype*
                             (,mname* ,mbody*)) ...))))]
          [,unmatched (error 'verify-scheme
                             "invalid class defn ~s"
                             cdef)])))
    (lambda (expr)
      (match expr
        [(,input-language (quote (program ,class-defns* ... ,body)))
         (let ([class-defns* (map (lambda (x)
                                    (process-class-defn x '()))
                                  class-defns*)])
           (let ([body (process-expr body (map cadr class-defns*))])
             `(,input-language '(program ,class-defns* ... ,body))))]))))
  
  