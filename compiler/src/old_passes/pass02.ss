;;; Pass 2: rename-var
;;; January 2001

;;; This pass renames each variable to insure that each variable has
;;; a unique name.  Further, it changes top-level variable references
;;; into (toplvl-varref <var>) and (toplvl-varassign! <var> <rhs>) forms.
;;;    This is also the place where it will be decided (based on the
;;; optimize level, what becomes a true primitive reference, and what
;;; stays merely a toplvl varref.  Right now all primitives
;;;

;;; The input language is the same as the input and output languages
;;; of Pass 1.  The output language differs in that all variable
;;; bindings are uniquely named, and that the aforementioned
;;; toplvl-varref and toplvl-varassign! syntaxes are added.

;;; The implementation requires constant?, scheme-primitive?, unique-name,
;;; get-formals, and cast-formals from helpers.ss.

;;; In addition to the current expression, this pass carries along
;;; an environment (association list) mapping variable names to
;;; unique variable names.  The environment is extended for lambda,
;;; let, and letrec expressions and consulted for variable
;;; references and assignments.

;;; RRN[2002.06.19] - now this mangles toplvl var names
;;;                   (as well as quoted symbols)

(define rename-var
  (let ()
    (define process-var
      (lambda (var bound)
        (let ([vpair (assq var bound)])
          (if vpair
              (cdr (assq var bound))
              (begin
                (error 'rename-var/process-var
                       "variable was not bound, how can this happen?: ~a ~a"
                       var bound)
                var)))))
    ;; This recurs over complex constants:
    (define mangle-datum
      (lambda (datum)
        (cond
          [(symbol? datum) (mangle-name datum)]
          [(pair? datum)
           (cons (mangle-datum (car datum))
                 (mangle-datum (cdr datum)))]
          [(vector? datum)
           (let ([v (make-vector (vector-length datum))])
             (do ([i 0 (add1 i)])
                 ([= i (vector-length datum)])
                 (vector-set! v i (mangle-datum (vector-ref datum i))))
             v)]
          ;; Other datums (numbers null etc) get left alone:
          [else datum])))
    (define process-reference
      (lambda (var env)
        (if (assq var env)
            (process-var var env)
            `(toplvl-varref ,(mangle-name var)))))
    (define process-assignment
      (lambda (var rhs env)
        (if (assq var env)
            `(set! ,(process-var var env) ,rhs)
            `(toplvl-varassign! ,(mangle-name var) ,rhs))))
    (define process-lambda-clause
      (lambda (formalexp expr expr* env)
        (let* ([formal* (get-formals formalexp)]
               [new-formal* (map unique-name formal*)]
               [env (append (map cons formal* new-formal*) env)])
          (let ([expr (process-expr expr env)]
                [expr* (process-expr* expr* env)])
            `(,(cast-formals new-formal* formalexp)
               ,expr ,@expr*)))))
    (define process-expr*
      (lambda (expr* env)
        (map (lambda (expr) (process-expr expr env)) expr*)))
    (define process-expr
      (lambda (expr env)
        (match expr
          [,const
            (guard (constant? const))
            const]
          [(quote ,datum)
           (guard (not (assq 'quote env)))
           `(quote ,(mangle-datum datum))]
          
          [(\#primitive ,prim)
           (process-expr `(\#primitive 2 ,prim) env)]
          [((\#primitive ,prim) ,rand* ...)
           (process-expr `((\#primitive 2 ,prim) ,rand* ...) env)]
          
          [(\#primitive ,n ,prim)
           (guard (not (assq prim env)) (scheme-primitive? prim))
           `(\#system-ref ',(mangle-name prim))]
          
          ;; Library primitives don't have an inline form, refer to the closure:
          [((\#primitive ,n ,prim) ,[rand*] ...)
           (guard (not (assq prim env))
                  (library-scheme-primitive? prim))
           `((|#system-ref| ',(mangle-name prim)) ,rand* ...)]
          
          ;; Specific optimization level of primitive ignored for now:
          [((\#primitive ,n ,prim) ,[rand*] ...)
           (guard (not (assq prim env))
                  (scheme-primitive? prim))
           `(,prim ,rand* ...)]
          
          ;; Only if the optimize level is greater than 1 do we
          ;; ban mutation of primitive names:
          [,var (guard (symbol? var)
                       (not (assq var env))
                       (scheme-primitive? var)
                       (>= (snet-optimize-level) 2))
            `(\#system-ref ',(mangle-name var))]
          [,var (guard (symbol? var))
            (process-reference var env)]
          
          [(set! ,var ,[rhs])
           (guard (not (assq 'set! env)))
           (process-assignment var rhs env)]
          [(set! (object-reference ,obj ,class ,field) ,[rhs])
           (guard? (memv obj env))
           (let ([obj (process-var var env)])
             (let ([dest (string->symbol
                           (string-append
                             (symbol->string obj)
                             "."
                             (symbol->string field)))])
               `(set! ,dest ,rhs)))]
          [(set! (static-ref ,class ,field) ,[rhs])
           (let ([dest (string->symbol
                         (string-append
                           (symbol->string class)
                           "."
                           (symbol->string field)))])
             `(set! ,dest ,rhs))]
          [(set! (this-ref ,class ,field) ,[rhs])
           `(set! ,field ,rhs)]
          [(if ,[test] ,[conseq])
           (guard (not (assq 'if env)))
           `(if ,test ,conseq)]
          [(if ,[test] ,[conseq] ,[altern])
           (guard (not (assq 'if env)))
           `(if ,test ,conseq ,altern)]
          [(begin ,[first-expr] ,[rest-expr*] ...)
           (guard (not (assq 'begin env)))
           `(begin ,first-expr ,rest-expr* ...)]
          [(lambda ,formalexp ,expr ,expr* ...)
           (guard (not (assq 'lambda env)))
           (cons 'lambda
                 (process-lambda-clause formalexp expr expr* env))]
          [(case-lambda [,formalexp ,expr ,expr* ...] ...)
           (guard (not (assq 'case-lambda env)))
           (cons 'case-lambda
                 (map (lambda (f e e*)
                        (process-lambda-clause f e e* env))
                      formalexp expr expr*))]
          [(let ([,lhs* ,[rhs*]] ...) ,expr ,expr* ...)
           (guard (not (assq 'let env)))
           (let ([new-lhs* (map unique-name lhs*)])
             (let ([env (append (map cons lhs* new-lhs*) env)])
               (let ([expr (process-expr expr env)]
                     [expr* (process-expr* expr* env)])
                 `(let ([,new-lhs* ,rhs*] ...) ,expr ,expr* ...))))]
          [(letrec ([,lhs* ,rhs*] ...) ,expr ,expr* ...)
           (guard (not (assq 'letrec env)))
           (let ([new-lhs* (map unique-name lhs*)])
             (let ([env (append (map cons lhs* new-lhs*) env)])
               (let ([rhs* (process-expr* rhs* env)]
                     [expr (process-expr expr env)]
                     [expr* (process-expr* expr* env)])
                 `(letrec ([,new-lhs* ,rhs*] ...) ,expr ,expr* ...))))]
          [(let-class (,class-defn* ...) ,body)
           (let ([class-defn* (map (lambda (x)
                                     (process-class-defn x env))
                                   class-defn*)]
                 [body (process-expr body env)])
             `(let-class ,class-defn* ,body))]
          [(new ,class-name ,[args*] ...)
           `(new ,class-name ,args* ...)]
          [(open-instance ,obj ,class-name ,[body])
           `(open-instance ,(process-var obj env) ,class-name ,body)]
          [(open-package ,pkg ,[body])
           `(open-package ,pkg ,body)]
          [(object-reference ,obj ,class ,field)
           `(object-reference ,(process-var obj env) ,class ,field)]
          [(invoke-method ,obj ,class ,method (,[arg*] ...))
           `(invoke-method ,(process-var obj env)
                           ,class ,method (,arg* ...))]
          [(foreign-call ,name ,type-sig (,[arg*] ...))
           `(foreign-call ,name ,type-sig (,arg* ...))]
          [(static-ref ,class ,field)
           `(static-ref ,class ,field)]
          [(this-ref ,class ,field)
           `(this-ref ,class ,field)]
          [(invoke-static-method ,class ,method-name ,type-sig (,[arg*] ...))
           `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))]
          
          ;; Only if the optimize level is greater than 1 do we
          ;; ban mutation of primitive names:
          [(,prim ,[rand*] ...)
           (guard (>= (snet-optimize-level) 2)
                  (not (assq prim env))
                  (scheme-primitive? prim)
                  (not (library-scheme-primitive? prim)))
           `(,prim ,rand* ...)]
          
          [(,[rator] ,[rand*] ...)
           `(,rator ,rand* ...)]
          [,unmatched (error 'rename-var "invalid syntax ~s" unmatched)])))
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
           (let ([ctor (process-expr ctor env)]
                 [mbody* (map (lambda (x) (process-expr x env)) mbody*)])
             `(define-class
                ,name ,base ,ctor
                (fields (,fmods** ... ,fname*) ...)
                (methods (,mmods** ... ,mtype*
                           (,mname* ,mbody*)) ...)))]
          [,unmatched (error 'rename-vars
                             "invalid class defn ~s"
                             cdef)])))
    (lambda (expr)
      (match expr
        [(,input-language (quote (program ,class-defns* ... ,body)))
         (let ([class-defns* (map (lambda (x)
                                    (process-class-defn x '()))
                                  class-defns*)])
           (let ([body (process-expr body '())])
             `(,input-language '(program ,class-defns* ... ,body))))]))))

