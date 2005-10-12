;;; Pass 11: remove-set!
;;; January 2001

;;; This pass introduces explicit locations for settable variables.
;;; It does this by (a) wrapping each lambda and let body with a let
;;; for the settable variables binding each variable to a pair
;;; whose car is the original value of the variable, (b) renaming
;;; the original variable to avoid duplicate names, and (c) converting
;;; settable variable references and assignments into calls to car
;;; and set-car!.

;;; (let ([x.2 '3] [y.1 '4])
;;;   (settable (y.1)
;;;     (letrec ([f.3 (lambda (z.4)
;;;                     (settable (z.4)
;;;                       (begin
;;;                         (set! z.4 '0)
;;;                         z.4)))])
;;;       (begin
;;;         (set! y.1 (+ y.1 '1))
;;;         (+ (f.3 x.2) y.1)))))
;;;
;;; becomes
;;;
;;; (let ([x.2 '3] [y.5 '4])
;;;   (let ([y.1 (cons y.5 (void))])
;;;     (letrec ([f.3 (lambda (z.6)
;;;                     (let ([z.4 (cons z.6 (void))])
;;;                       (begin
;;;                         (set-car! z.4 '0)
;;;                         (car z.4))))])
;;;       (begin
;;;         (set-car! y.1 (+ (car y.1) '1))
;;;         (+ (f.3 x.2) (car y.1))))))

;;; The input language is the same as the output language of Pass 8.

;;; The output language is the same, except that there are no set!
;;; expressions and let and lambda bodies are no longer wrapped in
;;; settable forms.  The now obsolete definition of settable is
;;; dropped from the output-language definition.

;;; <Input> ::= (<language-name> <Program>)
;;; <Program> ::= (program (<var>*) (<pkg>*) <class-def>* <Exp>)

;;; <Exp>  ::= (quote <imm>)
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (begin <Exp> <Exp>*)
;;;          | <Lambda>
;;;          | (let (<Decl>*) <Exp>)
;;;          | (letrec (<RecDecl>*) <Exp>)
;;;          | (<primitive> <Exp>*)
;;;          | (<Exp> <Exp>*)
;;;          | (toplvl-varref <var>)
;;;          | (toplvl-varassign! <var> <Exp>)
;;; <Decl> ::= (<var> <Exp>)
;;; <RecDecl> ::= (<var> <Lambda>)
;;; <Lambda> ::= (lambda <Formalexp> <Exp>)
;;;            | (case-lambda
;;;                ([<Formalexp> <Body>]* )
;;;                [(<a> . <b>) <n>]* )
;;; <Formalexp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)

;;; The implementation requires extended-scheme-primitive?, unique-name,
;;; get-formals, and cast-formals from helpers.ss.

(define remove-set!
  (let ()
    (define settable-bindings
      (lambda (var* set*)
        (if (null? var*)
            (values '() '())
            (let ([var (car var*)])
              (mvlet ([(var* binding*) (settable-bindings (cdr var*) set*)])
                (if (memq var set*)
                    (let ([tmp (unique-name var)])
                      (values
                        (cons tmp var*)
                        (cons `[,var (cons ,tmp (void))] binding*)))
                    (values (cons var var*) binding*)))))))
    (define process-lambda-clause
      (lambda (formalexp local-set* body set*)
        (if (null? local-set*)
            `(,formalexp ,(Expr body set*))
            (let ([formal* (get-formals formalexp)])
              (mvlet ([(formal* binding*)
                       (settable-bindings formal* local-set*)])
                `(,(cast-formals formal* formalexp)
                   (let ,binding*
                     ,(Expr body (append local-set* set*))))))
            )))
    (define Expr
      (lambda (expr set*)
        (match expr
          [(quote ,imm) `(quote ,imm)]
          [,var (guard (symbol? var)) (if (memq var set*) `(car ,var) var)]
          [(toplvl-varref ,var) `(toplvl-varref ,var)]
          [(toplvl-varassign! ,var ,[rhs])
           `(toplvl-varassign! ,var ,rhs)]
          [(if ,[test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(set! ,var ,[rhs])
           (if (memv var set*)
               `(set-car! ,var ,rhs)
               `(set-field! ,var ,rhs))]
          [(begin ,[expr*] ...) `(begin ,expr* ...)]
          
          [(lambda ,formalexp (settable (,local-set* ...) ,body))
           `(lambda ,@(process-lambda-clause formalexp local-set* body set*))]
          [(case-lambda ([,formalexp* (settable ,local-set** ,body*)] ...)
                        ,indexmap ...)
           `(case-lambda
              ,(map (lambda (f ls b)
                      (process-lambda-clause f ls b set*))
                    formalexp* local-set** body*)
              ,indexmap ...)]
          [(let ([,lhs* ,[rhs*]] ...) (settable () ,[body]))
           `(let ([,lhs* ,rhs*] ...) ,body)]
          [(let ([,lhs* ,[rhs*]] ...) (settable (,local-set* ...) ,body))
           (mvlet ([(lhs* binding*) (settable-bindings lhs* local-set*)])
             `(let ([,lhs* ,rhs*] ...)
                (let (,binding* ...)
                  ,(Expr body (append local-set* set*)))))]
          [(letrec ([,lhs* ,[rhs*]] ...) ,[body])
           `(letrec ([,lhs* ,rhs*] ...) ,body)]
          [(let-class (,class-defn* ...) ,[body])
           (let ([class-defn* (map (lambda (x)
                                     (process-class-defn x env)) class-defn*)])
             `(let-class ,class-defn* ,body))]
          [(new ,class-name ,[args*] ...)
           `(new ,class-name ,args* ...)]
          [(open-instance ,obj ,class-name ,[body])
           `(open-instance ,obj ,class-name ,body)]
          [(object-reference ,obj ,class ,field)
           `(object-reference ,obj ,class ,field)]
          [(invoke-method ,obj ,class ,method (,[arg*] ...))
           `(invoke-method ,obj ,class ,method (,arg* ...))]
          [(foreign-call ,name ,type-sig (,[arg*] ...))
           `(foreign-call ,name ,type-sig (,arg* ...))]
          [(static-ref ,class ,field)
           `(static-ref ,class ,field)]
          [(this-ref ,class ,field)
           `(this-ref ,class ,field)]
          [(invoke-static-method ,class ,method-name ,type-sig (,[arg*] ...))
           `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))]
          [(,prim ,[rand*] ...)
           (guard (extended-scheme-primitive? prim))
           `(,prim ,rand* ...)]
          [(,[rator] ,[rand*] ...)
           `(,rator ,rand* ...)]
          [,unmatched
            (error 'remove-set! "invalid expression: ~s"
                   unmatched)])))
    (define process-class-defn
      (lambda (cdef env)
        (match cdef
          [(define-class ,name ,base
             ,ctor
             (fields (,fmods** ... ,fname*) ...)
             (methods (,mmods** ... ,mtype*
                        (,mname* ,mbody*)) ...))
           (let ([ctor (Expr ctor env)]
                 [mbody* (map (lambda (x)
                                (Expr x env))
                              mbody*)])
             `(define-class
                ,name ,base ,ctor
                (fields (,fmods** ... ,fname*) ...)
                (methods (,mmods** ... ,mtype*
                           (,mname* ,mbody*)) ...)))]
          [,unmatched (error 'remove-set!
                             "invalid class defn ~s"
                             cdef)])))
    
    (lambda (prog)
      (match prog
        [(,input-language
           (quote (program ,sym* ,pkg* ,class-defns* ... ,body)))
         (let ([class-defns* (map (lambda (x)
                                    (process-class-defn x '()))
                                  class-defns*)])
           (let ([body (Expr body '())])
             `(,input-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))