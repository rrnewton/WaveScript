;;; Pass 14: remove-anonymous-lambda

;;; This pass ensures that all lambda expressions are explicitly named
;;; by letrec variables.  For each lambda that is not bound by a letrec,
;;; it performs one of the following three transformations:
;;;
;;; 1. (let (b ...) body) =>
;;;      (letrec (l-b ...)
;;;        (let (nl-b ...)
;;;          body))
;;;    where l-b ... is the subset of b ... that binds lambda expressions
;;;    and nl-b ... is the subset that binds binds non-lambda expressions.
;;;
;;; 2. ((lambda (formal ...) body) arg ...) =>
;;;      (let ((formal arg) ...) body)
;;;
;;; 3. (lambda (formal ...) body) =>
;;;      (letrec ((anon (lambda (formal ...) body))) anon)
;;;
;;; Transformation 2 is chosen in preference to transformation 3 when it
;;; applies.

;;; The input language is the same as the output language of Pass 9.

;;; The output language differs in that lambda expressions can appear
;;; only on the right-hand sides of letrec bindings.

;;; <Input> ::= (<language-name> <Program>)
;;; <Program> ::= (program (<var>*) (<pkg>*) <class-def>* <Exp>)
;;; <Exp>  ::= (quote <imm>)
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (begin <Exp> <Exp>*)
;;;          | (let ((<var> <Exp>)*) <Exp>)
;;;          | (letrec ((<var> <Lambda>)*) <Exp>)
;;;          | (<primitive> <Exp>*)
;;;          | (<Exp> <Exp>*)
;;;          | (toplvl-varref <var>)
;;;          | (toplvl-varassign! <var> <Exp>)
;;; <Lambda> ::= (lambda <Formalexp> <Exp>)
;;; <Formalexp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)

;;; The implementation requires extended-scheme-primitive?, unique-name,
;;; cast-args, and get-formals from helpers.ss.

(define remove-anonymous-lambda
  (let ()
    (import indexed-case-lambdas)
    (define make-let
      (lambda (what binding* body)
        (if (null? binding*)
            body
            `(,what ,binding* ,body))))
    (define separate-bindings
      (lambda (lhs* rhs*)
        (if (null? lhs*)
            (values '() '())
            (mvlet ([(nl-b* l-b*)
                     (separate-bindings (cdr lhs*) (cdr rhs*))])
              (let ([lhs (car lhs*)] [rhs (car rhs*)])
                (if (lambda? rhs) ;; lambda or case-lambda
                    (values nl-b*
                            (cons `[,lhs ,(process-lambda rhs)]
                                  l-b*))
                    (values (cons `[,lhs ,(Expr rhs)] nl-b*)
                            l-b*)))))))
    (define handle-let
      (lambda (lhs* rhs* body)
        (mvlet ([(nl-b* l-b*) (separate-bindings lhs* rhs*)])
          (make-let 'letrec l-b*
                    (make-let 'let nl-b* body)))))
    (define process-lambda
      (lambda (lmbda)
        (match lmbda
          [(lambda ,formalexp ,[Expr -> body])
           `(lambda ,formalexp ,body)]
          [(case-lambda
             ([,formalexp* ,[Expr -> body*]] ...)
             ,indexmap ...)
           `(case-lambda ([,formalexp* ,body*] ...) ,indexmap ...)]
          [,_ (error 'remove-anonymous-lambda.process-lambda
                     "not a lambda or case-lambda: ~s"
                     lmbda)])))
    (define Expr
      (lambda (expr)
        (match expr
          [(quote ,constant) `(quote ,constant)]
          [,var (guard (symbol? var)) var]
          [(toplvl-varref ,var) `(toplvl-varref ,var)]
          [(toplvl-varassign! ,var ,[rhs])
           `(toplvl-varassign! ,var ,rhs)]
          [(if ,[test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[expr*] ...) (make-begin expr*)]
          ;; Note we are not autorecurring on the rhs in these 2 cases:
          [(,caselamb ,rhs* ...)
           (guard (case-lambda? caselamb))
           (Expr `(,(case-lambda->lambda lamb (length rhs*))
                    ,rhs* ...))]
          [((lambda ,formalexp ,[body]) ,rhs* ...)
           (handle-let (get-formals formalexp)
                       (cast-args rhs* formalexp) body)]
          [,lamb
            (guard (lambda? lamb))
            (let ([anon (unique-name 'anon)])
              `(letrec ([,anon ,(process-lambda lamb)])
                 ,anon))]
          [(let ([,lhs* ,rhs*] ...) ,[body])
           (handle-let lhs* rhs* body)]
          [(letrec ([,lhs* ,lambda*] ...) ,[body])
           (guard (andmap lambda? lambda*))
           (make-let 'letrec
                     `([,lhs* ,(map process-lambda lambda*)] ...)
                     body)]
          
          [(let-class (,class-defn* ...) ,[body])
           (let ([class-defn* (map process-class-defn class-defn*)])
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
          [(set-field! ,field ,[rhs])
           `(set-field! ,field ,rhs)]
          [(invoke-static-method ,class ,method-name ,type-sig (,[arg*] ...))
           `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))]
          [(,prim ,[rand*] ...)
           (guard (extended-scheme-primitive? prim))
           `(,prim ,rand* ...)]
          [(,[rator] ,[rand*] ...)
           `(,rator ,rand* ...)]
          [,unmatched
            (error 'remove-anonymous-lambda "invalid expression: ~s"
                   unmatched)])))
    (define process-class-defn
      (lambda (cdef)
        (match cdef
          [(define-class ,name ,base
             ,ctor
             (fields (,fmods** ... ,fname*) ...)
             (methods (,mmods** ... ,mtype*
                        (,mname* ,mbody*)) ...))
           (let ([ctor (process-method ctor)]
                 [mbody* (map process-method mbody*)])
             `(define-class
                ,name ,base ,ctor
                (fields (,fmods** ... ,fname*) ...)
                (methods (,mmods** ... ,mtype*
                           (,mname* ,mbody*)) ...)))]
          [,unmatched (error 'remove-anonymous-lambda
                             "invalid class defn ~s"
                             cdef)])))
    (define process-method
      (lambda (meth)
        (match meth
          [(lambda ,args ,body)
           (let ([body (Expr body)])
             `(lambda ,args ,body))])))
    (lambda (prog)
      (match prog
        [(,input-language
           (quote (program ,sym* ,pkg* ,class-defns* ... ,body)))
         (let ([class-defns* (map process-class-defn class-defns*)])
           (let ([body (Expr body)])
             `(,input-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))
