;;; Pass 9: remove-impure-letrec
;;; January 2001

;;; A letrec is pure if none of the bound variables is settable and
;;; all of the right-hand-side expressions are lambda expressions,
;;; otherwise it is impure.

;;; This pass replaces impure letrecs with the standard let-and-set!
;;; expansion:
;;;
;;; (letrec ((x e) ...) body) =>
;;;   (let ((x (void)) ...)
;;;     (let ((t e) ...)
;;;       (set! x t) ...)
;;;     body)

;;; Pure letrecs are left as letrec expressions.

;;; The input language is the same as the output language of Pass 7.

;;; The output language is the same, except that the right-hand-sides
;;; of letrecs are constrained to be lambda expressions and letrec
;;; bodies are not wrapped in settable forms.

;;; <Input> ::= (<language-name> <Program>)
;;; <Program> ::= (program (<var>*) (<pkg>*) <class-def>* <Exp>)

;;; <Exp>  ::= (quote <imm>)
;;;          | <var>
;;;          | (set! <var> <Exp>)
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (begin <Exp> <Exp>*)
;;;          | <Lambda>
;;;          | (let (<Decl>*) (settable (<var>*) <Exp>))
;;;          | (letrec (<RecDecl>*) <Exp>)
;;;          | (<primitive> <Exp>*)
;;;          | (<Exp> <Exp>*)
;;;          | (toplvl-varref <var>)
;;;          | (toplvl-varassign! <var> <Exp>)
;;; <Decl> ::= (<var> <Exp>)
;;; <RecDecl> ::= (<var> <Lambda>)
;;; <Lambda> ::= (lambda <Formalexp> (settable (<var>*) <Exp>))
;;;            | (case-lambda [<Formalexp> (settable (<var>*) <Exp>)]*)
;;; <Formalexp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)

;;; The implementation requires make-begin, extended-scheme-primitive?, and
;;; unique-name from helpers.ss.

(define remove-impure-letrec
  (let ()
    (define lambda?
      (lambda (expr)
        (match expr
          [(lambda (,formal* ...) ,body) #t]
          [(case-lambda [(,formal** ...) ,body*] ...) #t]
          [,_ #f])))
    (define Expr
      (lambda (expr)
        (match expr
          [(quote ,imm) `(quote ,imm)]
          [,var (guard (symbol? var)) var]
          [(toplvl-varref ,var) `(toplvl-varref ,var)]
          [(toplvl-varassign! ,var ,[rhs])
           `(toplvl-varassign! ,var ,rhs)]
          [(set! ,var ,[rhs])
           `(set! ,var ,rhs)]
          [(if ,[test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[expr*] ...) `(begin ,expr* ...)]
          [(lambda ,formalexp (settable (,set* ...) ,[body]))
           `(lambda ,formalexp (settable (,set* ...) ,body))]
          [(case-lambda [,formalexp (settable (,set** ...) ,[body])] ...)
           `(case-lambda [,formalexp (settable (,set** ...) ,body)] ...)]
          [(let ([,lhs* ,[rhs*]] ...) (settable (,set* ...) ,[body]))
           `(let ([,lhs* ,rhs*] ...) (settable (,set* ...) ,body))]
          [(letrec ([,lhs* ,[rhs*]] ...) (settable (,set* ...) ,[body]))
           (cond
             [(and (null? set*) (andmap lambda? rhs*))
              `(letrec ([,lhs* ,rhs*] ...) ,body)]
             [else
               (let ([tmp* (map unique-name lhs*)])
                 `(let ([,lhs* (void)] ...)
                    (settable
                      (,lhs* ...)
                      ,(make-begin
                         `((let ([,tmp* ,rhs*] ...)
                             (settable ()
                                       ,(make-begin `((set! ,lhs* ,tmp*) ...))))
                           ,body)))))])]
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
          [(invoke-static-method ,class ,method-name ,type-sig (,[arg*] ...))
           `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))]
          [(,prim ,[rand*] ...)
           (guard (extended-scheme-primitive? prim))
           `(,prim ,rand* ...)]
          [(,[rator] ,[rand*] ...)
           `(,rator ,rand* ...)]
          [,unmatched
            (error 'remove-impure-letrec "invalid expression: ~s"
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
          [,unmatched (error 'remove-impure-letrec
                             "invalid class defn ~s"
                             cdef)])))
    (define process-method
      (lambda (meth)
        (match meth
          [(lambda ,args (settable ,set ,body))
           (let ([body (Expr body)])
             `(lambda ,args (settable ,set ,body)))])))
    
    (lambda (prog)
      (match prog
        [(,input-language
           (quote (program ,sym* ,pkg* ,class-defns* ... ,body)))
         (let ([class-defns* (map process-class-defn class-defns*)])
           (let ([body (Expr body)])
             `(,input-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))