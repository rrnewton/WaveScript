;;; Pass 8: uncover-settable
;;; January 2001

;;; This pass locates all settable variables (variables that appear
;;; on the left-hand side of a set!) and records each within the
;;; lambda, let, or letrec form that binds it.  The body of each
;;; lambda, let, and letrec expression is wrapped in a settable form
;;; listing the settable variables bound by the expression, e.g.:

;;; (let ([x.2 '3] [y.1 '4])
;;;   (letrec ([f.3 (lambda (z.4)
;;;                   (begin
;;;                     (set! z.4 '0)
;;;                     z.4))])
;;;     (begin
;;;       (set! y.1 (+ y.1 '1))
;;;       (+ (f.3 x.2) y.1))))
;;;
;;; becomes
;;;
;;; (let ([x.2 '3] [y.1 '4])
;;;   (settable (y.1)
;;;     (letrec ([f.3 (lambda (z.4)
;;;                     (settable (z.4)
;;;                       (begin
;;;                         (set! z.4 '0)
;;;                         z.4)))])
;;;       (settable ()
;;;         (begin
;;;           (set! y.1 (+ y.1 '1))
;;;           (+ (f.3 x.2) y.1))))))

;;; The input language is the same as the output language of Pass 6.

;;; Expressions output from this pass are in the same language,
;;; augmented with the new settable form.

;;; <Input> ::= (<language-name> <Program>)
;;; <Program> ::= (program (<var>*) (<pkg>*) <class-def>* <Exp>)

;;; <Exp>  ::= (quote <imm>)
;;;          | <var>
;;;          | (set! <var> <Exp>)
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (begin <Exp> <Exp>*)
;;;          | (lambda <FormalExp> (settable (<var>*) <Exp>))
;;;          | (case-lambda [<Formalexp> (settable (<var>*) <Exp>)]*)
;;;          | (let (<Decl>*) (settable (<var>*) <Exp>))
;;;          | (letrec (<Decl>*) (settable (<var>*) <Exp>))
;;;          | (<primitive> <Exp>*)
;;;          | (<Exp> <Exp>*)
;;;          | (toplvl-varref <var>)
;;;          | (toplvl-varassign! <var> <Exp>)
;;; <Decl> ::= (<var> <Exp>)
;;; <FormalExp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)

;;; The implementation requires difference, intersection,
;;; extended-scheme-primitive?, set-cons, get-formals, and
;;; union from helpers.ss.

;;; The implementation uses multiple return values to return both the
;;; rewritten expression and a set of settable variables found in the
;;; expression.

(define uncover-settable
  (let ()
    ;; Returns two values in a pair:
    (define process-lambda-clause
      (lambda (formalexp body body-set*)
        (let ([formals (get-formals formalexp)])
          (let ([set* (intersection formals body-set*)])
            (cons
              `(,formalexp (settable ,set* ,body))
              (difference body-set* set*))))))
    (define Expr
      (lambda (expr)
        (match expr
          [(quote ,imm) (values `(quote ,imm) '())]
          [,var (guard (symbol? var)) (values var '())]
          [(toplvl-varref ,var)
           (values `(toplvl-varref ,var) '())]
          [(toplvl-varassign! ,var ,[rhs rhssettable])
           (values `(toplvl-varassign! ,var ,rhs)
                   rhssettable)]
          [(set! ,var ,[rhs rhs-set*])
           (values
             `(set! ,var ,rhs)
             (if (memv #\. (string->list (symbol->string var)))
                 rhs-set*
                 (set-cons var rhs-set*)))]
          [(if ,[test test-set*] ,[conseq conseq-set*] ,[altern altern-set*])
           (values
             `(if ,test ,conseq ,altern)
             (union test-set* conseq-set* altern-set*))]
          [(begin ,[expr* expr-set**] ...)
           (values
             `(begin ,expr* ...)
             (apply union expr-set**))]
          [(lambda ,formalexp ,[body body-set*])
           (let ([processed
                   (process-lambda-clause formalexp body body-set*)])
             (let ([clause (car processed)]
                   [up-set* (cdr processed)])
               (values (cons 'lambda clause) up-set*)))]
          [(case-lambda [,formalexp* ,[body* body-set**]] ...)
           (let ([processed (map process-lambda-clause
                                 formalexp* body* body-set**)])
             (let ([clauses (map car processed)]
                   [up-set** (map cdr processed)])
               (values
                 (cons 'case-lambda clauses)
                 (apply union up-set**))))]
          [(let ([,lhs* ,[rhs* rhs-set*]] ...) ,[body body-set*])
           (let ([set* (intersection lhs* body-set*)])
             (values
               `(let ([,lhs* ,rhs*] ...)
                  (settable ,set* ,body))
               (union (apply union rhs-set*) (difference body-set* set*))))]
          [(letrec ([,lhs* ,[rhs* rhs-set*]] ...) ,[body body-set*])
           (let ([all-set* (union (apply union rhs-set*) body-set*)])
             (let ([set* (intersection lhs*
                                       (union (apply union rhs-set*) body-set*))])
               (values
                 `(letrec ([,lhs* ,rhs*] ...)
                    (settable ,set* ,body))
                 (difference all-set* set*))))]
          [(let-class (,class-defn* ...) ,[body body-set*])
           (let ([class-defn* (map process-class-defnclass-defn*)])
             (values `(let-class ,class-defn* ,body) body-set*))]
          [(new ,class-name ,[args* args-set**] ...)
           (values `(new ,class-name ,args* ...)
                   (apply append args-set**))]
          [(open-instance ,obj ,class-name ,[body body-set*])
           (values `(open-instance ,obj ,class-name ,body)
                   body-set*)]
          [(object-reference ,obj ,class ,field)
           (values `(object-reference ,obj ,class ,field) '())]
          [(invoke-method ,obj ,class ,method (,[arg* arg-set**] ...))
           (values `(invoke-method ,obj ,class ,method (,arg* ...))
                   (apply append arg-set**))]
          [(foreign-call ,name ,type-sig (,[arg* arg-set**] ...))
           (values `(foreign-call ,name ,type-sig (,arg* ...))
                   (apply append arg-set**))]
          [(static-ref ,class ,field)
           (values `(static-ref ,class ,field) '())]
          [(this-ref ,class ,field)
           (values `(this-ref ,class ,field) '())]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[arg* arg-set**] ...))
           (values `(invoke-static-method ,class ,method-name
                                          ,type-sig (,arg* ...))
                   (apply append arg-set**))]
          [(,prim ,[rand* rand-set*] ...)
           (guard (extended-scheme-primitive? prim))
           (values
             `(,prim ,rand* ...)
             (apply union rand-set*))]
          [(,[rator rator-set*]  ,[rand* rand-set*] ...)
           (values
             `(,rator ,rand* ...)
             (union rator-set* (apply union rand-set*)))]
          [,unmatched
            (error 'uncover-settable "invalid expression: ~s"
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
          [,unmatched (error 'uncover-settable
                             "invalid class defn ~s"
                             cdef)])))
    (define process-method
      (lambda (meth)
        (match meth
          [(lambda ,args ,body)
           (mvlet ([(body body-set*) (Expr body)])
             `(lambda ,args (settable ,body-set* ,body)))])))
    (lambda (expr)
      (match expr
        [(,input-language
           (quote (program ,sym* ,pkg* ,class-defns* ... ,body)))
         (let ([class-defns* (map process-class-defn class-defns*)])
           (mvlet ([(body body-set*) (Expr body)])
             `(uncover-settable-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))
  