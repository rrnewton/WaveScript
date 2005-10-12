;;; Pass 15: uncover-free
;;; January 2001

;;; This pass wraps the body of each lambda expression in a free form
;;; that lists the free variables of the lambda expression.  For
;;; example:
;;;
;;; (let ([x.2 '3] [y.1 '4])
;;;   (letrec ([f.3 (lambda (z.5) (* y.1 z.5))])
;;;     (letrec ([g.4 (lambda () (f.3 (+ x.2 y.1)))])
;;;       (g.4))))
;;;
;;; becomes
;;;
;;; (let ([x.2 '3] [y.1 '4])
;;;   (letrec ([f.3 (lambda (z.5) (free (y.1) (* y.1 z.5)))])
;;;     (letrec ([g.4 (lambda ()
;;;                     (free (f.3 x.2 y.1)
;;;                       (f.3 (+ x.2 y.1))))])
;;;       (g.4))))

;;; The input language is the same as the output language of Pass 10.

;;; The output language differs in that each lambda body is wrapped
;;; in a free form.

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
;;; <Lambda> ::= (lambda <Formalexp> (free (<var>*) <Exp>))
;;; <Formalexp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)

;;; The implementation requires difference, extended-scheme-primitive?,
;;; union, and get-formals from helpers.ss.

(define uncover-free
  (let ()
    (define LambdaClause
      (lambda (formalexp body)
        (mvlet ([(body body-free*) (Expr body)])
          (let ((free* (difference body-free* (get-formals formalexp))))
            (values
              `(,formalexp (free ,free* ,body))
              free*)))))
    (define Lambda
      (lambda (expr)
        (match expr
          [(lambda ,formalexp ,body)
           (mvlet ([(clause free*) (LambdaClause formalexp body)])
             (values `(lambda ,@clause) free*))]
          [(case-lambda ([,formalexp* ,body*] ...),indexmap ...)
           (let loop ([f* formalexp*] [b* body*]
                      [clauses '()] [free** '()])
             (if (null? f*)
                 (values
                   `(case-lambda ,(reverse clauses) ,@indexmap)
                   (apply union free**))
                 (mvlet ([(clause free*) (LambdaClause (car f*) (car b*))])
                   (loop (cdr f*) (cdr b*)
                         (cons clause clauses)
                         (cons free* free**)))))]
          [,unmatched
            (error 'uncover-free "invalid lambda expression: ~s"
                   unmatched)])))
    (define Expr
      (lambda (expr)
        (match expr
          [(quote ,imm) (values `(quote ,imm) '())]
          [,var (guard (symbol? var)) (values var (list var))]
          [(toplvl-varref ,var)
           (values `(toplvl-varref ,var) '())]
          [(toplvl-varassign! ,var ,[rhs rhs-free*])
           (values `(toplvl-varassign! ,var ,rhs) rhs-free*)]
          [(if ,[test test-free*] ,[conseq conseq-free*] ,[altern altern-free*])
           (values
             `(if ,test ,conseq ,altern)
             (union test-free* (union conseq-free* altern-free*)))]
          [(begin ,[expr* expr-free**] ...)
           (values
             `(begin ,expr* ...)
             (apply union expr-free**))]
          [(let ([,lhs* ,[rhs* rhs-free*]] ...) ,[body body-free*])
           (values
             `(let ([,lhs* ,rhs*] ...) ,body)
             (union (apply union rhs-free*) (difference body-free* lhs*)))]
          [(letrec ([,lhs* ,[Lambda -> rhs* rhs-free*]] ...) ,[body body-free*])
           (values
             `(letrec ([,lhs* ,rhs*] ...) ,body)
             (difference (union (apply union rhs-free*) body-free*) lhs*))]
          [(,prim ,[rand* rand-free*] ...)
           (guard (extended-scheme-primitive? prim))
           (values
             `(,prim ,rand* ...)
             (apply union rand-free*))]
          [(let-class (,class-defn* ...) ,[body body-free*])
           (let ([class-defn* (map process-class-defn class-defn*)])
             (values `(let-class ,class-defn* ,body) body-free*))]
          [(new ,class-name ,[args* args-free**] ...)
           (values `(new ,class-name ,args* ...)
                   (apply append args-free**))]
          [(open-instance ,obj ,class-name ,[body body-free*])
           (values `(open-instance ,obj ,class-name ,body)
                   body-free*)]
          [(object-reference ,obj ,class ,field)
           (values `(object-reference ,obj ,class ,field) '())]
          [(invoke-method ,obj ,class ,method (,[arg* arg-free**] ...))
           (values `(invoke-method ,obj ,class ,method (,arg* ...))
                   (apply append arg-free**))]
          [(foreign-call ,name ,type-sig (,[arg* arg-free**] ...))
           (values `(foreign-call ,name ,type-sig (,arg* ...))
                   (apply append arg-free**))]
          [(static-ref ,class ,field)
           (values `(static-ref ,class ,field) '())]
          [(this-ref ,class ,field)
           (values `(this-ref ,class ,field) '())]
          [(set-field! ,field ,[rhs rhs-free*])
           (values `(set-field! ,field ,rhs) rhs-free*)]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[arg* arg-free**] ...))
           (values `(invoke-static-method ,class ,method-name
                                          ,type-sig (,arg* ...))
                   (apply append arg-free**))]
          [(,[rator rator-free*] ,[rand* rand-free*] ...)
           (values
             `(,rator ,rand* ...)
             (union rator-free* (apply union rand-free*)))]
          [,unmatched
            (error 'uncover-free "invalid expression: ~s"
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
          [,unmatched (error 'verify-scheme
                             "invalid class defn ~s"
                             cdef)])))
    (define process-method
      (lambda (meth)
        (match meth
          [(lambda ,args ,body)
           (mvlet ([(body body-free*) (Expr body)])
             `(lambda ,args (free ,body-free* ,body)))])))
    (lambda (prog)
      (match prog
        [(,input-language
           (quote (program ,sym* ,pkg* ,class-defns* ... ,body)))
         (let ([class-defns* (map process-class-defn class-defns*)])
           (mvlet ([(body body-free*) (Expr body)])
             `(uncover-free-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))