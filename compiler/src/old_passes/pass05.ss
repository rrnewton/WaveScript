;;; Pass 5: remove-one-armed-if
;;; January 2001

;;; This pass replaces one-armed if expressions with two-armed if
;;; expressions by providing an explicit alternative, i.e.,
;;; (if e1 e2) => (if e1 e2 (void)).

;;; The input language is the same as the output language of Pass 4.

;;; Output from this pass is in the same language, except that
;;; there are no one-armed if expressions:

;;; <Exp>  ::= (quote <datum>)
;;;          | <var>
;;;          | (set! <var> <Exp>)
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (begin <Exp> <Exp>*)
;;;          | (lambda <FormalExp> <Exp>)
;;;          | (case-lambda [<Formalexp> <Exp>]*)
;;;          | (let (<Decl>*) <Exp>)
;;;          | (letrec (<Decl>*) <Exp>)
;;;          | (<primitive> <Exp>*)
;;;          | (<Exp> <Exp>*)
;;;          | (toplvl-varref <var>)
;;;          | (toplvl-varassign! <var> <Exp>)
;;; <Decl> ::= (<var> <Exp>)
;;; <FormalExp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)

;;; The implementation requires scheme-primitive? from helpers.ss.

(define remove-one-armed-if
  (let ()
    (define process-expr
      (lambda (expr)
        (match expr
          [(quote ,datum) `(quote ,datum)]
          [,var (guard (symbol? var)) var]
          [(toplvl-varref ,var) `(toplvl-varref ,var)]
          [(toplvl-varassign! ,var ,[rhs])
           `(toplvl-varassign! ,var ,rhs)]
          [(set! ,var ,[rhs])
           `(set! ,var ,rhs)]
          [(if ,[test] ,[conseq])
           `(if ,test ,conseq (void))]
          [(if ,[test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[expr*] ...)
           `(begin ,expr* ...)]
          [(lambda ,formals ,[body])
           `(lambda ,formals ,body)]
          [(case-lambda [,formals* ,[body*]] ...)
           `(case-lambda [,formals* ,body*] ...)]
          [(let ([,lhs* ,[rhs*]] ...) ,[body])
           `(let ([,lhs* ,rhs*] ...) ,body)]
          [(letrec ([,lhs* ,[rhs*]] ...) ,[body])
           `(letrec ([,lhs* ,rhs*] ...) ,body)]
          [(,prim ,[rand*] ...)
           (guard (scheme-primitive? prim))
           `(,prim ,rand* ...)]
          [(,[rator] ,[rand*] ...)
           `(,rator ,rand* ...)]
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
          [,unmatched
            (error 'remove-one-armed-if "invalid expression: ~s"
                   unmatched)])))
    (define process-class-defn
      (lambda (cdef)
        (match cdef
          [(define-class ,name ,base
             ,ctor
             (fields (,fmods** ... ,fname*) ...)
             (methods (,mmods** ... ,mtype*
                        (,mname* ,mbody*)) ...))
           (let ([ctor (process-expr ctor)]
                 [mbody* (map process-expr mbody*)])
             `(define-class
                ,name ,base ,ctor
                (fields (,fmods** ... ,fname*) ...)
                (methods (,mmods** ... ,mtype*
                           (,mname* ,mbody*)) ...)))]
          [,unmatched (error 'remove-one-armed-if
                             "invalid class defn ~s"
                             cdef)])))
    (lambda (expr)
      (match expr
        [(,input-language (quote (program ,pkg* ,class-defns* ... ,body)))
         (let ([class-defns* (map process-class-defn class-defns*)])
           (let ([body (process-expr body)])
             `(,input-language '(program ,pkg* ,class-defns* ... ,body))))]))))