;;; Pass 7: uncover-symbol-references
;;; March 2002

;;; This pass compiles a list of all symbol-references in the program:
;;; either quoted symbols or references to top-level variables.

;;; The only change in the language is that a list of used symbols is
;;; added to the (program ...) form:
;;; This doesn't require a new language definition, the new program form
;;; is handled by the language mechanism.  The (program ... body) simply
;;; form ignores all arguments but the last.

;;; <Prog> ::= (program (<var>) (<pkg-ref>*) <class-defs>* <Exp>)

;;; <Exp>  ::= (quote <imm>)
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

(define uncover-symbol-references
  (let ()
    (define Expr
      (lambda (expr)
        (match expr
          [(quote ,datum)
           (guard (symbol? datum))
           (values `(quote ,datum) (list datum))]
          [(quote ,datum)
           (values `(quote ,datum) '())]
          [,var (guard (symbol? var))
            (values var '())]
          [(toplvl-varref ,var)
           (values `(toplvl-varref ,var)
                   (list var))]
          [(toplvl-varassign! ,var ,[rhs rhssyms])
           (values `(toplvl-varassign! ,var ,rhs)
                   (union (list var) rhssyms))]
          [(set! ,var ,[rhs rhs-syms])
           (values `(set! ,var ,rhs) rhs-syms)]
          [(if ,[test test-s*] ,[conseq conseq-s*] ,[altern altern-s*])
           (values `(if ,test ,conseq ,altern)
                   (union test-s* conseq-s* altern-s*))]
          [(begin ,[expr* expr-s**] ...)
           (values `(begin ,expr* ...) (apply union expr-s**))]
          [(lambda ,formals ,[body body-s*])
           (values `(lambda ,formals ,body) body-s*)]
          [(case-lambda [,formals* ,[body* body-s**]] ...)
           (values `(case-lambda [,formals* ,body*] ...)
                   (apply append body-s**))]
          [(let ([,lhs* ,[rhs* rhs-s**]] ...) ,[body body-s*])
           (values `(let ([,lhs* ,rhs*] ...) ,body)
                   (union body-s* (apply union rhs-s**)))]
          [(letrec ([,lhs* ,[rhs* rhs-s**]] ...) ,[body body-s*])
           (values `(letrec ([,lhs* ,rhs*] ...) ,body)
                   (union body-s* (apply union rhs-s**)))]
          
          [(let-class (,class-defn* ...) ,[body body-s*])
           (let ([class-defn* (map process-class-defnclass-defn*)])
             (values `(let-class ,class-defn* ,body) body-s*))]
          [(new ,class-name ,[args* args-s**] ...)
           (values `(new ,class-name ,args* ...)
                   (apply union args-s**))]
          [(open-instance ,obj ,class-name ,[body body-s*])
           (values `(open-instance ,obj ,class-name ,body)
                   body-s*)]
          [(object-reference ,obj ,class ,field)
           (values `(object-reference ,obj ,class ,field) '())]
          [(invoke-method ,obj ,class ,method (,[arg* arg-s**] ...))
           (values `(invoke-method ,obj ,class ,method (,arg* ...))
                   (apply union arg-s**))]
          [(foreign-call ,name ,type-sig (,[arg* arg-s**] ...))
           (values `(foreign-call ,name ,type-sig (,arg* ...))
                   (apply union arg-s**))]
          [(static-ref ,class ,field)
           (values `(static-ref ,class ,field) '())]
          [(this-ref ,class ,field)
           (values `(this-ref ,class ,field) '())]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[arg* arg-s**] ...))
           (values `(invoke-static-method ,class ,method-name
                                          ,type-sig (,arg* ...))
                   (apply union arg-s**))]
          [(,prim ,[rand* rand-s**] ...)
           (guard (extended-scheme-primitive? prim))
           (values `(,prim ,rand* ...) (apply union rand-s**))]
          
          [(,[rator rator-s*] ,[rand* rand-s**] ...)
           (values `(,rator ,rand* ...)
                   (union rator-s* (apply union rand-s**)))]
          [,unmatched
            (error 'remove-complex-constant "invalid expression: ~s"
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
          [,unmatched (error 'remove-complex-constant
                             "invalid class defn ~s"
                             cdef)])))
    (define process-method
      (lambda (meth)
        (match meth
          [(lambda ,args ,body)
           (mvlet ([(body body-s*) (Expr body)])
             (if (null? body-s*)
                 `(lambda ,args ,body)
                 `(lambda ,args (let ,body-s* ,body))))])))
    (lambda (expr)
      (match expr
        [(,input-language (quote (program ,pkg* ,class-defns* ... ,body)))
         (let ([class-defns* (map (lambda (x)
                                    (process-class-defn x))
                                  class-defns*)])
           (mvlet ([(body sym*) (Expr body)])
             `(,input-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]
        ))))