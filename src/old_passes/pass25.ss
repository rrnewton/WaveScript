;===============================================================================
;;; Pass 25: uncover-calltype
;;; September 2001

;;; This pass (which isn't much of one) introduces two tags -- (tailcall ___)
;;; and (effectcall ___) -- which it wraps around the appropriate application
;;; expressions.  These are needed when the program gets flattened and
;;; normalize-context's hard work is dissolved.

;;; <Input> ::= (<language-name> <Program>)
;;; <Program> ::= (program (<var>*) (<pkg>*) <class-def>* <Letrec>)
;;; <Letrec>  ::= (letrec ((<var> <ClassDef>)*) <Tail>)
;;; <ClassDef> ::= (class-def (<freevar> ...) <Lambda>)
;;; <Lambda>  ::= (lambda <Formalexp> <Tail>)
;;; <Formalexp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)
;;; <Final>   ::= (quote <imm>)
;;;             | <var>
;;;             | (class <var>)
;;;             | (<value primitive> <Nontail>*)
;;;             | (tailcall (<Nontail> <Nontail>*))
;;;             | (toplvl-varref <var>)
;;; <Tail>    ::= (return <Final>)
;;;             | (if <Pred> <Tail> <Tail>)
;;;             | (begin <Effect>* <Tail>)
;;;             | (let ((<var> <Nontail>)*) <Tail>)
;;; <Nontail> ::= (quote <imm>)
;;;             | <var>
;;;             | (class <var>)
;;;             | (if <Pred> <Nontail> <Nontail>)
;;;             | (begin <Effect>* <Nontail>)
;;;             | (let ((<var> <Nontail>)*) <Nontail>)
;;;             | (<value primitive> <Nontail>*)
;;;             | (<Nontail> <Nontail>*)
;;;             | (toplvl-varref <var>)
;;; <Pred>    ::= (quote <boolean>)
;;;             | (if <Pred> <Pred> <Pred>)
;;;             | (begin <Effect>* <Pred>)
;;;             | (let ((<var> <Nontail>)*) <Pred>)
;;;             | (<predicate primitive> <Nontail>*)
;;; <Effect>  ::= (nop)
;;;             | (if <Pred> <Effect> <Effect>)
;;;             | (begin <Effect>* <Effect>)
;;;             | (let ((<var> <Nontail>)*) <Effect>)
;;;             | (<effect primitive> <Nontail>*)
;;;             | (effectcall (<Nontail> <Nontail>*))
;;;             | (toplvl-varassign! <var> <Exp>)

(define uncover-calltype
  (let ()
    (define Effect
      (lambda (expr)
        (match expr
          [(nop) '(nop)]
          [(toplvl-varassign! ,var ,[Nontail -> rhs])
           `(toplvl-varassign! ,var ,rhs)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> expr*] ... ,[expr])
           `(begin ,expr* ... ,expr)]
          [(let ([,lhs* ,[Nontail -> rhs*]] ...) ,[body])
           `(let ([,lhs* ,rhs*] ...) ,body)]
          [(let-class (,class-defn* ...) ,[body])
           (let ([class-defn* (map process-class-defn class-defn*)])
             `(let-class ,class-defn* ,body))]
          [(open-instance ,obj ,class-name ,[body])
           `(open-instance ,obj ,class-name ,body)]
          [(invoke-method ,obj ,class ,method (,[Nontail -> arg*] ...))
           `(invoke-method ,obj ,class ,method (,arg* ...))]
          [(foreign-call ,name ,type-sig (,[Nontail -> arg*] ...))
           `(foreign-call ,name ,type-sig (,arg* ...))]
          [(set-field! ,field ,[Nontail -> rhs])
           `(set-field! ,field ,rhs)]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[Nontail -> arg*] ...))
           `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))]
          [(,prim ,[Nontail -> rand*] ...)
           (guard (effect-primitive? prim))
           `(,prim ,rand* ...)]
          [(direct-call (class ,f) ,[Nontail -> rand*] ...)
           `(effectcall (direct-call (class ,f) ,rand* ...))]
          [(anon-call ,overflowed? ,[Nontail -> rator] ,[Nontail -> rand*] ...)
           `(effectcall (anon-call ,overflowed? ,rator ,rand* ...))]
          [,expr (error 'uncover-calltype
                        "invalid Effect expression: ~s"
                        expr)])))
    (define Pred
      (lambda (expr)
        (match expr
          [(quote ,b) (guard (boolean? b)) `(quote ,b)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> expr*] ... ,[expr])
           `(begin ,expr* ... ,expr)]
          [(let ([,lhs* ,[Nontail -> rhs*]] ...) ,[body])
           `(let ([,lhs* ,rhs*] ...) ,body)]
          [(let-class (,class-defn* ...) ,[body])
           (let ([class-defn* (map process-class-defn class-defn*)])
             `(let-class ,class-defn* ,body))]
          [(open-instance ,obj ,class-name ,[body])
           `(open-instance ,obj ,class-name ,body)]
          [(,prim ,[Nontail -> rand*] ...)
           (guard (predicate-primitive? prim))
           `(,prim ,rand* ...)]
          [,expr (error 'uncover-calltype
                        "invalid Pred expression: ~s"
                        expr)])))
    (define Nontail
      (lambda (expr)
        (match expr
          [(quote ,imm) `(quote ,imm)]
          [,var (guard (symbol? var)) var]
          [(toplvl-varref ,var) `(toplvl-varref ,var)]
          [(class ,var) `(class ,var)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> expr*] ... ,[expr])
           `(begin ,expr* ... ,expr)]
          [(let ([,lhs* ,[Nontail -> rhs*]] ...) ,[body])
           `(let ([,lhs* ,rhs*] ...) ,body)]
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
           (guard (value-primitive? prim))
           `(,prim ,rand* ...)]
          [(direct-call (class ,f) ,[rand*] ...)
           `(direct-call (class ,f) ,rand* ...)]
          [(anon-call ,overflowed? ,[rator] ,[rand*] ...)
           `(anon-call ,overflowed? ,rator ,rand* ...)]
          [,expr (error 'uncover-calltype
                        "invalid Nontail expression: ~s"
                        expr)])))
    (define Tail
      (lambda (expr)
        (match expr
          [(return ,[Final -> expr]) `(return ,expr)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> expr*] ... ,[expr])
           `(begin ,expr* ... ,expr)]
          [(let ([,lhs* ,[Nontail -> rhs*]] ...) ,[body])
           `(let ([,lhs* ,rhs*] ...) ,body)]
          [(let-class (,class-defn* ...) ,[body])
           (let ([class-defn* (map process-class-defn class-defn*)])
             `(let-class ,class-defn* ,body))]
          [(new ,class-name ,[Nontail -> args*] ...)
           `(new ,class-name ,args* ...)]
          [(open-instance ,obj ,class-name ,[body])
           `(open-instance ,obj ,class-name ,body)]
          [(object-reference ,obj ,class ,field)
           `(object-reference ,obj ,class ,field)]
          [(invoke-method ,obj ,class ,method (,[Nontail -> arg*] ...))
           `(invoke-method ,obj ,class ,method (,arg* ...))]
          [(foreign-call ,name ,type-sig (,[Nontail -> arg*] ...))
           `(foreign-call ,name ,type-sig (,arg* ...))]
          [(static-ref ,class ,field)
           `(static-ref ,class ,field)]
          [(this-ref ,class ,field)
           `(this-ref ,class ,field)]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[Nontail -> arg*] ...))
           `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))]
          [,expr (error 'uncover-calltype
                        "invalid Tail expression: ~s"
                        expr)])))
    (define Final
      (lambda (expr)
        (match expr
          [(quote ,imm) `(quote ,imm)]
          [,var (guard (symbol? var)) var]
          [(toplvl-varref ,var) `(toplvl-varref ,var)]
          [(class ,var) `(class ,var)]
          [(,prim ,[Nontail -> rand*] ...)
           (guard (value-primitive? prim))
           `(,prim ,rand* ...)]
          [(new ,class-name ,[Nontail -> args*] ...)
           `(new ,class-name ,args* ...)]
          [(object-reference ,obj ,class ,field)
           `(object-reference ,obj ,class ,field)]
          [(invoke-method ,obj ,class ,method (,[Nontail -> arg*] ...))
           `(invoke-method ,obj ,class ,method (,arg* ...))]
          [(foreign-call ,name ,type-sig (,[Nontail -> arg*] ...))
           `(foreign-call ,name ,type-sig (,arg* ...))]
          [(static-ref ,class ,field)
           `(static-ref ,class ,field)]
          [(this-ref ,class ,field)
           `(this-ref ,class ,field)]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[Nontail -> arg*] ...))
           `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))]
          [(direct-call (class ,f) ,[Nontail -> rand*] ...)
           `(tailcall (direct-call (class ,f) ,rand* ...))]
          [(anon-call ,overflowed? ,[Nontail -> rator] ,[Nontail -> rand*] ...)
           `(tailcall (anon-call ,overflowed? ,rator ,rand* ...))]
          [,expr (error 'uncover-calltype
                        "invalid Final expression: ~s" expr)])))
    (define Lambda
      (lambda (expr)
        (match expr
          [(lambda ,formalexp ,[Tail -> body])
           `(lambda ,formalexp ,body)])))
    (define ClassDef
      (lambda (expr)
        (match expr
          [(class-def (,free* ...) ,[Lambda -> body])
           `(class-def (,free* ...) ,body)])))
    (define Letrec
      (lambda (expr)
        (match expr
          [(letrec ([,lhs* ,[ClassDef -> rhs*]] ...) (entry-point ,main))
           `(letrec ([,lhs* ,rhs*] ...) (entry-point ,main))]
          [(letrec ([,lhs* ,[ClassDef -> rhs*]] ...)
             ,[Tail -> body])
           `(letrec ([,lhs* ,rhs*] ...) ,body)]
          )))
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
          [,unmatched (error 'uncover-calltype
                             "invalid class defn ~s"
                             cdef)])))
    (define process-method
      (lambda (meth)
        (match meth
          [(lambda ,args ,body)
           (let ([body (Letrec body)])
             `(lambda ,args ,body))])))
    (lambda (prog)
      (match prog
        [(,input-language
           (quote (program ,sym* ,pkg* ,class-defns* ... ,body)))
         (let ([class-defns* (map process-class-defn class-defns*)])
           (let ([body (Letrec body)])
             `(uncover-calltype-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))