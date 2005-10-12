;===============================================================================
;;; Pass 28: the-return-of-set!
;;; September 2001

;;; This pass converts each let binding into a set! and pushes set!
;;; expressions inside begin and if expressions.

;;; Output grammar:

;;; <Input>    ::= (<language-name> <Program>)
;;; <Program>  ::= (program (<var>*) (<pkg>*) <class-def>* <Letrec>)
;;; <Letrec>  ::= (letrec ((<var> <ClassDef>)*) <Local>)
;;; <ClassDef> ::= (class-def (<freevar> ...) <Lambda>)
;;; <Lambda>  ::= (lambda <Formalexp> <Local>)
;;; <Formalexp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)
;;; <Local>   ::= (local (<var>*) <Tail>)
;;; <Tail>    ::= (return <Final>)
;;;             | (if <Pred> <Tail> <Tail>)
;;;             | (begin <Effect>* <Tail>)
;;; <Final>   ::= (quote <imm>)
;;;             | <var>
;;;             | (class <var>)
;;;             | (<value primitive> <Nontail>*)
;;;             | (tailcall (<Nontail> <Nontail>*))
;;;             | (toplvl-varref <var>)
;;; <Nontail> ::= (quote <imm>)
;;;             | <var>
;;;             | (class <var>)
;;;             | (<value primitive> <Nontail>*)
;;;             | (if <Pred> <Nontail> <Nontail>)
;;;             | (begin <Effect>* <Nontail>)
;;; <Pred>    ::= (quote <boolean>)
;;;             | (if <Pred> <Pred> <Pred>)
;;;             | (begin <Effect>* <Pred>)
;;;             | (<predicate primitive> <Nontail>*)
;;; <Effect>  ::= (nop)
;;;             | (if <Pred> <Effect> <Effect>)
;;;             | (begin <Effect>* <Effect>)
;;;             | (set! <var> <Nontail>)
;;;             | (<effect primitive> <Nontail>*)
;;;             | (effectcall (<Nontail> <Nontail>*))
;;;             | (toplvl-varassign! <var> <Exp>)


;;; The implementation uses effect-primitive?, predicate-primitive?,
;;; and value-primitive? from helpers.ss.

(define the-return-of-set!
  (let ()
    (define postprocess-set!
      (lambda (lhs rhs)
        (match rhs
          [(if ,test ,conseq ,altern)
           `(if ,test ,(postprocess-set! lhs conseq) ,(postprocess-set! lhs altern))]
          [(begin ,expr* ... ,expr)
           `(begin ,expr* ... ,(postprocess-set! lhs expr))]
          [,rhs `(set! ,lhs ,rhs)])))
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
          [(tailcall (direct-call (class ,f) ,[Nontail -> rand*] ...))
           `(tailcall (direct-call (class ,f) ,rand* ...))]
          [(tailcall (anon-call ,overflowed?
                                ,[Nontail -> rator] ,[Nontail -> rand*] ...))
           `(tailcall (anon-call ,overflowed? ,rator ,rand* ...))]
          [,expr (error 'the-return-of-set!
                        "invalid Final expression: ~s" expr)])))
    (define Effect
      (lambda (expr)
        (match expr
          [(nop) '(nop)]
          [(toplvl-varassign! ,var ,[Nontail -> rhs])
           `(toplvl-varassign! ,var ,rhs)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> expr*] ... ,[expr])
           (make-begin `(,expr* ... ,expr))]
          [(let ([,lhs ,[Nontail -> rhs]]) ,[body])
           (make-begin `(,(postprocess-set! lhs rhs) ,body))]
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
           `(set! ,field ,rhs)]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[Nontail -> arg*] ...))
           `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))]
          [(,prim ,[Nontail -> rand*] ...)
           (guard (effect-primitive? prim))
           `(,prim ,rand* ...)]
          [(effectcall (anon-call ,overflowed?
                                  ,[Nontail -> rator] ,[Nontail -> rand*] ...))
           `(effectcall (anon-call ,overflowed? ,rator ,rand* ...))]
          [(effectcall (direct-call (class ,f) ,[Nontail -> rand*] ...))
           `(effectcall (direct-call (class ,f) ,rand* ...))]
          [,expr (error 'the-return-of-set!
                        "invalid Effect expression: ~s"
                        expr)])))
    (define Pred
      (lambda (expr)
        (match expr
          [(quote ,b) (guard (boolean? b)) `(quote ,b)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> expr*] ... ,[expr])
           (make-begin `(,expr* ... ,expr))]
          [(let ([,lhs ,[Nontail -> rhs]]) ,[body])
           (make-begin `(,(postprocess-set! lhs rhs) ,body))]
          [(let-class (,class-defn* ...) ,[body])
           (let ([class-defn* (map process-class-defn class-defn*)])
             `(let-class ,class-defn* ,body))]
          [(open-instance ,obj ,class-name ,[body])
           `(open-instance ,obj ,class-name ,body)]
          [(,prim ,[Nontail -> rand*] ...)
           (guard (predicate-primitive? prim))
           `(,prim ,rand* ...)]
          [,expr (error 'the-return-of-set!
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
           (make-begin `(,expr* ... ,expr))]
          [(let ([,lhs ,[Nontail -> rhs]]) ,[body])
           (make-begin `(,(postprocess-set! lhs rhs) ,body))]
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
          [,expr (error 'the-return-of-set!
                        "invalid Nontail expression: ~s"
                        expr)])))
    (define Tail
      (lambda (expr)
        (match expr
          [(return ,[Final -> expr]) `(return ,expr)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> expr*] ... ,[expr])
           (make-begin `(,expr* ... ,expr))]
          [(let ([,lhs ,[Nontail -> rhs]]) ,[body])
           (make-begin `(,(postprocess-set! lhs rhs) ,body))]
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
          [,expr (error 'the-return-of-set!
                        "invalid Tail expression: ~s"
                        expr)])))
    (define Lambda
      (lambda (expr)
        (match expr
          [(lambda ,formalexp (local (,local* ...) ,[Tail -> body]))
           `(lambda ,formalexp (local (,local* ...) ,body))])))
    (define ClassDef
      (lambda (expr)
        (match expr
          [(class-def (,free* ...) ,[Lambda -> body])
           `(class-def (,free* ...) ,body)])))
    (define Letrec
      (lambda (expr)
        (match expr
          [(letrec ([,lhs* ,[ClassDef -> rhs*]] ...)
             (entry-point ,main))
           `(letrec ([,lhs* ,rhs*] ...) (entry-point ,main))]
          [(letrec ([,lhs* ,[ClassDef -> rhs*]] ...)
             (local
               ,local*
               ,[Tail -> body]))
           `(letrec ([,lhs* ,rhs*] ...)
              (local ,local* ,body))]
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
          [,unmatched (error 'the-return-of-set!
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
             `(,input-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))





