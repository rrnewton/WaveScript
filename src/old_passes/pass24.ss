;===============================================================================
;;; Pass 24: uncover-return
;;; September 2001

;;; This pass introduces a (return __) tag which is wrapped around simple
;;; expressions (not let's and begins) in tail position (this doesn't become
;;; really relevent until flatten-if).

;;; Output grammar:

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
;;;             | (label <var>)
;;;             | (<value primitive> <Nontail>*)
;;;             | (<Nontail> <Nontail>*)
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
;;;             | (<Nontail> <Nontail>*)
;;;             | (toplvl-varassign! <var> <Exp>)

;;; The implementation uses effect-primitive?, predicate-primitive?,
;;; and value-primitive? from helpers.ss.

(define uncover-return
  (let ()
    (define Effect
      (lambda (expr)
        (match expr
          [(nop) '(nop)]
          [(toplvl-varassign! ,var ,[Nontail -> rhs])
           `(toplvl-varassign! ,var ,rhs)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[expr*] ... ,[expr])
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
          [(direct-call (class ,f) ,[Nontail -> obj] ,[Nontail -> rand*] ...)
           `(direct-call (class ,f) ,obj ,rand* ...)]
          [(anon-call ,overflowed? ,[Nontail -> rator] ,[Nontail -> rand*] ...)
           `(anon-call ,overflowed? ,rator ,rand* ...)]
          [,expr (error 'uncover-return
                        "invalid Effect expression: ~s" expr)])))
    (define Pred
      (lambda (expr)
        (match expr
          [(quote ,b) (guard (boolean? b)) `(quote ,b)]
          [(if ,[test] ,[conseq] ,[altern])
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
          [,expr (error 'uncover-return
                        "invalid Pred expression: ~s" expr)])))
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
          [(let ([,lhs* ,[rhs*]] ...) ,[body])
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
          [(direct-call (class ,f) ,[obj] ,[rand*] ...)
           `(direct-call (class ,f) ,obj ,rand* ...)]
          [(anon-call ,overflowed? ,[rator] ,[rand*] ...)
           `(anon-call ,overflowed? ,rator ,rand* ...)]
          [,expr (error 'uncover-return
                        "invalid Nontail expression: ~s"
                        expr)])))
    (define Tail
      (lambda (expr)
        (match expr
          [(quote ,imm) `(return (quote ,imm))]
          [,var (guard (symbol? var)) `(return ,var)]
          [(toplvl-varref ,var)
           `(return (toplvl-varref ,var))]
          [(class ,var) `(return (class ,var))]
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
          [(,prim ,[Nontail -> rand*] ...)
           (guard (value-primitive? prim))
           `(return (,prim ,rand* ...))]
          [(direct-call (class ,f) ,[Nontail -> obj] ,[Nontail -> rand*] ...)
           `(return (direct-call (class ,f) ,obj ,rand* ...))]
          [(anon-call ,overflowed? ,[Nontail -> rator] ,[Nontail -> rand*] ...)
           `(return (anon-call ,overflowed? ,rator ,rand* ...))]
          [,expr (error 'uncover-return
                        "invalid Tail expression: ~s"
                        expr)])))
    (define Lambda
      (lambda (expr)
        (match expr
          [(lambda ,formalexp  ,[Tail -> body])
           `(lambda ,formalexp ,body)])))
    (define ClassDef
      (lambda (expr)
        (match expr
          [(class-def (,free* ...) ,[Lambda -> body])
           `(class-def (,free* ...) ,body)])))
    (define Letrec
      (lambda (expr)
        (match expr
          ;; THIS IS CHEESY:
          ;; I don't treat the body of the letrec as tail, because
          ;; in the generated MSIL code a display call has to go
          ;; after the main code.  Perhaps I should just do
          ;; lift-letrec-body instead??? [01.10.02]
          ;[(letrec ([,lhs* ,[ClassDef -> rhs*]] ...) ,[Nontail -> main])
          [(letrec ([,lhs* ,[ClassDef -> rhs*]] ...)
             (entry-point ,main))
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
          [,unmatched (error 'uncover-return
                             "invalid class defn ~s"
                             cdef)])))
    (define process-method
      (lambda (meth)
        (match meth
          [(lambda ,args ,body)
           (let ([body (Letrec body)])
             `(lambda ,args ,body))])))
    ;---------------------------------------
    (lambda (prog)
      (match prog
        [(,input-language
           (quote (program ,sym* ,pkg* ,class-defns* ... ,body)))
         (let ([class-defns* (map process-class-defn class-defns*)])
           (let ([body (Letrec body)])
             `(uncover-return-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))