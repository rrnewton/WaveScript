;;; Pass 29: introduce-box
;;; September 2001
;===============================================================================

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
;;;             | (box <Type> <Tail>)
;;;             | (unbox <Type> <Tail>)
;;;             | <var>
;;;             | (class <var>)
;;;             | (<value primitive> <Nontail>*)
;;;             | (tailcall (<Nontail> <Nontail>*))
;;;             | (toplvl-varref <var>)
;;; <Nontail> ::= (quote <imm>)
;;;             | (box <Type> <Nontail>)
;;;             | (unbox <Type> <Nontail>)
;;;             | <var>
;;;             | (class <var>)
;;;             | (<value primitive> <Nontail>*)
;;;             | (if <Pred> <Nontail> <Nontail>)
;;;             | (begin <Effect>* <Nontail>)
;;;             | (toplvl-varref <var>)
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
;;; <Type>    ::= see type list in helpers.ss

(define introduce-box
  (let ()
    ;---------------------------------------
    (define puncture-code
      (lambda (fun expr)
        (match expr
          ;If the last thing is a branch-point, we don't want it:
          [(begin ,expr* ... ,expr)
           `(begin ,@expr* ,(puncture-code fun expr))]
          [(if ,test ,conseq ,altern)
           `(if ,test
                ,(puncture-code fun conseq)
                ,(puncture-code fun altern))]
          [,expr (fun expr)])))
    ;---------------------------------------
    (define do-prim-rands
      (let ([spit
              (lambda (prim expect get)
                (error 'introduce-box
                       "primitive ~a called with~a~a~a~a~a"
                       prim
                       " bad input type: "
                       expect " expected "
                       get " received."))])
        (lambda (prim rand*)
          (map
            (lambda (type rand)
              (if (and (not (eq? 'Tag type)) (boxed-type? type))
                  (puncture-code
                    (lambda (x)
                      (match x
                        [(box ,t (quote ,val))
                         (guard (constant? val))
                         (if (eq? type t)
                             `(quote ,val)
                             (spit prim type (constant->type val)))]
                        [(box ,t (,p ,rand* ...))
                         (guard (extended-scheme-primitive? p))
                         (if (eq? type t)
                             `(,p ,rand* ...)
                             (spit prim type t))]
                        [(box ,other)
                         (error 'introduce-box
                                "this should not be in ~a~a"
                                "a box expression: " other)]
                        [,x `(unbox ,type ,x)]))
                    rand)
                  rand))
            ;; The optional length argument locks down variable arity prims:
            (get-primitive-rand-types prim (length rand*))
            rand*))))
    ;---------------------------------------
    (define do-prim
      (lambda (prim rand*)
        (let ([t (get-primitive-return-type prim)])
          (if (boxed-type? t)
              `(box ,t (,prim ,@rand*))
              `(,prim ,@rand*)))))
    ;---------------------------------------
    (define Final
      (lambda (expr)
        (match expr
          [(quote ,imm)
           (let ([t (constant->type imm)])
             (if (boxed-type? t)
                 `(box ,t (quote ,imm))
                 `(quote ,imm)))]
          [,var (guard (symbol? var)) var]
          [(toplvl-varref ,var) `(toplvl-varref ,var)]
          [(class ,var) `(class ,var)]
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
          [(,prim ,[Nontail -> rand*] ...)
           (guard (value-primitive? prim))
           (do-prim prim (do-prim-rands prim rand*))]
          [(tailcall (direct-call (class ,f) ,[Nontail -> rand*] ...))
           `(tailcall (direct-call (class ,f) ,rand* ...))]
          [(tailcall (anon-call ,overflowed?
                                ,[Nontail -> rator] ,[Nontail -> rand*] ...))
           `(tailcall (anon-call ,overflowed? ,rator ,rand* ...))]
          [,expr (error 'introduce-box
                        "invalid Final expression: ~s" expr)])))
    (define Effect
      (lambda (expr)
        (match expr
          [(nop) '(nop)]
          [(toplvl-varassign! ,var ,[Nontail -> rhs])
           `(toplvl-varassign! ,var ,rhs)]
          [(set! ,lhs ,[Nontail -> rhs])
           `(set! ,lhs ,rhs)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> expr*] ... ,[expr])
           `(begin ,expr* ... ,expr)]
          [(let-class (,class-defn* ...) ,[body])
           (let ([class-defn* (map process-class-defn class-defn*)])
             `(let-class ,class-defn* ,body))]
          [(open-instance ,obj ,class-name ,[body])
           `(open-instance ,obj ,class-name ,body)]
          [(invoke-method ,obj ,class ,method (,[Nontail -> arg*] ...))
           `(invoke-method ,obj ,class ,method (,arg* ...))]
          [(foreign-call ,name ,type-sig (,[Nontail -> arg*] ...))
           `(foreign-call ,name ,type-sig (,arg* ...))]
          ;;; These should have disappeared ... maybe? ...
          ;[(set-field! ,field ,[Nontail -> rhs])
          ;`(set! ,field ,rhs)]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[Nontail -> arg*] ...))
           `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))]
          [(,prim ,[Nontail -> rand*] ...)
           (guard (effect-primitive? prim))
           `(,prim ,(do-prim-rands prim rand*) ...)]
          [(effectcall (anon-call ,overflowed?
                                  ,[Nontail -> rator] ,[Nontail -> rand*] ...))
           `(effectcall (anon-call ,overflowed? ,rator ,rand* ...))]
          [(effectcall (direct-call (class ,f) ,[Nontail -> rand*] ...))
           `(effectcall (direct-call (class ,f) ,rand* ...))]
          [,expr (error 'introduce-box
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
          [(let-class (,class-defn* ...) ,[body])
           (let ([class-defn* (map process-class-defn class-defn*)])
             `(let-class ,class-defn* ,body))]
          [(open-instance ,obj ,class-name ,[body])
           `(open-instance ,obj ,class-name ,body)]
          [(,prim ,[Nontail -> rand*] ...)
           (guard (predicate-primitive? prim))
           ;(do-prim prim rand*)
           `(,prim ,(do-prim-rands prim rand*) ...)
           ]
          [,expr (error 'introduce-box
                        "invalid Pred expression: ~s"
                        expr)])))
    (define Nontail
      (lambda (expr)
        (match expr
          [(quote ,imm)
           (let ([t (constant->type imm)])
             (if (boxed-type? t)
                 `(box ,t (quote ,imm))
                 `(quote ,imm)))]
          [,var (guard (symbol? var)) var]
          [(toplvl-varref ,var) `(toplvl-varref ,var)]
          [(class ,var) `(class ,var)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> expr*] ... ,[expr])
           `(begin ,expr* ... ,expr)]
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
           (do-prim prim (do-prim-rands prim rand*))]
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
           `(begin ,expr* ... ,expr)]
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
          [,expr (error 'introduce-box
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
           `(letrec ([,lhs* ,rhs*] ...)
              (entry-point ,main))]
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
          [,unmatched (error 'introduce-box
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
             `(introduce-box-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))