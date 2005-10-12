;;; Pass 30: flatten-if
;;; September 2001
;===============================================================================

;;; This pass squishes if's and introduces branches and code-labels in return.

;;: AND COLLAPSES THE CONTEXTS

;;: CodeBlock's are imperative and value constructs
;;; (a very different sort of begin)

;;; <Input>    ::= (<language-name> <Program>)
;;; <Program>  ::= (program (<var>*) (<pkg>*) <class-def>* <Letrec>)
;;; <Letrec>    ::= (letrec ((<var> <ClassDef>)*) <Local>)
;;; <ClassDef> ::= (class-def (<freevar> ...) <Lambda>)
;;; <Lambda>    ::= (lambda <Formalexp> <Local>)
;;; <Formalexp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)
;;; <Local>     ::= (local (<var>*) <CodeBlock>)
;;; <CodeBlock> ::= <Command>
;;;               | (code <Command>+)
;;; <Command>    ::= (nop)
;;;               | (box <Type> <Command>)
;;;               | (unbox <Type> <Command>)
;;;               | (quote <imm>)
;;;               | <var>
;;;               | (return <Final>)
;;;               | (branch-point <label-var>)
;;;               | (if (not <CodeBlock>) (branch <label-var>))
;;;               | (branch <label-var>)
;;;               | (set! <var> <Nontail>)
;;;               | (<scheme primitive> <CodeBlock>*)
;;;               | (effectcall (<CodeBlock> <CodeBlock>*))
;;;               | (toplvl-varref <var>)
;;;               | (toplvl-varassign! <var> <Exp>)
;;;               | (class <var>)  ?????????????????
;;; <Final>     ::= (quote <imm>)
;;;               | <var>
;;;               | (class <var>)  ??????????????????
;;;               | (<value primitive> <Nontail>*)
;;;               | (tailcall (<Nontail> <Nontail>*))
;;;               | (toplvl-varref <var>)

; THIS NEEDS SERIOUS TOUCHING UP, NOW THAT IF'S ARE FLATTENED,
; YOU CAN GET (quote <boolean>) IN EFFECT CONTEXT
; MIGHT WANT TO REWORK THE CONTEXTS FOR THIS, MAKE A 'CODE' VARIANT
; OR SOMETHING....


;>>>>>>>>>>>>>>>>>>>>>>>>>
;CHECK CAN RETURNS REALLY ONLY OCCUR IN TAIL???

;;; Labels are not to be mixed up with variable references.  Labels only
;;; occur in Effect context.  And now if's only occur in effect context.
;;; Also, all if's have a (not __) around the test-exp... this is sort
;;; of silly seeming, but it makes sense later (compiles into an MSIL
;;; brfalse to the alternative tag)

;;; NOTE_RRN[01.09.16]: the old (label ___) tag will be renamed to (method ___)
;;; when we get around to it.  The code for each function is now stored in a
;;; method which must be called, not a location which can be branched to
;;; (as it was with Sparc code).

(define flatten-if
  (let ()
    (define do-if
      (lambda (test conseq altern)
        (let ([altern-label (label-name 'altern)]
              [join-label (label-name 'join)])
          (make-code `((if (not ,test) (branch ,altern-label))
                       ,conseq
                       (branch ,join-label)
                       (branch-point ,altern-label)
                       ,altern
                       (branch-point ,join-label))))))
    (define do-tail-if
      (lambda (test conseq altern)
        (let ([altern-label (label-name 'altern)])
          (make-code `((if (not ,test) (branch ,altern-label))
                       ,conseq
                       (branch-point ,altern-label)
                       ,altern)))))
    (define Effect
      (lambda (expr)
        (match expr
          [(nop) '(nop)]
          [(toplvl-varref ,var) `(toplvl-varref ,var)]
          [(toplvl-varassign! ,var ,[Nontail -> rhs])
           `(toplvl-varassign! ,var ,rhs)]
          [(set! ,lhs ,[Nontail -> rhs])
           `(set! ,lhs ,rhs)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           (do-if test conseq altern)]
          [(begin ,[Effect -> expr*] ... ,[expr])
           (make-code `(,expr* ... ,expr))]
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
           `(,prim ,rand* ...)]
          [(effectcall (anon-call ,overflowed?
                                  ,[Nontail -> rator]
                                  ,[Nontail -> rand*] ...))
           `(effectcall (anon-call ,overflowed? ,rator ,rand* ...))]
          [(effectcall (direct-call (class ,f) ,[Nontail -> rand*] ...))
           `(effectcall (direct-call (class ,f) ,rand* ...))]
          [,expr (error 'flatten-if
                        "invalid Effect expression: ~s"
                        expr)])))
    (define Pred
      (lambda (expr)
        (match expr
          [(box ,t ,[expr]) `(box ,t ,expr)]
          [(unbox ,t ,[expr]) `(unbox ,t ,expr)]
          [(quote ,b) (guard (boolean? b)) `(quote ,b)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           (do-if test conseq altern)]
          [(begin ,[Effect -> expr*] ... ,[expr])
           (make-code `(,expr* ... ,expr))]
          [(let-class (,class-defn* ...) ,[body])
           (let ([class-defn* (map process-class-defn class-defn*)])
             `(let-class ,class-defn* ,body))]
          [(open-instance ,obj ,class-name ,[body])
           `(open-instance ,obj ,class-name ,body)]
          [(,prim ,[Nontail -> rand*] ...)
           (guard (predicate-primitive? prim))
           `(,prim ,rand* ...)]
          [,expr (error 'flatten-if
                        "invalid Pred expression: ~s"
                        expr)])))
    (define Nontail
      (lambda (expr)
        (match expr
          [(box ,t ,[expr]) `(box ,t ,expr)]
          [(unbox ,t ,[expr]) `(unbox ,t ,expr)]
          [(quote ,imm) `(quote ,imm)]
          [,var (guard (symbol? var)) var]
          [(toplvl-varref ,var) `(toplvl-varref ,var)]
          [(class ,var) `(class ,var)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           (do-if test conseq altern)]
          [(begin ,[Effect -> expr*] ... ,[expr])
           (make-code `(,expr* ... ,expr))]
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
          [,expr (error 'flatten-if
                        "invalid Nontail expression: ~s"
                        expr)])))
    (define Tail
      (lambda (expr)
        (match expr
          [(return ,[Final -> expr]) `(return ,expr)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           (do-tail-if test conseq altern)]
          [(begin ,[Effect -> expr*] ... ,[expr])
           (make-code `(,expr* ... ,expr))]
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
          [,expr (error 'flatten-if
                        "invalid Tail expression: ~s"
                        expr)])))
    (define Final
      (lambda (expr)
        (match expr
          [(box ,t ,[expr]) `(box ,t ,expr)]
          [(unbox ,t ,[expr]) `(unbox ,t ,expr)]
          [(quote ,imm) `(quote ,imm)]
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
           `(,prim ,rand* ...)]
          [(tailcall (direct-call (class ,f) ,[Nontail -> rand*] ...))
           `(tailcall (direct-call (class ,f) ,rand* ...))]
          [(tailcall (anon-call ,overflowed?
                                ,[Nontail -> rator]
                                ,[Nontail -> rand*] ...))
           `(tailcall (anon-call ,overflowed? ,rator ,rand* ...))]
          [,expr (error 'flatten-if
                        "invalid Final expression: ~s" expr)])))
    (define Lambda
      (lambda (expr)
        (match expr
          [(lambda ,formalexp
             (local (,local* ...) ,[Tail -> body]))
           `(lambda ,formalexp
              (local (,local* ...) ,body))])))
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
           (printf "RRN: This case get's called? hmm...~n")
           `(letrec ([,lhs* ,rhs*] ...)
              (local ,local* (code ,body)))]
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
          [,unmatched (error 'flatten-if
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
             `(flatten-if-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))