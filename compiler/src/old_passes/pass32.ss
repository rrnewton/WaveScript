;;; Pass 32 Clarify Types
;;; September 2001
;===============================================================================

;;; RRN[2002.06.11] This pass was totally misconceieved.  The types
;;; for boxes and unboxes must come from the primitives which spawned those
;;; boxes and unboxes.  Thus this type "clarification" must be made back
;;; in introduce-box.



;;; <Input>    ::= (<language-name> <Program>)
;;; <Program>  ::= (program (<var>*) (<pkg>*) <class-def>* <Letrec>)
;;; <Letrec>  ::= (letrec ((<var> <ClassDef>)*) <Local>)
;;; <ClassDef> ::= (class-def (<freevar> ...) <Lambda>)
;;; <Lambda>  ::= (lambda <Formalexp> <Local>)
;;; <Formalexp> ::= <var>
;;;               | (<var>*)
;;;               | (<var>* . <var>)
;;; <Local>   ::= (local (<var>*) (code <Command>+))
;;; <Command> ::= (nop)
;;;             | (return)
;;;             | (branch-point <label-var>)
;;;             | (ifnot (branch <label-var>))
;;;             | (branch <label-var>)
;;;             | (store! <var>)
;;;             | (toplvl-store! <var>)
;;;             | (push <imm>)
;;;             | (primcall <primitive> <numrands> <Tag>*)
;;;             | (call <num>)
;;;             | (tailcall <num>)
;;;             | (box <Type>)
;;;             | (unbox <Type>)
;;; <Imm>     ::= <var> | <int> | (toplvl-varref <var>)

;;; <Type>    ::= Int32
;;; No other types at present need to be boxed and unboxed.. Everything else
;;; is boxed for its entire lifespan.

;This is a simple pass right now.  It figures out what types boxes and
;unboxes need, and makes them stand-alone instructions, annotated with the
;appropriate types.  Presently I can't think of anything else that needs this
;treatment.

#;(define clarify-types
(let ()
  (define Command
    (lambda (expr)
      (match expr
        [(nop) '(nop)]
        [(class ,var)
         (error 'clarify-types
                "RYan really doesn't know what to do in this case
                You've got a class-label in Final context: ~n~s"
                expr)
         (list 'push `(label ,var))]
        [(return) '(return)]
        [(branch ,label) `(branch ,label)]
        [(branch-point ,label) `(branch-point ,label)]
        [(ifnot (branch ,label)) `(ifnot (branch ,label))]
        [(toplvl-store! ,var) `(toplvl-store! ,var)]
        [(store! ,var) `(store! ,var)]
        [(primcall ,prim ,numargs ,tag* ...)
         (guard (extended-scheme-primitive? prim))
         `(primcall ,prim ,numargs ,tag* ...)]
        ;-----
        [(call overflowed) '(call overflowed)]
        [(tailcall overflowed) '(tailcall overflowed)]
        ;-----
        [(call ,num) `(call ,num)]
        [(tailcall ,num) `(tailcall ,num)]
        ;-----
        [(call ',class ,num) `(call ',class ,num)]
        [(tailcall ',class ,num) `(tailcall ',class ,num)]
        ;-----
        [(push ,imm)
         ;("Pushing: " imm)
         `(push ,imm)]
        [(pop) '(pop)]
        [(box ,[Box -> expr type])
         (make-code `(,expr (box ,type)))]
        [(unbox ,[Unbox -> expr type])
         (make-code `(,expr (unbox ,type)))]
        ;-----
        [(code ,[expr*] ...)
         `(code ,expr* ...)]
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
         (make-code `((foreign-call ,name ,type-sig (,arg* ...)) (return)))]
        [(static-ref ,class ,field)
         `(static-ref ,class ,field)]
        [(this-ref ,class ,field)
         `(this-ref ,class ,field)]
        [(invoke-static-method ,class ,method-name ,type-sig (,[arg*] ...))
         `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))]
        [,expr (error 'clarify-types
                      "invalid Command expression: ~s"
                      expr)])))
  
  ;;+++++++++++++++++++++++++++++++++++++++++++++++++
  ;; COPPING OUT FOR NOW ON THE BELOW TWO FUNCTIONS:::
  (define Box
    (lambda (expr)
      (match expr
        [(push ,num) (guard (fx-integer? num))
         (values expr 'Int32)]
        [(push ,num) (guard (flonum? num))
         (values expr 'Float64)]
        [(push ,char) (guard (char? char))
         (values expr 'Char)]
        [(push ,imm)
         (error 'clarify-types
                "non numeric value getting pushed and boxed;~a~s"
                " I don't believe that should be happening: "
                `(push ,imm))]
        [(primcall ,prim ,tag* ...) (values expr 'Int32)]
        ;;;+++++++++++++++++++++++++++++++
        ;;; TEMPORARY, VERY BAD:::
        [(code ,[Command -> expr*] ...)
         (values `(code ,@expr*) 'Int32)]
        ;;;+++++++++++++++++++++++++++++++
        [,expr (error 'clarify-types
                      "Box is around something that isn't a~a~s"
                      " push/primcall expression:"  expr)])))
  (define Unbox
    (lambda (expr)
      (match expr
        [(push ,num) (guard (number? num))
         (error 'clarify-types
                "unbox of a numeric value=senseless: ~s"
                `(unbox (push ,num)))]
        [(push ,var) (guard (symbol? var))
         (values expr 'Int32)]
        [(push (toplvl-varref ,var)) (guard (symbol? var))
         (values expr 'Int32)]
        [(push ,imm)
         (error 'clarify-types
                "Unbox is around a push expression that doesn't,~a~s"
                " contain either a number or var:"  expr)]
        [(primcall ,prim ,tag* ...) (values expr 'Int32)]
        ;;; TEMPORARY, VERY BAD:::
        [(code ,[Command -> expr*] ...)
         (values `(code ,@expr*) 'Int32)]
        [(call overflowed) (values expr 'Int32)]
        [(call ,num) (values expr 'Int32)]
        [(call ,class ,num) (values expr 'Int32)]
        [,expr (error 'clarify-types
                      "Unbox is around something that isn't a,~a~s"
                      " push/primcall/call expression:"  expr)])))
  (define Lambda
    (lambda (expr)
      (match expr
        [(lambda ,formalexp
           (local (,local* ...)
                  (code ,[Command -> cmd*] ...)))
         `(lambda ,formalexp
            (local (,local* ...)
                   ,(make-code cmd*)))]
        [(lambda ,formalexp
           (local (,local* ...)
                  ,[Command -> cmd*] ...))
         `(lambda ,formalexp
            (local (,local* ...)
                   ,cmd* ...))]
        )))
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
             (code
               ,[Command -> body*] ...)))
         `(letrec ([,lhs* ,rhs*] ...)
            (local ,local* ,(make-code body*)))]
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
        [,unmatched (error 'introduce-stack
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
           `(clarify-types-language
              '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))