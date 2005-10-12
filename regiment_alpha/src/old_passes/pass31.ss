;;; Pass 31: introduce-stack
;;; September 2001
;===============================================================================

;;>>>>>>>>>>>>>>>>>>>>>>
;;; MSIL requires us to unbox our objects when we want use MSIL primitive
;;; operations on them.  Unfortunately, because of the code layout under the
;;; stack architecture, there's no way to do the unboxing locally when
;;; generating the code for the primitive operations.  The unboxing needs to
;;; happen at the time an operand is pushed onto the stack (which may be many
;;; lines above where the operation itself is invoked).  So this pass will
;;; add (box __) and (unbox ___) forms so that we'll know where to insert
;;; these instructions later.

;INPUT LANG


;;>>>>>>>>>>>>>>>>>>>>>>
;;; Before a (tailcall ___) tag was introduced, now we introduce
;;; 'call' and 'primcall' so that all app-exp's have an associated tag.
;;; Call is a thunk...

;;; EVERYTHING is adapted... set! now sets a var to the thing on the
;;; top of the stack...

;;; <Input>    ::= (<language-name> <Program>)
;;; <Program>  ::= (program (<var>*) (<pkg>*) <class-def>* <Letrec>)
;;; <Letrec>  ::= (letrec ((<var> <ClassDef>)*) <Local>)
;;; <ClassDef> ::= (class-def (<freevar> ...) <Lambda>)
;;; <Lambda>  ::= (lambda <Formalexp> <Local>)
;;; <Formalexp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)
;;; <Local>   ::= (local (<var>*) (code <Command>+))
;;; <Command> ::= (nop)

;;;             | (pop)    ;;; <-Sure?

;;;             | (return)
;;;             | (branch-point <label-var>)
;;;             | (ifnot (branch <label-var>))
;;;             | (branch <label-var>)
;;;             | (store! <var>)
;;;             | (toplvl-store! <var>)
;;;             | (push <Imm>)
;;;             | (primcall <primitive> <numrands> <Tag>*)
;;;             | (call <num>)   ;;; number of arguments
;;;             | (box <Type>)
;;;             | (unbox <Type>)
;;; <Imm>     ::= <var> | <int> | (toplvl-varref <var>)



;;; TALK ABOUT ODD CONDITION OF WRAPPING TAG TO DENOTE
;;; "ADD INSTRUCTION AFTER THIS"

;   NOTE TO SELF
;   CHECK AND SEE IF YOU NEED TO USE MAKE-BEGIN IN SO MANY PLACES

; VALUES PUSH THEMSELVES... JUST STRING THEM OUT

;;!!!!!!!!!!!!!!!!!!!!!!!
; SERIOUSLY NEED TO ADDRESS THE REPRESENTATION FOR BOOLEANS
;;!!!!!!!!!!!!!!!!!!!!!!!

;===============================================================================

(define introduce-stack
  (let ()
    #;(define puncture-code
        (lambda (fun expr)
          (match expr
            ;If the last thing is a branch-point, we don't want it:
            [(code ,expr* ... (branch-point ,lab))
             (append
               (puncture-code fun `(code ,expr* ...))
               `((branch-point ,lab)))]
            [(code ,expr* ... ,expr)
             `(code ,@expr* ,(puncture-code fun expr))]
            [,expr (fun expr)])))
    (define wrap-box
      (lambda (expr type)
        (match expr
          [(unbox ,t ,e) (list e)]
          [(box ,t ,e) (error 'introduce-stack
                              "Can't wrap a box around a box: ~a" expr)]
          [,e  `(,e (box ,type))])))
    (define wrap-unbox
      (lambda (expr type)
        (match expr
          [(box ,t ,e) (list e)]
          [(unbox ,t ,e) (error 'introduce-stack
                                "Can't unbox twice: ~a" expr)]
          [,e `(,e (unbox ,type))])))
    ;---------------------------------------
    (define do-prim
      (lambda (prim rand*)
        (let* ([ls (map cons (get-primitive-rand-types prim (length rand*))
                        rand*)])
          (let ([tagrands
                  (map cdr (filter (lambda (pr)
                                     (eq? (car pr) 'Tag)) ls))]
                [stackrands
                  (map (lambda (x) (CodeBlock (cdr x)))
                       (filter (lambda (pr)
                                 (not (eq? (car pr) 'Tag)))
                               ls))])
            (make-code
              `(,@stackrands
                  (primcall ,prim ,(length stackrands)
                            ,@tagrands)))))))
    ;---------------------------------------
    ;;; [01.09.30]
    ;;; FOR NOW, I handle numbers of arguments exceeding the basic 6
    ;;; by a separate pass that introduces a let binding for each
    ;;; such call.  I DON'T NEED an extra local variable to pass
    ;;; a vector of parameters to a function -- C# uses one, but it
    ;;; would be possible to just create the array, then
    ;;; duplicate it, set a slot, dup it, set a slot.... until it's
    ;;; full.  Is this in any way better than introducing a local
    ;;; each time we need to make a call? (Does C# introduce 2 locals
    ;;; for 2 calls??).  ASK KENT.
    (define do-app
      (lambda (calltype rator rand*)
        ;(if (<= (length rand*) SETTINGS:ARGS_HARDCODED)
        (make-code
          `(,rator ,@rand*
             (,@calltype ,(length rand*))))
        ;(make-code
        ;`(,rator
        ;,@(list-head rand* SETTINGS:ARGS_HARDCODED)
        ;,@(do-extra-rands
        ;(list-tail rand* SETTINGS:ARGS_HARDCODED))
        ;(,calltype ,(length rand*)))))))
        ))
    ;---------------------------------------
    ;(define do-extra-rands
    ;(lambda (rands)
    ;
    ;))
    ;---------------------------------------
    (define Command
      (lambda (expr)
        ;(disp "Running command : " expr)
        (match expr
          [(box ,t ,[CodeBlock -> cmd])
           (make-code (wrap-box cmd t))]
          [(unbox ,t ,[CodeBlock -> cmd])
           (make-code (wrap-unbox cmd t))]
          [(nop) '(nop)]
          
          [(quote ,sym) (guard (symbol? sym))
           `(push ',sym)]
          
          [(quote ,imm) `(push ,imm)]
          [,var (guard (symbol? var)) `(push ,var)]
          [(toplvl-varref ,var) `(push (toplvl-varref ,var))]
          [(class ,var)
           (error 'introduce-stack
                  "RYan really doesn't know what to do in this case
                  You've got a class-label in COMMAND context: ~n~a"
                  expr)
           (list 'push `(class ,var))]
          [(return ,[Final -> expr])
           (make-code `(,expr (return)))]
          [(branch ,label) `(branch ,label)]
          [(branch-point ,label) `(branch-point ,label)]
          [(if (not ,[CodeBlock -> test]) (branch ,label))
           (make-code `(,test (ifnot (branch ,label))))]
          
          [(toplvl-varassign! ,var ,[CodeBlock -> rhs])
           (make-code `(,rhs (toplvl-store! ,var)))]
          [(set! ,lhs ,[CodeBlock -> rhs])
           (make-code `(,rhs (store! ,lhs)))]
          
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
          [(,prim ,rand* ...)
           (guard (extended-scheme-primitive? prim))
           (do-prim prim rand*)]
          [(effectcall (anon-call ,overflowed?
                                  ,[CodeBlock -> rator]
                                  ,[CodeBlock -> rand*] ...))
           (case overflowed?
             [(overflowed)
              (make-code
                `(,rator ,@rand*
                   (call overflowed) (pop)))]
             [(complete)
              (make-code `(,(do-app `(call) rator rand*)
                            (pop)))])]
          [(effectcall (direct-call (class ,f) ,[CodeBlock -> obj]
                                    ,[CodeBlock -> rand*] ...))
           (make-code `(,(do-app `(call ',f) obj rand*)
                         (pop)))]
          ;Nontail, Noneffect calls:
          [(anon-call ,overflowed?
                      ,[CodeBlock -> rator]
                      ,[CodeBlock -> rand*] ...)
           (case overflowed?
             [(overflowed)
              (make-code
                `(,rator ,@rand*
                   (call overflowed)))]
             [(complete)
              (do-app `(call) rator rand*)])]
          [(direct-call (class ,f)
                        ,[CodeBlock -> obj]
                        ,[CodeBlock -> rand*] ...)
           (do-app `(call ',f) obj rand*)]
          [,expr (error 'introduce-stack
                        "invalid Command expression: ~s"
                        expr)])))
    (define Final
      (lambda (expr)
        (match expr
          [(box ,t ,[cmd])
           (make-code (wrap-box cmd t))]
          [(unbox ,t ,[cmd])
           (make-code (wrap-unbox cmd t))]
          [(quote ,sym) (guard (symbol? sym))
           `(push ',sym)]
          [(quote ,imm) `(push ,imm)]
          [,var (guard (symbol? var)) `(push ,var)]
          [(toplvl-varref ,var) `(push (toplvl-varref ,var))]
          [(class ,var)
           (error 'introduce-stack
                  "RYan really doesn't know what to do in this case
                  You've got a class-label in FINAL context: ~n~a"
                  expr)
           #;(list 'push `(class ,var))]
          [(new ,class-name ,[Command -> args*] ...)
           `(new ,class-name ,args* ...)]
          [(object-reference ,obj ,class ,field)
           `(object-reference ,obj ,class ,field)]
          [(invoke-method ,obj ,class ,method (,[Command -> arg*] ...))
           `(invoke-method ,obj ,class ,method (,arg* ...))]
          [(foreign-call ,name ,type-sig (,[Command -> arg*] ...))
           `(foreign-call ,name ,type-sig (,arg* ...))]
          [(static-ref ,class ,field)
           `(static-ref ,class ,field)]
          [(this-ref ,class ,field)
           `(this-ref ,class ,field)]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[Command -> arg*] ...))
           `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))]
          [(,prim ,rand* ...)
           (guard (extended-scheme-primitive? prim))
           (do-prim prim rand*)]
          [(tailcall (anon-call
                       ,overflowed?
                       ,[CodeBlock -> rator]
                       ,[CodeBlock -> rand*] ...))
           (case overflowed?
             [(overflowed)
              (make-code
                `(,rator ,@rand*
                   (tailcall overflowed)))]
             [(complete)
              (do-app `(tailcall) rator rand*)])]
          [(tailcall (direct-call (class ,f) ,[CodeBlock -> obj]
                                  ,[CodeBlock -> rand*] ...))
           (do-app `(tailcall ',f) obj rand*)]
          [,expr (error 'introduce-stack
                        "invalid Final expression: ~s" expr)])))
    (define CodeBlock
      (lambda (expr)
        (match expr
          [(code ,[Command -> expr*] ...) (make-code expr*)]
          [,[Command -> expr] expr])))
    (define Lambda
      (lambda (expr)
        (match expr
          [(lambda ,formalexp (local (,local* ...)
                                     ,[CodeBlock -> body]))
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
               ,[CodeBlock -> body]))
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
             `(introduce-stack-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))

;===============================================================================

