;;; Pass 33: uncover-pushvariant
;;;   This pass changes each push into a ldc, ldloc, ldarg, or ldfld.
;===============================================================================

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

;;;             | (ldc <imm>)
;;;             | (ldstr <string>)
;;;             | (ldloc <localname>)
;;;             | (ldarg <formalname>)
;;;             | (ldfld <freevarname>)
;;;             | (toplvl-ldfld <varname>)
;;;             | (toplvl-ldfld-quoted <varname>)
;;;             | (primcall <primitive> <numrands> <Tag>*)
;;;             | (call <num>)
;;;             | (tailcall <num>)
;;;             | (box <Type>)
;;;             | (unbox <Type>)

;;; In the output of this pass, push's are replaced with more specific
;;; load primitives.


;;; NO LONGER TRUE =======================================================
;;;And a new list of variables is added to the local
;;; syntax.  These variables are the local copies of those formals that
;;; catch multiple arguments.  The local copies become necessary because
;;; those multi-arg formals must be converted from array form to list form
;;; (MSIL's mechanism for variable arguments uses arrays).
;;; NO LONGER TRUE =======================================================

;;; Uses get-formals from helpers.ss

(define uncover-pushvariant
  (let ()
    #;(define list-index
        (lambda (x ls)
          (cond
            [(eq? x (car ls)) 0]
            [else (add1 (list-index x (cdr ls)))])))
    (define Command
      (lambda (classname freevars formals locals)
        (lambda (expr)
          (match expr
            [(nop) '(nop)]
            [(class ,var)
             (error 'uncover-pushvariant
                    "RYan really doesn't know what to do in this case
                    You've got a class-label in Final context: ~n~a"
                    expr)
             (list 'push `(label ,var))]
            [(return) '(return)]
            [(branch ,label) `(branch ,label)]
            [(branch-point ,label) `(branch-point ,label)]
            [(ifnot (branch ,label)) `(ifnot (branch ,label))]
            
            [(store! ,var) `(store! ,var)]
            [(toplvl-store! ,var) `(toplvl-store! ,var)]
            
            ;;; here for args to obj etc
            [(code ,[arg*] ...)
             `(code ,arg* ...)]
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
            [(primcall ,prim ,numrands ,tag* ...)
             (guard (extended-scheme-primitive? prim))
             `(primcall ,prim ,numrands ,tag* ...)]
            [(call overflowed) '(call overflowed)]
            [(tailcall overflowed) '(tailcall overflowed)]
            ;-----
            [(call ,num) `(call ,num)]
            [(tailcall ,num) `(tailcall ,num)]
            ;-----
            [(call ',class ,num) `(call ',class ,num)]
            [(tailcall ',class ,num) `(tailcall ',class ,num)]
            
            [(push ,v)
             (match v
               [(quote ,sym) `(toplvl-ldfld-quoted ,sym)]
               [,str (guard (string? str)) `(ldstr ,str)]
               [(toplvl-varref ,v) `(toplvl-ldfld ,v)]
               [,v (guard (symbol? v) (memq v freevars))
                 `(ldfld (quote ,classname) ,v)]
               [,v (guard (symbol? v) (memq v formals))
                 ;`(ldarg ,(list-index v formals))
                 `(ldarg ,v)]
               [,v (guard (symbol? v) (memq v locals))
                 `(ldloc ,v)]
               [,v (guard (constant? v))
                 `(ldc ,v)]
               [,else (error `uncover-pushvariant
                             "Here is a push of an invalid object: ~s"
                             `(push ,v))])]
            [(pop) '(pop)]
            [(box ,type) `(box ,type)]
            [(unbox ,type) `(unbox ,type)]
            [,expr (error 'uncover-pushvariant
                          "invalid Command expression: ~s"
                          expr)]))))
    (define Lambda
      (lambda (expr classname freevar*)
        ;; What happen here is very delicate.  List-formals, as 'a'
        ;; in (lambda (b . a) ...), need to be referenced as local
        ;; variables.  This is because they need to be constructed
        ;; from the array that comes in under the MSIL variable-arity
        ;; mechanism.  Namespace for locals can overlap with namespace
        ;; for arguments in MSIL; so I will use the same name for the
        ;; argument-version of a list-formal, and for its corresponding
        ;; local variable.
        ;;   However, for the moment local-copies of these formals are
        ;; kept separate from the normal locals.  Another list is added
        ;; to make (local (<multi-arg-formal>*) (<local>*) <body>)
        (match expr
          [(lambda ,formalexp
             (local (,local* ...)
                    (code ,[(Command classname freevar*
                                     (get-formals formalexp)
                                     local*)
                            -> cmd*]
                          ...)))
           `(lambda ,formalexp
              (local ,local* ,(make-code cmd*)))]
          [(lambda ,formal-exp
             (local (,local* ...)
                    ,[(Command classname freevar*
                               (get-formals formalexp) local*)
                      -> cmd*]
                    ...))
           `(lambda ,formalexp
              (local ,local* ,cmd* ...))]
          )))
    (define ClassDef
      (lambda (expr classname)
        (match expr
          [(class-def (,free* ...) ,body)
           `(class-def (,free* ...) ,(Lambda body classname free*))])))
    (define Letrec
      (lambda (expr)
        (match expr
          [(letrec ([,lhs* ,rhs*] ...)
             (entry-point ,main))
           `(letrec ([,lhs*
                       ,(map (lambda (lhs rhs)
                               (ClassDef rhs lhs))
                             lhs* rhs*)] ...)
              (entry-point ,main))]
          [(,args (letrec ([,lhs* ,[(lambda (x)
                                      (ClassDef x '())) -> rhs*]] ...)
                    (local
                      ,local*
                      (code
                        ,body* ...))))
           (let ([body* (map (Command '() '() args local*) body*)])
             `(letrec ([,lhs* ,rhs*] ...)
                (local ,local*
                       (code ,body* ...))))]
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
          [,unmatched (error 'uncover-pushvariant
                             "invalid class defn ~s"
                             cdef)])))
    (define process-method
      (lambda (meth)
        (match meth
          [(lambda ,args ,body)
           (let ([body (Letrec (list args body))])
             `(lambda ,args ,body))])))
    ;---------------------------------------
    (lambda (prog)
      (match prog
        [(,input-language
           (quote (program ,sym* ,pkg* ,class-defns* ... ,body)))
         (let ([class-defns* (map process-class-defn class-defns*)])
           (let ([body (Letrec body)])
             `(uncover-pushvariant-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))