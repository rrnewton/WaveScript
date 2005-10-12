;;; Pass 34: uncover-maxstack
;;; October 2001
;===============================================================================

;;; This pass analyzes the input program and determines what the max stack for
;;; the corresponding MSIL program needs to be.

;;; <Input>    ::= (<language-name> <Program>)
;;; <Program>  ::= (program (<var>*) (<pkg>*) <class-def>* <Letrec>)
;;; <Letrec>  ::= (letrec ((<var> <ClassDef>)*) <Local>)
;;; <ClassDef> ::= (class-def (<freevar> ...) <Lambda>)
;;; <Lambda>  ::= (slambda (<var>*) <Local>)
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
;;;             | (call)
;;;             | (tailcall)
;;;             | (box <Type>)
;;;             | (unbox <Type>)

;;; <Type>    ::= Int32 | Char
;;; No other types at present need to be boxed and unboxed.. Everything else
;;; is boxed for its entire lifespan.


;; Jason's version
#;(define do-primitive
(case-lambda
  [(numargs current max)
   (if (zero? numargs)
       (let ([new-current (add1 current)])
         (if (> new-current max)
             (values new-current (add1 max))
             (values new-current max)))
       (values (- current (sub1 numargs)) max))]
  [(numargs total-stack-space current max)
   (let ([diff-stack-space (abs (- total-stack-space numargs))])
     (if (> (+ current diff-stack-space) max)
         (values (- current (sub1 numargs)) (+ max diff-stack-space))
         (values (- current (sub1 numargs)) max)))]))



(define uncover-maxstack
  (let ()
    ;; This function provides information on how much extra stack space
    ;; each primitive needs at the most demanding intermediate point in
    ;; its execution.  For a more detailed explanation look at its
    ;; definition in emission.ss:
    (define get-primitive-stack-space
      (let () (import emission-lib)
        get-primitive-stack-space-requirement))
    (define do-primitive
      (lambda (prim numargs current pastmax)
        (let ([current-peak
                (+ current (get-primitive-stack-space prim numargs))]
              [new-current (+ (- current numargs)
                              (if (effect-primitive? prim) 0 1))])
          (values new-current (max current-peak pastmax)))))
    (define maximum
      (lambda x (apply max x)))
    (define get-maxstack
      (lambda (cmd* init-current init-max)
        (letrec ([get-maxstack-loop-entrance
                   (lambda (cmd* current-depth maxstack)
                     (define get-maxstack-loop
                       (case-lambda
                         [(current-depth maxstack)
                          (get-maxstack-loop-entrance (cdr cmd*)
                                                      current-depth maxstack)]
                         [(current-depth maxstack split-expr)
                          (match split-expr
                            [(branch ,label)
                             (get-maxstack-loop-entrance
                               (remove-until cmd* label)
                               current-depth maxstack)]
                            [(ifnot (branch ,label))
                             (max (get-maxstack-loop-entrance
                                    (remove-until cmd* label)
                                    current-depth maxstack)
                                  (get-maxstack-loop-entrance
                                    (cdr cmd*)
                                    current-depth maxstack))]
                            [,expr (error
                                     'uncover-maxstack
                                     "invalid split expression in get-maxstack: ~s"
                                     expr)])]))
                     
                     (if (or (null? cmd*) (not current-depth)) maxstack
                         (call-with-values
                           (lambda ()
                             (Command (car cmd*) current-depth maxstack))
                           get-maxstack-loop)))]
                 
                 [remove-until
                   (lambda (cmd* branchpoint)
                     (if (null? cmd*)
                         (error 'uncover-maxstack
                                "branch to ~s with no corresponding branchpoint"
                                branchpoint)
                         (match (car cmd*)
                           [(branch-point ,label) (guard (eq? label branchpoint))
                            (cdr cmd*)]
                           [,else (remove-until (cdr cmd*) branchpoint)])))])
          (get-maxstack-loop-entrance cmd* init-current init-max))))
    (define Command
      (lambda (expr current max)
        (match expr
          [(nop) (values current max)]
          [(return) (values #f max)]
          [(branch ,label) (values current max `(branch ,label))]
          [(branch-point ,label) (values current max)]
          [(ifnot (branch ,label))
           (values (sub1 current) max `(ifnot (branch ,label)))]
          [(store! ,var)
           (values (sub1 current) max)]
          [(toplvl-store! ,var)
           (let ([local-peak (+ 1 current)])
             (if (> local-peak max)
                 (values (sub1 current) local-peak)
                 (values (sub1 current) max)))]
          
          [(primcall ,prim ,numargs ,tag* ...)
           (guard (extended-scheme-primitive? prim))
           (do-primitive prim numargs current max)]
          
          
          [(call overflowed) (values (- current
                                        SETTINGS:ARGS_HARDCODED) max)]
          [(let-class (,class-defn* ...) ,[bodycurr bodymax])
           (values bodycurr bodymax)]
          [(code ,[cmdcur* cmdmax*] ...)
           (values (add1 current) (add1 max))]
          [(new ,class-name ,args* ...)
           (let ([newcurr (+ (length args*) current)])
             (if (> newcurr max)
                 (values newcurr newcurr)
                 (values newcurr max)))]
          [(open-instance ,obj ,class-name ,[bodycurr bodymax])
           (values bodycurr bodymax)]
          [(object-reference ,obj ,class ,field)
           (if (= current max)
               (values (add1 current) (add1 current))
               (values (add1 current) max))]
          [(invoke-method ,obj ,class ,method (,arg* ...))
           (let ([newcurr (+ current (length arg*))])
             (if (> newcurr max)
                 (values newcurr newcurr)
                 (values newcurr max)))]
          [(foreign-call ,name ,type-sig (,arg* ...))
           (let ([newcurr (+ current (length arg*))])
             (if (> newcurr max)
                 (values newcurr newcurr)
                 (values newcurr max)))]
          [(static-ref ,class ,field)
           (if (= current max)
               (values (add1 current) (add1 current))
               (values (add1 current) max))]
          [(this-ref ,class ,field)
           (if (= current max)
               (values (add1 current) (add1 current))
               (values (add1 current) max))]
          [(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))
           (let ([newcurr (+ current (length arg*))])
             (if (> newcurr max)
                 (values newcurr newcurr)
                 (values newcurr max)))]
          [(tailcall overflowed)
           (values (- current SETTINGS:ARGS_HARDCODED) max)]
          [(call ,num)
           (if (zero? num)
               (let ([new-current (add1 current)])
                 (if (> new-current max)
                     (values new-current (add1 max))
                     (values new-current max)))
               (values (- current num) max))]
          [(tailcall ,num)
           (if (zero? num)
               (let ([new-current (add1 current)])
                 (if (> new-current max)
                     (values new-current (add1 max))
                     (values new-current max)))
               (values (- current num) max))]
          
          [(call ',class ,num)
           (if (zero? num)
               (let ([new-current (add1 current)])
                 (if (> new-current max)
                     (values new-current (add1 max))
                     (values new-current max)))
               (values (- current num) max))]
          [(tailcall ',class ,num)
           (if (zero? num)
               (let ([new-current (add1 current)])
                 (if (> new-current max)
                     (values new-current (add1 max))
                     (values new-current max)))
               (values (- current num) max))]
          [(ldc ,imm)
           (let ([new-current (add1 current)])
             (if (> new-current max)
                 (values new-current (add1 max))
                 (values new-current max)))]
          [(ldstr ,str)
           (let ([new-current (add1 current)])
             (if (> new-current max)
                 (values new-current (add1 max))
                 (values new-current max)))]
          [(ldarg ,v)
           (let ([new-current (add1 current)])
             (if (> new-current max)
                 (values new-current (add1 max))
                 (values new-current max)))]
          [(ldloc ,v)
           (let ([new-current (add1 current)])
             (if (> new-current max)
                 (values new-current (add1 max))
                 (values new-current max)))]
          [(ldfld (quote ,class) ,v)
           (let ([new-current (add1 current)])
             (if (> new-current max)
                 (values new-current (add1 max))
                 (values new-current max)))]
          [(toplvl-ldfld ,v)
           (let ([new-current (+ 1 current)])
             (if (> new-current max)
                 (values new-current (add1 max))
                 (values new-current max)))]
          [(toplvl-ldfld-quoted ,v)
           (let ([new-current (+ 1 current)])
             (if (> new-current max)
                 (values new-current (add1 max))
                 (values new-current max)))]
          
          [(pop) (values (sub1 current) max)]
          [(box ,type) (values current max)]
          [(unbox ,type) (values current max)]
          ;[(print) (values (sub1 current) max)]
          [,expr (error 'uncover-max-stack
                        "invalid Command expression: ~s"
                        expr)])))
    (define ClassDef
      (lambda (expr)
        (match expr
          [(class-def (,free* ...)
                      (lambda ,formalexp
                        (local (,local* ...)
                               (code ,cmd* ...))))
           (let ([maxstack (get-maxstack cmd* 1 1)])
             `(class-def (,free* ...)
                         (lambda ,formalexp
                           (maxstack ,maxstack
                                     (local (,local* ...)
                                            (code ,cmd* ...)))))
             )]
          [(class-def (,free* ...)
                      (lambda ,formalexp
                        (local (,local* ...)
                               ,cmd* ...)))
           (let ([maxstack (get-maxstack cmd* 1 1)])
             `(class-def (,free* ...)
                         (lambda ,formalexp
                           (maxstack ,maxstack
                                     (local (,local* ...)
                                            (code ,cmd* ...))))))]
          )))
    (define Letrec
      (lambda (expr)
        (match expr
          [(letrec ([,lhs* ,[ClassDef -> rhs*]] ...)
             (entry-point ,main))
           `(letrec ([,lhs* ,rhs*] ...)
              (entry-point ,main))]
          [(letrec ([,lhs* ,[ClassDef -> rhs*]] ...)
             (local ,formlocal* ,local*
                    (code ,body* ...)))
           (let ([maxstack (get-maxstack body* 1 1)])
             `(letrec ([,lhs* ,rhs*] ...)
                (maxstack ,maxstack
                          (local ,formlocal* ,local*
                                 (code ,body* ...)))))]
          )))
    (define process-class-defn
      (lambda (defn)
        (match cdef
          [(define-class ,name ,base
             ,[process-method -> ctor]
             (fields (,fmods** ... ,fname*) ...)
             (methods (,mmods** ... ,mtype*
                        (,mname* ,[process-method -> mbody*])) ...))
           `(define-class
              ,name ,base ,ctor
              (fields (,fmods** ... ,fname*) ...)
              (methods (,mmods** ... ,mtype*
                         (,mname* ,mbody*)) ...))]
          [,unmatched (error 'uncover-max-stack
                             "invalid class defn ~s"
                             cdef)])))
    (define process-method
      (lambda (meth)
        (match meth
          [(lambda ,args ,[Letrec -> body])
           `(lambda ,args ,body)])))
    (lambda (prog)
      (match prog
        [(,input-language
           (quote (program ,sym* ,pkg* ,class-defns* ... ,body)))
         (let ([body (Letrec body)])
           `(uncover-maxstack-language
              '(program ,sym* ,pkg* ,class-defns* ... ,body)))]))))