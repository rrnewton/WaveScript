;===============================================================================
;;; Pass 27: uncover-local
;;; September 2001

;;; This pass associates with each lambda expression a list of the
;;; let-bound variables that appear within the lambda expression.
;;; Additionally, it associates such a list with the body of the
;;; toplevel letrec expression.

;;; Output grammar:

;;; <Input>    ::= (<language-name> <Program>)
;;; <Program>  ::= (program (<var>*) (<pkg>*) <class-def>* <Letrec>)
;;; <Letrec>   ::= (letrec ((<var> <ClassDef>)*) (local (<var>*) <Tail>))
;;; <ClassDef> ::= (class-def (<freevar> ...) <Lambda>)
;;; <Lambda>   ::= (lambda <Formalexp> (local (<var>*) <Tail>))
;;; <Formalexp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)
;;; <Final>    ::= (quote <imm>)
;;;              | <var>
;;;              | (class <var>)
;;;              | (<value primitive> <Nontail>*)
;;;              | (tailcall (<Nontail> <Nontail>*))
;;;              | (toplvl-varref <var>)
;;; <Tail>     ::= (return <Final>)
;;;              | (if <Pred> <Tail> <Tail>)
;;;              | (begin <Effect>* <Tail>)
;;;              | (let ((<var> <Nontail>)) <Tail>)
;;; <Nontail>  ::= (quote <imm>)
;;;              | <var>
;;;              | (class <var>)
;;;              | (if <Pred> <Nontail> <Nontail>)
;;;              | (begin <Effect>* <Nontail>)
;;;              | (let ((<var> <Nontail>)) <Nontail>)
;;;              | (<value primitive> <Nontail>*)
;;;              | (<Nontail> <Nontail>*)
;;;              | (toplvl-varref <var>)
;;; <Pred>     ::= (quote <boolean>)
;;;              | (if <Pred> <Pred> <Pred>)
;;;              | (begin <Effect>* <Pred>)
;;;              | (let ((<var> <Nontail>)) <Pred>)
;;;              | (<predicate primitive> <Nontail>*)
;;; <Effect>   ::= (nop)
;;;              | (if <Pred> <Effect> <Effect>)
;;;              | (begin <Effect>* <Effect>)
;;;              | (let ((<var> <Nontail>)) <Effect>)
;;;              | (<effect primitive> <Nontail>*)
;;;              | (effectcall (<Nontail> <Nontail>*))
;;;              | (toplvl-varassign! <var> <Exp>)

;;; The implementation uses effect-primitive?, predicate-primitive?,
;;; and value-primitive? from helpers.ss.


;;; RRN 01.09.16 -- I wonder... this is a pass where splitting
;;; the pass up into procedures that match the variants in the grammar,
;;; results in a *huge* amount of excess work.  This could be done
;;; by a much shorter procedure, ignorant of all the different contexts,
;;; concerned only with pulling out the variables bound in let's...
;;; This is making me want to lean towards having very short, loose passes
;;; and then frequent verify passes...  Although then again, I understand
;;; that matching the grammar removes a lot of the thinking; it just seems
;;; to increase typing... sigh

;;; As an experiment I will make this shorter version and keep them both:

(define uncover-local
  (let ()
    (define Effect
      (lambda (expr)
        (match expr
          [(nop) (values '(nop) '())]
          [(toplvl-varassign! ,var ,[Nontail -> rhs local*])
           (values `(toplvl-varassign! ,var ,rhs) local*)]
          [(if ,[Pred -> test test-local*]
               ,[conseq conseq-local*]
               ,[altern altern-local*])
           (values
             `(if ,test ,conseq ,altern)
             (append test-local* conseq-local* altern-local*))]
          [(begin ,[Effect -> expr* local**] ... ,[expr local*])
           (values
             `(begin ,expr* ... ,expr)
             (append (apply append local**) local*))]
          [(let ([,lhs ,[Nontail -> rhs rhs-local*]]) ,[body body-local*])
           (values
             `(let ([,lhs ,rhs]) ,body)
             (cons lhs (append rhs-local* body-local*)))]
          [(let-class (,class-defn* ...) ,[body body-local*])
           (let ([class-defn* (map process-class-defn class-defn*)])
             (values
               `(let-class ,class-defn* ,body)
               body-local*))]
          [(open-instance ,obj ,class-name ,[body body-local*])
           (values
             `(open-instance ,obj ,class-name ,body)
             body-local*)]
          [(invoke-method ,obj ,class ,method
                          (,[Nontail -> arg* arg-local**] ...))
           (values
             `(invoke-method ,obj ,class ,method (,arg* ...))
             (apply append arg-local**))]
          [(foreign-call ,name ,type-sig
                         (,[Nontail -> arg* arg-local**] ...))
           (values
             `(foreign-call ,name ,type-sig (,arg* ...))
             (apply append arg-local**))]
          [(set-field! ,field ,[Nontail -> rhs rhs-local*])
           (values `(set-field! ,field ,rhs) rhs-local*)]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[Nontail -> arg* arg-local**] ...))
           (values
             `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))
             (apply append arg-local**))]
          [(,prim ,[Nontail -> rand* local**] ...)
           (guard (effect-primitive? prim))
           (values `(,prim ,rand* ...)
                   (apply append local**))]
          [(effectcall (direct-call (class ,f)
                                    ,[Nontail -> rand* rand-local**] ...))
           (values
             `(effectcall (direct-call (class ,f) ,rand* ...))
             (apply append rand-local**))]
          [(effectcall (anon-call ,overflowed? ,[Nontail -> rator rator-local*]
                                  ,[Nontail -> rand* rand-local**] ...))
           (values `(effectcall (anon-call ,overflowed? ,rator ,rand* ...))
                   (append rator-local*
                           (apply append rand-local**)))]
          [,expr (error 'uncover-local
                        "invalid Effect expression: ~s"
                        expr)])))
    (define Pred
      (lambda (expr)
        (match expr
          [(quote ,b) (guard (boolean? b)) (values `(quote ,b) '())]
          [(if ,[Pred -> test test-local*]
               ,[conseq conseq-local*]
               ,[altern altern-local*])
           (values
             `(if ,test ,conseq ,altern)
             (append test-local* conseq-local* altern-local*))]
          [(begin ,[Effect -> expr* local**] ... ,[expr local*])
           (values
             `(begin ,expr* ... ,expr)
             (append (apply append local**) local*))]
          [(let ([,lhs ,[Nontail -> rhs rhs-local*]]) ,[body body-local*])
           (values
             `(let ([,lhs ,rhs]) ,body)
             (cons lhs (append rhs-local* body-local*)))]
          [(let-class (,class-defn* ...) ,[body body-local*])
           (let ([class-defn* (map process-class-defn class-defn*)])
             (values
               `(let-class ,class-defn* ,body)
               body-local*))]
          [(open-instance ,obj ,class-name ,[body body-local*])
           (values `(open-instance ,obj ,class-name ,body) body-local*)]
          [(,prim ,[Nontail -> rand* local**] ...)
           (guard (predicate-primitive? prim))
           (values `(,prim ,rand* ...)
                   (apply append local**))]
          [,expr (error 'uncover-local
                        "invalid Pred expression: ~s"
                        expr)])))
    (define Nontail
      (lambda (expr)
        (match expr
          [(quote ,imm) (values `(quote ,imm) '())]
          [,var (guard (symbol? var)) (values var '())]
          [(toplvl-varref ,var)
           (values `(toplvl-varref ,var) '())]
          [(class ,var) (values `(class ,var) '())]
          [(if ,[Pred -> test test-local*]
               ,[conseq conseq-local*]
               ,[altern altern-local*])
           (values
             `(if ,test ,conseq ,altern)
             (append test-local* conseq-local* altern-local*))]
          [(begin ,[Effect -> expr* local**] ... ,[expr local*])
           (values
             `(begin ,expr* ... ,expr)
             (append (apply append local**) local*))]
          [(let ([,lhs ,[Nontail -> rhs rhs-local*]]) ,[body body-local*])
           (values
             `(let ([,lhs ,rhs]) ,body)
             (cons lhs (append rhs-local* body-local*)))]
          [(let-class (,class-defn* ...) ,[body body-local*])
           (let ([class-defn* (map process-class-defn class-defn*)])
             (values `(let-class ,class-defn* ,body) body-local*))]
          [(new ,class-name ,[args* args-local**] ...)
           (values
             `(new ,class-name ,args* ...)
             (apply append args-local**))]
          [(open-instance ,obj ,class-name ,[body body-local*])
           (values `(open-instance ,obj ,class-name ,body) body-local*)]
          [(object-reference ,obj ,class ,field)
           (values `(object-reference ,obj ,class ,field) '())]
          [(invoke-method ,obj ,class ,method (,[arg* arg-local**] ...))
           (values `(invoke-method ,obj ,class ,method (,arg* ...))
                   (apply append arg-local**))]
          [(foreign-call ,name ,type-sig (,[arg* arg-local**] ...))
           (values
             `(foreign-call ,name ,type-sig (,arg* ...))
             (apply append arg-local**))]
          [(static-ref ,class ,field)
           (values `(static-ref ,class ,field) '())]
          [(this-ref ,class ,field)
           (values `(this-ref ,class ,field) '())]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[arg* arg-local**] ...))
           (values
             `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))
             (apply append arg-local**))]
          [(,prim ,[Nontail -> rand* local**] ...)
           (guard (value-primitive?
                    prim
                    ;;WONDERFUL--this test caused an error:
                    ;;guard cannot refer to return-value variable local**
                    ;;Which means that match is implemented properly!
                    #;(begin
                        (fprintf
                          (console-output-port)
                          "This is a temporary test, prim is:
                          ~a~, locals are: ~a n"
                          prim local**) prim)))
           (values `(,prim ,rand* ...)
                   (apply append local**))]
          
          [(direct-call (class ,f) ,[Nontail -> rand* rand-local**] ...)
           (values
             `(direct-call (class ,f) ,rand* ...)
             (apply append rand-local**))]
          [(anon-call ,overflowed? ,[Nontail -> rator rator-local*]
                      ,[Nontail -> rand* rand-local**] ...)
           (values `(anon-call ,overflowed? ,rator ,rand* ...)
                   (append rator-local*
                           (apply append rand-local**)))]
          [,expr (error 'uncover-local
                        "invalid Nontail expression: ~s"
                        expr)])))
    (define Tail
      (lambda (expr)
        (match expr
          [(return ,[Final -> expr final-local*])
           (values `(return ,expr) final-local*)]
          [(if ,[Pred -> test test-local*]
               ,[conseq conseq-local*]
               ,[altern altern-local*])
           (values
             `(if ,test ,conseq ,altern)
             (append test-local* conseq-local* altern-local*))]
          [(begin ,[Effect -> expr* local**] ... ,[expr local*])
           (values
             `(begin ,expr* ... ,expr)
             (append (apply append local**) local*))]
          [(let ([,lhs ,[Nontail -> rhs rhs-local*]]) ,[body body-local*])
           (values
             `(let ([,lhs ,rhs]) ,body)
             (cons lhs (append rhs-local* body-local*)))]
          [(let-class (,class-defn* ...) ,[body body-local*])
           (let ([class-defn* (map process-class-defn class-defn*)])
             (values
               `(let-class ,class-defn* ,body)
               body-local*))]
          [(new ,class-name ,[Nontail -> args* args-local**] ...)
           (values
             `(new ,class-name ,args* ...)
             (apply append args-local**))]
          [(open-instance ,obj ,class-name ,[body body-local*])
           (values
             `(open-instance ,obj ,class-name ,body)
             body-local*)]
          [(object-reference ,obj ,class ,field)
           (values `(object-reference ,obj ,class ,field) '())]
          [(invoke-method ,obj ,class ,method
                          (,[Nontail -> arg* arg-local**] ...))
           (values
             `(invoke-method ,obj ,class ,method (,arg* ...))
             (apply append arg-local**))]
          [(foreign-call ,name ,type-sig (,[Nontail -> arg* arg-local**] ...))
           (values
             `(foreign-call ,name ,type-sig (,arg* ...))
             (apply append arg-local**))]
          [(static-ref ,class ,field)
           (values `(static-ref ,class ,field) '())]
          [(this-ref ,class ,field)
           (values `(this-ref ,class ,field) '())]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[Nontail -> arg* arg-local**] ...))
           (values
             `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))
             (apply append arg-local**))]
          [,expr (error 'uncover-local
                        "invalid Tail expression: ~s"
                        expr)])))
    (define Final
      (lambda (expr)
        (match expr
          [(quote ,imm) (values `(quote ,imm) '())]
          [,var (guard (symbol? var)) (values var '())]
          [(toplvl-varref ,var)
           (values `(toplvl-varref ,var) '())]
          [(class ,var) (values `(class ,var) '())]
          [(new ,class-name ,[Nontail -> args* arg-local**] ...)
           (values
             `(new ,class-name ,args* ...)
             (apply append arg-local**))]
          [(object-reference ,obj ,class ,field)
           (values `(object-reference ,obj ,class ,field) '())]
          [(invoke-method ,obj ,class ,method
                          (,[Nontail -> arg* arg-local**] ...))
           (values
             `(invoke-method ,obj ,class ,method (,arg* ...))
             (apply append arg-local**))]
          [(foreign-call ,name ,type-sig (,[Nontail -> arg* arg-local**] ...))
           (values
             `(foreign-call ,name ,type-sig (,arg* ...))
             (apply append arg-local**))]
          [(static-ref ,class ,field)
           (values `(static-ref ,class ,field) '())]
          [(this-ref ,class ,field)
           (values `(this-ref ,class ,field) '())]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[Nontail -> arg* arg-local**] ...))
           (values
             `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))
             (apply append arg-local**))]
          [(,prim ,[Nontail -> rand* local**] ...)
           (guard (value-primitive? prim))
           (values `(,prim ,rand* ...) (apply append local**))]
          [(tailcall
             (direct-call (class ,f)
                          ,[Nontail -> rand* rand-local**] ...))
           (values
             `(tailcall (direct-call (class ,f) ,rand* ...))
             (apply append rand-local**))]
          [(tailcall (anon-call ,overflowed? ,[Nontail -> rator rator-local*]
                                ,[Nontail -> rand* rand-local**] ...))
           (values `(tailcall (anon-call ,overflowed? ,rator ,rand* ...))
                   (append rator-local*
                           (apply append rand-local**)))]
          [,expr (error 'uncover-local
                        "invalid Final expression: ~s" expr)])))
    (define Lambda
      (lambda (expr)
        (match expr
          [(lambda ,formalexp ,[Tail -> body local*])
           `(lambda ,formalexp (local (,local* ...) ,body))])))
    (define ClassDef
      (lambda (expr)
        (match expr
          [(class-def (,free* ...) ,[Lambda -> body])
           `(class-def (,free* ...) ,body)])))
    (define Letrec
      (lambda (expr)
        (match expr
          [(letrec ([,lhs* ,[ClassDef -> rhs*]] ...) (entry-point ,main))
           `(letrec ([,lhs* ,rhs*] ...)
              (entry-point ,main))]
          [(letrec ([,lhs* ,[ClassDef -> rhs*]] ...)
             ,[Tail -> body body-local])
           `(letrec ([,lhs* ,rhs*] ...) (local ,body-local ,body))]
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
          [,unmatched (error 'remove-nonunary-let
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
             `(uncover-local-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))