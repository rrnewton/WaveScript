
;;; Pass 17: lift-letrec
;;; January 2001

;;; This pass makes all lambda bindings global, i.e., places them into
;;; a single top-level letrec expression that contains the remainder of
;;; the transformed program.  This transformation is possible since the
;;; lambda expressions no longer have free variables.

;;; (letrec ([map$1
;;;           (lambda (cp.10 f.6 ls.5)
;;;             (bind-free (cp.10 map.1)
;;;               (if (null? ls.5)
;;;                   '()
;;;                   (cons (f.6 (car ls.5))
;;;                         ((label map$1) map.1 f.6 (cdr ls.5))))))])
;;;   (closures ([map.1 (label map$1) map.1])
;;;     (letrec ([f$4
;;;               (lambda (cp.9 x.2)
;;;                 (bind-free (cp.9)
;;;                   (letrec ([anon$7
;;;                             (lambda (cp.8 y.3)
;;;                               (bind-free (cp.8 x.2) (* x.2 y.3)))])
;;;                     (closures ([anon.7 (label anon$7) x.2]) anon.7))))])
;;;       (closures ([f.4 (label f$4)])
;;;         ((label map$1)
;;;          map.1
;;;          ((label f$4) f.4 '7)
;;;          (cons '1 (cons '2 (cons '3 '()))))))))
;;;
;;; becomes
;;;
;;; (letrec ([anon$7
;;;           (lambda (cp.8 y.3) (bind-free (cp.8 x.2) (* x.2 y.3)))]
;;;          [f$4
;;;           (lambda (cp.9 x.2)
;;;             (bind-free (cp.9)
;;;               (closures ([anon.7 (label anon$7) x.2]) anon.7)))]
;;;          [map$1
;;;           (lambda (cp.10 f.6 ls.5)
;;;             (bind-free (cp.10 map.1)
;;;               (if (null? ls.5)
;;;                   '()
;;;                   (cons (f.6 (car ls.5))
;;;                         ((label map$1) map.1 f.6 (cdr ls.5))))))])
;;;   (closures ([map.1 (label map$1) map.1])
;;;     (closures ([f.4 (label f$4)])
;;;       ((label map$1)
;;;        map.1
;;;        ((label f$4) f.4 '7)
;;;        (cons '1 (cons '2 (cons '3 '())))))))

;;; The input language is the same as the output language of Pass 12.

;;; The output language differs in that letrec is found only at the
;;; top level of the program.


;;; <Input> ::= (<language-name> <Program>)
;;; <Program> ::= (program (<var>*) (<pkg>*) <class-def>* <Letrec>)
;;; <Letrec> ::= (letrec ((<var> <Lambda>)*) <Exp>)
;;; <Lambda> ::= (lambda <Formalexp> (bind-free (<var> <var>*) <Exp>))
;;; <Exp>  ::= (quote <imm>)
;;;          | (label <var>)
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (begin <Exp> <Exp>*)
;;;          | (let ((<var> <Exp>)*) <Exp>)
;;;          | (closures ([<var> <Exp> <var>*] ...) <Exp>)
;;;          | (<primitive> <Exp>*)
;;;          | (<Exp> <Exp>*)
;;;          | (toplvl-varref <var>)
;;;          | (toplvl-varassign! <var> <Exp>)
;;; <Formalexp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)

;;; The implementation requires extended-scheme-primitive? from helpers.ss.

(define lift-letrec
  (let ()
    (define Lambda
      (lambda (expr)
        (match expr
          [(lambda ,formalexp
             (bind-free (,free* ...)
                        ,[Expr -> body body-decl*]))
           (values
             `(lambda ,formalexp (bind-free (,free* ...) ,body))
             body-decl*)]
          [,unmatched
            (error 'lift-letrec "invalid lambda expression: ~s"
                   unmatched)])))
    (define Expr
      (lambda (expr)
        (match expr
          [(quote ,imm) (values `(quote ,imm) '())]
          [(label ,lab) (values `(label ,lab) '())]
          [,var (guard (symbol? var)) (values var '())]
          ;;; <Input> ::= (<language-name> <Program>)
          ;;; <Program> ::= (program (<var>*) (<pkg>*) <class-def>* <Exp>)
          ;;;          | (toplvl-varref <var>)
          ;;;          | (toplvl-varassign! <var> <Exp>)
          [(toplvl-varref ,var)
           (values `(toplvl-varref ,var) '())]
          [(toplvl-varassign! ,var ,[rhs rhs-decl*])
           (values `(toplvl-varassign! ,var ,rhs) rhs-decl*)]
          [(if ,[test test-decl*] ,[conseq conseq-decl*] ,[altern altern-decl*])
           (values
             `(if ,test ,conseq ,altern)
             (append test-decl* conseq-decl* altern-decl*))]
          [(begin ,[expr* expr-decl**] ...)
           (values
             `(begin ,expr* ...)
             (apply append expr-decl**))]
          [(let ([,lhs* ,[rhs* rhs-decl*]] ...) ,[body body-decl*])
           (values
             `(let ([,lhs* ,rhs*] ...) ,body)
             (append (apply append rhs-decl*) body-decl*))]
          ; bit of a cheat: we don't process code* since we know they are
          ; label expressions even though we call them Exps
          [(letrec ([,lhs* ,[Lambda -> rhs* rhs-decl*]] ...)
             (closures ([,name* ,code* ,free** ...] ...)
                       ,[body body-decl*]))
           (values
             `(closures ([,name* ,code* ,free** ...] ...) ,body)
             (append
               (apply append rhs-decl*)
               body-decl*
               `([,lhs* ,rhs*] ...)))]
          [(let-class (,class-defn* ...) ,[body body-fn*])
           (let ([class-defn* (map process-class-defn class-defn*)])
             (values `(let-class ,class-defn* ,body) body-fn*))]
          [(new ,class-name ,[args* args-fn**] ...)
           (values `(new ,class-name ,args* ...)
                   (apply append args-fn**))]
          [(open-instance ,obj ,class-name ,[body body-fn*])
           (values `(open-instance ,obj ,class-name ,body)
                   body-fn*)]
          [(object-reference ,obj ,class ,field)
           (values `(object-reference ,obj ,class ,field) '())]
          [(invoke-method ,obj ,class ,method (,[arg* arg-fn**] ...))
           (values `(invoke-method ,obj ,class ,method (,arg* ...))
                   (apply append arg-fn**))]
          [(foreign-call ,name ,type-sig (,[arg* arg-fn**] ...))
           (values `(foreign-call ,name ,type-sig (,arg* ...))
                   (apply append arg-fn**))]
          [(static-ref ,class ,field)
           (values `(static-ref ,class ,field) '())]
          [(this-ref ,class ,field)
           (values `(this-ref ,class ,field) '())]
          [(set-field! ,field ,[rhs rhs-fn*])
           (values `(set-field! ,field ,rhs) rhs-fn*)]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[arg* arg-fn**] ...))
           (values `(invoke-static-method ,class ,method-name
                                          ,type-sig (,arg* ...))
                   (apply append arg-fn**))]
          [(,prim ,[rand* rand-decl*] ...)
           (guard (extended-scheme-primitive? prim))
           (values
             `(,prim ,rand* ...)
             (apply append rand-decl*))]
          [(,[rator rator-decl*] ,[rand* rand-decl*] ...)
           (values
             `(,rator ,rand* ...)
             (append rator-decl* (apply append rand-decl*)))]
          [,unmatched
            (error 'lift-letrec "invalid expression: ~s"
                   unmatched)])))
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
          [,unmatched (error 'verify-scheme
                             "invalid class defn ~s"
                             cdef)])))
    (define process-method
      (lambda (meth)
        (match meth
          [(lambda ,args ,body)
           (mvlet ([(body fns) (Expr body)])
             `(lambda ,args (letrec ,fns ,body)))])))
    (lambda (prog)
      (match prog
        [(,input-language
           (quote (program ,sym* ,pkg* ,class-defns* ... ,body)))
         (let ([class-defns* (map process-class-defn class-defns*)])
           (mvlet ([(body body-fn) (Expr body)])
             `(,input-language
                '(program ,sym* ,pkg* ,class-defns* ...
                          (letrec ,body-fn ,body)))))]))))