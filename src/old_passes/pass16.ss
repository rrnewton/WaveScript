;;; Pass 16: convert-closure
;;; January 2001

;;; This pass converts lambda expressions into explicit closure creation,
;;; separating the code for a procedure from its free variables.
;;; After this pass, lambda expressions represent first-order procedures,
;;; i.e., they have no free variables.  Each receives an explicit closure
;;; pointer as its first argument, from which its (former) free variables
;;; can be extracted.

;;; This conversion is accomplished via a simple transformation of
;;; letrec expressions:
;;;
;;; (letrec ([lhs (lambda (formal ...)
;;;                  (free (free ...) lbody))]
;;;          ...)
;;;   body)
;;;
;;; is transformed to
;;;
;;; (letrec ([lab (lambda (cp formal ...)
;;;                 (bind-free (cp free ...)
;;;                   lbody))]
;;;          ...)
;;;   (closures ([lhs (label lab) free ...] ...)
;;;     body))
;;;
;;; where each lab is a distinct unique name.  For example,
;;;
;;; (let ([x.1 '17])
;;;   (letrec ([f.3 (lambda (y.5)
;;;                   (free (g.2 x.1)
;;;                     (g.2 (+ x.1 y.5))))]
;;;            [g.2 (lambda (z.4)
;;;                   (free ()
;;;                     (+ z.4 '1)))])
;;;     (f.3 (g.2 '2))))
;;;
;;; becomes
;;;
;;; (let ([x.1 '17])
;;;   (letrec ([f$3 (lambda (cp.7 y.5)
;;;                   (bind-free (cp.7 g.2 x.1)
;;;                     (g.2 (+ x.1 y.5))))]
;;;            [g$2 (lambda (cp.6 z.4)
;;;                   (bind-free (cp.6)
;;;                     (+ z.4 '1)))])
;;;     (closures ([f.3 (label f$3) g.2 x.1] [g.2 (label g$2)])
;;;       (f.3 (g.2 '2)))))

;;; The letrec-bound variables are referenced using the syntax
;;; (label <var>).  These variables will ultimately become assembly
;;; language labels for the blocks of code that implement teh
;;; corresponding procedure.  Each lambda expression now accepts
;;; an additional argument, the closure pointer; this argument is
;;; supplied implicitly by an (anonymous) call to a procedure
;;; bound by a closures form.

;;; The input language is the same as the output language of Pass 11.

;;; The output language differs in that letrec-bound variables are
;;; now referenced by the syntax (label <var>), free forms have
;;; been replaced by bind-free forms, and a new closures form has
;;; been introduced.

;;; <Input> ::= (<language-name> <Program>)
;;; <Program> ::= (program (<var>*) (<pkg>*) <class-def>* <Exp>)
;;; <Exp>  ::= (quote <imm>)
;;;          | (label <var>)
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (begin <Exp> <Exp>*)
;;;          | (let ((<var> <Exp>)*) <Exp>)
;;;          | (letrec ((<var> <Lambda>)*)
;;;          |   (closures ([<var> <Exp> <var>*] ...)
;;;          |     <Exp>))
;;;          | (<primitive> <Exp>*)
;;;          | (<Exp> <Exp>*)
;;;          | (toplvl-varref <var>)
;;;          | (toplvl-varassign! <var> <Exp>)
;;; <Lambda> ::= (lambda <Formalexp> (bind-free (<var> <var>*) <Exp>))
;;; <Formalexp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)

;;; The implementation requires code-name, extended-scheme-primitive?,
;;; and unique-name from helpers.ss.

(define convert-closure
  (let ()
    (define Expr
      (lambda (expr)
        (match expr
          [(quote ,imm) `(quote ,imm)]
          [,var (guard (symbol? var)) var]
          [(toplvl-varref ,var) `(toplvl-varref ,var)]
          [(toplvl-varassign! ,var ,[rhs])
           `(toplvl-varassign! ,var ,rhs)]
          [(if ,[test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[expr*] ...) `(begin ,expr* ...)]
          [(let ([,lhs* ,[rhs*]] ...) ,[body])
           `(let ([,lhs* ,rhs*] ...) ,body)]
          
          ;;=====================================================
          
          [(letrec ([,lhs* (lambda ,formalexp*
                             (free (,free** ...) ,[lbody*]))] ...)
             ,[body])
           (let ([label* (map code-name lhs*)]
                 [cp* (map (lambda (x) (unique-name 'cp)) lhs*)])
             `(letrec (,@(map
                           (lambda (label cp formalexp free* lbody)
                             `[,label
                                (lambda
                                  ;; This cons is tricky, think about it:
                                  ,(cons cp formalexp)
                                  (bind-free (,cp ,free* ...)
                                             ,lbody))])
                           label* cp* formalexp* free** lbody*))
                (closures
                  (,@(map (lambda (lhs label free*)
                            `[,lhs (label ,label) ,free* ...])
                          lhs* label* free**))
                  ,body)))]
          
          ;;=====================================================
          
          [(letrec ([,lhs* (lambda (,formal** ...)
                             (free (,free** ...) ,[lbody*]))] ...)
             ,[body])
           (let ([label* (map code-name lhs*)]
                 [cp* (map (lambda (x) (unique-name 'cp)) lhs*)])
             `(letrec (,@(map (lambda (label cp formal* free* lbody)
                                `[,label
                                   (lambda
                                     (,cp ,formal* ...)
                                     (bind-free (,cp ,free* ...)
                                                ,lbody))])
                              label* cp* formal** free** lbody*))
                (closures
                  (,@(map (lambda (lhs label free*)
                            `[,lhs (label ,label) ,free* ...])
                          lhs* label* free**))
                  ,body)))]
          
          
          ;;;==================================================
          
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
          [(set-field! ,field ,[rhs])
           `(set-field! ,field ,rhs)]
          [(invoke-static-method ,class ,method-name ,type-sig (,[arg*] ...))
           `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))]
          [(,prim ,[rand*] ...)
           (guard (extended-scheme-primitive? prim))
           `(,prim ,rand* ...)]
          [(,[rator] ,[rand*] ...)
           `(,rator ,rand* ...)]
          [,unmatched
            (error 'convert-closure "invalid expression: ~s"
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
          [(lambda ,args (free ,free ,body))
           (let ([body (Expr body)])
             `(lambda ,args ,body))])))
    (lambda (prog)
      (match prog
        [(,input-language
           (quote (program ,sym* ,pkg* ,class-defns* ... ,body)))
         (let ([class-defns* (map process-class-defn class-defns*)])
           (let ([body (Expr body)])
             `(convert-closure-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))