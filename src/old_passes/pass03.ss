;;; Pass 3: remove-implicit-begin
;;; January 2001

;;; UMM it looks like craig made this pass filter pkg references upward??
;;; Sigh, what a mess to untangle.


;;; This pass makes explicit the begin expressions that are implicit in
;;; the bodies of lambda, let, and letrec.

;;; The input language is the same as the input and output languages
;;; of Passes 1 and 2.

;;; Output from this pass is in the same language, except that lambda,
;;; let, and letrec bodies contain exactly one expression.

;;; <Prog> ::= ???? Do this later

;;; <Exp>  ::= <constant>
;;;          | (quote <datum>)
;;;          | <var>
;;;          | (set! <var> <Exp>)
;;;          | (if <Exp> <Exp>)
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (begin <Exp> <Exp>*)
;;;          | (lambda <Formalexp> <Exp>)
;;;          | (case-lambda [<Formalexp> <Exp>]*)
;;;          | (let (<Decl>*) <Exp>)
;;;          | (letrec (<Decl>*) <Exp>)
;;;          | (<primitive> <Exp>*)
;;;          | (<Exp> <Exp>*)
;;;          | (toplvl-varref <var>)
;;;          | (toplvl-varassign! <var> <Exp>)
;;; <Decl> ::= (<var> <Exp>)
;;; <Formalexp> ::= <var>
;;;               | (<var>*)
;;;               | (<var>* . <var>)

;;; Although not represented by this grammar, begin expressions
;;; cannot directly contain other begin expressions in the output,
;;; since the implementation flattens nested begin expressions.

;;; The implementation requires constant?, scheme-primitive?, and
;;; make-begin from helpers.ss.

(define remove-implicit-begin
  (let ()
    (define process-expr
      (lambda (expr)
        (match expr
          [,const
            (guard (constant? const))
            (values const '())]
          [(toplvl-varref ,var)
           (values `(toplvl-varref ,var) '())]
          [(toplvl-varassign! ,var ,[rhs rhspkg*])
           (values `(toplvl-varassign! ,var ,rhs)
                   rhspkg*)]
          [(quote ,datum)
           (values `(quote ,datum) '())]
          [,var (guard (symbol? var))
            (values var '())]
          [(set! ,var ,[rhs rpkg*])
           (values `(set! ,var ,rhs) rpkg*)]
          [(if ,[test tpkg*] ,[conseq cpkg*])
           (values `(if ,test ,conseq)
                   (append tpkg* cpkg*))]
          [(if ,[test tpkg*] ,[conseq cpkg*] ,[altern apkg*])
           (values `(if ,test ,conseq ,altern)
                   (append tpkg* cpkg* apkg*))]
          [(begin ,[expr epkg*] ,[expr* epkg**] ...)
           (values (make-begin `(,expr ,expr* ...))
                   (append (apply append epkg**) epkg*))]
          [(lambda ,formals ,[expr epkg*] ,[expr* epkg**] ...)
           (values `(lambda ,formals
                      ,(make-begin `(,expr ,expr* ...)))
                   (append (apply append epkg**) epkg*))]
          [(case-lambda [,formals* ,[expr* epkg**] ,[expr** epkg***] ...] ...)
           (values `(case-lambda
                      ,@(map (lambda (f e e*)
                               `[,f ,(make-begin (cons e e*))])
                             formals* expr* expr**))
                   (apply append
                          (apply append epkg** epkg***)))]
          [(let ((,lhs* ,[rhs* rpkg**]) ...) ,[expr epkg*] ,[expr* epkg**] ...)
           (values `(let ([,lhs* ,rhs*] ...)
                      ,(make-begin `(,expr ,expr* ...)))
                   (append (apply append rpkg**) (apply append epkg**) epkg*))]
          [(letrec ((,lhs* ,[rhs* rpkg**]) ...) ,[expr epkg*] ,[expr* epkg**] ...)
           (values `(letrec ([,lhs* ,rhs*] ...)
                      ,(make-begin `(,expr ,expr* ...)))
                   (append (apply append rpkg**) (apply append epkg**) epkg*))]
          [(let-class (,class-defn* ...) ,[body bpkg*])
           (mvlet ([(class-defn* cpkg**) (map process-class-defn class-defn*)])
             (values `(let-class ,class-defn* ,body)
                     (append (apply append cpkg**) bpkg*)))]
          [(new ,class-name ,[args* apkg**] ...)
           (values `(new ,class-name ,args* ...)
                   (apply append apkg**))]
          [(open-instance ,obj ,class-name ,[body bpkg*])
           (values `(open-instance ,obj ,class-name ,body) bpkg*)]
          [(open-package ,pkg ,[body bpkg*])
           (values body (cons pkg bpkg*))]
          [(object-reference ,obj ,class ,field)
           (values `(object-reference ,obj ,class ,field) '())]
          [(invoke-method ,obj ,class ,method (,[arg* apkg**] ...))
           (values `(invoke-method ,obj ,class ,method (,arg* ...))
                   (apply append apkg**))]
          [(foreign-call ,name ,type-sig (,[arg* apkg**] ...))
           (values `(foreign-call ,name ,type-sig (,arg* ...))
                   (apply append apkg**))]
          [(static-ref ,class ,field)
           (values `(static-ref ,class ,field) '())]
          [(this-ref ,class ,field)
           (values `(this-ref ,class ,field) '())]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[arg* apkg**] ...))
           (values
             `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))
             (apply append apkg**))]
          [(,prim ,[rand* rpkg**] ...)
           (guard (scheme-primitive? prim))
           (values `(,prim ,rand* ...)
                   (apply append rpkg**))]
          [(,[rator rpkg*] ,[rand* rpkg**] ...)
           (values `(,rator ,rand* ...) (append (apply append rpkg**) rpkg*))]
          [,unmatched
            (error 'remove-implicit-begin "invalid expression: ~s"
                   unmatched)])))
    (define process-class-defn
      (lambda (cdef)
        (match cdef
          [(define-class ,name ,base
             ,ctor
             (fields (,fmods** ... ,fname*) ...)
             (methods (,mmods** ... ,mtype*
                        (,mname* ,mbody*)) ...))
           (mvlet ([(ctor cpkg*) (process-expr ctor)]
                   [(mbody* mpkg**) (mvmap2 process-expr mbody*)])
             (values `(define-class
                        ,name ,base ,ctor
                        (fields (,fmods** ... ,fname*) ...)
                        (methods (,mmods** ... ,mtype*
                                   (,mname* ,mbody*)) ...))
                     (append (apply append mpkg**) cpkg*)))]
          [,unmatched (error 'remove-implicit-begin
                             "invalid class defn ~s"
                             cdef)])))
    (lambda (expr)
      (match expr
        [(,input-language (quote (program ,class-defns* ... ,body)))
         (mvlet ([(cdef* cpkg**) (mvmap2 process-class-defn class-defns*)])
           (mvlet ([(body bpkg*) (process-expr body)])
             `(,input-language
                '(program ,(append (apply append cpkg**) bpkg*)
                          ,class-defns* ... ,body))))]))))
  
(define mvmap2
  (lambda (f ls)
    (let loop ([ls ls][ls1 '()][ls2 '()])
      (if (null? ls)
          (values (reverse ls1) (reverse ls2))
          (mvlet ([(new1 new2) (f (car ls))])
            (loop (cdr ls) (cons new1 ls1) (cons new2 ls2)))))))