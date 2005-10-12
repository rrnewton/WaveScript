;;; Pass 0: object-system-preprocess
;;; March 2002

;;;
;;; Craig Citro
;;;

;;; This pass normalizes all uses of the object-related syntaxes
;;; before the verify-scheme pass gets at them. It's reasonable to
;;; do this beforehand, because eventually the small transformations
;;; done here will all become part of our reader

;;; Well-formed input to this pass is in the following language:

;;; <Prog> ::= <Exp>
;;;          | (program <class-def> ... <Exp>)
;;;

;;; <Exp>  ::= <constant>
;;;          | (quote <datum>)
;;;          | <var>
;;;          | (set! <var> <Exp>)
;;;          | (if <Exp> <Exp>)
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (begin <Exp> <Exp>*)
;;;          | (lambda <Formalexp> <Exp> <Exp>*)
;;;          | (case-lambda [<Formalexp> <Exp> <Exp>*]*)
;;;          | (let (<Decl>*) <Exp> <Exp>*)
;;;          | (letrec (<Decl>*) <Exp> <Exp>*)
;;;          | (<primitive> <Exp>*)
;;;          | (<Exp> <Exp>*)
;;; <Decl> ::= (<var> <Exp>)
;;; <Formalexp> ::= <var>
;;;               | (<var>*)
;;;               | (<var>* . <var>)

;;; A <constant> is a boolean or integer.  A <datum> is a <constant>,
;;; list or pair of <datum>, or vector of <datum>.  A <var> is a
;;; symbol.  A <primitive> is one of the symbols in the set {-, *, +, <,
;;; <=, =, add1, boolean?, car, cdr, cons, eq?, integer?, make-vector,
;;; not, null?, pair?, procedure?, set-car!, set-cdr!, sub1, vector?,
;;; vector-length, vector-ref, vector-set!, void, zero?}.

;;; Output from this pass is in the same language.

;;; The implementation requires constant?, datum?, keyword?,
;;; scheme-primitive?, set?, and the list scheme-primitives from
;;; helpers.ss.

(define object-system-preprocess
  (let ()
    ;;; CC
    ;;;
    ;;; This handles transforming a.b.c into the appropriate
    ;;;  ref type
    ;;; It assumes type object if something is referenced
    ;;;  and no class is known
    (define process-var
      (lambda (what var env)
        (let ([var-ls (breakdown var #\.)])
          (if (= 1 (length var-ls))
              var
              (mvlet ([(classes instances foreign this)
                       (split-env env)])
                (cond
                  [(memv (car var-ls) classes)
                   `(static-ref ,@var-ls)]
                  [(assq (car var-ls) instances) =>
                   (lambda (x)
                     `(obj-ref ,(car x) ,(cadr x) ,(cadr var-ls)))]
                  [(memv (car var-ls) foreign)
                   `(foreign-call ,var)]
                  [(assq (car var-ls) this) =>
                   (lambda (x)
                     `(this-ref ,(car x) ,(cadr x) ,(cadr var-ls)))]
                  [else (error 'object-system-preprocess
                               "unknown reference: ~s" var)]))))))
    (define split-env
      (lambda (env)
        (let loop ([classes '()][inst '()][foreign '()][this '()][env env])
          (match env
            [()
             (values classes inst foreign this)]
            [((class-name ,name) ,env ...)
             (loop (cons name classes) inst foreign this env)]
            [((package ,name) ,env ...)
             (loop classes inst (cons name foreign) this env)]
            [((instance ,class ,name) ,env ...)
             (loop classes (cons (list name class) inst) foreign this env)]
            [((this ,field ,class) ,env ...)
             (loop classes inst foreign (cons (list field class) this) env)]
            [,unmatched
              (error 'split-env "unknown env entry: ~s" env)]))))
    (define breakdown-old
      (lambda (name sym)
        (let loop ([symls (string->list (symbol->string name))]
                   [varls '()]
                   [buf '()])
          (cond
            [(null? symls)
             (reverse (cons (string->symbol (list->string (reverse buf)))
                            varls))]
            [(eqv? (car symls) sym)
             (loop (cdr symls)
                   (cons (string->symbol (list->string (reverse buf)))
                         varls)
                   '())]
            [else
              (loop (cdr symls)
                    varls
                    (cons (car symls) buf))]))))
    (define breakdown
      (lambda (name sym)
        (let loop ([symls (string->list (symbol->string name))]
                   [buf '()])
          (cond
            [(null? symls)
             (list (string->symbol (list->string (reverse buf))))]
            [(eqv? (car symls) sym)
             (list
               (string->symbol (list->string (reverse buf)))
               (string->symbol (list->string (cdr symls))))]
            [else
              (loop (cdr symls)
                    (cons (car symls) buf))]))))
    ;; Transform any local defines into a letrec:
    #;(define letrec-definitions
        (lambda (expr* env)
          (let loop ([defn* '()] [body* '()]
                     [expr* expr*])
            (if (null? expr*)
                `(letrec (,@(reverse defn*)) ,@(reverse body*))
                (match (car expr*)
                  [(define ,lhs ,rhs)
                   (loop (cons (list lhs (process-expr rhs env)) defn*)
                         body* (cdr expr*))]
                  [,other
                    (loop defn* (cons (process-expr other env) body*)
                          (cdr expr*))])))))
    (define hashprim?
      (lambda (s)
        (or (eq? s '\#primitive)
            (eq? s '\#primitive2))))
    (define process-expr*
      (lambda (expr* env)
        (map (lambda (expr) (process-expr expr env)) expr*)))
    (define process-prim-exp
      (lambda (e)
        (letrec ([loop
                   (lambda (e)
                     (match e
                       [,s (guard (symbol? s)) s]
                       [(,hp ,n ,x) (loop x)]
                       [(,hp ,x) (loop x)]))])
          (match e
            [(,hp ,n ,x)
             `(\#primitive ,n ,(loop x))]
            [(,hp ,x)
             `(\#primitive ,(snet-optimize-level) ,(loop x))]))))
    (define prim-exp?
      (lambda (e)
        (match e
          [(\#primitive ,x) #t]
          [(\#primitive ,n ,x) #t]
          [(\#primitive2 ,x) #t]
          [(\#primitive2 ,n ,x) #t]
          [,other #f])))
    (define process-expr
      (lambda (expr env)
        (match expr
          [,const
            (guard (constant? const))
            const]
          [(quote ,datum)
           `(quote ,datum)]
          [,var
            (guard (symbol? var))
            (process-var "reference" var env)]
          
          ;; If we're running chez in optimize lvl 2, we have some
          ;; annoyances with primitive names becoming #%'d:
          #;[(\#define-primitive (#%primitive ,n ... ,v) ,rhs)
             (disp "got define prim on a #%")
             (process-expr `(\#define-primitive ,v ,rhs) env)]
          [(\#define-primitive ,v ,[rhs])
           `(let ([val ,rhs])
              (set! ,v val)
              ((\#primitive ,(snet-optimize-level) \#system-set!) ',v val)
              #;(if ((\#primitive ,(snet-optimize-level) procedure?) val)
                    ((\#primitive ,(snet-optimize-level) \#set-closure-name!)
                     val ',(symbol->string v)))
              )]
          
          ;; Sets the system value for a symbol table entry:
          [(set! ,var ,[rhs])
           `(set! ,(process-var "assignment" var env) ,rhs)]
          [(if ,[test] ,[conseq])
           `(if ,test ,conseq)]
          [(if ,[test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[first-expr] ,[rest-expr*] ...)
           `(begin ,first-expr ,rest-expr* ...)]
          
          [(lambda ,formals ,expr* ...)
           (let ([expr* (process-expr* expr* env)])
             `(lambda ,formals ,expr* ...))]
          [(case-lambda [,formals* ,expr** ...] ...)
           (let ([expr**
                   (map (lambda (expr*)
                          (process-expr* expr* env))
                        expr**)])
             `(case-lambda [,formals* ,expr** ...] ...))]
          [(let ([,lhs* ,[rhs*]] ...) ,expr* ...)
           (let ([expr* (process-expr* expr* env)])
             `(let ([,lhs* ,rhs*] ...) ,expr* ...))]
          [(letrec ([,lhs* ,rhs*] ...) ,expr* ...)
           (let ([expr* (process-expr* expr* env)]
                 [rhs* (process-expr* rhs* env)])
             `(letrec ([,lhs* ,rhs*] ...) ,expr* ...))]
          
          [(let-class (,class-defn* ...) ,body* ...)
           (let ([class-defn* (map (lambda (x)
                                     (process-class-defn x env))
                                   class-defn*)])
             (let ([env (append (map (lambda (x)
                                       `(class-name ,(cadr x)))
                                     class-defn*) env)])
               (let ([body* (map (lambda (x)
                                   (process-expr x env))
                                 body*)])
                 `(let-class ,class-defn* (begin ,body* ...)))))]
          [(new ,class-name ,[args*] ...)
           `(new ,class-name ,args* ...)]
          [(open-instance ,obj ,class-name
                          ,body* ...)
           (let ([env (cons `(instance ,class-name ,obj) env)])
             (let ([body* (map (lambda (x)
                                 (process-expr x env))
                               body*)])
               `(open-instance ,obj ,class-name (begin ,body* ...))))]
          [(open-package ,package ,body* ...)
           (let ([env (cons `(package ,package) env)])
             (let ([body* (map (lambda (x)
                                 (process-expr x env))
                               body*)])
               `(open-package ,package (begin ,body* ...))))]
          [(,keyword ,form* ...)
           (guard (keyword? keyword))
           (error 'object-system-preprocess
                  "invalid syntax ~s" `(,keyword ,form* ...))]
          
          ;; Here we clean up the weirdness resulting from my
          ;; abuse of the #% syntax.
          [,primexp
            (guard (prim-exp? primexp))
            (process-prim-exp primexp)]
          [(,primexp ,[rand*] ...)
           (guard (prim-exp? primexp))
           `(,(process-prim-exp primexp) ,rand* ...)]
          
          
          [(,prim ,[rand*] ...)
           (guard (>= (snet-optimize-level) 2)
                  (scheme-primitive? prim))
           `(,prim ,rand* ...)]
          [(,[rator] ,[rand*] ...)
           (if (or (not (list? rator))
                   (not (memv (car rator) '(foreign-call obj-ref static-ref))))
               `(,rator ,rand* ...)
               (case (car rator)
                 [(foreign-call)
                  `(foreign-call ,(cadr rator) '() ,rand*)]
                 [(obj-ref)
                  `(invoke-method
                     ,(cadr rator) ,(caddr rator) ,(cadddr rator)
                     ,rand*)]
                 [(static-ref)
                  `(invoke-static-method
                     ,(cadr rator) ,(caddr rator)
                     '() ;;; dummy fake typesig
                     ,rand*)]
                 [(this-ref)
                  `(invoke-method
                     ,(cadr rator) ,(caddr rator) ,(cadddr rator)
                     ,rand*)]
                 ))]
          [,unmatched
            (error 'object-system-preprocess "invalid syntax ~s" unmatched)])))
    (define sort-out
      (lambda (decl*)
        (let loop ([fdecl* '()][mdecl* '()][decl* decl*])
          (cond
            [(null? decl*) (values fdecl* mdecl*)]
            [(not (list? (car decl*)))
             (error 'object-system-preprocess
                    "invalid class member ~s"
                    (car decl*))]
            [(eqv? (caar decl*)
                   'fields)
             (loop (cons (car decl*) fdecl*) mdecl* (cdr decl*))]
            [(eqv? (caar decl*)
                   'methods)
             (loop fdecl* (cons (car decl*) mdecl*) (cdr decl*))]
            [else
              (error 'object-system-preprocess
                     "invalid class member ~s"
                     (car decl*))]))))
    (define normalize-fdecl
      (lambda (fdecl*)
        (let loop ([fdecl* fdecl*][nf '()])
          (if (null? fdecl*)
              (cons 'fields nf)
              (loop (cdr fdecl*) (append (factor-fdecl (cdar fdecl*)) nf))))))
    ;;; CC
    ;;; "factor-fdecl" because it "factors" out the attributes
    (define factor-fdecl
      (lambda (fdecl)
        (let ([fdecl-split
                (lambda (fdecl)
                  (let loop ([mods '()][names fdecl])
                    (cond
                      [(null? fdecl) (error 'object-system-preprocess
                                            "Invalid field ~s"
                                            fdecl)]
                      [(attribute? (car names))
                       (loop (cons (car names) mods)
                             (cdr names))]
                      [else (values (reverse mods) names)])))])
          (mvlet ([(mods names) (fdecl-split fdecl)])
            (map (lambda (x)
                   (append mods (list x)))
                 names)))))
    (define normalize-mdecl
      (lambda (mdecl*)
        (let loop ([mdecl* mdecl*][nm '()])
          (if (null? mdecl*)
              (cons 'methods nm)
              (loop (cdr mdecl*) (append (factor-mdecl (cdar mdecl*)) nm))))))
    (define factor-mdecl
      (lambda (mdecl)
        (let ([mdecl-split
                (lambda (mdecl)
                  (let loop ([mods '()][pairs mdecl])
                    (cond
                      [(null? mdecl) (error 'object-system-preprocess
                                            "Invalid method ~s"
                                            mdecl)]
                      [(attribute? (car pairs))
                       (loop (cons (car pairs) mods)
                             (cdr pairs))]
                      [else
                        (let inner-loop ([tsig (reverse pairs)][p '()])
                          (if (or (null? tsig)
                                  (not (list? (car tsig))))
                              (values (reverse mods)
                                      (reverse tsig)
                                      (reverse p))
                              (inner-loop (cdr tsig)
                                          (cons (car tsig) p))))])))])
          (mvlet ([(mods tsig pairs) (mdecl-split mdecl)])
            (map (lambda (x)
                   (append mods (list tsig x)))
                 pairs)))))
    (define process-mdecl
      (lambda (mdecl env)
        (let ([decl (rac mdecl)])
          `(,@(rdc mdecl)
              [,(car decl) ,(process-expr (cadr decl) env)]))))
    (define process-class-defn
      (lambda (cdef env)
        (match cdef
          [(define-class ,Name ,base
             ,ctor
             ,decl* ...)
           (mvlet ([(fdecl* mdecl*) (sort-out decl*)])
             (let ([fdecl* (normalize-fdecl fdecl*)]
                   [mdecl* (normalize-mdecl mdecl*)])
               ;;; CC
               ;;; no process-fdecl because there's nothing to do to them
               (let ([env
                       (cons `(class-name ,Name) env)])
                 (let ([mdecl* (cons 'methods
                                     (map (lambda (x)
                                            (process-mdecl
                                              x
                                              (append (map
                                                        (lambda (x)
                                                          (list 'this
                                                                (rac x)
                                                                Name))
                                                        (cdr fdecl*))
                                                      env)))
                                          (cdr mdecl*)))]
                       [ctor (process-expr ctor env)])
                   `(define-class ,Name ,base ,ctor ,fdecl* ,mdecl*)))))]
          [,unmatched (error 'object-system-preprocess
                             "Invalid define-class: ~s"
                             cdef)])))
    (define split-defns
      (lambda (stmt*)
        (let loop ([cdefns '()][stmt-ls stmt*])
          (cond
            [(null? stmt-ls) (error 'object-system-preprocess
                                    "Program contains no body ~s"
                                    (cons 'program stmt*))]
            [(or (not (list? (car stmt-ls)))
                 (not (eqv? (caar stmt-ls) 'define-class)))
             (values (reverse cdefns) (cons 'begin stmt-ls))]
            [else (loop (cons (car stmt-ls) cdefns)
                        (cdr stmt-ls))]))))
    (define process-prog
      (lambda (prog)
        (match prog
          [(program ,stmt* ...)
           (mvlet ([(class-defns body) (split-defns stmt*)])
             (let ([class-defns (map (lambda (x)
                                       (process-class-defn x '()))
                                     class-defns)])
               (let ([body (process-expr body (map (lambda (x)
                                                     (list 'class-name x))
                                                   (map cadr class-defns)))])
                 `(base-language '(program ,@class-defns ,body)))))]
          [,expr
            (let ([body (process-expr expr '())])
              `(base-language '(program ,body)))])))
    
    (lambda (expr)
      (process-prog expr)
      )))