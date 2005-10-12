;;; Pass 21: Reduce Primitives.2
;===============================================================================
;;; This pass reduces 'compound' or 'derived' primitives (the ones found in
;;; the global list derived-scheme-primitives) into expressions using only
;;; basic primitives.

;;; The output language will be the same as the input language, except
;;; in that primitives from the derived-scheme-primitives list should
;;; not occur.

(define reduce-primitives.2
  (let ()
    (define Expr
      (lambda (expr)
        (match expr
          [(quote ,imm) `(quote ,imm)]
          [(class ,lab) `(class ,lab)]
          [,var (guard (symbol? var)) var]
          [(toplvl-varref ,var) `(toplvl-varref ,var)]
          [(toplvl-varassign! ,var ,[rhs])
           `(toplvl-varassign! ,var ,rhs)]
          [(if ,[test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[expr*] ...) `(begin ,expr* ...)]
          [(let ([,lhs* ,[rhs*]] ...) ,[body])
           `(let ([,lhs* ,rhs*] ...) ,body)]
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
          ;; These cases dissolve specific derived primitives:
          [(add1 ,[rand]) `(+ '1 ,rand)]
          [(sub1 ,[rand]) `(+ '-1 ,rand)]
          [(> ,[a] ,[b])
           (let ([v1 (unique-name 'gttmp)]
                 [v2 (unique-name 'gttmp)])
             `(let ([,v1 ,a] [,v2 ,b])
                (if (< ,a ,b) '#f
                    (if (= ,a ,b) '#f
                        '#t))))]
          [(>= ,[a] ,[b]) `(not (< ,a ,b))]
          [(<= ,a ,b) `(not ,(Expr `(> ,a ,b)))]
          [(apply ,[fun] ,[obj*] ... ,[lst])
           `(apply ,fun
                   ,(let loop ((obj* obj*))
                      (if (null? obj*) lst
                          `(cons ,(car obj*)
                                 ,(loop (cdr obj*))))))]
          ;; vector and list dissolve into make-vector and cons
          ;; If one desired, these list and vector definitions could
          ;; be abolished in favor of their only being library functions.
          [(vector ,[rand*] ...)
           (let ([tmpname*
                   (mapright
                     unique-name
                     (make-list (length rand*) 'vecloc))]
                 [vec (unique-name 'vec)])
             `(let ([,tmpname* ,rand*] ...)
                (let ([,vec (make-vector ',(length rand*))])
                  (begin
                    ,@(map (lambda (tmpname i)
                             `(vector-set! ,vec ',i ,tmpname))
                           tmpname* (iota (length tmpname*)))
                    ,vec))))]
          [(list ,[rand*] ...)
           (let ([tmpname*
                   (mapright
                     unique-name
                     (make-list (length rand*) 'vecloc))])
             `(let ([,tmpname* ,rand*] ...)
                ,(let loop ([names tmpname*])
                   (if (null? names)
                       ''()
                       `(cons ,(car names)
                              ,(loop (cdr names)))))))]
          [(,prim ,[rand*] ...)
           (guard (extended-scheme-primitive? prim))
           `(,prim ,rand* ...)]
          ;This is a direct, call eh?  How should I get rid of it?
          [(direct-call (class ,f) ,[obj] ,[rand*] ...)
           `(direct-call (class ,f) ,obj ,rand* ...)]
          [(anon-call ,overflowed? ,[rator] ,[rand*] ...)
           `(anon-call ,overflowed? ,rator ,@rand*)]
          [,expr (error 'reduce-primitives.2 "invalid expression: ~s" expr)])))
    (define Lambda
      (lambda (expr)
        (match expr
          [(lambda ,formalexp ,[Expr -> body])
           `(lambda ,formalexp ,body)])))
    (define ClassDef
      (lambda (expr)
        (match expr
          [(class-def (,free* ...) ,[Lambda -> body])
           `(class-def (,free* ...) ,body)])))
    (define Letrec
      (lambda (expr)
        (match expr
          [(letrec ([,lhs* ,[ClassDef -> rhs*]] ...) ,body)
           `(letrec ([,lhs* ,rhs*] ...)
              ,(Expr body))])))
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
          [,unmatched (error 'reduce-primitives.2
                             "invalid class defn ~s"
                             cdef)])))
    (define process-method
      (lambda (meth)
        (match meth
          [(lambda ,args ,body)
           (let ([body (Letrec body)])
             `(lambda ,args ,body))])))
    (lambda (prog)
      (match prog
        [(,input-language
           (quote (program ,sym* ,pkg* ,class-defns* ... ,body)))
         (let ([class-defns* (map process-class-defn class-defns*)])
           (let ([body (Letrec body)])
             `(,input-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))

