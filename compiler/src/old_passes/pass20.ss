;;; Pass 20: Reduce Primitives.1
;===============================================================================
;;; This prepares the input for the next pass, by means of splitting the
;;; operands for certain variable-arity primitives.
;;; For example (+ 1 2 3) becomes (+ (+ 1 2) 3)

;;; The output language is the same as the input language.  Only now many
;;; primitives will be found only with fixed rather than variable arguments.

;;; [2002.06.11]RRN - just made the minus primitive reduce to plus (except
;;; one argument minus's (negation) still occur).

(define reduce-primitives.1
  (let ()
    ;; Might want to replace this with one that makes balanced trees:
    ;; Right now this associates left, which is necessary for minus(-):
    ;; Further, this is unnecessarily generalized to make trees for
    ;; operators of more than 2 arguments; right now (2002.07.30) only
    ;; binary operators are used.
    (define make-operand-tree
      (lambda (prim origrand* n)
        (let loop ([rand* origrand*]
                   [len (length origrand*)])
          (cond
            [(= len n) `(,prim ,@rand*)]
            [(> len n)
             `(,prim ,(loop (list-head rand* (- len (sub1 n)))
                            (- len (sub1 n)))
                ,@(list-tail rand* (- len (sub1 n))))]
            [(< len n)
             (error 'make-operand-tree
                    "primitive ~s: argument list ~s cannot be split~a~s"
                    prim origrand* " up into segments of length " n)]))))
    ;;-------------------------------------------------
    ;; This is for handling the repetitive +,-,fx+,... type operators.
    (define-syntax accumulating-op
      (syntax-rules ()
        [(_ prim rands targetnum special-case* ...)
         (accumulating-op-helper
           prim rands targetnum
           (list (lambda () special-case*) ...))]))
    ;; The first cases (0 args, 1 arg, etc) are handled
    ;; by some number of respective specialcasethnks:
    (define accumulating-op-helper
      (lambda (prim rands targetnum specialcasethnk*)
        (let loop ([rnd* rands] [spclcase* specialcasethnk*])
          (cond
            [(null? spclcase*)
             (make-operand-tree prim rands targetnum)]
            [(null? rnd*) ((car spclcase*))]
            [else (loop (cdr rnd*) (cdr spclcase*))]))))
    ;;-------------------------------------------------
    ;; And this is for the transitive =,<,<=,>,>= operators
    ;; All of these are binary, and we generalize for ternary
    ;; or quaternary operators (as we did in split operands).
    (define transitive-predicate-op
      (lambda (prim rands)
        (cond
          [(null? rands)
           (error 'transitive-predicate-op
                  "primitive ~s can't have 0 arguments!  ~a"
                  prim
                  "Actually, we shouldn't even get to this error!")]
          [(null? (cdr rands)) `(begin ,(car rands) '#t)]
          [(null? (cddr rands)) `(,prim ,@rands)]
          ;; Here we do a tiny bit of optimization.  We don't
          ;; bother making temporary copies for the first and
          ;; last rands.  They are only compared once each.
          [else
            (let ([vars (map unique-name
                             (make-list (- (length rands) 2)
                                        'randtmp))])
              `(let ,(map list vars (cdr (rdc rands)))
                 
                 ,(let loop ([cur (car rands)]
                             [rst (append vars (list (rac rands)))])
                    (if (null? rst)
                        ''#t
                        `(if (,prim ,cur ,(car rst))
                             ,(loop (car rst) (cdr rst))
                             '#f)))))])))
    ;;-------------------------------------------------
    (define transitive-predicate-op?
      (lambda (x)
        (memq x '(= < <= > >=
                    fx= fx< fx<= fx> fx>=
                    fl= fl< fl<= fl> fl>=
                    ))))
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
          
          ;; All these cases split up the arguments to certain primitives:
          [(,tpo ,[rand*] ...)
           (guard (transitive-predicate-op? tpo))
           (transitive-predicate-op tpo rand*)]
          
          ;; +'s only have 2 arguments after this pass; 1 argument +'s
          ;; cannot dissappear, as this would change error behaviour.
          ;; (Same goes for many other primitives.)
          [(+ ,[rand*] ...)
           (accumulating-op '+ rand* 2
                            '(quote 0)
                            `(+ '0 ,(car rand*)))]
          [(* ,[rand*] ...)
           (accumulating-op '* rand* 2
                            '(quote 1)
                            `(* '1 ,(car rand*)))]
          ;; After this pass, '-' only takes one argument.
          ;; (as you can see this case is  a bit special
          ;; and doesn't use the helper function)
          [(- ,[rand*] ...)
           (cond
             [(null? rand*)
              (error 'reduce-primitives.1
                     "primitive - cannot take 0 arguments")]
             [(null? (cdr rand*))
              `(- ,(car rand*))]
             [(null? (cddr rand*))
              `(+ ,(car rand*) (- ,(cadr rand*)))]
             [else `(+ ,(car rand*)
                       (- ,(make-operand-tree
                             '+ (cdr rand*) 2)))])]
          [(fx+ ,[rand*] ...)
           (accumulating-op 'fx+ rand* 2
                            '(quote 0) `(fx+ '0 ,(car rand*)))]
          ;; Does the MSIL primitive 'sub' have the same efficiency as 'add'?
          [(fx- ,[rand*] ...)
           (accumulating-op
             'fx- rand* 2
             (error 'reduce-primitives.1
                    "primitive fx- cannot take 0 arguments")
             `(fx- '0 ,(car rand*)))]
          [(fx* ,[rand*] ...)
           (accumulating-op 'fx* rand* 2
                            '(quote 1) `(fx* '1 ,(car rand*)))]
          [(fx/ ,[rand*] ...)
           (accumulating-op
             'fx/ rand* 2
             (error 'reduce-primitives.1
                    "primitive fx/ cannot take 0 arguments")
             `(fx/ '1 ,(car rand*)))]
          
          [(fl+ ,[rand*] ...)
           (accumulating-op 'fl+ rand* 2
                            '(quote 0.0) `(fl+ '0.0 ,(car rand*)))]
          [(fl- ,[rand*] ...)
           (accumulating-op
             'fl- rand* 2
             (error 'reduce-primitives.1
                    "primitive fl- cannot take 0 arguments")
             `(fl- '0.0 ,(car rand*)))]
          [(fl* ,[rand*] ...)
           (accumulating-op 'fl* rand* 2
                            '(quote 1.0) `(fl* '1.0 ,(car rand*)))]
          [(fl/ ,[rand*] ...)
           (accumulating-op
             'fl/ rand* 2
             (error 'reduce-primitives.1
                    "primitive fl/ cannot take 0 arguments")
             `(fl/ '1.0 ,(car rand*)))]
          
          [(string-append ,[rand*] ...)
           (accumulating-op
             'string-append rand* 2
             '(quote "")
             `(string-append ,(car rand*) (quote "")))]
          
          ;; Other primitives fall through here:
          [(,prim ,[rand*] ...)
           (guard (extended-scheme-primitive? prim))
           `(,prim ,rand* ...)]
          ;This is a direct, call eh?  How should I get rid of it?
          [(direct-call (class ,f) ,[obj] ,[rand*] ...)
           `(direct-call (class ,f) ,obj ,rand* ...)]
          [(anon-call ,overflowed? ,[rator] ,[rand*] ...)
           `(anon-call ,overflowed? ,rator ,@rand*)]
          [,expr (error 'reduce-primitives.1
                        "invalid expression: ~s" expr)])))
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
          [,unmatched (error 'reduce-primitives.1
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

