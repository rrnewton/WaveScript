;;; Pass 10: convert-case-lambda
;;; Ryan Newton

;;; This pass converts case-lambdas into a canonical form that we can easily
;;; transform to a .NET class.  Hereafter case-lambdas will be structured
;;; as follows:
;;;  (case-lambda
;;;    ([<Formalexp> <Body>]* )
;;;    [(<a> . <b>) <n>]* )
;;; The first argument is an ordered list of clauses, clauses which actually
;;; have the potential to be called.  The following arguments each bind a
;;; numeric range with an index into the clause-list.  These numeric ranges
;;; code for numbers of incoming parameters.

;;; Output language:

;;; <Input> ::= (<language-name> <Program>)
;;; <Program> ::= (program (<var>*) (<pkg>*) <class-def>* <Exp>)

;;; <Exp>  ::= (quote <imm>)
;;;          | <var>
;;;          | (set! <var> <Exp>)
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (begin <Exp> <Exp>*)
;;;          | <Lambda>
;;;          | (let (<Decl>*) <Body>)
;;;          | (letrec (<RecDecl>*) <Exp>)
;;;          | (<primitive> <Exp>*)
;;;          | (<Exp> <Exp>*)
;;;          | (toplvl-varref <var>)
;;;          | (toplvl-varassign! <var> <Exp>)
;;; <Decl> ::= (<var> <Exp>)
;;; <RecDecl> ::= (<var> <Lambda>)
;;; <Lambda> ::= (lambda <Formalexp> <Body>)
;;;            | (case-lambda
;;;                ([<Formalexp> <Body>]* )
;;;                [(<a> . <b>) <n>]* )
;;; <Formalexp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)
;;; <Body> ::= (settable (<var>*) <Exp>)

(module indexed-case-lambdas
        (lambda? case-lambda?
                 case-lambda->lambda)
        
(define lambda?
  (lambda (expr)
    (or (case-lambda? expr)
        (match expr
          [(lambda ,formalexp ,body)
           (guard (formalexp? formalexp)) #t]
          [,_ #f]))))
  
(define case-lambda?
  (lambda (expr)
    (match expr
      [(case-lambda
         ([,formalexp* ,body*] ...)
         [(,a . ,b) ,n] ...)
       (guard (andmap formalexp? formalexp*))
       #t]
      [,_ #f])))
  
(define case-lambda->lambda
  (lambda (icl n)
    (let loop ([index-clauses (cddr icl)])
      (cond
        [(null? index-clauses)
         (error 'case-lambda->lambda
                "incorrect number of arguments to procedure: ~s"
                n)]
        [(<= (caaar index-clauses) n (cdaar index-clauses))
         `(lambda ,@(list-ref (cadr icl)
                              (cadar index-clauses)))]
        [else (loop (cdr index-clauses))]))))
) ; End module
(import indexed-case-lambdas)

;;; The implementation requires make-begin, extended-scheme-primitive?, and
;;; unique-name from helpers.ss.
(define convert-case-lambda
  (let ()
    ;--------------------------------------
    (define formals->range
      (lambda (pat)
        (match pat
          [,pat (guard (symbol? pat)) '(0 . +inf.0)]
          [(,v ...) (cons (length v) (length v))]
          [(,v ... . ,l) (cons (length v) +inf.0)])))
    (define process-case-lambda
      (lambda (origclauses)
        ;; Replace bodies with indices:
        (let (#;[clauses (map cons
                              (map car origclauses)
                              (iota (length origclauses)))])
          (let ([rangedclauses
                  (let ([patterns (map car origclauses)])
                    (let loop ([alist '()]
                               [indices (iota (length origclauses))]
                               [ranges (map formals->range patterns)])
                      (if (null? indices)
                          alist
                          (loop
                            (merge-range alist (car ranges) (car indices))
                            (cdr indices)
                            (cdr ranges)))))])
            
            (let* ([used (sort < (list->set (map cadr rangedclauses)))]
                   [substitutions (map cons used (iota (length used)))])
              `(case-lambda
                 ,(map (lambda (i) (list-ref origclauses i)) used)
                 ,@(substitute-indices rangedclauses substitutions))
              )))))
    (define substitute-indices
      (lambda (clauses indexalist)
        (if (null? clauses)
            '()
            (cons (list (caar clauses)
                        (cdr (assq (cadar clauses) indexalist)))
                  (substitute-indices (cdr clauses) indexalist)))))
    (define merge-range
      (lambda (origalist range rhs)
        (let ([start (car range)]
              [end (cdr range)])
          (let loop ([alist origalist])
            (if (null? alist) (list (list range rhs))
                (let ([a (caaar alist)] [b (cdaar alist)])
                  ;; We're examing this one range-pair in the alist;
                  ;; Their are six ways that this range and our new
                  ;; range can interact:
                  ;;   1) start end a b
                  ;;   2) a b start end
                  ;;   3) a start end b
                  ;;   4) start a end b
                  ;;   5) a start b end
                  ;;   6) start a b end
                  (cond
                    [(< end a) (cons (list range rhs) alist)]        ; (1
                    [(< b start) (cons (car alist)                   ; (2
                                       (loop (cdr alist)))]
                    [(<= a start end b) alist]                       ; (3
                    [(and (< start a) (<= end b))                    ; (4
                     (cons (list (cons start (sub1 a)) rhs) alist)]
                    [(<= a start b end)                              ; (5
                     (cons (car alist)
                           (merge-range
                             (cdr alist)
                             (cons (add1 b) end)
                             rhs))]
                    [(and (< start a) (< b end))                     ; (6
                     (cons (list (cons start (sub1 a)) rhs)
                           (cons (car alist)
                                 (merge-range
                                   (cdr alist)
                                   (cons (add1 b) end)
                                   rhs)))]
                    [else
                      (error merge-range
                             "This must be incorrect input: ~s ~s ~s"
                             alist range rhs)])))))))
    ;--------------------------------------
    (define Expr
      (lambda (expr)
        (match expr
          [(quote ,imm) `(quote ,imm)]
          [,var (guard (symbol? var)) var]
          [(toplvl-varref ,var) `(toplvl-varref ,var)]
          [(toplvl-varassign! ,var ,[rhs])
           `(toplvl-varassign! ,var ,rhs)]
          [(set! ,var ,[rhs])
           `(set! ,var ,rhs)]
          [(if ,[test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[expr*] ...) `(begin ,expr* ...)]
          [(lambda ,formalexp (settable (,set* ...) ,[body]))
           `(lambda ,formalexp (settable (,set* ...) ,body))]
          [(case-lambda
             [,formalexp* (settable (,set** ...) ,[body*])] ...)
           (process-case-lambda
             `([,formalexp* (settable (,set** ...) ,body*)] ...))]
          [(let ([,lhs* ,[rhs*]] ...) (settable (,set* ...) ,[body]))
           `(let ([,lhs* ,rhs*] ...) (settable (,set* ...) ,body))]
          [(letrec ([,lhs* ,[rhs*]] ...)
             (settable (,set* ...) ,[body]))
           `(letrec ([,lhs* ,rhs*] ...)
              (settable (,set* ...) ,body))]
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
          [(,prim ,[rand*] ...)
           (guard (extended-scheme-primitive? prim))
           `(,prim ,rand* ...)]
          [(,[rator] ,[rand*] ...)
           `(,rator ,rand* ...)]
          [,unmatched
            (error 'remove-impure-letrec "invalid expression: ~s"
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
          [,unmatched (error 'remove-impure-letrec
                             "invalid class defn ~s"
                             cdef)])))
    (define process-method
      (lambda (meth)
        (match meth
          [(lambda ,args (settable ,set ,body))
           (let ([body (Expr body)])
             `(lambda ,args (settable ,set ,body)))])))
    
    (lambda (prog)
      (match prog
        [(,input-language
           (quote (program ,sym* ,pkg* ,class-defns* ... ,body)))
         (let ([class-defns* (map process-class-defn class-defns*)])
           (let ([body (Expr body)])
             `(convert-case-lambda-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))