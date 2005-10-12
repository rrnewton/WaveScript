;;; Pass 22: normalize-context
;;; January 2001
;===============================================================================

;;; An expression can appear in one of three contexts:
;;;
;;;   effect, where the resulting value is not needed, e.g., all but
;;;     the last subexpression of a begin;
;;;
;;;   predicate, where the expression's boolean value determines
;;;     control flow, e.g., the test part of an if; and
;;;
;;;   value, where the complete value is needed, e.g., the right-hand
;;;     side of a letrec.
;;;
;;; Scheme allows any expression to appear in any of these contexts.
;;; While this is a simple model for the user, it makes more work for
;;; the compiler.  For example, the compiler must handle calls to < in
;;; both predicate and value contexts.  When used in a predicate context,
;;; < directs the flow of control, but when used in a value context, <
;;; returns a value, either #t or #f.
;;;
;;; This pass limits the contexts in which many expressions and primitive
;;; calls, like calls to <, can occur, usually by translating them into
;;; equivalent code that does the same operation in a different way.
;;; For example, it replaces calls to < in value context with an if
;;; expression which evaluates explicitly to either #t or #f depending
;;; on the result of the < call.

;;; The input language is the same as the output language of Pass 15.

;;; The output language differs in that limits the contexts in which
;;; certain expressions can appear.

;;; <Input> ::= (<language-name> <Program>)
;;; <Program> ::= (program (<var>*) (<pkg>*) <class-def>* <Letrec>)
;;; <Letrec>  ::= (letrec ((<var> <ClassDef>)*) <Value>)
;;; <ClassDef> ::= (class-def (<freevar> ...) <Lambda>)
;;; <Lambda>  ::= (lambda <Formalexp> <Value>)
;;; <Formalexp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)
;;; <Value>   ::= (quote <imm>)
;;;             | (class <var>)
;;;             | <var>
;;;             | (if <Pred> <Value> <Value>)
;;;             | (begin <Effect>* <Value>)
;;;             | (let ((<var> <Value>)*) <Value>)
;;;             | (<value primitive> <Value>*)
;;;             | (<Value> <Value>*)
;;;             | (toplvl-varref <var>)
;;; <Pred>    ::= (quote <boolean>)
;;;             | (if <Pred> <Pred> <Pred>)
;;;             | (begin <Effect>* <Pred>)
;;;             | (let ((<var> <Value>)*) <Pred>)
;;;             | (<predicate primitive> <Value>*)
;;; <Effect>  ::= (nop)
;;;             | (if <Pred> <Effect> <Effect>)
;;;             | (begin <Effect>* <Effect>)
;;;             | (let ((<var> <Value>)*) <Effect>)
;;;             | (<effect primitive> <Value>*)
;;;             | (<Value> <Value>*)
;;;             | (toplvl-varassign! <var> <Exp>)

;;;
;;; The new syntax (nop) replaces (void) in effect contexts.

;;; This pass also performs a couple of simple optimizations:
;;;
;;;   * (if (quote imm) e1 e2) => e2 if imm is #f otherwise e2
;;;
;;;   * (if (not e1) e2 e3) => (if e1 e3 e2)
;;;
;;;   * constants, variable references, class references, and
;;;     non-effect primitive calls are dropped in effect contexts
;;;
;;; The first two are done primarily to make the code we generate
;;; cleaner; the latter is done because it's easier to drop them
;;; than any other mechanism for taking them out of effect contexts.

;;; The implementation uses effect-primitive?, predicate-primitive?,
;;; and value-primitive? from helpers.ss.

(define normalize-context
  (let ()
    (define expand-not
      (lambda (expr)
        `(if ,expr '#f '#t)))
    (define predicafy
      (lambda (expr)
        (expand-not `(eq? ,expr '#f))))
    (define unpredicafy
      (lambda (expr)
        `(if ,expr '#t '#f)))
    (define optimize-if
      (lambda (test conseq altern)
        (match test
          [(if ,test '#f '#t) (optimize-if test altern conseq)]
          [(quote ,imm) (if imm conseq altern)]
          [,test `(if ,test ,conseq ,altern)])))
    (define postprocess-begin
      (lambda (expr* expr)
        (let ([expr* (remove '(nop) expr*)])
          (if expr
              (make-begin `(,@expr* ,expr))
              (if (null? expr*)
                  '(nop)
                  (make-begin expr*))))))
    (define Value
      (lambda (expr)
        (match expr
          [(quote ,imm) `(quote ,imm)]
          [,var (guard (symbol? var)) var]
          [(toplvl-varref ,var) `(toplvl-varref ,var)]
          [(toplvl-varassign! ,var ,[rhs])
           `(begin (toplvl-varassign! ,var ,rhs) (void))]
          [(class ,lab) `(class ,lab)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           (optimize-if test conseq altern)]
          [(begin ,[Effect -> expr*] ... ,[expr])
           (postprocess-begin expr* expr)]
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
           `(begin (set-field! ,field ,rhs) (nop))]
          [(invoke-static-method ,class ,method-name ,type-sig (,[arg*] ...))
           `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))]
          [(not ,[Pred -> rand]) (expand-not rand)]
          [(,prim ,[rand*] ...)
           (guard (predicate-primitive? prim))
           (unpredicafy `(,prim ,rand* ...))]
          [(,prim ,[rand*] ...)
           (guard (value-primitive? prim))
           `(,prim ,rand* ...)]
          [(,prim ,[rand*] ...)
           (guard (effect-primitive? prim))
           `(begin (,prim ,rand* ...) (void))]
          [(direct-call (class ,f) ,[obj] ,[rand*] ...)
           `(direct-call (class ,f) ,obj ,rand* ...)]
          [(anon-call ,overflowed? ,[rator] ,[rand*] ...)
           `(anon-call ,overflowed? ,rator ,rand* ...)]
          [,expr (error 'normalize-context
                        "invalid Value expression: ~s" expr)])))
    (define Pred
      (lambda (expr)
        (match expr
          [(quote ,imm) `(quote ,(if imm #t #f))]
          [,var (guard (symbol? var)) (predicafy var)]
          [(class ,lab) '(quote #t)]
          [(toplvl-varref ,var)
           (predicafy `(toplvl-varref ,var))]
          [(toplvl-varassign! ,var ,[Value -> rhs])
           `(begin (toplvl-varassign! ,var ,rhs) '#t)]
          [(if ,[test] ,[conseq] ,[altern])
           (optimize-if test conseq altern)]
          [(begin ,[Effect -> expr*] ... ,[expr])
           (postprocess-begin expr* expr)]
          [(let ([,lhs* ,[Value -> rhs*]] ...) ,[body])
           `(let ([,lhs* ,rhs*] ...) ,body)]
          [(let-class (,class-defn* ...) ,[body])
           (let ([class-defn* (map process-class-defn class-defn*)])
             `(let-class ,class-defn* ,body))]
          [(new ,class-name ,[Effect -> args*] ...)
           (postprocess-begin args* '#t)]
          [(open-instance ,obj ,class-name ,[body])
           `(open-instance ,obj ,class-name ,body)]
          [(object-reference ,obj ,class ,field)
           (predicafy `(object-reference ,obj ,class ,field))]
          [(invoke-method ,obj ,class ,method (,[Value -> arg*] ...))
           (predicafy `(invoke-method ,obj ,class ,method (,arg* ...)))]
          [(foreign-call ,name ,type-sig (,[Value -> arg*] ...))
           (predicafy `(foreign-call ,name ,type-sig (,arg* ...)))]
          [(static-ref ,class ,field)
           (predicafy `(static-ref ,class ,field))]
          [(this-ref ,class ,field)
           (predicafy `(this-ref ,class ,field))]
          [(set-field! ,field ,[Value -> rhs])
           (postprocess-begin `((set-field! ,field ,rhs)) '#t)]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[Value -> arg*] ...))
           (predicafy
             `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...)))]
          [(not ,[rand]) (expand-not rand)]
          [(,prim ,[Value -> rand*] ...)
           (guard (predicate-primitive? prim))
           `(,prim ,rand* ...)]
          [(,prim ,[Value -> rand*] ...)
           (guard (value-primitive? prim))
           (predicafy `(,prim ,rand* ...))]
          [(,prim ,[Value -> rand*] ...)
           (guard (effect-primitive? prim))
           `(begin (,prim ,rand* ...) '#t)]
          [(direct-call (class ,f) ,[Value -> obj] ,[Value -> rand*] ...)
           (predicafy `(direct-call (class ,f) ,obj ,rand* ...))]
          [(anon-call ,overflowed? ,[Value -> rator] ,[Value -> rand*] ...)
           (predicafy `(anon-call ,overflowed? ,rator ,rand* ...))]
          [,expr (error 'normalize-context
                        "invalid Pred expression: ~s" expr)])))
    (define Effect
      (lambda (expr)
        (match expr
          [(quote ,imm) '(nop)]
          [,var (guard (symbol? var)) '(nop)]
          [(class ,lab) '(nop)]
          ;;; Here we get a variable reference in effect context, and
          ;;; we transform it into (set! <var> <var>):
          [(toplvl-varref ,var)  ;; This is cheesy, but what else to do?
           `(toplvl-varassign! ,var (toplvl-varref ,var))]
          
          [(toplvl-varassign! ,var ,[Value -> rhs])
           `(toplvl-varassign! ,var ,rhs)]
          [(if ,[Pred -> test] ,[conseq] ,[altern])
           (optimize-if test conseq altern)]
          [(begin ,[expr*] ...) (postprocess-begin expr* #f)]
          [(let ([,lhs* ,[Value -> rhs*]] ...) ,[body])
           `(let ([,lhs* ,rhs*] ...) ,body)]
          [(let-class (,class-defn* ...) ,[body])
           (let ([class-defn* (map process-class-defn class-defn*)])
             `(let-class ,class-defn* ,body))]
          
          [(new ,class-name ,[args*] ...)
           (postprocess-begin args* #f)]
          [(open-instance ,obj ,class-name ,[body])
           `(open-instance ,obj ,class-name ,body)]
          [(object-reference ,obj ,class ,field)
           '(nop)]
          [(invoke-method ,obj ,class ,method (,[Value -> arg*] ...))
           `(invoke-method ,obj ,class ,method (,arg* ...))]
          [(foreign-call ,name ,type-sig (,[Value -> arg*] ...))
           `(foreign-call ,name ,type-sig (,arg* ...))]
          [(static-ref ,class ,field)
           '(nop)]
          [(this-ref ,class ,field)
           '(nop)]
          [(set-field! ,field ,[Value -> rhs])
           `(set-field! ,field ,rhs)]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[Value -> arg*] ...))
           `(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))]
          
          [(not ,[rand]) rand]
          [(,prim ,[rand*] ...)
           (guard (or (predicate-primitive? prim) (value-primitive? prim)))
           (postprocess-begin rand* #f)]
          [(,prim ,[Value -> rand*] ...)
           (guard (effect-primitive? prim))
           `(,prim ,rand* ...)]
          [(direct-call (class ,f) ,[Value -> obj] ,[Value -> rand*] ...)
           `(direct-call (class ,f) ,obj ,rand* ...)]
          [(anon-call ,overflowed? ,[Value -> rator] ,[Value -> rand*] ...)
           `(anon-call ,overflowed? ,rator ,rand* ...)]
          [,expr (error 'normalize-context
                        "invalid Effect expression: ~s" expr)])))
    (define Lambda
      (lambda (expr)
        (match expr
          [(lambda ,formalexp ,[Value -> body])
           `(lambda ,formalexp ,body)])))
    (define ClassDef
      (lambda (expr)
        (match expr
          [(class-def (,free* ...) ,[Lambda -> body])
           `(class-def (,free* ...) ,body)])))
    (define Letrec
      (lambda (expr)
        (match expr
          [(letrec ([,lhs* ,[ClassDef -> rhs*]] ...) ,[Value -> body])
           `(letrec ([,lhs* ,rhs*] ...) ,body)])))
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
          [,unmatched (error 'normalize-context
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
             `(normalize-context-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))