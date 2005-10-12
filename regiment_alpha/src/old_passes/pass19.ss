;;; Pass 19: convert-excessive-args
;;; September 2001
;===============================================================================
;;;   This pass converts procedure calls with more than SETTINGS:ARGS_HARDCODED
;;; operands into ones with only SETTINGS:ARGS_HARDCODED operands.  It does
;;; this by putting excess into a list paramater.
;;;   Further, this pass handles calls to procedures with variable operand
;;; counts by, in the case of direct calls, explicitely placing the actual
;;; parameters in a list.  Anonymous calls must necessarily be handled the
;;; same way as any other anonymous calls (hence the name).

;;; <Input> ::= (<language-name> <Program>)
;;; <Program> ::= (program (<var>*) (<pkg>*) <class-def>* <Letrec>)
;;; <Letrec>   ::= (letrec ((<var> <ClassDef>)*) <Exp>)
;;; <ClassDef> ::= (class-def (<freevar> ...) <Lambda>)
;;; <Lambda>   ::= (lambda <Formalexp> <Exp>)
;;; <Formalexp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)
;;; <Exp>       ::= (quote <imm>)
;;;               | (class <var>)
;;;               | <var>
;;;               | (if <Exp> <Exp> <Exp>)
;;;               | (begin <Exp> <Exp>*)
;;;               | (let ((<var> <Exp>)*) <Exp>)
;;;               | (<primitive> <Exp>*)
;;;               | (<Exp> <Exp>^0-6)
;;;               | (toplvl-varref <var>)
;;;               | (toplvl-varassign! <var> <Exp>)

;;; The implementation uses iota and extended-scheme-primitive?
;;; from helpers.ss.

(define convert-excessive-args
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
          ;================================================
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
          ;================================================
          [(,prim ,[rand*] ...)
           (guard (extended-scheme-primitive? prim))
           `(,prim ,rand* ...)]
          
          ;This is a direct, call eh?  How should I get rid of it?
          [(direct-call (class ,f) ,[obj] ,[rand*] ...)
           `(direct-call (class ,f) ,obj ,rand* ...)]
          
          [(anon-call ,[rator] ,[rand*] ...)
           (if (> (length rand*) SETTINGS:ARGS_HARDCODED)
               `(anon-call
                  overflowed
                  ,rator ,@(list-head rand* (sub1 SETTINGS:ARGS_HARDCODED))
                  (list ,@(list-tail rand* (sub1 SETTINGS:ARGS_HARDCODED))))
               `(anon-call complete ,rator ,@rand*))]
          
          [,expr (error 'convert-excessive-args "invalid expression: ~s" expr)])))
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
          [,unmatched (error 'convert-excessive-args
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
             `(convert-excessive-args-language
                '(program ,sym* ,pkg* ,class-defns* ... ,body))))]))))

#!eof

;;; <Program> ::= (let () <output-language-definition> <Exp>)
;;; <Exp>  ::= (quote <imm>)
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (begin <Exp> <Exp>*)
;;;          | (let ((<var> <Exp>)*) <Exp>)
;;;          | (letrec ((<var> <Lambda>)*) <Exp>)
;;;          | (<primitive> <Exp>*)
;;;          | (<Exp> <Exp>*)
;;; <Lambda> ::= (newlambda (<var>* <Exp>)

(define convert-excessive-args
  (let ()
    (define Expr
      (lambda (expr)
        (match expr
          [(quote ,imm) `(quote ,imm)]
          [,var (guard (symbol? var)) var]
          [(if ,[test] ,[conseq] ,[altern])
           `(if ,test ,conseq ,altern)]
          [(begin ,[expr*] ...) `(begin ,expr* ...)]
          [(let ([,lhs* ,[rhs*]] ...) ,[body])
           `(let ([,lhs* ,rhs*] ...) ,body)]
          [(letrec ([,lhs* (lambda (,formal** ...) ,[lbody*])] ...)
             ,[body])
           `(letrec ([,lhs* (newlambda (,formal** ...) ,lbody*)] ...)
              ,body)]
          [(,prim ,[rand*] ...)
           (guard (scheme-primitive? prim))
           `(,prim ,rand* ...)]
          [(,[rator] ,[rand*] ...)
           (if (> (length rand*)
                  SETTINGS:ARGS_HARDCODED)
               (let ([vec (unique-name 'argspill)])
                 `(let ([,vec (make-vector
                                ',(- (length rand*)
                                     SETTINGS:ARGS_HARDCODED))])
                    ,@(map (lambda (arg i)
                             `(vector-set! ,vec ',i ,arg))
                           (list-tail rand* SETTINGS:ARGS_HARDCODED)
                           (iota (- (length rand*)
                                    SETTINGS:ARGS_HARDCODED)))
                    (,rator ,@(list-head rand* SETTINGS:ARGS_HARDCODED)
                      ,vec)))
               `(,rator ,@rand*))]
          [,unmatched
            (disp "hmm?????")
            (error 'convert-excessive-args "invalid expression: ~s"
                   unmatched)])))
    
    (lambda (prog)
      (match prog
        [(let () ,input-language-definition ,expr)
         `(convert-excessive-args-language (quote ,(Expr expr)))]))))