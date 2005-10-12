;;; Pass 6: remove-complex-constant
;;; January 2001

;;; This pass rewrites the code so that complex constants, such as
;;; '(1 2 3) and '#(1 (2 3) 4), are created using explicit list- and
;;; vector-creation operators.

;;; RRN[2002.06.08]-- now this pass also rewrites bignums as primcalls
;;; to the internal primitive "make-bignum".  Hence, subsequent passes
;;; had better use extended-scheme-primitive? to make sure that they'll
;;; catch internal primitives.

;;; [RRN 2002.06.11]- I just made this expand out -'s into +'s if
;;; all their arguments but the first are constant (it's easy to do
;;; it here, because we can just flip the signs on the constants).

;;; remove-complex-constant arranges for complex constant values to
;;; be built only once, when the program begins, by wrapping each
;;; program that contains complex constants in a let expression
;;; binding temporary variables to the expressions that create the
;;; complex constants and rewriting the original quote expressions
;;; as references to these temporaries.  For example,
;;;
;;; (let ((f (lambda () '(1 2 3))))
;;;   (cons (f) (f)))
;;;
;;; becomes
;;;
;;; (let ((tmp (cons '1 (cons '2 (cons '3 '())))))
;;;   (let ((f (lambda () tmp)))
;;;     (cons (f) (f))))

;;; The input language is the same as the output language of Pass 5.

;;; Output from this pass is in the same language, except that
;;; quote expressions now contain only immediates.

;;; <Prog> ::= (program (<pkg-ref>*) <class-defs>* <Exp>)

;;; <Exp>  ::= (quote <imm>)
;;;          | <var>
;;;          | (set! <var> <Exp>)
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (begin <Exp> <Exp>*)
;;;          | (lambda <FormalExp> <Exp>)
;;;          | (case-lambda [<Formalexp> <Exp>]*)
;;;          | (let (<Decl>*) <Exp>)
;;;          | (letrec (<Decl>*) <Exp>)
;;;          | (<primitive> <Exp>*)
;;;          | (<Exp> <Exp>*)
;;;          | (toplvl-varref <var>)
;;;          | (toplvl-varassign! <var> <Exp>)
;;; <Decl> ::= (<var> <Exp>)
;;; <FormalExp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)


;;; The implementation requires immediate?, iota, scheme-primitive?,
;;; and unique-name from helpers.ss.

;;; The implementation uses multiple return values to return both
;;; the rewritten expression and a list of bindings for temporary
;;; variables to constant-creation expressions.

(define remove-complex-constant
  (let ()
    (define datum->code
      (let* ([pow32 (expt 2 32)]
             [pow31 (expt 2 31)]
             [convert-to-signed
               (lambda (n)
                 (if (< n pow31) n
                     (- (- pow32 n))))])
        (lambda (x)
          (cond
            [(and (integer? x) (not (fx-integer? x)));; Bignums
             ;; This breaks up bignums into a number of fixnum chunks:
             ;; BUT, it pretends fixnums are unsigned:
             ;; This could be made more efficient if it could somehow
             ;; pass in a ulong array of bigits directly.
             ;; BUT, this will only be used for complex constants,
             ;; so it should only run once per program execution, and
             ;; efficiency should not be *that* important:
             (let ([neg (if (< x 0) ''#t ''#f)]
                   [num (if (< x 0) (- x) x)])
               `(make-bignum
                  ,neg
                  (vector
                    ,@(let loop ([n num])
                        (if (< n pow32)
                            `((quote ,(convert-to-signed n)))
                            (cons `(quote ,(convert-to-signed
                                             (modulo n pow32)))
                                  (loop (quotient n pow32))))))))]
            [(pair? x)
             `(cons ,(datum->code (car x))
                    ,(datum->code (cdr x)))]
            [(vector? x)
             (let ([tmp (unique-name 'tmp)] [n (vector-length x)])
               `(let ([,tmp (make-vector ',n)])
                  ,(make-begin
                     `(,@(map (lambda (x i)
                                `(vector-set! ,tmp ',i ,(datum->code x)))
                              (vector->list x)
                              (iota n))
                         ,tmp))))]
            [else `(quote ,x)]))))
    (define negate-datum
      (lambda (datum)
        (cond
          [(number? datum) (Expr `(quote ,(- datum)))]
          [else (error 'remove-complex-constant.negate-datum
                       "cannot negate non-numeric datum: ~s" datum)])))
    (define Expr
      (lambda (expr)
        (match expr
          [(quote ,datum)
           (guard (immediate? datum))
           (values `(quote ,datum) '())]
          ;; Symbols are not *technically* immediate (they are boxed);
          ;; however, neither are they complex.
          [(quote ,datum)
           (guard (symbol? datum))
           (values `(quote ,datum) '())]
          [(quote ,datum)
           (let ([tmp (unique-name 'tmp)])
             (values tmp `((,tmp ,(datum->code datum)))))]
          [,var (guard (symbol? var))
            (values var '())]
          [(toplvl-varref ,var)
           (values `(toplvl-varref ,var)
                   '())]
          [(toplvl-varassign! ,var ,[rhs rhsbinds])
           (values `(toplvl-varassign! ,var ,rhs)
                   rhsbinds)]
          [(set! ,var ,[rhs rhs-b*])
           (values `(set! ,var ,rhs) rhs-b*)]
          [(if ,[test test-b*] ,[conseq conseq-b*] ,[altern altern-b*])
           (values `(if ,test ,conseq ,altern)
                   (append test-b* conseq-b* altern-b*))]
          [(begin ,[expr* expr-b**] ...)
           (values `(begin ,expr* ...) (apply append expr-b**))]
          
          [(lambda ,formals ,[body body-b*])
           (values `(lambda ,formals ,body) body-b*)]
          [(case-lambda [,formals* ,[body* body-b**]] ...)
           (values `(case-lambda [,formals* ,body*] ...)
                   (apply append body-b**))]
          [(let ([,lhs* ,[rhs* rhs-b**]] ...) ,[body body-b*])
           (values `(let ([,lhs* ,rhs*] ...) ,body)
                   (append (apply append rhs-b**) body-b*))]
          [(letrec ([,lhs* ,[rhs* rhs-b**]] ...) ,[body body-b*])
           (values `(letrec ([,lhs* ,rhs*] ...) ,body)
                   (append (apply append rhs-b**) body-b*))]
          [(let-class (,class-defn* ...) ,[body body-b*])
           (let ([class-defn* (map process-class-defnclass-defn*)])
             (values `(let-class ,class-defn* ,body) body-b*))]
          [(new ,class-name ,[args* args-b**] ...)
           (values `(new ,class-name ,args* ...)
                   (apply append args-b**))]
          [(open-instance ,obj ,class-name ,[body body-b*])
           (values `(open-instance ,obj ,class-name ,body)
                   body-b*)]
          [(object-reference ,obj ,class ,field)
           (values `(object-reference ,obj ,class ,field) '())]
          [(invoke-method ,obj ,class ,method (,[arg* arg-b**] ...))
           (values `(invoke-method ,obj ,class ,method (,arg* ...))
                   (apply append arg-b**))]
          [(foreign-call ,name ,type-sig (,[arg* arg-b**] ...))
           (values `(foreign-call ,name ,type-sig (,arg* ...))
                   (apply append arg-b**))]
          [(static-ref ,class ,field)
           (values `(static-ref ,class ,field) '())]
          [(this-ref ,class ,field)
           (values `(this-ref ,class ,field) '())]
          [(invoke-static-method ,class ,method-name ,type-sig
                                 (,[arg* arg-b**] ...))
           (values `(invoke-static-method ,class ,method-name
                                          ,type-sig (,arg* ...))
                   (apply append arg-b**))]
          ;; Expand certain minuses:
          ;; This is basically an optimization for subtracting from variables:
          [(- (quote ,[negate-datum -> datum datum-b*]))
           (values datum datum-b*)]
          [(- ,[rand rand-b*]
              (quote ,[negate-datum -> datum* datum-b**]) ...)
           (if (null? datum*) ;; This is the one argument -
               (values `(- ,rand) rand-b*)
               (values
                 `(+ ,rand  ,datum* ...)
                 (append rand-b* (apply append datum-b**))))]
          [(,prim ,[rand* rand-b**] ...)
           (guard (scheme-primitive? prim))
           (values `(,prim ,rand* ...) (apply append rand-b**))]
          
          [(,[rator rator-b*] ,[rand* rand-b**] ...)
           (values `(,rator ,rand* ...)
                   (append rator-b* (apply append rand-b**)))]
          [,unmatched
            (error 'remove-complex-constant "invalid expression: ~s"
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
          [,unmatched (error 'remove-complex-constant
                             "invalid class defn ~s"
                             cdef)])))
    (define process-method
      (lambda (meth)
        (match meth
          [(lambda ,args ,body)
           (mvlet ([(body body-b*) (Expr body)])
             (if (null? body-b*)
                 `(lambda ,args ,body)
                 `(lambda ,args (let ,body-b* ,body))))])))
    (lambda (expr)
      (match expr
        [(,input-language (quote (program ,pkg* ,class-defns* ... ,body)))
         (let ([class-defns* (map (lambda (x)
                                    (process-class-defn x))
                                  class-defns*)])
           (mvlet ([(body body-b*) (Expr body)])
             (if (null? body-b*)
                 `(remove-complex-constant-language
                    '(program ,pkg* ,class-defns* ... ,body))
                 `(remove-complex-constant-language
                    '(program ,pkg* ,class-defns* ...
                              (let ,body-b* ,body))))))]))))