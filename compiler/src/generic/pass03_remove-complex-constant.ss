;;; Pass 6: remove-complex-constant

;;; RRN[2004.04.24] This pass has been resurected.  The old
;;; functionality described below is largely outdated.

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

;;; <Pgm>  ::= (<language-name> (quote <Exp>))
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= 
;;;            (quote <imm>)
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (lambda <Formalexp> <Exp>)
;;;          | (letrec (<Decl>*) <Exp>)
;;;          | (<primitive> <Exp>*)
;;; <Formalexp> ::= (<var>*)


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
	   ;; Don't have bignums in the lang right now:
;	   [(and (integer? x) (not (fx-integer? x)));; Bignums
             ;; This breaks up bignums into a number of fixnum chunks:
             ;; BUT, it pretends fixnums are unsigned:
             ;; This could be made more efficient if it could somehow
             ;; pass in a ulong array of bigits directly.
             ;; BUT, this will only be used for complex constants,
             ;; so it should only run once per program execution, and
             ;; efficiency should not be *that* important:
;             (let ([neg (if (< x 0) ''#t ''#f)]
;                   [num (if (< x 0) (- x) x)])
;               `(make-bignum
;                  ,neg
;                  (vector
;                    ,@(let loop ([n num])
;                        (if (< n pow32)
;                            `((quote ,(convert-to-signed n)))
;                            (cons `(quote ,(convert-to-signed
;                                             (modulo n pow32)))
;                                  (loop (quotient n pow32))))))))]
            [(pair? x)
             `(cons ,(datum->code (car x))
                    ,(datum->code (cdr x)))]

	    ;; Don't have vectors either.
;             [(vector? x)
;              (let ([tmp (unique-name 'tmp)] [n (vector-length x)])
;                `(let ([,tmp (make-vector ',n)])
;                   ,(make-begin
;                      `(,@(map (lambda (x i)
;                                 `(vector-set! ,tmp ',i ,(datum->code x)))
;                               (vector->list x)
;                               (iota n))
;                          ,tmp))))]

            [else `(quote ,x)]))))

    (define negate-datum
      (lambda (datum)
        (cond
          [(number? datum) (process-expr `(quote ,(- datum)))]
          [else (error 'remove-complex-constant.negate-datum
                       "cannot negate non-numeric datum: ~s" datum)])))
    (define process-expr
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

          [(if ,[test test-b*] ,[conseq conseq-b*] ,[altern altern-b*])
           (values `(if ,test ,conseq ,altern)
                   (append test-b* conseq-b* altern-b*))]
          
          [(lambda ,formals ,[body body-b*])
           (values `(lambda ,formals ,body) body-b*)]

          [(letrec ([,lhs* ,[rhs* rhs-b**]] ...) ,[body body-b*])
           (values `(letrec ([,lhs* ,rhs*] ...) ,body)
                   (append (apply append rhs-b**) body-b*))]


          ;; Expand certain minuses:
          ;; This is basically an optimization for subtracting from variables:
;          [(- (quote ,[negate-datum -> datum datum-b*]))
;           (values datum datum-b*)]
;          [(- ,[rand rand-b*]
;              (quote ,[negate-datum -> datum* datum-b**]) ...)
;           (if (null? datum*) ;; This is the one argument -
;               (values `(- ,rand) rand-b*)
;               (values
;                 `(+ ,rand  ,datum* ...)
;                 (append rand-b* (apply append datum-b**))))]

          [(,prim ,[rand* rand-b**] ...)
           (guard (scheme-primitive? prim))
           (values `(,prim ,rand* ...) (apply append rand-b**))]
          
          [,unmatched
            (error 'remove-complex-constant "invalid expression: ~s"
                   unmatched)])))

    (define process-method
      (lambda (meth)
        (match meth
          [(lambda ,args ,body)
           (mvlet ([(body body-b*) (process-expr body)])
             (if (null? body-b*)
                 `(lambda ,args ,body)
                 `(lambda ,args (let ,body-b* ,body))))])))

    (lambda (expr)
      (match expr
        [(,input-language (quote (program ,body)))
           (mvlet ([(body body-b*) (process-expr body)])
             (if (null? body-b*)
                 `(,input-language ;remove-complex-constant-language
		   '(program ,body))
                 `(,input-language ;remove-complex-constant-language
		   '(program 
		     (letrec ,body-b* ,body)))))]))
    ))
