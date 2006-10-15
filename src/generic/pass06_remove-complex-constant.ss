;;; Pass 6: remove-complex-constant

;;; RRN[2004.04.24] This pass has been resurected.  The old
;;; functionality described below is largely outdated.

;;; This pass rewrites the code so that complex constants, such as
;;; '(1 2 3) and '#(1 (2 3) 4), are created using explicit list- and
;;; vector-creation operators.

;;; remove-complex-constant arranges for complex constant values to
;;; be built only once, when the program begins, by wrapping each
;;; program that contains complex constants in a let expression
;;; binding temporary variables to the expressions that create the
;;; complex constants and rewriting the original quote expressions
;;; as references to these temporaries.  For example,
;;;


;;; The input language is the same as the output language of Pass 5.

;;; Output from this pass is in the same language, except that
;;; quote expressions now contain only immediates.

;;; <Pgm>  ::= (<language-name> (quote (program <Exp>)))
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= 
;;;            (quote <imm>)
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (lambda <Formalexp> <Exp>)
;;;          | (letrec (<Decl>*) <Exp>)
;;;          | (<primitive> <Exp>*)
;;; <Formalexp> ::= (<var>*)

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
            [(pair? x)
             `(cons ,(datum->code (car x))
                    ,(datum->code (cdr x)))]
            [else `(quote ,x)]))))

    (define negate-datum
      (lambda (datum)
        (cond
          [(number? datum) (process-expr `(quote ,(- datum)))]
          [else (error 'remove-complex-constant.negate-datum
                       "cannot negate non-numeric datum: ~s" datum)])))

    (define process-expr
      (core-generic-traverse
       ;; Driver:
       (lambda (x fallthrough)
	 (match x 
          [(quote ,datum)
           (guard (constant? datum)) ;; Not required to be immediate?
           (vector `(quote ,datum) '())]
	  ;; [2006.10.14] Umm we shouldn't be supporting symbols:
          [(quote ,datum)
           (guard (symbol? datum))
           (vector `(quote ,datum) '())]
          [(quote ,datum)
           (let* ([tmp (unique-name 'tmp)]
		  [exp (datum->code datum)]
		  ;; Null tenv is ok, it's just a constant:
		  [type (recover-type exp (empty-tenv))]
		  )
             (vector tmp `((,tmp ,type ,exp))))]

          [(lambda ,formals ,types ,[result])
	   (match result
	     [#(,body ,body-b*) 
	      ;;(vector `(lambda ,formals ,body) body-b*)
	      ;; [2005.12.08] Modifying this so it doesn't (yet) lift them all the way up to the top.
	      (vector `(lambda ,formals ,types (letrec ,body-b* ,body)) ())]
	     )]

	  [,other (fallthrough other)])
	  )
       ;; Fuser:
       ;(match-lambda (#(,exps ,binds) ,k)
       ;(vector (apply k exps) (apply append binds)))
       (lambda (results k)
	 (match results
	   [(#(,exps ,binds) ...) (vector (apply k exps) (apply append binds))]
	   [,other (error 'remove-complex-constant:process-expr 
			  "bad intermediate result: ~s" other)]))
      ))

#;
    (define process-expr
      (lambda (expr)
        (match expr
          [(quote ,datum)
           (guard (constant? datum)) ;; Not required to be immediate?
           (values `(quote ,datum) '())]
          ;; Symbols are not *technically* immediate (they are boxed);
          ;; however, neither are they complex.
          [(quote ,datum)
           (guard (symbol? datum))
           (values `(quote ,datum) '())]
          [(quote ,datum)
           (let* ([tmp (unique-name 'tmp)]
		  [exp (datum->code datum)]
		  ;; Null tenv is ok, it's just a constant:
		  [type (recover-type exp (empty-tenv))]
		  )
             (values tmp `((,tmp ,type ,exp))))]
          [,var (guard (symbol? var))
            (values var '())]

          [(if ,[test test-b*] ,[conseq conseq-b*] ,[altern altern-b*])
           (values `(if ,test ,conseq ,altern)
                   (append test-b* conseq-b* altern-b*))]
          
          [(lambda ,formals ,types ,[body body-b*])
	   ;(values `(lambda ,formals ,body) body-b*)
	   ;; [2005.12.08] Modifying this so it doesn't (yet) lift them all the way up to the top.
	   (values `(lambda ,formals ,types (letrec ,body-b* ,body)) ())
	   ]

          [(letrec ([,lhs* ,type* ,[rhs* rhs-b**]] ...) ,[body body-b*])
           (values `(letrec ([,lhs* ,type* ,rhs*] ...) ,body)
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

	  ;; No user apps left at this point:
          [(,prim ,[rand* rand-b**] ...)
           (guard (regiment-primitive? prim))
           (values `(,prim ,rand* ...) (apply append rand-b**))]
          
          [,unmatched
            (error 'remove-complex-constant "invalid expression: ~s"
                   unmatched)])))


    (lambda (expr)
      (match expr
        [(,input-language (quote (program ,body ,type)))
           (let-match ([#(,body ,body-b*) (process-expr body)])
             (if (null? body-b*)
                 `(,input-language ;remove-complex-constant-language
		   '(program ,body ,type))
                 `(,input-language ;remove-complex-constant-language
		   '(program 
		     (letrec ,body-b* ,body)
		     ,type))))]))
    ))
