;;; Pass 6: remove-complex-constant

;;; RRN[2004.04.24] This pass has been resurected.  The old
;;; functionality described below is largely outdated.

;;; This pass rewrites the code so that complex constants, such as
;;; '(1 2 3) and '#(1 (2 3) 4), are created using explicit list- and
;;; vector-creation operators.

;;; NOTE: [2007.03.11] currently, array constants are not reduced.


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

(module remove-complex-constant mzscheme  
  (require "../../../plt/common.ss"
           "reduce-primitives.ss")
  (provide remove-complex-constant remove-complex-constant-grammar) 
  (chezimports)
  
;; TODO: FILL THIS IN:
(define remove-complex-constant-grammar reduce-primitives-grammar)

(define-pass remove-complex-constant
  [Expr (lambda (x fallthrough)
	  (match x 
          [(quote ,datum)
           (guard (constant? datum)) ;; Not required to be immediate?
           (vector `(quote ,datum) '())]
	  ;; [2006.10.14] Umm we shouldn't be supporting symbols:
          [(quote ,datum)
           (guard (symbol? datum))
           (vector `(quote ,datum) '())]
          [(quote ,datum)
           (let* ([tmp (unique-name 'tmp)])
	     (let-values ([(exp type) (datum->code datum)])	       
	       (vector tmp `((,tmp ,type ,exp)))))]
          [(lambda ,formals ,types ,[result])
	   (match result
	     [#(,body ,body-b*) 
	      ;;(vector `(lambda ,formals ,body) body-b*)
	      ;; [2005.12.08] Modifying this so it doesn't (yet) lift them all the way up to the top.
	      (vector `(lambda ,formals ,types (letrec ,body-b* ,body)) ())]
	     )]
	  [,other (fallthrough other)]))]

  [Fuser (lambda (results k)
	 (match results
	   [(#(,exps ,binds) ...) (vector (apply k exps) (apply append binds))]
	   [,other (error 'remove-complex-constant:process-expr 
			  "bad intermediate result: ~s" other)]))]

  [Program 
   (lambda (prog process-expr)
     (match prog
       [(,input-language (quote (program ,body ,meta* ... ,type)))
	(let-match ([#(,body ,body-b*) (process-expr body)])
	  (if (null? body-b*)
	      `(remove-complex-constant-language
		'(program ,body ,meta* ... ,type))
	      `(remove-complex-constant-language
		'(program 
		     (letrec ,body-b* ,body)
		   ,meta* ...  ,type))))]))]

  ;; Works just for lists right now.
  (define datum->code
    (let* ([pow32 (expt 2 32)]
	   [pow31 (expt 2 31)]
	   [convert-to-signed
	    (lambda (n)
	      (if (< n pow31) n
		  (- (- pow32 n))))])
      (lambda (x)
	;(DEBUGASSERT pair? x)
	;; Null tenv is ok, it's just a constant:
	(let ([type (recover-type `',x (empty-tenv))])
	  (values 
	   (let loop ([x x])
	     (cond		     
	      [(pair? x)	       
	       `(cons ,(first-value (datum->code (car x)))
		      ,(loop (cdr x)))]
	      ;; Respect the invariant that nulls have type assertions:
	      [(null? x) (ASSERT type)	       
	       `(assert-type ,type (quote ,x))]
	      [else `(quote ,x)]
	      ;;[else (error 'datum->code "unhandled complex constant: ~s" x)]
	      ))
	   type)))))
#;
  (define negate-datum
    (lambda (datum)
        (cond
	 [(number? datum) (process-expr `(quote ,(- datum)))]
	 [else (error 'remove-complex-constant.negate-datum
		      "cannot negate non-numeric datum: ~s" datum)])))
  )


) ; End module