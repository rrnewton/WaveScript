
;===============================================================================
;;---- Pass 07 ---- Remove Complex Opera*
;This pass guarantees that each subexpression (operator or operand) of a call
;or primitive call is either a lexical variable or a constant. When an
;expression is complex (neither a lexical variable nor a constant), this pass
;replaces it with a new lexical variable and binds the variable to the
;expression's value in an enclosing let expression. When more than one
;subexpression is complex, the new variables are bound in parallel by a single
;let expression.
;   Ryan Newton
;===============================================================================

;;; Input Language

;;; <Pgm>  ::= (<language-name> (quote (program <Let>)))
;;; <Let>  ::= (lazy-letrec (<Decl>*) <Exp>))
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= 
;;;            (quote <imm>)
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <Exp>*)
;;; <Formalexp> ::= (<var>*)

;;; Output Language

;;; <Pgm>  ::= (<language-name (quote (program <Let>)))
;;; <Let>  ::= (lazy-letrec (<Decl>*) <var>)
;;; <Decl> ::= (<var> <Exp>)
;;; <Simple> ::= (quote <imm>) | <var> 
;;; <Formalexp> ::= (<var>*)
;;; <Exp>  ::= <Simple>
;;;          | (if <Simple> <Simple> <Simple>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <Simple>*)


;===============================================================================

;;; <Prog>    ::= (let () <LanguageDefn>
;;;                 (letrec ((<Var> <Proc>)*)
;;;                   <Exp>))
;;; <Proc>    ::= (lambda (<Var> <Var>*) <Exp>)
;;; <Closure> ::= (make-closure <Var> <Var>*)
;;; <Exp>     ::= (quote <Lit>)
;;;             | <Var>
;;;             | (if <Exp> <Exp> <Exp>)
;;;             | (begin <Exp> <Exp>)
;;;             | (let ((<Var> <Exp>)+) <Exp>)
;;;             | (let ((<Var> <Closure>)+)
;;;                 (begin (closure-set! <Var> <Number> <Var>)+
;;;                        <Exp>))
;;;             | (<Prim> <Exp>*)
;;;             | (anonymous-call <Exp> <Exp>)
;;;             | (<Var> <Exp>*)

;;; Output Language

;;; <Prog>    ::= (let () <LanguageDefn>
;;;                 (letrec ((<Var> <Proc>)*)
;;;                   <Exp>))
;;; <Proc>    ::= (lambda (<Var> <Var>*) <Exp>)
;;; <Closure> ::= (make-closure <Var> <Var>*)
;;; <Exp>     ::= (quote <Lit>)
;;;             | <Var>
;;;             | (if <Exp> <Exp> <Exp>)
;;;             | (begin <Exp> <Exp>)
;;;             | (let ((<Var> <Exp>)+) <Exp>)
;;;             | (let ((<Var> <Closure>)+)
;;;                 (begin (closure-set! <Var> <Number> <Var>)+
;;;                        <Exp>))
;;;             | (<Prim> <SimpleExp>*)
;;;             | (anonymous-call <SimpleExp> <SimpleExp>)
;;;             | (<Var> <SimpleExp>*)
;;; <SimpleExp> ::= <Lit> | <Var>

;===============================================================================

(define remove-complex-opera*
  (let ()

    ;; [2004.06.28] HUH!!?? It looks like process-opera* is not
    ;; CALLED!  Garbage collect this if you get a chance... -=<TODO>=-

    ;===========================================================================
    ;process-opera* takes a list of opera*, and returns 2 values as follows:
    ;1) a new list of opera* (with complex opera* replaced by var refs)
    ;2) a list of bindings, containing bindings which bind new names to
    ;complex expressions.  There is a binding of this sort for each complex
    ;opera* that occured in the input list.
    (define process-opera*
      (lambda (operalst)
        (if (null? operalst)
            (values '() '())
            (match (car operalst)
                   [(quote ,lit)
                    (let-values
                      ([(ops binds) (process-opera* (cdr operalst))])
                      (values (cons (car operalst) ops) binds))]
                   [,var
                     (guard (symbol? var))
                     (let-values
                       ([(ops binds) (process-opera* (cdr operalst))])
                       (values (cons (car operalst) ops) binds))]
                   [,else
                     (let ((new-var (unique-name (meaningful-name (car operalst)))))
                       (let-values
                         ([(ops binds) (process-opera* (cdr operalst))])
                         (values
                           (cons new-var ops)
                           (cons `[,new-var ,(car operalst)] binds))))]))))
    
    ;; This is purely for readability, instead of just tmp_99, I try
    ;; to give things reasonable names based on what kinds of values
    ;; they carry.
    (define (meaningful-name exp)
;      (disp "meaningful" exp)
      (match exp
	     [,prim
	      (guard (regiment-primitive? prim))
	      (symbol-append 'tmp_ prim)]
	     [(,prim ,args ...)
	      (guard (regiment-primitive? prim))
	       (if (basic-primitive? prim)
		   'tmp_basic
		   (symbol-append 'tmp_ prim))
		   #;(case prim
		     [(circle circle-at) 'tmp-circ]
		     [(khood khood-at) 'tmp-khood]
		     [(anchor anchor-at) 'tmp-anch]
		     [(rmap) 'tmp-rmap]
		     [(smap) 'tmp-smap]
		     [(rfold) 'tmp-rfold]
		     [else 'tmp_unknpr])
		   ]
	     [(lambda ,form ,bod) 'tmp-func]
	     ;; Will this happen??!: [2004.06.28]
	     [,otherwise 'tmp-nonprim]))

    (define (simple? x) 
      (match x
          [(quote ,imm) #t]
          [,var (guard (symbol? var) (not (regiment-constant? var))) #t]
	  [,otherwise #f]))

    (define (make-simple x)
      (if (simple? x) 
	  (values x '())
	  (mvlet ([(res binds) (process-expr x)]
		  [(name) (unique-name (meaningful-name x))])
		 (values name
			 (cons (list name res) binds)))))

    (define process-expr
      (lambda (expr)
        (match expr
	  [,x (guard (simple? x)) (values x '())]
          [(if ,a ,b ,c)
	   (mvlet ([(test test-decls) (make-simple a)]
		   [(conseq conseq-decls) (make-simple b)]
		   [(altern altern-decls) (make-simple c)])
	     (values `(if ,test ,conseq ,altern) 
		     (append test-decls conseq-decls altern-decls)))]
	  [(lambda ,formalexp ,[process-letrec -> body])
	   (values `(lambda ,formalexp ,body) '())]

	  ;;; Constants:
	  [,prim (guard (regiment-primitive? prim))
		 (values prim '())
;		 (let ((intermediate (unique-name (meaningful-name prim))))
;		   (values intermediate
;			   `([,intermediate ,prim])))
		 ]
          [(,prim ,rand* ...) (guard (regiment-primitive? prim))	   
	   (let ((intermediate
		  (map (lambda (rand) 
			 (mvlet ([(res binds) (make-simple rand)])
				(list res binds)))
		       rand*)))
	     (values (cons prim (map car intermediate))
		     (apply append (map cadr intermediate))))]
          [,unmatched
	   (error 'lift-letrec "invalid expression: ~s"
		  unmatched)])))  
    
    ;===========================================================================
    ;; LetrecExpr -> LetrecExpr
    (define process-letrec
      (lambda (letrec-exp)
        (match letrec-exp
               [(lazy-letrec ((,lhs* ,[process-expr -> rhs* rhs-decls*]) ...) 
			     ,simple)
	       `(lazy-letrec ,(append (map list lhs* rhs*) 
				      (apply append rhs-decls*)) ,simple)]

               [,else (error
		       'remove-complex-opera*
		       "lazy-letrec expression is incorrectly formatted:~s"
		       letrec-exp)])))

    ;===========================================================================
'    (define LANGUAGE-DEFN
      '(begin
         (define make-closure
           (lambda (code i) (make-procedure code (make-vector i))))
         (define closure-ref
           (lambda (cp i) (vector-ref (procedure-env cp) i)))
         (define closure-set!
           (lambda (cp i v) (vector-set! (procedure-env cp) i v)))
         (define-syntax anonymous-call
                        (syntax-rules ()
                                      [(_ e0 e1 ...)
                                       (let ([t e0])
                                         ((procedure-code t) t e1 ...))]))
         (define-record procedure ((immutable code) (immutable env)))))

    ;===========================================================================
    (lambda (program)
      (match program
             [(,input-lang '(program ,[process-letrec -> letexp]))
	      `(,input-lang '(program ,letexp))]      
             [,else (error 'remove-complex-opera*
                           "Invalid input: ~a" program)]))
    ))
;===============================================================================


(define these-tests
  (map
   (lambda (x)
     (let ((prog (car x)) (res (cadr x)))	
       `[(remove-complex-opera* '(some-lang '(program ,prog)))
	 (some-lang '(program ,res))])) 
   '(
     [(lazy-letrec ((x (cons '3 (cons '4 (cons '5 '()))))) x) unspecified]
     )
   )
  ) 

(define test-this
  (default-unit-tester 
    "Remove-Complex-Opera: Pass to flatten expressions by simplifying arguments."
    these-tests))
  

(define test11 test-this)
(define tests11 these-tests)
(define test-remove-complex-opera test-this)
(define tests-remove-complex-opera these-tests)

;==============================================================================




