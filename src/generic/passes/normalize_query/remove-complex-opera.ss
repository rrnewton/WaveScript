
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

    (define (make-simple x tenv)
      (if (simple-expr? x)
	  (values x '())
	  (let-match ([#(,res ,binds) (process-expr x tenv)])
	    (mvlet (
		    [(type) (recover-type x tenv)]
		    [(name) (unique-name (meaningful-name x))])
	      (values name
		      (cons (list name type res) binds))))))

    (define (make-simples ls tenv)
      (let ((intermediate
	     (map (lambda (rand) 
		    (mvlet ([(res binds) (make-simple rand tenv)])
		      (cons res binds)))
	       ls)))
	(values (map car intermediate)
		(apply append (map cdr intermediate)))))

    (define (process-expr expr tenv)
      (core-generic-traverse/types 
       (lambda (expr tenv fallthrough)
	 (match expr
	   [,x (guard (simple-expr? x)) (vector x '())]

	   ;; Todo: could put all this work in the fuser.

	   [(if ,a ,b ,c)
	    (mvlet ([(test test-decls)     (make-simple a tenv)]
		    [(conseq conseq-decls) (make-simple b tenv)]
		    [(altern altern-decls) (make-simple c tenv)])
	      (vector `(if ,test ,conseq ,altern)
		      (append test-decls conseq-decls altern-decls)))]	   

	   [(lambda ,formals ,types ,body)
	    (let ([body (process-letrec body (tenv-extend tenv formals types))])
	      (vector `(lambda ,formals ,types ,body) '()))]

	   [(tupref ,n ,m ,x)
	    (mvlet ([(res binds) (make-simple x tenv)])
	      (vector `(tupref ,n ,m ,res) binds)
	      )]
	   [(tuple ,args ...)
	    (mvlet ([(args binds) (make-simples args tenv)])
	      (vector `(tuple ,args ...) binds))]

	   ;; Constants:
	   [,prim (guard (regiment-primitive? prim))
		  (vector prim '())
					;		 (let ((intermediate (unique-name (meaningful-name prim))))
					;		   (vector intermediate
					;			   `([,intermediate ,prim])))
		  ]
	   [(,prim ,rand* ...) (guard (regiment-primitive? prim))
	    (mvlet ([(args binds) (make-simples rand* tenv)])
	      (vector `(,prim ,args ...) binds))]

	   [,other (fallthrough other tenv)]
	   ))
       (lambda (results reconstr)
	 (match results
	   [(#(,exps ,decls) ...)
	    (vector (apply reconstr exps) 
		    (apply append decls))]))
       expr tenv))

;       (lambda (expr tenv)
; 	(DEBUGASSERT (tenv? tenv))
; 	 (match expr
; 	   [,x (guard (simple-expr? x)) (values x '())]
; 	   [(if ,a ,b ,c)
; 	    (mvlet ([(test test-decls) (make-simple a tenv)]
; 		    [(conseq conseq-decls) (make-simple b tenv)]
; 		    [(altern altern-decls) (make-simple c tenv)])
; 	      (values `(if ,test ,conseq ,altern)
; 		      (append test-decls conseq-decls altern-decls)))]
; 	   [(lambda ,formals ,types ,body)
; 	    (let ([body (process-letrec body (tenv-extend tenv formals types))])
; 	      (values `(lambda ,formals ,types ,body) '()))]

; 	   ;; Constants:
; 	   [,prim (guard (regiment-primitive? prim))
; 		  (values prim '())
; 					;		 (let ((intermediate (unique-name (meaningful-name prim))))
; 					;		   (values intermediate
; 					;			   `([,intermediate ,prim])))
; 		  ]
; 	   [(,prim ,rand* ...) (guard (regiment-primitive? prim))	   
; 	    (let ((intermediate
; 		   (map (lambda (rand) 
; 			  (mvlet ([(res binds) (make-simple rand tenv)])
; 			    (list res binds)))
; 		     rand*)))
; 	      (values (cons prim (map car intermediate))
; 		      (apply append (map cadr intermediate))))]
;	   [,unmatched
;	    (error 'remove-complex-opera "invalid expression: ~s"
;		   unmatched)]
;         ))
    
    ;===========================================================================
    ;; LetrecExpr -> LetrecExpr
    (define process-letrec
      (lambda (letrec-exp tenv)
	(DEBUGASSERT (tenv? tenv))
        (match letrec-exp
	  [(lazy-letrec ((,lhs* ,type* ,rhs*) ...) ,simple)
	   (let ((newenv (tenv-extend tenv lhs* type*)))
	     (mvlet (((rhs* rhs-decls*) ; This is an awkward way to loop across rhs*:
		      (let loop ((ls rhs*) (acc ()) (declacc ()))
			(if (null? ls) (values (reverse! acc) (reverse! declacc))
			    (let-match ([#(,r ,rd) (process-expr (car ls) newenv)])
			      (loop (cdr ls) (cons r acc) (cons rd declacc)))))))
	       `(lazy-letrec ,(append (map list lhs* type* rhs*)
				      (apply append rhs-decls*)) ,simple)))]
	  [,else (error
		  'remove-complex-opera*
		  "lazy-letrec expression is incorrectly formatted:\n~s"
		  letrec-exp)])))

    ;===========================================================================
    (lambda (program)
      (match program
             [(,input-lang '(program ,letexp ,type))
	      `(remove-complex-opera*-language '(program ,(process-letrec letexp (empty-tenv)) ,type))]
             [,else (error 'remove-complex-opera*
                           "Invalid input: ~a" program)]))
    ))
;===============================================================================


(define these-tests
  (map
   (lambda (x)
     (let ((prog (car x)) (res (cadr x)))	
       `[(remove-complex-opera* '(some-lang '(program ,prog notype)))
	 (remove-complex-opera*-language '(program ,res notype))]))
   '(
     [(lazy-letrec ([x (List Int) (cons '3 (cons '4 (cons '5 '())))]) x) unspecified]
     )
   )
  ) 

(define test-this
  (default-unit-tester 
    "11: Remove-Complex-Opera: Pass to flatten expressions by simplifying arguments."
    these-tests))
  

(define test11 test-this)
(define tests11 these-tests)
(define test-remove-complex-opera test-this)
(define tests-remove-complex-opera these-tests)

;==============================================================================




