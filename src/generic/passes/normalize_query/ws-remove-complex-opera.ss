
;===============================================================================
;;;;      ---- Pass Remove Complex Opera* (WaveScript Version) ---- 
;;;;
;;;; This pass guarantees that each subexpression of the following
;;;; constructs is either a lexical variable or a constant.
;;;;
;;;;  *) function applications (primitive or otherwise)
;;;;  *) conditional test expression 
;;;;
;;;; When an expression is complex (neither a lexical variable nor a
;;;; constant), this pass replaces it with a new lexical variable and
;;;; binds the variable to the expression's value in an enclosing let
;;;; expression. When more than one subexpression is complex, the new
;;;; variables are bound in parallel by a single let expression.
;;;;
;;;; NOTE: polymorphic constants (e.g. null list) are *not* considered
;;;; simple for the purposes of this pass.
;;;; 
;;;; .author Ryan Newton

;===============================================================================

(module ws-remove-complex-opera mzscheme
  (require "../../../plt/common.ss"
	   ;"../../compiler_components/hm_type_inference.ss"
	   "../../compiler_components/type_environments.ss"
	   "ws-remove-letrec.ss")
  (provide ws-remove-complex-opera*
	   ws-remove-complex-opera*-grammar
           test-ws-remove-complex-opera)
  (chezimports)

;; All expressions are re-written to reflect simplicity constraints:
  (define ws-remove-complex-opera*-grammar
    (append
     '(
       ;; (Expr ('app Expr ...))  
       (Expr Var) 
       (Expr Const)
       (Expr ('unionN Simple ...))
       (Expr ('tuple Simple ...))
       (Expr ('tupref Int Int Simple))
       (Expr (Prim Simple ...)) 
       (Expr ('set! Var Simple))

       ;; Could require that these Expr positions be Lets:
       (Expr ('if Simple Expr Expr))
       (Expr ('for (Var Simple Simple) Expr))
       (Expr ('while Expr Expr))
       (Expr ('lambda (LHS ...) (Type ...) Expr))
       (Expr ('assert-type Type Expr))
       (Expr ('begin Expr ...))      

       (Expr ('foreign-app Const Var Expr ...))

       ;(Expr ('let ((LHS Type Expr) ...) Expr))

       ;; Now iterate is syntax:
       (Expr ('iterate ('let ((LHS Type Expr) ...)
			 ('lambda (Var Var) (Type Type) Expr)) Simple))

       ;(Expr LetOrSimple)
       ;(LetOrSimple Simple)
       ;(LetOrSimple ('let ((LHS Type Expr) ...) LetOrSimple))
       ;(LetOrSimple ('begin LetOrSimple ...))

       (Expr ('let ((LHS Type Expr) ...) Expr))

       (Simple ('assert-type Type Simple))
       (Simple Var)   
       (Simple Const)
       ;; Could simplify things by making this a quoted constant:
       (Simple ('tuple)))
     (filter (lambda (x) (match x 
			   [(Expr . ,_) #f]
			   [(Prim 'iterate) #f]
			   [,else #t]))
       remove-letrec-grammar
       )))

(define-pass ws-remove-complex-opera*

    ;; This is the standard one from regiment_helpers.ss
    (define ws-rem-complex:simple? simple-expr?)

    
    #;
    (define ws-rem-complex:simple?
      (lambda (x)
	(and (simple-expr? x)
	     (not (null? x))
	     )))


    (define (make-lets decls body)
      (if (null? decls) 
	  (begin 
	    ;; Can be any 'Block', not just simple expression:
	    #;
	    (ASSERT (lambda (x) (or (ws-rem-complex:simple? x)
				    (and (list? x) (eq? (car x) 'let))))
		    body)
		 body)
	  `(let (,(car decls)) ,(make-lets (cdr decls) body))
	  ))

    ;; Coerces an expression to be simple, producing new bindings.
    ;; .returns A simple expression and a list of new decls.
    (define (make-simple x tenv)
	(let ()
	  (IFCHEZ (import rn-match) (begin))
	  (if (ws-rem-complex:simple? x)
	      (values x '())
	      (let-match ([#(,res ,binds) (process-expr x tenv)])
		(mvlet (
			;; Maybe some of my inefficiency is coming from here:
			[(type) 
			 `',(unique-name "unknown-type")
			 ;(begin  (recover-type x tenv))
			 ]
			[(name) (unique-name 'tmp 
					     #;(meaningful-name x)
					     )])
		  (values name
			  (snoc (list name type res) binds)))))))
    ;; Same thing but for a list of expressions.
    ;; Returns a list of *all* the bindings appended together.
    (define (make-simples ls tenv)
      (if (null? ls) (values '() '())
	  (mvlet ([(first binds1) (make-simple (car ls) tenv)]
		  [(rest binds2) (make-simples (cdr ls) tenv)])
	    (values (cons first rest)
		    (append binds1 binds2)))))

    ;; .returns A vector containing an expression and a list of new decls.
    ;; The returned expression should be simple.
    (define (process-expr expr tenv)
      (IFCHEZ (import rn-match) (begin))
      (match expr
	   [,x (guard (ws-rem-complex:simple? x)) (vector x '())]
	   
	   [(quote ,comple-const) (vector `',comple-const '())]

	   [(lambda ,formals ,types ,body)
	    (let-values ([(body decls) (make-simple body (tenv-extend tenv formals types))])
	      ;; Decls don't get lifted up past the lambda:
	      (vector `(lambda ,formals ,types 
			     ,(if (not (null? decls))
				  (make-lets decls body)
				  body))
		    '()))]

	   [(assert-type ,ty ,[e])
	    (let-match ([#(,e ,decls) e])
	      (vector `(assert-type ,ty  ,e)
		      decls))]

	   [(let () ,body)	
	    (let-values ([(body bdecls) (make-simple body tenv)])
	      (vector (make-lets bdecls body) '()))]

	   [(let ([,v ,ty ,e] ,rest ...) ,bod)
	    (let-values ([(rhs rdecls) (make-simple e tenv)])
	    (let-match  ([#(,rst ,decls) (process-expr 
					  `(let ,rest ,bod)
				            (tenv-extend tenv 
						(list v) (list ty)))])
	      (vector rst
		      (append rdecls 
			      `([,v ,ty ,rhs])
			      decls))
	      ))]

	   [(if ,a ,b ,c)
	    (mvlet ([(test test-decls)     (make-simple a tenv)])
	      (let-values ([(conseq conseq-decls) (make-simple b tenv)]
			   [(altern altern-decls) (make-simple c tenv)])
		(vector `(if ,test 
			     ,(make-lets conseq-decls conseq) 
			     ,(make-lets altern-decls altern))
			test-decls)
		))]

	   ;; For now don't lift out an iterate's lambda!	   
	   [(iterate (let ([,v* ,ty* ,[(lambda (x) (make-simple x tenv)) -> rhs* rdecls*]] ...) ,fun) ,source)
	    (let-match ([#(,f ,fdecl) (process-expr fun (tenv-extend tenv v* ty*))])
	      (ASSERT null? fdecl)
	      (mvlet ([(src sdecl) (make-simple source tenv)])
		;(ASSERT null? sdecl)
		;(display-constrained "simple iterate source: " `[,src 100] "\n")
		(vector `(iterate (let ,(map list
					  v* ty*
					  (map make-lets rdecls* rhs*))
				    ,f)
				  ,src)
                        sdecl
			)))]
	   [(iterate . ,_) (error 'ws-remove-complex-opera* "bad iterate: ~s" _)]

	   [(unionN ,strms ...)
	    (let-values ([(v* decls) (make-simples strms tenv)])
	      (vector `(unionN ,@v*)
		      decls))]

	   ;; SIGH, side effects...  Here we lift bindings up to the
	   ;; top of each subexpression but no further.  Don't want to
	   ;; reorder side effects.
	   [(begin ,[e*] ...)   
	    (let ()
	      (vector `(begin . ,(map (match-lambda (#(,e ,decls))
					(if (null? decls) e
					    (make-lets decls e)))
				   e*))
		      '()))]

; 	   [(begin ,[e]) e]
; 	   [(begin ,[(lambda (x) (make-simple x tenv)) -> e* edecls*] ...)
; 	    (vector `(begin ,@(map make-lets edecls* e*))
; 		    '())]

	   ;; Make set!'s rhs simple:
	   [(set! ,v ,e)
	    (mvlet ([(e2 decls) (make-simple e tenv)])
	      (vector `(set! ,v ,e2) decls))]
	   [(construct-data ,tc ,e)
	    (mvlet ([(e2 decls) (make-simple e tenv)])
	      (vector `(construct-data ,tc ,e2) decls))]
	   [(foreign-app ',realname ,rator ,e* ...)
	    (ASSERT (symbol? rator))
	    (mvlet ([(args binds) (make-simples e* tenv)])
	      (vector `(foreign-app ',realname ,rator ,@args) binds))]

	   ;; Don't lift anything out of this special syntax:
	   [(foreign ',name ',files) (vector `(foreign ',name ',files) ())]

	   ;; Make start and end simple.
	   [(for (,i ,st ,en) ,bod)
	    (mvlet ([(st stdecls) (make-simple st tenv)]
		    [(en endecls) (make-simple en tenv)])
	      (let ([newenv (tenv-extend tenv (list i) '(Int))])
		(let-values ([(body decls) (make-simple bod newenv)])
		  (vector `(for (,i ,st ,en)
			       ,(make-lets decls body))
			  (append stdecls endecls))
		  )))]

	   ;; Make test simple:
	   [(while ,tst ,bod)
	    (mvlet ([(tst tstdecls) (make-simple tst tenv)])
	      (let-values ([(body decls) (make-simple bod tenv)])
		(vector `(while ,(make-lets tstdecls tst) 
				,(make-lets decls body))
			())))]

	   [(tupref ,n ,m ,x)
	    (mvlet ([(res binds) (make-simple x tenv)])
	      (vector `(tupref ,n ,m ,res) binds)
	      )]
	   [(tuple ,args ...)
	    (mvlet ([(args binds) (make-simples args tenv)])
	      (vector `(tuple ,@args) binds))]

	   [(,prim ,rand* ...) (guard (regiment-primitive? prim))
	    (mvlet ([(args binds) (make-simples rand* tenv)])
	      (vector `(,prim ,@args) binds))]

	   ;; Constants:
	   [,prim (guard (regiment-primitive? prim))
		  (vector prim '())]

	   [,other (error 'ws-remove-complex-opera* "didn't handle expr: ~s" other)]
	   ;[,other (fallthrough other tenv)]
	   ))
    
    ;===========================================================================
    [OutputGrammar ws-remove-complex-opera*-grammar]
    [Program
     (lambda (prog _)
      (IFCHEZ (import rn-match) (begin))
      (match prog
             [(,input-lang '(program ,exp ,meta* ... ,type))
	      (let-values ([(newbod bnds) (make-simple exp 
						       (grab-init-tenv meta*)
						       )])
		`(ws-remove-complex-opera*-language 
		  '(program ,(if (null? bnds) newbod	
				 (make-lets bnds newbod)
				 ) 
		     ,@meta* ,type))
		)]
             [,else (error 'ws-remove-complex-opera*
                           "Invalid input: ~a" prog)]))])
;===============================================================================


(define-testing these-tests
  (map
   (lambda (x)
     (let ((prog (car x)) (res (cadr x)))	
       `[(ws-remove-complex-opera* '(some-lang '(program ,prog notype)))
	 (ws-remove-complex-opera*-language '(program ,res notype))]))
   '(
     ;[(lazy-letrec ([x (List Int) (cons '3 (cons '4 (cons '5 '())))]) x) unspecified]
     )
   )
  ) 

(define-testing test-this
  (default-unit-tester 
    "11: WS-Remove-Complex-Opera: Pass to flatten expressions by simplifying arguments."
    these-tests))
  

(define test11 test-this)
(define tests11 these-tests)
(define test-ws-remove-complex-opera test-this)
(define tests-ws-remove-complex-opera these-tests)

;==============================================================================

) ; End module
