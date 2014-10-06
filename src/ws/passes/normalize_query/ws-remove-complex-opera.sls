#!r6rs

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


;;;; [2007.10.16] Modifying this pass to not treat a "deref" as complex.
;;;; Accomplishing that by simply changing simple-expr?.

;===============================================================================

(library (ws passes normalize_query ws-remove-complex-opera)
  (export ws-remove-complex-opera*
	   ws-remove-complex-opera*-grammar
           test-ws-remove-complex-opera)
  (import (except (rnrs (6)) error) (ws common)
	  ;(ws util rn-match) ;; TEMPTOGGLE
	  (ws passes normalize_query ws-remove-letrec))

  ;; The grammar is re-written to reflect simplicity constraints:
  (define ws-remove-complex-opera*-grammar
    (append
     '(
       ;; (Expr ('app Expr ...))  
       (Expr Var) 
       (Expr Const)
       (Expr ('unionN Simple Simple ...))
       (Expr ('tuple Simple ...))
       (Expr ('tupref Int Int Simple))
       (Expr (Prim Simple ...)) 
       (Expr ('set! Var Simple))

       ;; Could require that these Expr positions be Lets:
       (Expr ('if Simple Expr Expr))
       (Expr ('wscase Simple (Var Expr) ...))

       (Expr ('for (Var Simple Simple) Expr))
       (Expr ('while Expr Expr))
       (Expr ('lambda (LHS ...) (Type ...) Expr))
       (Expr ('assert-type Type Expr))
       (Expr ('begin Expr ...))      

       (Expr ('foreign-app Const Simple Simple ...))
       (Expr ('construct-data Var Simple ...))

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

    ;; This is the standard one from wavescript_helpers.ss
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
			[(name) (unique-name "tmpsmp" 
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
	   
	   [(quote ,complex-const) (vector `',complex-const '())]

	   [(lambda ,formals ,types ,body)
	    (let-values ([(body decls) (make-simple body (tenv-extend tenv formals types))])
	      ;; Decls don't get lifted up past the lambda:
	      (vector `(lambda ,formals ,types 
			     ,(if (not (null? decls))
				  (make-lets decls body)
				  body))
		    '()))]

	   [(src-pos ,_ ,[e]) e]
	   [(assert-type ,ty ,[e])
	    (let-match ([#(,e ,decls) e])
	      (vector `(assert-type ,ty  ,e)
		      decls))]


;; REDONE: this will not simplify RHS's
	   [(let () ,[body]) body]
	   [(let ([,v ,ty ,[e]] ,rest ...) ,bod)
	    (let-match ([#(,rhs ,rdecls) e])
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

	   [(wscase ,x (,TC* ,[rhs*]) ...)
	    (match rhs*
	      [(#(,rhs* ,rhsdecl*) ...)
	       ;(ASSERT (curry andmap null?) rhsdecl*)
	       (mvlet ([(x xdecls) (make-simple x tenv)])
		 (vector `(wscase ,x 
			    ,@(map 
				(lambda (TC rhs rhsdecl)
				  (unless (eq? TC default-case-symbol)
				    ;; All the other RHS's should be just lambdas... 
				    ;; and they shouldn't produce bindings:
				    (ASSERT null? rhsdecl))
				  (list TC rhs))
				TC* rhs* rhsdecl*))
			 (apply append xdecls rhsdecl*)))])]
	   
   ;; For now don't lift out an iterate's lambda!	   
   ;; [2008.04.08] Modifying this to not pointlessly simplify rhs's:
   ;;
   ;; FIXME FIXME I tried to fix this to not simplify rhs, but I
   ;; believe this triggered some other bug in reference counting.
;; Works:
;     [(iterate ,annot (let ([,v* ,ty* ,[(lambda (x) (make-simple x tenv)) -> rhs* rdecls*]] ...) ,fun) ,source)
;; Broken:
     [(iterate ,annot (let ([,v* ,ty* ,[_rhs*]] ...) ,fun) ,source)
	    (let-match ([#(,f ,fdecl) (process-expr fun (tenv-extend tenv v* ty*))]
			[(#(,rhs* ,rdecls*) ...) _rhs*]
			)
	      (ASSERT null? fdecl)
	      (mvlet ([(src sdecl) (make-simple source tenv)])
		;(ASSERT null? sdecl)
		;(display-constrained "simple iterate source: " `[,src 100] "\n")
		(vector `(iterate ,annot (let ,(map list
					  v* ty*
					  (map make-lets rdecls* rhs*))
				    ,f)
				  ,src)
                        sdecl
			)))]
	   [(iterate . ,_) (error 'ws-remove-complex-opera* "bad iterate: ~s" _)]

       [(unionN ,annot ,strms ...)
	(let-values ([(v* decls) (make-simples strms tenv)])
	  (vector `(unionN ,annot ,@v*)
		  decls))]

      [(_merge ,annot ,s1 ,s2)
       (let-values ([(v* decls) (make-simples `(,s1 ,s2) tenv)])
         (vector `(_merge ,annot ,s1 ,s2) decls))]

      [(readFile ,annot ,rand* ...)
       (mvlet ([(args binds) (make-simples rand* tenv)])
         (vector `(readFile ,annot ,@args) binds))]

      [(timer ,annot ,rand* ...)
       (mvlet ([(args binds) (make-simples rand* tenv)])
         (vector `(timer ,annot ,@args) binds))]

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
	   [(construct-data ,tc ,e* ...)
	    (mvlet ([(e2* decls) (make-simples e* tenv)])
	      (vector `(construct-data ,tc ,@e2*) decls))]
	   [(foreign-app ',realname ,e* ...)
	    ;(ASSERT (symbol? rator))
	    (mvlet ([(args binds) (make-simples e* tenv)])
	      (vector `(foreign-app ',realname ,@args) binds))]

	   ;; Don't lift anything out of this special syntax:
	   [(foreign        ',name ',files) (vector `(foreign        ',name ',files) '())]
	   [(foreign_source ',name ',files) (vector `(foreign_source ',name ',files) '())]

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
			'())))]

	   [(tupref ,n ,m ,x)
	    (mvlet ([(res binds) (make-simple x tenv)])
	      (vector `(tupref ,n ,m ,res) binds)
	      )]
	   [(tuple ,args ...)
	    (mvlet ([(args binds) (make-simples args tenv)])
	      (vector `(tuple ,@args) binds))]

	   ;; [2008.08.06] Allowing limited functions.
	   [(app ,rator ,rand* ...)
	    (ASSERT symbol? rator)
	    (mvlet ([(args binds) (make-simples rand* tenv)])
	      (vector `(app ,rator ,@args) binds))]

	   [(,prim ,rand* ...) (guard (wavescript-primitive? prim))
	    (mvlet ([(args binds) (make-simples rand* tenv)])
	      (vector `(,prim ,@args) binds))]
	   
	   ;; Constants:
	   [,prim (guard (wavescript-primitive? prim))
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


(define-testing test-ws-remove-complex-opera
  (default-unit-tester 
    "11: WS-Remove-Complex-Opera: Pass to flatten expressions by simplifying arguments."
  (map
   (lambda (x)
     (let ((prog (car x)) (res (cadr x)))	
       `[(ws-remove-complex-opera* '(some-lang '(program ,prog notype)))
	 (ws-remove-complex-opera*-language '(program ,res notype))]))
   '(
     ;[(lazy-letrec ([x (List Int) (cons '3 (cons '4 (cons '5 '())))]) x) unspecified]
    ))))

;==============================================================================

) ; End module
