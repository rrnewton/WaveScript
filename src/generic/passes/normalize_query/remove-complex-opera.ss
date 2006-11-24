
;===============================================================================
;;   ---- Pass Remove Complex Opera* ---- 
;This pass guarantees that each subexpression (operator or operand) of a call
;or primitive call is either a lexical variable or a constant. When an
;expression is complex (neither a lexical variable nor a constant), this pass
;replaces it with a new lexical variable and binds the variable to the
;expression's value in an enclosing let expression. When more than one
;subexpression is complex, the new variables are bound in parallel by a single
;let expression.
;   Ryan Newton
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

    ;; Coerces an expression to be simple, producing new bindings.
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

    ;; .returns An expression and a list of new decls.
    (define (process-expr expr tenv)
      (core-generic-traverse/types 
       (lambda (expr tenv fallthrough)
	 (match expr
	   [,x (guard (simple-expr? x)) (vector x '())]

	   ;; Todo: could put all this work in the fuser.

	   ;; THIS SHOULDN'T BE RIGHT FOR WAVESCRIPT:
	   ;; DEPENDS ON LAZINESS/PURITY:
	   [(if ,a ,b ,c)
	    (mvlet ([(test test-decls)     (make-simple a tenv)]
		    [(conseq conseq-decls) (make-simple b tenv)]
		    [(altern altern-decls) (make-simple c tenv)])
	      ;(if (WAVESCRIPT_INVOCATION) ...)
	      (vector `(if ,test ,conseq ,altern)
		      (append test-decls conseq-decls altern-decls))
	      )]

	   [(lambda ,formals ,types ,body)
	    (let-match ([#(,body ,decls) (process-expr body (tenv-extend tenv formals types))])	      
	      ;; Decls don't get lifted up past the lambda:
	      (vector `(lambda ,formals ,types 
			     ,(if (not (null? decls))
				  `(lazy-letrec ,decls ,body)
				  body))
		    '()))]

	   ;; For now don't lift out an iterate's lambda!
	   [(iterate ,[fun] ,source)
	    (let-match ([#(,f ,decl1) fun])
	      (mvlet ([(s decl2) (make-simple source tenv)])
		(display-constrained "simple iterate source: " `[,s 100] "\n")
		(vector `(iterate ,f ,s)
			(append decl1 decl2)))
	      )]
	   [(iterate . ,_) (error 'remove-complex-opera* "bad iterate: ~s" _)]

	   ;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME:	   
	   ;; SIGH, side effects... really need to fix this:
	   [(begin ,[e*] ...)
	    (vector `(begin ,@(map (match-lambda (#(,e ,decls)) `(lazy-letrec ,decls ,e))
				e*))
		    '())
	    ]
	   ;[(set! ,v ,e)]
	   [(for (,i ,st ,en) ,bod)
	    (mvlet ([(st stdecls) (make-simple st tenv)]
		    [(en endecls) (make-simple en tenv)])
	      (let ([newenv (tenv-extend tenv (list i) '(Int))])
		(let-match ([#(,body ,decls) (process-expr bod newenv)])
		  (vector `(for (,i ,st ,en)
			       (lazy-letrec ,decls ,body))
			  (append stdecls endecls))
		  )))]
   
	   
	   ;; Also bindings are not listed past a letrec... that's odd.
	   [(lazy-letrec . ,rest)
	    (vector (process-letrec `(lazy-letrec . ,rest) tenv)
		    '())]

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
    
    ;===========================================================================
    ;; LetrecExpr -> LetrecExpr
    (define process-letrec
      (lambda (letrec-exp tenv)
	(DEBUGASSERT (tenv? tenv))
        (match letrec-exp
	  [(lazy-letrec ((,lhs* ,type* ,rhs*) ...) ,origbod)
	   (let ((newenv (tenv-extend tenv lhs* type*)))	     
	     (mvlet ([(bod boddecls) (make-simple origbod newenv)]
		     [(rhs* rhs-decls*) ; This is an awkward way to loop across rhs*:
		      (let loop ((ls rhs*) (acc ()) (declacc ()))			
			(if (null? ls) (values (reverse! acc) (reverse! declacc))
			    (let-match ([#(,r ,rd) (process-expr (car ls) newenv)])
			      ;(display-constrained "Looping.. " `[,(car ls) 50] "\n")
			      (loop (cdr ls) (cons r acc) (cons rd declacc)))))])
	       `(lazy-letrec ,(append (map list lhs* type* rhs*)
				      (apply append rhs-decls*)
				      boddecls)
			     ,bod)))]
	  [,else (error
		  'remove-complex-opera*
		  "lazy-letrec expression is incorrectly formatted:\n~s"
		  letrec-exp)])))

    ;===========================================================================
    (lambda (program)
      (match program
             [(,input-lang '(program ,exp ,type))
	      (let-match ([#(,newbod ,bnds) (process-expr exp (empty-tenv))])
		`(remove-complex-opera*-language 
		  '(program ,(if (null? bnds) newbod	
				 `(lazy-letrec ,bnds ,newbod)
				 ) ,type))
		)]
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

