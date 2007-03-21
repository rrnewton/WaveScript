
;===============================================================================
;;;;                ---- Pass Remove Complex Opera* ---- 
;;;;
;;;; This pass guarantees that each subexpression of the following
;;;; constructs is either a lexical variable or a constant.
;;;;
;;;;  *) function applications (primitive or otherwise)
;;;;  *) conditional test expression (and conseq/altern for Regiment but not WS)
;;;;  *) the right-hand-side expressions of a let/letrec
;;;;
;;;; When an expression is complex (neither a lexical variable nor a
;;;; constant), this pass replaces it with a new lexical variable and
;;;; binds the variable to the expression's value in an enclosing let
;;;; expression. When more than one subexpression is complex, the new
;;;; variables are bound in parallel by a single let expression.
;;;;
;;;; .author Ryan Newton

;===============================================================================

(module remove-complex-opera mzscheme
  (require "../../../plt/common.ss")
  (provide remove-complex-opera*
           test-remove-complex-opera)
  (chezimports)

(define remove-complex-opera*
  (let ()
    
    ;; This is purely for readability, instead of just tmp_99, I try
    ;; to give things reasonable names based on what kinds of values
    ;; they carry.
    (define (meaningful-name exp)
      (match exp
	     [,prim
	      (guard (regiment-primitive? prim))
	      (symbol-append 'tmp_ prim)]
	     [(,prim ,args ...)
	      (guard (regiment-primitive? prim))
	       (if (basic-primitive? prim)
		   'tmp_basic
		   (symbol-append 'tmp_ prim))
		   #;
	           (case prim
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
	     [,otherwise 'tmp]))

    (define  (make-letrec decls body)
      (if (null? decls) body
	  `(lazy-letrec ,decls ,body)
	  ))

    ;; Coerces an expression to be simple, producing new bindings.
    ;; .returns A simple expression and a list of new decls.
    (define (make-simple x tenv)
      (if (simple-expr? x)
	  (values x '())
	  (let-match ([#(,res ,binds) (process-expr x tenv)])
	    (mvlet (
		    [(type) (recover-type x tenv)]
		    [(name) (unique-name (meaningful-name x))])
	      (values name
		      (cons (list name type res) binds))))))

    ;; Same thing but for a list of expressions.
    ;; Returns a list of *all* the bindings appended together.
    (define (make-simples ls tenv)
      (let ((intermediate
	     (map (lambda (rand) 
		    (mvlet ([(res binds) (make-simple rand tenv)])
		      (cons res binds)))
	       ls)))
	(values (map car intermediate)
		(apply append (map cdr intermediate)))))

    ;; .returns A vector containing an expression and a list of new decls.
    ;; The returned expression should be simple.
    (define (process-expr expr tenv)
      (core-generic-traverse/types 
       (lambda (expr tenv fallthrough)
	 (match expr
;	   [,x (guard (simple-expr? x)) (vector x '())]

	   [(lambda ,formals ,types ,body)
	    (let-match ([#(,body ,decls) (process-expr body (tenv-extend tenv formals types))])	      
	      ;; Decls don't get lifted up past the lambda:
	      (vector `(lambda ,formals ,types 
			     ,(if (not (null? decls))
				  (make-letrec decls body)
				  body))
		    '()))]
	   ;; Also bindings are not listed past a letrec... that's odd.
	   [(lazy-letrec . ,rest)
	    (vector (process-letrec `(lazy-letrec . ,rest) tenv)
		    '())]

	   ;; Todo: could put all this work in the fuser.

	   ;; THIS SHOULDN'T BE RIGHT FOR WAVESCRIPT:
	   ;; DEPENDS ON LAZINESS/PURITY:
	   [(if ,a ,b ,c)
	    ;; [2007.03.20] Eliminating this:  This pass is no longer used for WS:
	    (if #f ;(memq (compiler-invocation-mode) '(wavescript-simulator wavescript-compiler))
		(mvlet ([(test test-decls)     (make-simple a tenv)])
		  (let-match ([#(,conseq ,conseq-decls) (process-expr b tenv)]
			      [#(,altern ,altern-decls) (process-expr c tenv)])
		    (vector `(if ,test 
				 ,(make-letrec conseq-decls conseq) 
				 ,(make-letrec altern-decls altern))
			    test-decls)
		    ))
		;; Otherwise it's plain regiment.
	     (mvlet ([(test test-decls)     (make-simple a tenv)]
		     [(conseq conseq-decls) (make-simple b tenv)]
		     [(altern altern-decls) (make-simple c tenv)])
	       (vector `(if ,test ,conseq ,altern)
		       (append test-decls conseq-decls altern-decls))
	       ))]

	   ;; For now don't lift out an iterate's lambda!
	   [(iterate ,[fun] ,source)
	    (let-match ([#(,f ,decl1) fun])
	      (mvlet ([(s decl2) (make-simple source tenv)])
		(display-constrained "simple iterate source: " `[,s 100] "\n")
		(vector `(iterate ,(make-letrec decl1 f) ,s)
			;(append decl1 decl2)
			decl2
			))
	      )]
	   [(iterate . ,_) (error 'remove-complex-opera* "bad iterate: ~s" _)]

	   ;; SIGH, side effects...  Here we lift bindings up to the
	   ;; top of each subexpression but no further.  Don't want to
	   ;; reorder side effects.
	   [(begin ,[e*] ...)
	    (vector `(begin ,@(map (match-lambda (#(,e ,decls)) 
				     (make-letrec decls e))
				e*))
		    '())]

	   ;; Make set!'s rhs simple:
	   [(set! ,v ,e)
	    (mvlet ([(e2 decls) (make-simple e tenv)])
	      (vector `(set! ,v ,e2) decls))]

	   ;; Make start and end simple.
	   [(for (,i ,st ,en) ,bod)
	    (mvlet ([(st stdecls) (make-simple st tenv)]
		    [(en endecls) (make-simple en tenv)])
	      (let ([newenv (tenv-extend tenv (list i) '(Int))])
		(let-match ([#(,body ,decls) (process-expr bod newenv)])
		  (vector `(for (,i ,st ,en)
			       ,(make-letrec decls body))
			  (append stdecls endecls))
		  )))]   	   

	   [(tupref ,n ,m ,x)
	    (mvlet ([(res binds) (make-simple x tenv)])
	      (vector `(tupref ,n ,m ,res) binds)
	      )]
	   [(tuple ,args ...)
	    (mvlet ([(args binds) (make-simples args tenv)])
	      (vector `(tuple ,args ...) binds))]

	   [(,prim ,rand* ...) (guard (regiment-primitive? prim))
	    (mvlet ([(args binds) (make-simples rand* tenv)])
	      (vector `(,prim ,args ...) binds))]

	   ;; Constants:
	   [,prim (guard (regiment-primitive? prim))
		  (vector prim '())]

	   ;[,other (error 'remove-complex-opera* "didn't handle expr: ~s" other)]
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
;			(if (null? ls) (values acc declacc)
			    (let-match ([#(,r ,rd) (process-expr (car ls) newenv)])
			      ;(display-constrained "Looping.. " `[,(car ls) 50] "\n")
			      (loop (cdr ls) (cons r acc) (cons rd declacc)))))])
	       (make-letrec (append (map list lhs* type* rhs*)
				    (apply append rhs-decls*)
				    boddecls)
			    bod)))]
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


(define-testing these-tests
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

(define-testing test-this
  (default-unit-tester 
    "11: Remove-Complex-Opera: Pass to flatten expressions by simplifying arguments."
    these-tests))
  

(define test11 test-this)
(define tests11 these-tests)
(define test-remove-complex-opera test-this)
(define tests-remove-complex-opera these-tests)

;==============================================================================

) ; End module
