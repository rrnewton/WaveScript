

;;;; .title Pass: Add Data Flow
;;;; .author Ryan Newton

;;;; This pass undoes the control indirection introduced by maps/filters/folds.
;;;; It explicitely attaches a table to the output program which binds
;;;; variable names to the expression that generates that data value.

(module pass17_add-data-flow mzscheme

  (require 
	   "../generic/constants.ss"
           "../plt/iu-match.ss"
	   "../plt/prim_defs.ss"
           ;"../plt/cheztrace.ss"  ;; HUH?!  Shouldn't have to include this with helpers.ss included, but I do.
           ;(all-except "../plt/helpers.ss" test-this these-tests trace-define trace-let trace-lambda)
           (all-except "../plt/pass20_deglobalize.ss" test-this these-tests)
           (all-except "../plt/helpers.ss" test-this these-tests)
           (all-except "../plt/regiment_helpers.ss" test-this these-tests)
           (all-except "../plt/hm_type_inference.ss"  test-this these-tests)
           (all-except "../plt/grammar_checker.ss" test-this these-tests)
;           (all-except "../plt/regiment_helpers.ss" test-this these-tests)
           )

;  (provide deglobalizeh 
;;	   test-this these-tests test12 tests12)
#;  (provide deglobalize test20 tests20 test-deglobalize tests-deglobalize 
           ;; Temporarily exposing.
           delazy-bindings)

(provide add-data-flow test-this test17b test-add-data-flow expr->allvars

	 ;; TEMP:
	 dfg? extend-dfg empty-dfg
	 
	 )

(chezimports )

;  (require "../plt/cheztrace.ss")
  
(define worldsym 'THEWORLD)

;======================================================================
;; I'm introducing the idea of a "Code Location" which currently is
;; just the decl ripped out of the letrec that contains the expression.

    ;; symbol, dfg -> codeloc
    (define (lookup s dfg) 
      (let ([entry (assq s dfg)])
	(match entry
	  [#f (error 'lookup "No binding for ~s in ~s\n" s dfg )]
	  [,other 
	   (DEBUGASSERT (codeloc? (cadr other)))
	   (cadr other)] ;; Return the codeloc
	  )))
    (define (dfg? ls)
      (andmap (lambda (x)
                (match x
                  [(,v ,cl) (and (symbol? v) (codeloc? cl))]
;                  [(,v ,ws) (and (symbol? v) (eq? worldsym ws))]
                  [,else #f]))
              ls))
    (define (codeloc? ls)
      (match ls
	[,ws (guard (eq? ws worldsym)) #t]
	[',ws (guard (eq? ws worldsym)) 
	      (error 'codeloc? "This shouldn't happen: got quoted worldsym")]
	[[,name ,ty (,annots ...) ,rhs] 
	 (and (symbol? name) (type? ty))]
	[,else #f]))

    (define (codeloc->expr x)
      (match x 
	[,w (guard (eq? w worldsym)) #f] ;; No good answer for world.
	[[,__ ,___ ,____ ,expr] expr]
	[,other (error 'codeloc->expr "unmatched datum: ~s" other)]))

    (define extend-dfg
      (case-lambda 
	[(dfg name* codeloc*)
	 (DEBUGASSERT (and (list? name*) (andmap symbol? name*)))
	 (DEBUGMODE (for-each (lambda (x) (ASSERT (codeloc? x))) codeloc*))         
	 (append (map list name* codeloc*) dfg)]
	[(dfg binds)
	 (extend-dfg dfg (map car binds) (map cadr binds))
	 ]))
    (define (empty-dfg) ())



    ;; This is just used for debugging below.
    (define (expr->allvars e)
      (match e
	[(quote ,const)             ()]
	[,var (guard (symbol? var)) ()]
	[(if ,[t] ,[c] ,[a]) (append t c a)]
	[(lambda ,v* ,ty* ,[bod]) (append v* bod)]
	[(lazy-letrec ,binds ,tail)
	 (apply append (map car binds)
		(map expr->allvars (map last binds)))]
	[(,prim ,[rand*] ...)	   
	 (guard (regiment-primitive? prim))
	 (apply append rand*)]
	[,unmatched
	 (error 'expr->allvars "invalid syntax ~s" unmatched)]
	))

;======================================================================

(define these-tests '())

(define add-data-flow
  (build-compiler-pass ;; This wraps the main function with extra debugging machinery
   'add-data-flow
   `(input)
   `(output )
  (let ()

    (define process-primapp 0)
    (define global-tenv 'uninitialized)

    (define (letrec-extend-tenv tenv binds)      
      (tenv-extend tenv
		   (map car binds) 
		   (map cadr binds)))

    ;; The easiest way to proceed is to suck out all the types from
    ;; the whole program.  Names are unique, so this is all the info
    ;; we need.
    (define (extract-whole-tenv expr)
;      (regiment-generic-traverse
;       (lambda (x autoloop)
;	 [(lambda ,v* ,ty* ,bod)
;	  (tenv-extend (loop bod) v* ty*)
;	  ])
;       )
      (match expr
	[,atom (guard (atom? atom)) (empty-tenv)]
	[(quote ,_)                 (empty-tenv)]
	
	[(if ,[t] ,[c] ,[a]) (tenv-append t c a)]
	[(lambda ,v* ,ty* ,[bod]) (tenv-extend bod v* ty*)]
	[(lazy-letrec ([,v* ,ty* ,annots* ,[rhs*]] ...) ,[bod])
	 (apply tenv-append (tenv-extend bod v* ty* #t) rhs*)]
	
	[(,prim ,[rand*] ...) (guard (regiment-primitive? prim))
	 (apply tenv-append rand*)]
	[,other (error 'extract-whole-tenv "bad regiment expression: ~s" other)]
	))

    ;; Returns the inner simple expression that generates the return value.
    ;; Expression, DFG -> Codeloc
    ;; .param currentloc The current code-location for the expression being processed, 
    ;; or #f if we're not inside the letrec yet.
    (define (get-tail expr currentloc env)
      (DEBUGASSERT (or currentloc (symbol? expr) (and (list? expr) (eq? 'lazy-letrec (car expr)))))
      (match expr
	  [,var (guard (symbol? var)) 
		(let ([cl (lookup var env)])		  
		  (get-region-tail (codeloc->expr cl) env))]
	  [(lazy-letrec ,binds ,var)
           (ASSERT (symbol? var)) ;; The letrec body must already be lifted.
	   ;; ERROR ERROR: Need to extend env.
	   ;(inspect (assq var binds))
	   ;(get-tail (rac (rac (assq var binds))) env)
	   (let ([cl (assq var binds)])
	     (get-tail (codeloc->expr cl) cl 
		       (extend-dfg env (map car binds) binds)))]
	  ;; Can't go deeper:
	  [(if ,test ,conseq ,altern)  currentloc]
	  [,expr                       currentloc]))

    ;; This takes an expression of type Region 'a, and tries to return
    ;; the piece of code which generates an *element* of that region.
    ;; Expression, Codeloc, DFG -> Codeloc
    (define (get-region-tail expr env)
      (DEBUGASSERT (dfg? env))
      ;; The currentloc should only be false of the expr IS a letrec.
      ;(DEBUGASSERT (or currentloc (symbol? expr) (and (list? expr) (eq? 'lazy-letrec (car expr)))))
      (let ([result
	;; If the type is Region, then the member elements are type
	;; Node, and the only thing that generates that is the world itself.
	(if (let ([ty (recover-type expr global-tenv)])
	      (or (equal? ty 'Region)
		  (equal? ty '(Area Node))))
	    worldsym

	    (match expr
	      [,var (guard (symbol? var)) 
		    (let ([cl (lookup var env)])		      
		      (get-region-tail (codeloc->expr cl) env))]
	      [(quote ,const) (error 'get-region-tail "const: ~s" const)]

	      [(lazy-letrec ,binds ,var)
	       (let ([cl (assq var binds)])		 
		 (get-region-tail (codeloc->expr cl) env))]

	      [(khood ,_ ,__) 
	       (error 'get-region-tail "Shouldn't get to this case, should have been caught above.")]

	      [(rmap ,rat ,rand)
	       (ASSERT (simple-expr? rand))
	       (if (regiment-primitive? rat)
		   ;rat
		   (error 'get-region-tail "non-eta'd primitive as rmap operator: ~a" rat)
		   (let ([newrand (process-expr rand env)])
		     (match (or (and (symbol? rat) (codeloc->expr (lookup rat env)))
				rat)
		       [(lambda (,v) ,ty ,expr)
			(get-tail expr #f env)])))]

	      ;; An rfilter doesn't change the value-producing program point.
	      [(rfilter ,rat ,rand)  (get-region-tail rand env)]
	      

	      [(liftsig ,areasig) (get-region-tail areasig env)]

	      [,expr (error 'get-region-tail "unhandled expression: ~s\n" expr)]
	      [,expr `(NOTHIN ,expr)])
	    )])
	(DEBUGASSERT (or (codeloc? result) (eq? worldsym result)))
	result
	))

    ;; This returns a list of [<Var> <Expr>] bindings associating variables
    ;; to the primitive application expressions that generate them.
    ;; .param expr The expression to process.
    ;; .param env  The data-flow bindings established thus far.
    (define process-expr
      (lambda (expr env)
        (DEBUGASSERT (dfg? env))
        (let ([result (match expr
          [(quote ,const)             ()]
          [,var (guard (symbol? var)) ()]

	  ;; TODO: FIXME: HANDLE SMAP AS WELL:

	  [(,mapfun ,rat ,rand)
	   (guard (memq mapfun '(rmap rfilter)))
	   (let ([randbinds (process-expr rand env)])
             ;(if (not (null? newbinds)) (inspect newbinds))
	     (match (or (and (symbol? rat) (codeloc->expr (lookup rat env)))
			rat)
	       [(lambda (,v) ,ty ,bod)
                ;; First, produce a data-flow binding for the lambda's argument.
                (let* ([lambind `[,v ,(get-region-tail rand (extend-dfg env randbinds))]]
                       [newbinds (cons  lambind randbinds)])
                  ;; Now, produce bindings for the body of the lambda.

                  (append (process-expr bod (extend-dfg env (list (car lambind))
							(list (cadr lambind))))
			  newbinds)
                  )]
               [,other (error 'add-data-flow:process-expr "could not find code for rator: ~s" rat)]
               ))]

	  ;; We accumulate "data flow" information as we go through the bindings.
	  ;; NOTE NOTE:  This assumes that they're ordered and non-recursive.
	  [(lazy-letrec ,binds ,tail)
	   (DEBUGASSERT (symbol? tail))

           ;; The lack of free variables within lambdas makes this easy.
	   ;; We scan down the whole list of bindings before we go into the lambdas in the RHSs.
	   (let* ([letrecbinds (map list (map car binds) binds)]
                  [newenv (extend-dfg env (map car binds) binds)]
		  [innerbinds 
		   (let declloop ([binds binds] ;(delazy-bindings binds (list tail))]
				  [newbinds ()])
			      (if (null? binds) newbinds
				  (match (car binds)
				    [[,lhs ,ty ,annots ,rhs]
				     ;; This RHS can only depend on dataflow information 
				     ;; INSIDE bindings that come before it:
				     (let ([innards (process-expr rhs (extend-dfg newenv newbinds))])
				       (declloop (cdr binds)
						 (append ;`([,lhs ,rhs])						  
							 innards newbinds)))])))])
	     ;; This should not happen:
	     ;(DEBUGASSERT (null? (intersection (map car innerbinds) (map car letrecbinds))))
	     ;(inspect (vector (map car innerbinds) (map car letrecbinds)))
	     (append innerbinds letrecbinds)
	     ;(list->set (append innerbinds letrecbinds))
	     ;innerbinds
	     )]

	  [(lambda ,v* ,ty* ,[bod]) bod]
          [(if ,[t] ,[c] ,[a]) (append t c a)]
          [(,prim ,[rand*] ...)	   
           (guard (regiment-primitive? prim))
	   (apply append rand*)]

          [,unmatched
	   (error 'add-data-flow "invalid syntax ~s" unmatched)])])
	  
	  ;; Invariant: The result that we return should not overlap
	  ;; with the environment that we take from above.  Otherwise
	  ;; we'd produce duplicates.
;; TEMP:: DISABLING THIS CHECK:
;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
#;
	  (DEBUGMODE 
	   (let ([overlap (intersection (map car env) (map car result))])
	     (ASSERT (null? overlap))))
	  
	  result)))

    ;; Doing some internal testing here:
    ;--------------------------------------------------------
    (set! these-tests
	  (append `(["Just a letrec"
		     (map car (,process-expr 
		      '(lazy-letrec
			((resultofanonlambda_8 Integer ((heartbeat #f)) '389))
			resultofanonlambda_8)
		      (,empty-dfg)))
		     (resultofanonlambda_8)]
		    ["Nested letrec on rhs"
		     (map car (,process-expr 
		      '(lazy-letrec
			((resultofanonlambda_8 Integer ()
					       (lazy-letrec ((var_2 Integer () '389))
							    var_2)))
			resultofanonlambda_8)
		      (,empty-dfg)))
		     ,(lambda (x)
			(set-eq? x '(var_2 resultofanonlambda_8)))]
		    ["Two bindings, one nested letrec"
		     (map car (,process-expr 
		      '(lazy-letrec ([resultofanonlambda_8 Integer () '89]
				     [var_2 Integer () 
					    (lazy-letrec ([foo Integer () '100]
							  [res1 Integer () (+ foo '389)])
						    res1)]
				     [res2 Integer () (+ resultofanonlambda_8 var_2)])
				    res2)
		      (,empty-dfg)))
		     ,(lambda (x)
			(set-eq? x '(foo res1 resultofanonlambda_8 var_2 res2)))]

		    ["Data flow graph?"
		     (,dfg? '((x ,worldsym)
			      (f (f _ () (lambda (x) (_) x)))
			      (v (v _ () (rmap f world)))))
		     #t]

		    )
		  these-tests))
    ;--------------------------------------------------------
    
    (lambda (expr)
      (match expr
	[(,input-language (quote (program (props ,proptable ...) 
				   (control-flow ,cfg ...)
				   ,letexpr
				   ,type)))
	 (set! global-tenv (extract-whole-tenv letexpr))

	 (let ([dfg (process-expr letexpr (empty-dfg))])
	   
	   ;; INVARIANT: Make sure we got all the bindings and only the bindings.

;; TEMP:: DISABLING THIS CHECK:
;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
#;
	   (DEBUGMODE (let ([allvars (expr->allvars letexpr)])
			(DEBUGASSERT (= (length allvars) (length dfg)))
			(DEBUGASSERT (set-equal? allvars (map car dfg)))))

	   ;;(inspect global-tenv)
	   `(,input-language (quote (program (props ,proptable ...) 
				      (control-flow ,cfg ...)
				      (data-flow ,@dfg)
				      ,letexpr
				      ,type))))
	 ])))))

;================================================================================

(define nested_testprog 
  `(lang '(program (props )
	    (control-flow )	   
	    (lazy-letrec
	     ([w Region () world]
	      [h (Area Region) ()
		 (rmap (lambda (n) (Node) 
			       (lazy-letrec ([a Anchor () (node->anchor n)]
					     [kh Region () (khood a '2)])
					    kh))
		       w)]
	      [getid (Node -> Integer) ()
		     (lambda (nd) (Node) (nodeid nd))]
	      [h2 (Area (Area Integer)) ()
		  (rmap (lambda (r1) (Region) 
				(lazy-letrec ([res_r1 (Area Integer) ()
						      (rmap getid r1)])
					     res_r1))
			h)]
	      [v (Area Integer) ()
		 (rmap (lambda (r2) ((Area Integer)) 
			       (rfold + '0 r2)) h2)]
	      )
	     v)
	    (Area Integer))))

(define test-this
  (default-unit-tester 
    "Pass 17: Add data flow"
    (append these-tests
    `(
      ["Does it produce the right bindings on a simple example?"
       (,deep-assq 'data-flow
	(add-data-flow 
	 `(lang '(program (props )
		   (control-flow )
		   (lazy-letrec
		    ([f _ () (lambda (x) (_) x)]
		     [v _ () (rmap f world)])
		    v)
		   _))))
       ,(lambda (x) 
	  (and (pair? x) (not (null? x))
	       (set-equal? 
#;
		(map (lambda (b) 
		       (if (eq? worldsym (cadr b)) worldsym
			   (list (car b) (last (cadr b)))))
		  (cdr x))
#;
		`((v (rmap f world)) 		    
		  (x ,worldsym)		
		  (f (lambda (x) (_) x)))

		(cdr x)
		`((x ,worldsym)
		  (f (f _ () (lambda (x) (_) x)))
		  (v (v _ () (rmap f world))))
		)))]

      ["Now let's look at nested regions."
       (assq 'r2
	     (cdr (,deep-assq 'data-flow
			      (add-data-flow ',nested_testprog))))
       ;(r2 (rmap getid r1))
       ;; [2006.04.04] Now it's bound to the code location with the canonical name:
       (r2 [res_r1 (Area Integer) () (rmap getid r1)])
       ]

      ["Make sure it generates all the bindings it should."
       (map car
            (cdr (,deep-assq 'data-flow (add-data-flow ',nested_testprog))))
       ,(lambda (x)
         (set-equal? x 
		     ;'(r2 nd r1 n h h2 getid v ret)
		     '(r2 nd res_r1 r1 a kh n w h getid h2 v)
		     ))]

      ))))


(define test17b test-this)
(define test-add-data-flow test-this)

) ;; End module

;(require pass17_add-data-flow)  (test17b)


