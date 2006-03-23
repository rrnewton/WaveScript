

;;;; .title Pass: Add Data Flow
;;;; .author Ryan Newton

;;;; This pass undoes the control indirection introduced by maps/filters/folds.
;;;; It explicitely attaches a table to the output program which binds
;;;; variable names to the expression that generates that data value.

(module pass17_add-data-flow mzscheme

   (require 
	   "../generic/constants.ss"
;           "../plt/chez_compat.ss"
           "../plt/iu-match.ss"
	   "../plt/prim_defs.ss"
           (all-except "../plt/hm_type_inference.ss"  test-this these-tests)
           (all-except "../plt/grammar_checker.ss" test-this these-tests)
           (all-except "../plt/helpers.ss" test-this these-tests)
;           (all-except "../plt/regiment_helpers.ss" test-this these-tests)
           )

;  (provide deglobalizeh 
;;	   test-this these-tests test12 tests12)
#;  (provide deglobalize test20 tests20 test-deglobalize tests-deglobalize 
           ;; Temporarily exposing.
           delazy-bindings)

  (provide add-data-flow test-this test17b test-add-data-flow)

  (chezimports )

(define worldsym 'THEWORLD)

(define add-data-flow
  (build-compiler-pass ;; This wraps the main function with extra debugging
   'add-data-flow
   `(input)
   `(output )
  (let ()

    (define process-primapp 0)
    (define global-tenv 'uninitialized)

    (define (lookup s ls) (cadr (assq s ls)))

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
	[(lazy-letrec ([,v* ,ty* ,annotations ... ,[rhs*]] ...) ,[bod])
	 (apply tenv-append (tenv-extend bod v* ty* #t) rhs*)]
	
	[(,prim ,[rand*] ...) (guard (regiment-primitive? prim))
	 (apply tenv-append rand*)]
	[,other (error 'extract-whole-tenv "bad regiment expression: ~s" other)]
	))

    (trace-define (get-tail expr env)
      (let loop () 
	(match expr
	  [,var (guard (symbol? var)) 
		(get-region-tail (lookup var env) env)]	      
	  [(lazy-letrec ,binds ,var)
	   ;; ERROR ERROR: Need to extend env.
	   (get-tail (rac (assq var binds)) env)]
	  ;; Can't go deeper:
	  [(if ,test ,conseq ,altern)         expr]
	  [,expr expr])))

    ;; This takes an expression of type Region 'a, and tries to return
    ;; the piece of code which generates an *element* of that region.
    (trace-define (get-region-tail expr env)
      (let loop () 
	;; If the type is Region, then the member elements are type
	;; Node, and the only thing that generates that is the world itself.
	(if (let ([ty (recover-type expr global-tenv)])
	      (or (equal? ty 'Region)
		  (equal? ty '(Area Node))))
	    worldsym

	    (match expr
	      [,var (guard (symbol? var)) 
		    (get-region-tail (lookup var env) env)]
	      [(lazy-letrec ,binds ,var)
	       (get-region-tail (last (assq var binds))
				env)]

	      [(rmap ,rat ,rand)
	       (if (regiment-primitive? rat)
		   ;rat
		   (error 'get-region-tail "non eta'd primitive as rmap operator: ~a" rat)
		   (let ([newrand (process-expr rand env)])
		     (match (or (and (symbol? rat) (lookup rat env))
				rat)
		       [(lambda (,v) ,ty ,expr)
			(get-tail expr env)
			;(get-region-tail expr env)
			]))
		   )]

	      [,expr `(NOTHIN ,expr)])
	    )))

    ;; This returns a list of [<Var> <Expr>] bindings from variables,
    ;; to the primitive application expressions that generate them.
    (trace-define process-expr
      (lambda (expr env)
        (trace-match P-E expr
          [(quote ,const)             ()]
          [,var (guard (symbol? var)) ()]

	  [(rmap ,rat ,rand)
	       (let ([newbinds (process-expr rand env)])
		 (match (or (and (symbol? rat) (lookup rat env))
				rat)
		   [(lambda (,v) ,ty ,expr)
		    `([,v ,(get-region-tail rand (append newbinds env))])]))
	   ]

	  ;; We accumulate "data flow" information as we go through the bindings.
	  ;; NOTE NOTE:  This assumes that they're ordered and non-recursive.
	  [(lazy-letrec ,binds ,tail)
           ;; The lack of free variables within lambdas makes this easy.
	   ;; We scan down the whole list of bindings before we go into the lambdas in the RHSs.
	   (append 

	    (trace-let loop ([binds binds] [newenv ()])
	      (match binds
		[()  newenv]
		[([,lhs ,ty #|,annots ...|# ,rhs] . ,rest)
		 
		 (loop rest 
		       (append `([,lhs ,rhs])
			       (process-expr rhs (append newenv env))
			       newenv))]))
	    #;	 
	    (let loop ([binds binds] [env ()])
	      (match binds
		[()  env]
		[([,lhs ,ty ,rhs] . ,rest)
		 (loop rest (append (process-expr rhs env) env))]))
	    env
	    )]

	  [(lambda ,v* ,ty* ,expr)
	   (append (process-expr expr env) env)]

#;
          [(if ,test ,conseq ,altern)
	   (values `(if ,test ,conseq ,altern) unknown-place unknown-place)]

          [(,prim ,[rand*] ...)	   
           (guard (regiment-primitive? prim))
	   (apply append rand*)]

          [,unmatched
	   (error 'add-data-flow "invalid syntax ~s" unmatched)])))
    
    (lambda (expr)
      (match expr
	[(,input-language (quote (program (props ,proptable ...) 
				   (control-flow ,cfg ...)
				   ,letexpr
				   ,type)))
	 
	 (set! global-tenv (extract-whole-tenv letexpr))
;	 (inspect global-tenv)
	 (process-expr letexpr ())

	 ])))))

(define test-this
  (default-unit-tester 
    "Pass 17: Add data flow"
    `(

      ["Does it produce the right bindings on a simple example?"
       (add-data-flow 
	`(lang '(program (props )
		   (control-flow )	   
		   (lazy-letrec
		    ([f _ (lambda (x) (_) x)]
		     [v _ (rmap f world)])
		    v)
		   _)))
	((v (rmap f world)) (x ,worldsym) (f (lambda (x) (_) x)))]

      ["Now let's look at nested regions."
       (assq 'r2
	     (add-data-flow 
	      `(lang '(program (props )
			(control-flow )	   
			(lazy-letrec
			 ([h (Area Region)
			     (rmap (lambda (n) (Node) (khood (node->anchor n) 2)) world)]
			  [h2 (Area (Area Integer))
			      (rmap (lambda (r1) (Region) (rmap getid r1)) h)]
			  [getid (Node -> Integer)
				 (lambda (nd) (Node) (nodeid nd))]
			  [v (Area Integer)
			     (rmap (lambda (r2) ((Area Integer)) 
					   (rfold + 0 r2)) h2)]
			  )
			 v)
			(Area Integer)))))
       (r2 (rmap getid r1))]

      )))


(define test17b test-this)
(define test-add-data-flow test-this)

) ;; End module

#| 




...

|#