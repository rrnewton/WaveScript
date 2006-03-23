

;;;; .title Pass: Add Data Flow
;;;; .author Ryan Newton

;;;; This pass undoes the control indirection introduced by maps/filters/folds.
;;;; It explicitely attaches a table to the output program which binds
;;;; variable names to the expression that generates that data value.

(module pass17_add-data-flow mzscheme

   (require (lib "include.ss")
	   (lib "trace.ss")
	   "../generic/constants.ss"
           "../plt/iu-match.ss"
           "../plt/hashtab.ss"
	   "../plt/prim_defs.ss"
           "../plt/grammar_checker.ss"
           (all-except "../plt/tsort.ss" test-this these-tests)
           (all-except "../plt/tml_generic_traverse.ss" test-this these-tests)
           (all-except "../plt/helpers.ss" test-this these-tests)
           (all-except "../plt/regiment_helpers.ss" test-this these-tests))

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

    (trace-define (lookup s ls) (cadr (assq s ls)))

    (define (letrec-extend-tenv tenv binds)      
      (tenv-extend tenv
		   (map car binds) 
		   (map cadr binds)))

    (trace-define (get-tail expr)
      (let loop () 
	(match expr
	  [(lazy-letrec ,binds ,var)
	   (get-tail (cadr (assq var binds)))]
	  ;; Can't go deeper:
	  [(if ,test ,conseq ,altern)         expr]
	  [,expr expr])))

    ;; This takes an expression of type Region 'a, and tries to return
    ;; the piece of code which generates an *element* of that region.
    (trace-define (get-region-tail expr env tenv)
	(DEBUGASSERT (tenv? tenv))
      (let loop () 
	;; If the type is Region, then the member elements are type
	;; Node, and the only thing that generates that is the world itself.
	(if (let ([ty (recover-type expr tenv)])
	      (or (equal? ty 'Region)
		  (equal? ty '(Area Node))))
	    worldsym
	    (match expr

	      ;; Khood just returns a region of nodes.
					;[(khood ,_ ,__) worldsym]
					;[world worldsym]

	      [,var (guard (symbol? var)) 
		    (get-region-tail (lookup var env) env tenv)]
	      [(lazy-letrec ,binds ,var)
	       (get-region-tail (cadr (assq var binds))
				env
				(letrec-extend-tenv tenv binds))]

	      [(rmap ,rat ,rand)
	       (let ([newrand (process-expr rand env tenv)])
		 (match (lookup rat env)
		   [(lambda (,v) ,ty ,expr)
		    (get-region-tail expr env tenv)]))]

	      [,expr 'NOTHIN])
	    )))

    (trace-define process-expr
      (lambda (expr env tenv)
	(DEBUGASSERT (tenv? tenv))
        (trace-match P-E expr
          [(quote ,const)             ()]
          [,var (guard (symbol? var)) ()]

	  [(rmap ,rat ,rand)
	   (let ([newbinds (process-expr rand env tenv)])
	     (match (lookup rat env)
	       [(lambda (,v) ,ty ,expr)
		`([,v ,(get-region-tail rand (append newbinds env) tenv)])]))]

	  ;; We accumulate "data flow" information as we go through the bindings.
	  ;; NOTE NOTE:  This assumes that they're ordered and non-recursive.
	  [(lazy-letrec ,binds ,tail)
	   (let ([newtenv (letrec-extend-tenv tenv binds)])
	   ;; The lack of free variables within lambdas makes this easy.
	   ;; We scan down the whole list of bindings before we go into the lambdas in the RHSs.
	   (append 

	    (trace-let loop ([binds binds] [newenv ()])
	      (match binds
		[()  newenv]
		[([,lhs ,ty ,rhs] . ,rest)
		 
		 (loop rest 
		       (append `([,lhs ,rhs])
			       (process-expr rhs (append newenv env) newtenv)
			       newenv))]))
	    #;	 
	    (let loop ([binds binds] [env ()])
	      (match binds
		[()  env]
		[([,lhs ,ty ,rhs] . ,rest)
		 (loop rest (append (process-expr rhs env) env))]))
	    env
	    )
	   )]

	  [(lambda ,v* ,ty* ,expr)
	   (append (process-expr expr env 
				 (tenv-extend tenv v* ty*))
		   env)]

#;
          [(if ,test ,conseq ,altern)
	   (values `(if ,test ,conseq ,altern) unknown-place unknown-place)]
#;
          [(,prim ,rand* ...)	   
           (guard (regiment-primitive? prim))
	   (process-primapp prim rand*)]

          [,unmatched
	   (error 'add-data-flow "invalid syntax ~s" unmatched)])))
    
    (lambda (expr)
      (match expr
	[(,input-language (quote (program (props ,proptable ...) 
				   (control-flow ,cfg ...)
				   ,letexpr
				   ,type)))
	 (process-expr letexpr () (empty-tenv))

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


      )))


(define test17b test-this)
(define test-add-data-flow test-this)

) ;; End module

#| 




...

|#