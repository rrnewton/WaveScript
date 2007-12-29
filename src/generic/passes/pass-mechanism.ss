;;;; .title Fancy syntactic sugar for building pass objects.
;;;; .author Ryan Newton
;;;; 
;;;; This file declares "define-pass" which is a super-sweet syntactic
;;;; sugar for concisely defining pass-transformations.  It lets you
;;;; optionally provide only the cases you need.

(module pass-mechanism mzscheme 
  (require "../../plt/iu-match.ss"
           "../util/helpers.ss"
           (only "../constants.ss" chezimports ASSERT DEBUGASSERT define-testing cond-expand)
           "../compiler_components/regiment_helpers.ss"
           "../compiler_components/reg_core_generic_traverse.ss"
	   "../compiler_components/hm_type_inference.ss"
	   
	   "pass-mechanism_basic.ss" 
           )

  (provide define-pass fuse-passes/disjoint fuse-passes
	   test-pass-mechanism
	   ;P1 P2 F G
	   )
  (chezimports)
  (cond-expand 
   [plt 
    (require-for-syntax "../../plt/chez_compat.ss"
					;(only "../util/helpers.ss" compose)
			"../compiler_components/reg_core_generic_traverse.ss")]
   [else])

;; Usage:
;;   (define-pass <pass-name> <clauses and defs> ...)
;; Clauses can be:
;;   [InputGrammar <g>]
;;   [OutputGrammar <g>]
;;      These define input/output grammars for the pass in the style of grammar_checker.ss.
;;      These grammars are checked only in debugmode. See the obsoleted build-compiler-pass function.
;;
;;   [Program <fun: prog, ExprFun -> pass-result>]
;;      This form takes a user procedure to handle the top-level input to the pass, the "program".
;;
;;   [Expr <fun: expr, FallthroughFun -> intermediate>]
;;      This form takes an expression-handling procedure in the style of core-generic-traverse.
;;   [Expr/Types <fun: expr, type-env, FallthroughFun -> intermediate>]
;;      This form takes an expression-handling procedure in the style of core-generic-traverse/types.
;;      Cannot be used with Expr, or Bindings.
;;
;;   [Fuser <fun: [intermediate ...], <fun: [intermediate ...] -> expr> -> intermediate>]
;;      This form takes a result-fusing procedure in the style of core-generic-traverse.
;;
;;   [Bindings <fun: vars types exprs reconstr exprfun -> intermediate>]
;;      UNFINISHED:
;;
;;      This has a complex interface, but allows abstraction over
;;      *all* variable binding forms in the Regiment/Wavescript
;;      language.  The user function is stuck *between* the "Expr"
;;      function and the auto-traversal function provided by
;;      core-generic-traverse.  That is, for a given expression, the
;;      "Expr" function gets the first shot at it.  If it passes the
;;      expression on to its "continuation", then the "Bindings"
;;      function gets a shot.  (The bindings function will catch any
;;      binding form.)  And finally, if the expression is not a
;;      binding form it falls through to the auto-traversal function.
;;
;;      The user function provided takes six arguments:
;;         vars:  The variables bound within this scope.
;;         types: The types of those variables.
;;         exprs: The expressions within the scope.
;;         reconstr:       Function to reconstruct a binding expression from vars, types, exprs.
;;         exprfun:        Entrypoint to the complete expression-handling procedure.  (For use on subexpressions.)
;;
(define-syntax define-pass
  (lambda (x)
    (syntax-case x ()
      [(_ name clauses ...)
       (let ([gen-code 
	      (lambda (ingram outgram bnds expr/types expr prog fuser extra-defs)

		(define output-language 
		  (datum->syntax-object #'name
		   (string->symbol 
		    (string-append (symbol->string (syntax-object->datum #'name)) "-language"))))
		(define inspec (if ingram 
				   (with-syntax ([ig ingram]) #'`(input (grammar ,ig PassInput)))
				   #''(input)))
		(define outspec (if outgram 
				    (with-syntax ([og outgram]) #'`(output (grammar ,og PassInput)))
				    #''(output)))
		(define gentrav
		  (begin 
		    (if (and expr expr/types)
			(syntax-error (format "Cannot have both Expr and Expr/Types clause in define-pass:\n ~s" #'_)))
		    (if expr/types
			#'(lambda (d f e . tenv) 
			    (if (null? tenv) 
				(core-generic-traverse/types d f e)
				(core-generic-traverse/types d f e (car tenv))))
			#'(lambda (d f e . _) (core-generic-traverse d f e)))))

		(define default-expr (or expr expr/types #'(lambda (x fallthrough) (fallthrough x))))
		(define default-fuser (or fuser #'(lambda (ls k) (apply k ls))))
		(define default-prog 
		  (with-syntax ([ol output-language])
		    #'(lambda (pr Expr) 
			(match pr
			  [(,input-language (quote (program ,body . ,metadata )))
			   ;; The last entry in metadata is a type!
			   ;; Should associate a tag with it so that it can be treated the same.
			   (let* ([realmeta (rdc metadata)]
				  [initial-tenv (grab-init-tenv realmeta)]
				  [uniondefs (or (assq 'union-types realmeta) '(union-types))])
			     ;; Ensure that the output has a (union-types) form.
			     `(ol '(program ,(Expr body initial-tenv) 
				     ;; This forces that there be a union-types entry in the output:
					;,uniondefs ,@(remq uniondefs metadata)
				     . ,metadata
				     )))]
			  [,other (error 'name "\nBad pass input:\n   ~s\n" other)])
			)))

		;; Fill in some defaults:
		(set! expr  (or expr  default-expr))
		(set! fuser (or fuser default-fuser))
		(set! prog  (or prog  default-prog))

		;; If the user provides a function for variable bindings, we
		;; paste that together with the "Expr" function.
		;; [2006.11.01] Changed this to not use quasisyntax
		(if bnds 
		    (set! expr
		    (with-syntax ([expr expr] 
				  [fuser fuser]
				  [funbody bnds]
				  [ellipses (datum->syntax-object #'_ '...)]
				  )
		      #'(lambda (x fallthrough)
			  (define fuse fuser)
			  (expr x 
				;; We stick the varbinds handling between the user code and the automatic looping.
				(lambda (x)
				  ;; Now we set up the varbinds and give them to the userfun.
				  (let ([fun funbody])
				    (match x

				      [(lambda ,vars ,types ,bod)
				       (fun vars types (list bod) 
					    ;; Reconstruct function:
					    (lambda (vars types results)
					      (fuser results
						     (lambda (bod)			
						       `(lambda ,vars ,types ,bod)
						       )))
					    ;; Expression function:
					    process-expr)]

				      [(letrec ,binds ,body)
				       (guard (andmap (lambda (ls) (= (length ls) 3)) binds))
				       (let ([vars (map car binds)]
					     [types (map cadr binds)]
					     [rhs*  (map caddr binds)])
					 (fun vars types (cons body rhs*)
					      (lambda (vars types exprs)
						(fuser exprs
						       (lambda (bod . rhs*)
							 `(letrec ,(map list vars types rhs*) ,bod)
							 )))
					      process-expr))]

				      [(let ,binds ,body)
				       (guard (andmap (lambda (ls) (= (length ls) 3)) binds))
				       (let ([vars (map car binds)]
					     [types (map cadr binds)]
					     ;; We do the RHSs because the user doesn't get them:
					     [rhs*  (map process-expr (map caddr binds))])
					 (fun vars types (list body)
					      (lambda (vars types results)
						(DEBUGASSERT (= 1 (length results)))
						(fuser (append results rhs*)
						       (lambda (bod . newrhs*)
							 (DEBUGASSERT (= (length newrhs*) (length rhs*)))
							 `(let ,(map list vars types newrhs*)
							    ,bod))))
					      process-expr))]

				      [(for (,i ,[process-expr -> st] ,[process-expr -> en]) ,bod)
				       (fun (list i) '(Int) (list bod)
					    (lambda (vars types results)
					      (ASSERT (equal? types '(Int)))
					      (fuser (list st en (car results))
						     (lambda (st en bod) 
						       `(for (,(car vars) ,st ,en) ,bod))))
					    process-expr)]

				      ;; To catch errors:
				      [(,head ,other ellipses)
				       (guard (memq head '(for letrec let let* lazy-letrec lambda)))
				       (error 'pass-mechanism:Bindings 
					      "unhandled or badly formed binding syntax: ~s" 
					      (cons head other))]

				      ;; TODO: for, ... anything else?
				      
				      ;; If it's not a variable-binding construct, we just let it 
				      ;; go through to the final, auto-looping fallthrough:
				      [,other 
				       (ASSERT (compose not binding-form?) other)
				       (fallthrough other)]
				      ))))))))






		(with-syntax ([(i o b e p f inspec outspec (extra-defs ...) gentrav)
			       (list ingram outgram bnds expr prog fuser
				     inspec outspec extra-defs gentrav)])
		  ;; Now we plug in defaults:
		  #;
		  (if (and prog (or bnds expr))
		      (error 'define-pass "cannot define both Program and Bindings/Expr: ~s" 
			     #'_))

		  #'(define name 
		      (let () 
			extra-defs ...
			;; Jeez, this is lame but just so the compiler associates the name with the closure:
			(letrec ([name (lambda (prog) (tmp prog))]
				  [process-expr (lambda args (apply gentrav e f args))]
				  [tmp (build-compiler-pass 
					'name 
					inspec 

					outspec 
					(lambda (x) 					  
					  (if (eq? x 'get-expr-driver)
					      ;; If we get this message we just return our driver function:
					      e ;; The unadorned driver function.					      
					      (p x process-expr))))])
				 name
				 )))

		))])
	 ;; Here are all the default values for these components:
	 (let loop ([cl (syntax->list #'(clauses ...))]
		    [ingram #f]
		    [outgram #f]
		    [bnds #f]
		    [expr/types #f]
		    [expr #f]
		    [prog #f]
		    [fuser #f]
		    [extras '()])
	   #|
	   (let ([tf (lambda (x) (if x #t #f))])
	     (printf "loop ~s ~s ~s ~s e/t:~s e:~s ~s ~s ~s\n"
		     (length cl)
		     (tf ingram) (tf outgram) (tf  bnds) (tf expr/types) (tf expr) (tf prog) (tf fuser) (tf  extras)))
	   |#

	   (if (null? cl)
	       (gen-code ingram outgram bnds expr/types expr prog fuser (reverse extras))
	       (syntax-case (car cl) (InputGrammar OutputGrammar Bindings Expr/Types Expr Program Fuser)
			    [(InputGrammar g)  (loop (cdr cl) #'g    outgram bnds expr/types expr prog fuser extras)]
			    [(OutputGrammar g) (loop (cdr cl) ingram #'g     bnds expr/types expr prog fuser extras)]
			    [(Bindings f)      (loop (cdr cl) ingram outgram #'f  expr/types expr prog fuser extras)]
			    [(Expr/Types f)    (loop (cdr cl) ingram outgram bnds #'f        expr prog fuser extras)]
			    [(Expr f)          (loop (cdr cl) ingram outgram bnds expr/types #'f  prog fuser extras)]
			    [(Program f)       (loop (cdr cl) ingram outgram bnds expr/types expr #'f  fuser extras)]
			    [(Fuser f)         (loop (cdr cl) ingram outgram bnds expr/types expr prog #'f   extras)]
			    [else (loop (cdr cl) ingram outgram bnds expr/types expr prog fuser 
					(cons (car cl) extras))]
			    ))))
       ])))

;; This has not been sufficiently tested yet:
(define (fuse-passes/disjoint P1 P2)
  (define A (P1 'get-expr-driver))
  (define B (P2 'get-expr-driver))
  (trace-define fused
      (lambda (x fallthru)
	(A x (lambda (y) ;; y may be either straight fallthru or a subexpr
	  ;; Either way it goes into B:
	  (B y (lambda (z)
		 (if (eq? y z)
		     ;; If it fell through B, we know A was done with it and it's ready for fallthru:
		     (fallthru z)
		     ;; This indicates a match-recursion has occured.
		     ;; z is a subexpression of y.
		     ;; We need to start back at the top:
		     (fused z fallthru))))))))
  (lambda (p) (apply-to-program-body (core-generic-traverse fused) p)))

;; This has not been sufficiently tested yet:
;;
;; Whenever an expression *IS* handled by A, we must reprocess the
;; entire resulting subtree by B.
(define (fuse-passes P1 P2)
  (define A (P1 'get-expr-driver))
  (define B (P2 'get-expr-driver))
  (define A-pass (core-generic-traverse A))
  (define B-pass (core-generic-traverse B))
  (define fused
    (lambda (x fallthru)
      (let* ([redo-this-tree? #t]
	     [result
	     (A x (lambda (y) ;; y may be either straight fallthru or a subexpr
		    (if (eq? y x)
			;; Only if x fell straight through may we pass on to B.
			(begin 
			  (set! redo-this-tree? #f)
			  (B y (lambda (z)
			       (if (eq? y z)
				   (fallthru z)
				   ;; This indicates a match-recursion has occured.
				   ;; We need to start back at the top.
				   (fused z fallthru)))))
			;; Otherwise we mark this entire subtree as needing to be 
			;; reprocessed by B and we continue normally applying only A.
			(begin
			  ;(set! redo-this-tree? #t)
			  (A-pass y))
			)))])
	(printf "   RESULT: ~s\n" result)
	(if redo-this-tree?
	    (B-pass result)
	    result))))
  (core-generic-traverse fused))




;(eval '(define-pass P1 [Expr (lambda (x f) (if (equal? x ''3) ''333 (f x)))]))
;(eval '(define-pass P2 [Expr (lambda (x f) (if (equal? x ''4) ''444 (f x)))]))
;(eval '(define F (fuse-passes/disjoint P1 P2)))

#;
(begin (define-pass P1 [Expr (lambda (x f) (match x ['3 ''333] [,_ (f x)]))])
       (define-pass P2 [Expr (lambda (x f) (match x ['4 ''444] [,_ (f x)]))])
       (define-pass P3 [Expr (lambda (x f) (match x [',n (guard (number? n)) `',(add1 n)] [,_ (f x)]))])
       (define F (fuse-passes/disjoint P1 P3))
       (define G (fuse-passes P1 P3))
       (print-graph #f)
       )


(define-testing these-tests
  '(
    ["Disjoint pass fusion."
     (let ()
       (define-pass P1 [Expr (lambda (x f) (match x ['3 ''333] [,_ (f x)]))])
       (define-pass P3 [Expr (lambda (x f) (match x [',n (guard (number? n)) `',(add1 n)] [,_ (f x)]))])
       (define F (fuse-passes/disjoint P1 P3))
       (F '(lang '(program (_+_ '3 (*_ '1 (_-_ '4 '3))) 'ty))))
     (lang '(program (_+_ '333 (*_ '2 (_-_ '5 '333))) 'ty))]
    ["Verify that the non-disjoint fusion actually applies the second pass to the output of the first."
     (let ()
       (define-pass P1 [Expr (lambda (x f) (match x ['3 ''333] [,_ (f x)]))])
       (define-pass P3 [Expr (lambda (x f) (match x [',n (guard (number? n)) `',(add1 n)] [,_ (f x)]))])
       (define G (fuse-passes P1 P3))
       ;(G '(lang '(program (_+_ '3 (*_ '1 (_-_ '4 '3))) 'ty)))
       (G '(_+_ '3 (*_ '1 (_-_ '4 '3)))))
     (_+_ '334 (*_ '2 (_-_ '5 '334)))]))


(define-testing test-pass-mechanism 
  (default-unit-tester "Machinery for building compiler passes."
    these-tests))


#;
     (lambda (x fallthru)
       (let fploop ((x x))
	 (E1 x (lambda (y) 
           ;; If they're the same then 'x' was just passed through.
	   (E2 y (lambda (z)
	     (if (eq? y z)
		 ;; If it passed through E2, on to auto-traverse:
		 (fallthru z)
		 ;; E2 handled y and 'z' is a sub-expression of y.
		 ;; But E1 hasn't had a shot at z yet.
		 ;; Start again at the top:
		 (fploop z)))))))
       )


 




) ;; End module.

;(require pass-mechanism)
