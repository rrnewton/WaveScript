
;;;; Pass: Eta-Primitives
;;;; [2004.06.24]

;;;; This simple pass eta expands all operand-context references to
;;;; primitives, hereafter primitive-names will only occur in operator
;;;; context.  This is just a simple, regularizing pass.

(module eta-primitives mzscheme
  (require "../../../plt/common.ss"
           (all-except "rename-vars.ss"  these-tests test-this)
	   )
  (provide eta-primitives eta-primitives-grammar test-eta-primitives)
  (chezimports)

  ;; In the output grammar varrefs are no longer allowed to refer to primitives.
  (define eta-primitives-grammar
    (let* ([base-grammar rename-vars-grammar]
	   [varclause (assq 'Var base-grammar)]
	   [newgram (remq varclause base-grammar)]
	   [new_is-var? 
	    (lambda (x)
	      (and (symbol? x)
		   (not (token-machine-keyword? x))
		   (or (regiment-constant? x) 
		       (not (regiment-primitive? x)))))])
     (ASSERT varclause)
     (cons `[Var ,new_is-var?]  newgram)))

  ;; [2006.10] Rewrote again to use define-pass:
  ;; [2006.10.06] Rewriting to use generic-traversal:
  (define-pass eta-primitives
      (define type-constructors 'uniniti)
    [OutputGrammar eta-primitives-grammar]
    [Expr
     (letrec ([processExpr 
	       (lambda (x fallthrough)
		 (match x
		   ;; Variable References to primitives are rewritten:
		   [,var (guard (symbol? var) (regiment-primitive? var))
			 (let* ([possible-formals '(a b c d e f g h i j)]
				[args (cadr (get-primitive-entry var))])
			   (if (regiment-constant? var)
			       var
			       (let ([formals (map unique-name (list-head possible-formals (length args)))])
				 `(lambda ,formals 
					; Primitive types:
				    ,(map export-type (rdc (rdc (prim->type var))))
				    (,var ,@formals)))))]
		   
		   ;; As are variable references that are data constructors.
		   [,tc (guard (symbol? tc) (assq tc type-constructors))
		     (match (assq tc type-constructors) 
		       [(,name ,ty* ...)
			(let ([formal* (map (lambda (_) (unique-name 'x)) ty*)])		     
			  ;(printf "ETA TC: ~s ~s\n" tc formal*) (exit -1)
			  `(lambda ,formal* ,ty*
			     (construct-data ,tc ,@formal*)))])
		     ]

		   ;; Primitives that are applied with "app" have it taken away:
		   [(app ,prim ,[rands] ...)
		    (guard (regiment-primitive? prim))
		    `(,prim ,rands ...)]
		   
		   [,other (fallthrough other)]
		   ))])
       processExpr)] 
    [Program (lambda (x Expr) 
	       (match x 
		 [(,lang '(program ,e ,meta* ... ,ty))
		  (fluid-let ([type-constructors 
			       (match (assq 'union-types meta*)
				 [#f ()]
				 [(union-types [,name* ,variant** ...] ...)
				  (apply append variant**)])])
;		    (inspect type-constructors)
		    `(eta-primitives-language '(program ,(Expr e) ,@meta* ,ty)))]))])

  (define-testing these-tests
     `(
	["Simple test of eta-primitives"
	 (reunique-names (eta-primitives '(base-language
			   '(program
				(rfold _+_ 0 (rmap nodeid (khood (anchor-at 50 10) 2)))
			      (union-types) 
			      (Stream Int)))))
	 (eta-primitives-language
	  '(program
	       (rfold
		(lambda (a b) (Int Int) (_+_ a b))
		0
		(rmap
		 (lambda (a_1) (Node) (nodeid a_1))
		 (khood (anchor-at 50 10) 2)))
	     (union-types)
	     (Stream Int)))]
	))

  (define-testing test-this 
    (default-unit-tester " 1: Eta-Primitives: remove non-operator usages of primitive names."
      these-tests))

  (define test-eta-primitives test-this)

) ;; End module
