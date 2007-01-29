
;;;; Pass: Eta-Primitives
;;;; [2004.06.24]

;;;; This simple pass eta expands all operand-context references to
;;;; primitives, hereafter primitive-names will only occur in operator
;;;; context.  This is just a simple, regularizing pass.

(module eta-primitives mzscheme
  (require "../../../plt/common.ss"
           ;"desugar-misc.ss"
	   )
  (provide eta-primitives eta-primitives-grammar test-eta-primitives)
  (chezimports)

  ;; In the output grammar varrefs are no longer allowed to refer to primitives.
  (define eta-primitives-grammar
    (let* ([varclause (assq 'Var initial_regiment_grammar)]
	   [newgram (remq varclause initial_regiment_grammar)]
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
			       (let ([formals (list-head possible-formals (length args))])
				 `(lambda ,formals 
					; Primitive types:
				    ,(map export-type (rdc (rdc (prim->type var))))
				    (,var ,@formals)))))]

		   ;; Primitives that are applied with "app" have it taken away:
		   [(app ,prim ,[rands] ...)
		    (guard (regiment-primitive? prim))
		    `(,prim ,rands ...)]
		   
		   [,other (fallthrough other)]
		   ))])
       processExpr)])


  (define test-this 
    (default-unit-tester " 1: Eta-Primitives: remove non-operator usages of primitive names."
      `(
	["Simple test of eta-primitives"
	 (eta-primitives '(base-language
			   '(program
				(rfold +_ 0 (rmap nodeid (khood (anchor-at 50 10) 2)))
			      (Stream Int))))
	 (eta-primitives-language
	  '(program
	       (rfold
		(lambda (a b) (Int Int) (+_ a b))
		0
		(rmap
		 (lambda (a) (Node) (nodeid a))
		 (khood (anchor-at 50 10) 2)))
	     (Stream Int)))]
	)))

  (define test-eta-primitives test-this)

) ;; End module
