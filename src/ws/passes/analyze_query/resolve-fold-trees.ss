

;;;; After this pass, all letrec entries containing fold primitives gain an extra annotation:
;;;;<br>
;;;;<br>  (tree <name>)
;;;;<br>

;;;; Where <name> is either a variable name referring to the part of
;;;; the program that produces the khood being folded OR is the
;;;; world primitive.

;;;; This annotation is used by deglobalize to choose aggregation trees.

(module resolve-fold-trees mzscheme
  (require "../../../plt/common.ss")  
  (provide resolve-fold-trees test17c test-resolve-fold-trees)
  (chezimports )

(define resolve-fold-trees
  (lambda (prog)
    (match prog
	[(,input-language (quote (program (props ,proptable ...) 
				   (control-flow ,cfg ...)
				   (data-flow ,dfg ...)
				   ,letexpr
				   ,type)))

	 (define process-expr 
	   (lambda (expr)
	     (match expr
	       [(quote ,const)             `(quote ,const)]
	       [,var (guard (symbol? var))  var]
	       [(lazy-letrec ,binds ,tail)
		`(lazy-letrec
		  ,(map (match-lambda ((,lhs ,ty ,annots ,rhs))
			  (match rhs
			    [(rfold ,fun ,seed ,reg)
			     `[,lhs ,ty ((tree ,(find-khood reg)) . ,annots)
				    ,(process-expr rhs)]]
			    ;; rdump works basically the same as rfold
			    [(rdump ,reg)
			     `[,lhs ,ty ((tree ,(find-khood reg)) . ,annots)
				    ,(process-expr rhs)]]

			    [,rhs `[,lhs ,ty ,annots ,(process-expr rhs)]]))
		     binds)
		  ,tail)]
	       [(lambda ,v* ,ty* ,[bod]) `(lambda ,v* ,ty* ,bod)]
	       [(if ,[t] ,[c] ,[a])      `(if ,t ,c ,a)]

	       [(tuple ,[args] ...) `(tuple ,args ...)]
	       [(tupref ,n ,m ,[x]) `(tupref ,n ,m ,x)]

	       [(,prim ,[rand*] ...)
		(guard (wavescript-primitive? prim))
		`(,prim ,rand* ...)]
	       [,unmatched
		(error 'resolve-fold-trees "invalid syntax ~s" unmatched)])	
	     ))
	 
	 (define find-khood 
	   (lambda (name)
	     (let loop ([x name])
	       (DEBUGASSERT (symbol? x)) ;; Had better be a variable.
	       (if (eq? x 'world)
		   'world
		   (match (assq x dfg) 
		     [#f (error 'find-khood "name not in data-flow graph: ~s \ndfg: ~s" x dfg)]
		     [(,_ [,name ,ty ,annots (khood . ,__)])          name]
		     [(,_ [,__  ,___ ,____ (rmap ,f ,r)])           (loop r)]
		     [(,_ [,__  ,___ ,____ (rfilter ,f ,r)])        (loop r)]
		     [(,_ [,__  ,___ ,____ ,v]) (guard (symbol? v)) (loop v)]

		     [,other #f] ;; Failure, for example, a conditional.

		     [,other (error 'find-khood "cannot propagate through ~s" other)]
		     )))))

	 `(,input-language (quote (program (props ,proptable ...) 
				    (control-flow ,cfg ...)
				    (data-flow ,dfg ...)
				    ,(process-expr letexpr)
				    ,type)))])))

(define-testing test-this
  (default-unit-tester 
    "Pass 17: Resolve Fold Trees"
    `(
#;
      [""
	(resolve-fold-trees
	 `(lang '(program (props )
		   (control-flow )
		   (data-flow (v (rmap f world)) 		    
			      (x THEWORLD)		
			      (f (lambda (x) (_) x)))
		   (lazy-letrec
		    ([f _ () (lambda (x) (_) x)]
		     [v _ () (rmap f world)])
		    v)
		   )))
	]

#;
      ["Now let's look at nested regions."
       (assq 'r2
	     (cdr (deep-assq 'data-flow
			(add-data-flow 
			 `(lang '(program (props )
				   (control-flow )	   
				   (lazy-letrec
				    ([h (Area Region) ()
					(rmap (lambda (n) (Node) (khood (node->anchor n) 2)) world)]
				     [h2 (Area (Area Int)) ()
					 (rmap (lambda (r1) (Region) (rmap getid r1)) h)]
				     [getid (Node -> Int) ()
					    (lambda (nd) (Node) (nodeid nd))]
				     [v (Area Int) ()
					(rmap (lambda (r2) ((Area Int)) 
						      (rfold + 0 r2)) h2)]
				     )
				    v)
				   (Area Int)))))))
       (r2 (rmap getid r1))]

      )))


(define test17c test-this)
(define test-resolve-fold-trees test-this)


)