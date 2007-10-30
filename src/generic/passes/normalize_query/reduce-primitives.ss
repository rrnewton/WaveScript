;; Pass reduce-primitives 

;;; This pass simplifies the progam, basically it reduces
;;; rewrites primitive expressions into other primitive exprssions.


(module reduce-primitives mzscheme
  (require "../../../plt/common.ss"
           "../static_elaborate/static-elaborate.ss")
  (provide reduce-primitives reduce-primitives-grammar)
  (chezimports)

  ;;; PRIMITIVES ELIMINATED:
  (define eliminated-prims '(circle-at khood-at anchor-at head tail))

  ;; TODO: FILL THIS IN:
  (define reduce-primitives-grammar 
    (filter (lambda (prod)
	      (match prod
		[(Prim ',sym) (guard (memq sym eliminated-prims)) #f]
		[,_ #t]))
      static-elaborate-grammar))

(define-pass reduce-primitives
  [OutputGrammar reduce-primitives-grammar]
  [Expr (lambda (x fallthrough)	 
	  (match x
       [(iterate ,annot ,[f] ,[s])
        (process-primapp 'iterate `(,annot ,f ,s))]
	    [(,prim ,[rand*] ...)
	     (guard (regiment-primitive? prim))
	     (process-primapp prim rand*)]
	    [,other (fallthrough other)]))]
  (define process-primapp
    (lambda (prim args)
      (match (cons prim args)
	[(circle-at ,loc ,rad)
	 `(circle (anchor-at ,loc) ,rad)]
	[(khood-at ,loc ,rad)
	 `(khood (anchor-at ,loc) ,rad)]
	;; This builds a function to optimize distance from an x,y position.
	[(anchor-at ,x ,y) 
	 (let ([xvar (unique-name 'xpos)]
	       [yvar (unique-name 'ypos)])
	   `(anchor-maximizing 
					;		    (letrec ([,xvar Int ,x]
					;			     [,yvar Int ,y])
	     (lambda (n) (Node) (-_ '0 (+_ (^_ (-_ (sense '"xpos" n) ,x) '2) 
					   (^_ (-_ (sense '"ypos" n) ,y) '2))))
	     world))]

	[(node_to_anchor ,n) `(node->anchor ,n)]

	[(head ,ls) `(car ,ls)]
	[(tail ,ls) `(cdr ,ls)]	

	[,orig orig])))
  )

) ; End Module
