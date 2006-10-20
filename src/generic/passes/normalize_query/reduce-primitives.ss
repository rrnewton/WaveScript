;; Pass reduce-primitives 

;;; This pass simplifies the progam, basically it reduces
;;; rewrites primitive expressions into other primitive exprssions.

;;; PRIMITIVES ELIMINATED:
;;;   circle-at


(define-pass reduce-primitives
  [Expr (lambda (x fallthrough)	 
	  (match x
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
					;		    (letrec ([,xvar Integer ,x]
					;			     [,yvar Integer ,y])
	     (lambda (n) (Node) (- '0 (+ (^ (- (sense 'xpos n) ,x) '2) 
					 (^ (- (sense 'ypos n) ,y) '2))))
	     world))]

	[(node_to_anchor ,n) `(node->anchor ,n)]
	

	[,orig orig])))
  )
