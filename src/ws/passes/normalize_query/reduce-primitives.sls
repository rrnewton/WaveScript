#!r6rs

;; Pass reduce-primitives 

;;; This pass simplifies the progam, basically it reduces
;;; rewrites primitive expressions into other primitive exprssions.

(library (ws passes normalize_query reduce-primitives)
  (export reduce-primitives reduce-primitives-grammar)
  (import (except (rnrs (6)) error) (ws common)
	  (ws passes static_elaborate static-elaborate)
	  )

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
             (lambda (n) (Node) (_-_ '0 (_+_ (^_ (_-_ (sense '"xpos" n) ,x) '2) 
                                           (^_ (_-_ (sense '"ypos" n) ,y) '2))))
             world))]

        [(node_to_anchor ,n) `(node->anchor ,n)]

        [(head ,ls) `(car ,ls)]
        [(tail ,ls) `(cdr ,ls)]	

	;; [2008.01.07] TEMP only implementing this primitive in wsc2:
	[(List:is_null ,ls) 
	 (guard (not (wsc2-variant-mode? (compiler-invocation-mode))))
         `(wsequal? ,ls '())]

        [,orig orig])))

  [OutputGrammar reduce-primitives-grammar]
  [Expr (lambda (x fallthrough)	 
	  (match x
       [(,prim ,annot ,[rand*] ...)
        (guard (and (pair? annot) (eq? (car annot) 'annotations)))
        (process-primapp prim `(,annot ,@rand*))]
	    [(,prim ,[rand*] ...)
	     (guard (regiment-primitive? prim))
	     (process-primapp prim rand*)]
	    [,other (fallthrough other)]))]
  )

) ; End Module
