;; [2004.08.20]
;; This creates a separate and parallel control flow graph.

;; (amap f (circle (anchor-at '(30 40)) 50))
;;  -> (soc anchor circle amap)

;; (amap f (union r1 r2))
;;  -> (soc r1 amap)
;;  -> (soc r2 amap)


(define add-control-flow
  (lambda (expr)
    (match expr
	   [(,input-language (quote (program (props ,proptable ...) ,letexpr)))

    ;; Returns control flow graph
    (define (process-let expr)
      (disp "processing let" expr)
      (match expr
	 [ (lazy-letrec ([,lhs* ,heartbeat* ,[expr-dependencies -> deps*] ...) ,expr)
	   (map (lambda (lhs deps)
		  (map (lambda (x) `(,x ,lhs)) deps))
		
	     
	     `(lazy-letrec ([,lhs* ,heartbeat* ,form* ,memb* ,rhs*] ...) ,expr)]

	 [,other (error 'add-control-flow:process-let "bad lazy-letrec expression: ~s" other)]))


    (define (process-primapp prim args)
      (let ([expr (cons prim args)])
	(case prim
	  [(anchor-at) (values expr unknown-place (new-place))]
	  ;; Both of these start in the center and spread to some extent.
	  [(circle khood) (values expr (new-place) (list (new-place)))]

	  ;; Can we say something about cluster?  Disregarding the
	  ;; *type* cluster does not change the physical extent...
	  ;; Maps are more straightforward, they don't change extent.
	  [(cluster rmap) 
	   (let ([newp (list (new-place))])
	     (values expr newp newp))]

	  ;; This is a real challenge.  The simplest rfold uses a tree
	  ;; that brings all the data to a leader node within the
	  ;; region.  If that's the case we need some way to express
	  ;; the constraint that the resulting place is one of the
	  ;; places in the initial region.
	  ;;   BUT we might use a different tree for the fold.  For
	  ;; example we might fold up on the global tree, in which
	  ;; case the final "place" is the SOC.  FOR NOW, we're just
	  ;; going to assume all folds go to the SOC.
	  [(rfold) (values expr (list (new-place)) 'SOC)]
	  
	  ;; A signal lives at one place... an smap keeps that place the same (for now). 
	  [(smap) 	   
	   (let ([newp (new-place)])
	     (values expr newp newp))]
		  
	  [else (if (basic-primitive? prim)
		    (values expr noplace noplace)
		    (error 'addplaces:process-primapp "unhandled prim: ~s" prim))]
	  )))


    (define process-expr
      (lambda (expr)
	(disp "process expr" expr)
        (match expr
          [(quote ,const) (values `(quote ,const) noplace noplace)]
          [,var (guard (symbol? var)) (values var noplace noplace)]
          [(lambda ,formalexp ,expr)
	   (values (process-let expr) noplace noplace)]
	  ;; Hmm... if I can tell at compile time I should narrow this!
          [(if ,test ,conseq ,altern)
	   (values `(if ,test ,conseq ,altern) unknown-place unknown-place)]
          [(,prim ,rand* ...)	   
           (guard (regiment-primitive? prim))
	   (process-primapp prim rand*)]
          [,unmatched
	   (error 'addplaces:process-let "invalid syntax ~s" unmatched)])))


    (let ([leaves (filter (lambda (entry) (memq 'leaf entry)) proptable)])
      `(,input-language (quote (program (props ,proptable ...)
					`(,@(map (lambda (x) `(SOC ,x)) leaves)
					  ,@(process-let letexpr))
					'letexpr
					))))]
	   )))
