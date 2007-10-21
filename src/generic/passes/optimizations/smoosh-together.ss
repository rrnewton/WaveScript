;;;; .title Smoosh the program together, removing spurious variable bindings.
;;;; .author Ryan Newton

;;;; This pass does a very simple form of inlining.  There are no
;;;; functions (much less recursive ones), so it's simply a matter of
;;;; inlining let-bound variables that have only one reference
;;;; (assuming the code is pure and moving it won't cross any relevant
;;;; side-effect boundaries (or loop boundaries for that matter)).
;;;; Also, any let-binding to a simple constant can be inlined.

;;;; The entire point of this pass is to make it easy for subsequent optimizations


;;;; Note: it wouldn't hurt to perform dead code elimination before
;;;; this.  But if it happens right after metaprogram eval, then
;;;; that's just as good.

;;;; NOTE FIXME This pass may eventually be converted to deal with the
;;;; whole program, and with side effects, and to avoid pushing extra
;;;; computation into loops... Until then this pass only works on the
;;;; Stream "spine" of the program.

(module smoosh-together mzscheme
  (require "../../../plt/common.ss"
	   ;"reduce-primitives.ss"
	   )
  (provide smoosh-together)
  (chezimports tsort)

;; What's the best algorithm for this pass?
; When processing a node in the tree, say (+ X Y),
; we don't know if X or Y contains refs to an in-scope variable V.

; Assuming single thread implementation... could maintain a mutable
; structure that tracks refs to a variable.  The first time you see it
; you *tentatively* inline...  Make some record of that spot.. then if
; you see it again, you kill that delayed inline.

; But if we're not doing any DCE, we're not changing the values of any
; of the reference counts.  Thus a perfectly reasonable strategy
; is to refcount once, and then go through once, inlining everything
; with refcount 1.


  (define-pass smoosh-together
    (define reftable 'uninitttt)    
    (define env ()) ;; Map var to rhs's *CODE*

    ;(define (pure-expr? xp))
    ;; FIXME: Unimplemented because there will be no side effects in the stream spine.
    (define (read-only-expr? xp) #t)

    (define Expr
      (lambda (xp fallthru)
	(define (loop x) (Expr x fallthru))
	(match xp
	  ;; Inline if refcount is 1.
	  [,vr (guard (symbol? vr))
	       (let ([rc (hashtab-get reftable vr)])
		 (printf "    REF Var ~s with RC ~s\n" vr rc)
;		 (inspect env)
		 (ASSERT (not (fxzero? rc)))
		 (if (fx= rc 1)
		     ;; Transform as we inline:
		     (loop (cdr (assq vr env)))
		     vr))]

	  ;; For now we simply don't go inside lambda's:
	  ;[(lambda ,v* ,ty* ,bod) `(lambda ,v* ,ty* ,bod)]
	  [(iterate ,_ ,[strm]) `(iterate ,_ ,strm)]

	  [(,lett ([,lhs* ,ty* ,_rhs*] ...) ,_bod)
	   (guard (memq lett '(let let* letrec)))
;	   (ASSERT (not (eq? lett 'letrec))) ;; TEMPTOGGLE, FIXME:
	   (fluid-let ([env (append (map cons lhs* _rhs*) env)])
;	     (printf " Extended env: ~s\n" env)
	     (let* ([bod (loop _bod)]
		    [newbinds 
		     (filter id		      
		       (map
			(lambda (lhs ty _rhs)
			  (let ([rc (hashtab-get reftable lhs)])
			    (ASSERT rc)
			    (printf "  BIND Var ~s with RC ~s\n" lhs rc)
			    (cond
			     [(fx> rc 1) (list lhs ty (loop _rhs))]
			     [(fx= rc 1) #f]
			     [else 
			      (DEBUGASSERT (fx= rc 0))
			      ;; Doing dead code elimination here:
			      ;; Can only do it if the rhs doesn't side effect.
			      ;; Really this should happen *before* smoosh together.
			      (if (read-only-expr? _rhs)
				  #f
				  (loop _rhs))])))
		    lhs* ty* _rhs*))])
	     `(,lett ,newbinds ,bod)
	     ))]

#;
	  [,frm (guard (binding-form? frm))
		(mvlet ([(vars types rhs* other k) (binding-form-visit-knowncode frm)])
		  (fluid-let ([env (append (map cons vars rhs*) env)])
;		    (inspect (vector 'extended env ))
		    (let* ([binds (map list vars types (map loop rhs*))]
			   ;; What binds are left after inlining?

			   ;; This doesn't work, can't quite use visit-knowncode to eliminate bindings.
			   [newbinds 
			    (filter (lambda (bind)
				      (let ([rc (hashtab-get reftable (car bind))])
					(printf "Var ~s with RC ~s\n" (car bind) rc)
					(ASSERT rc)
					;; Doing dead code elimination here:
					;; Really this should happen *before* smoosh together.
					;(ASSERT "smoosh-together shouldn't find dead code" 
					;	id (not (fxzero? rc)))
					;; If RHS is unknown (e.g. lambda) can't eliminate this binding:
					(and (caddr bind) (fx> rc 1))))
			      binds)])
		      (k (map car newbinds)
			 (map cadr newbinds)
			 (map caddr newbinds)		      
			 (map loop other)))))]
	  [,oth (fallthru oth)])))
    [Expr Expr]
    [Program (lambda (prog Expr)
	       (match prog
		 [(,lang '(program ,bod ,meta* ... ,finaltyp))
		 (fluid-let ([reftable (core-refcount bod)])
		   `(,lang '(program ,(Expr bod) ,@meta* ,finaltyp)))]))])



 
) ;; End module.


#;
(smoosh-together 
 '(foolang 
   '(program (let ([x (Stream #()) (timer '3.0)])
	       (prim_window x '39))
      (Stream #()))))




;; ================================================================================
;; JUNK



#;
    [Bindings
     (lambda (vars types exprs reconstr exprfun fallthrough)
       (cheap-fluid-let ([env (append (map list vars types) env)])			
         (let* ([refcounts (map (lambda (vr) (hashtab-get reftable vr)) vars)]
		[results (map exprfun exprs)]
		[newbinds ]
		

		[newvars (map (lambda (v) (assq v allcounts)) vars)])
	   (vector (reconst newvars types rhs*)
		   (subtract allcounts vars))))

     )]


#; #;
  ;; Carry down an environment that contains the actual rhs code
  (define (Expr env)
    (lambda (xp fallthru)
      (match xp
      [,form (guard (binding-form? form))
	     (let ([scoped (binding-form->scoped-exprs form)]
		   [vars (binding-form->vars form)]
		   [others (binding-form->unscoped-exprs form)])
	       ?????????)]
      [,oth (fallthru oth)])))
  (define (smoosh-together prog)
    (match prog
      [(program ',bod ,meta* ... ,finaltyp)
       (fluid-let ([reftable (core-refcount bod)])
	 (Expr bod))])
    
    (define reftable 'uninitttt)    

    [Program (lambda (prog Expr)
	       )])