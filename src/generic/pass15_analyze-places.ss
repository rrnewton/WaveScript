;; [2004.10.07]

;; This pass does some *very* simple dataflow analysis and infers
;; place equivalences and memberships.  Equivalent places are renamed
;; appropriately.  Membership is express by a name existing in a the
;; place-list which it participates in.


;;; <Pgm>  ::= (program (props <CatEntry>*) 
;;;                     (control-flow <CFG>*) 
;;;             <Let>)

;;; <PM> ::= [<Place> (X_<n>)]
;;; <Place>     ::= X?       (Unknown place)
;;;               | _        (no place)
;;;               | SOC      (Source of Control)
;;;               | X_<n>    (Some place...)
;;;               | (X_<n1> X_<n2> ...)  
;;;      (Some set of places containing at least X_n1 X_n2 etc)

;; X? != X? 
;; X_3 == X_3
;; X_3 \in (X_3)


;; Input language: output of add-places
;; Output language: same.


(define analyze-places
  (lambda (expr)
    (match expr
	   [(,input-language (quote (program (props ,proptable ...) 
					     (control-flow ,cfg ...)
					     ,letexpr)))

    ;; Constants defined in constants.ss
    ;(define unknown-place '?) ;'X?)
    ;(define noplace '_)
	   
    (define (process-let expr)
;      (disp "processing let" expr)
      (match expr
	 [ (lazy-letrec ([,lhs* ,heartbeat* ,[process-expr -> rhs* form* memb*]] ...) ,expr)

;	  (lazy-letrec ([,lhs* ,heartbeat* ,rhs*] ...) ,expr)

;	  (let ([stuff (map (lambda (rhs) (call-with-values (lambda () (process-expr rhs)) (lambda args args)))
;			    rhs*)]
;		[form* (map (lambda (_) 0) rhs*)]
;		[memb* (map (lambda (_) 0) rhs*)]
;		)
;	    (disp "got stuff" stuff)
;	    (disp "for rhs" rhs*)

	  `(lazy-letrec ([,lhs* ,heartbeat* ,form* ,memb* ,rhs*] ...) ,expr)]
	 [,other (error 'add-places:process-let "bad lazy-letrec expression: ~s" other)]))
    
    (define (new-place) (unique-name 'X))

    ;; Returns expression, form-place, memb-place
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
		    (error 'add-places:process-primapp "unhandled prim: ~s" prim))]
	  )))
	  
;	       [(anchor-at ,loc) (values expr '_ (new-place))]
;	       [(circle ,anch ) (values expr '_ (new-place))]

    (define process-expr
      (lambda (expr)
;	(disp "process expr" expr)
        (match expr
          [(quote ,const) (values `(quote ,const) noplace noplace)]
          [,var (guard (symbol? var)) (values var noplace noplace)]
          [(lambda ,formalexp ,expr)
	   (values `(lambda ,formalexp ,(process-let expr))
		   noplace noplace)]
	  ;; Hmm... if I can tell at compile time I should narrow this!
          [(if ,test ,conseq ,altern)
	   (values `(if ,test ,conseq ,altern) unknown-place unknown-place)]
          [(,prim ,rand* ...)	   
           (guard (regiment-primitive? prim))
	   (process-primapp prim rand*)]
          [,unmatched
	   (error 'add-places:process-let "invalid syntax ~s" unmatched)])))
    
    `(add-places-language (quote (program (props ,proptable ...)
					  (control-flow ,cfg ...)
					  ,(process-let letexpr))))])))