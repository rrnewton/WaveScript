;; [2004.08.13]
;; Adding this to try to clean things up, and to try to start to
;; reason about events.

;; Every named value in the program (all the let-bindings) get an
;; associated formation and membership place.  Basically, these places
;; are associated with the execution of the primitive that produces
;; that value.

;; (One thing I was considering was having *blocks* of data-flow
;; assigned form/memb locations... but I didn't get this fully worked
;; out.)


;; Input language is core plus edge annotations, plus heartbeats, plus CFG

;;; <Pgm>  ::= (program (props <CatEntry>*) (control-flow <CFG>*) <Let>)
;;; <CatEntry>* ::= [<Name> <Prop>*]
;;; <CFG>  ::= (<var>*)
;;; <Prop> ::= region | anchor | local | distributed | final | leaf
;;; <Let>  ::= (lazy-letrec (<Decl>*) <var>)
;;; <Decl> ::= (<var> <Heartbeat> <Exp>) 
;;; <Heartbeat> ::= <Float> | #f | +inf.0
;;; <Exp>  ::= <Simple>
;;;          | (if <Simple> <Simple> <Simple>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <Simple>*)
;;; <Formalexp> ::= (<var>*)
;;; <Simple> ::= (quote <Lit>) | <Var>

;; Output language expands the <Decl> even more with formation and membership places
;;; <Decl> ::= (<var> <Heartbeat> <FormPlace> <MembPlace> <Exp>) 
;;; <FormPlace> ::= <Place>
;;; <MembPlace> ::= <Place>
;;; <Place>     ::= X?       (Unknown place)
;;;               | _        (no place)
;;;               | SOC      (Source of Control)
;;;               | X_<n>    (Some place...)
;;;               | (X_<n>)  (Some set of places (an area)...)

;; NOTE: An unknown place is not necessarily known to be a single location or an area!! 

;;; [2004.08.13] Need to consider conditionals:
;;;   (if #t (circle...) (khood...))
;;; While they have the same "type", there will obviously be different
;;; places associated with different branches of a conditional.  So
;;; what should my analysis do?  Well if it's not known till runtime
;;; what will come out, this is really a place where it has to say
;;; *unknown*!
;;;   There *are* however circumstances where the two branches might
;;; memb in the same locations, say, if they're both maps over the
;;; same region.

;;; [2004.10.06] What does No Place mean?  Does it mean that it
;;; happens at the SOC, that it doesn't matter where it happens?  I
;;; don't know if this is a meaningful symbol...


;;; This is just a helper function that shows the places for an annotated program:
(define (getplaces p)
  (match p
	 [(,input-language (quote (program (props ,proptable ...) 
					   (control-flow ,cfg ...)
					   (lazy-letrec ,binds ,expr))))
	  (map (lambda (x) (list-head x 4)) binds)]))

(define add-places
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
