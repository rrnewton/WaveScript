;; [2004.08.13]
;; Adding this to try to clean things up, and to try to start to
;; reason about events.

;; Input language is core plus edge annotations plus heartbeats.

;;; <Pgm>  ::= (program (props <CatEntry>*) <Let>)
;;; <CatEntry>* ::= [<Name> <Prop>*]
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
;;; <FormPlace> = 
;;; <MembPlace> ::= X?       (Unknown place)
;;;               | _        (no place)
;;;               | SOC      (Source of Control)
;;;               | X_<n>    (Some place...)

;;; [2004.08.13] Need to consider conditionals:
;;;   (if #t (circle...) (khood...))
;;; While they have the same "type", there will obviously be different
;;; places associated with different branches of a conditional.  So
;;; what should my analysis do?  Well if it's not known till runtime
;;; what will come out, this is really a place where it has to say
;;; *unknown*!

;;; This is just a helper function that shows the places for an annotated program:
(define (getplaces p)
  (match p
	 [(,input-language (quote (program (props ,proptable ...) (lazy-letrec ,binds ,expr))))
	  (map (lambda (x) (list-head x 4)) binds)]))

(define addplaces
  (lambda (expr)
    (match expr
	   [(,input-language (quote (program (props ,proptable ...) ,letexpr)))

    (define unknown-place '?) ;'X?)
    (define noplace '_)
	   
    (define (process-let expr)
      (disp "processing let" expr)
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
	 [,other (error 'addplaces:process-let "bad lazy-letrec expression: ~s" other)]))
    
    (define (new-place) (unique-name 'X))

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
	  
;	       [(anchor-at ,loc) (values expr '_ (new-place))]
;	       [(circle ,anch ) (values expr '_ (new-place))]

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
    
    `(,input-language (quote (program (props ,proptable ...) 
				      ,(process-let letexpr))))])))
