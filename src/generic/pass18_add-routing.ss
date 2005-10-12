;; INCOMPLETE

;; [2004.08.15]

;; This looks at the place information attached to the bindings.  For
;; adjacent primitives which have not been determined to match up in
;; location this pass puts in explicit routing constructs.

;; Input language is the same, but it adds an internal form ROUTE.
;; This just adds a node in the dataflow graph which explicitely does
;; the routing of a value between locations (when it comes to token
;; machines.)
;;  This syntax can only occur around the rhs of each let binding.

;; Now in reality there are four different kinds of routing:
;;   Point -> Point
;;   Area  -> Point
;;   Point -> Area
;;   Area  -> Area
;; As well as broadcasts to unknown places.  These will need handling...

;; IN THE FUTURE, this pass may do some analysis to try to infer
;; whether, say, a given place is contained in another set of places
;; (no routing required).  For now it always adds a routing command as
;; long as the start/end places are not *identical*.


;;; <Pgm>  ::= (program (props <CatEntry>*) (control-flow <CFG>*) <Let>)
;;; <CatEntry>* ::= [<Name> <Prop>*]
;;; <CFG>  ::= (<var>*)
;;; <Prop> ::= region | anchor | local | distributed | final | leaf
;;; <Let>  ::= (lazy-letrec (<Decl>*) <var>)
;;; <Decl> ::= (<var> <Heartbeat> <FormPlace> <MembPlace> <Exp>) 
;;; <Heartbeat> ::= <Float> | #f | +inf.0
;;; <Exp>  ::= (ROUTE <StartPlace> <EndPlace> <BasicExp>)
;;;          | <BasicExp>
;;; <BasicExp> ::= <Simple>
;;;              | (if <Simple> <Simple> <Simple>)
;;;              | (lambda <Formalexp> <Let>)
;;;              | (<primitive> <Simple>*)
;;; <Simple> ::= (quote <Lit>) | <Var>
;;; <Formalexp> ::= (<var>*)
;;; <Place> ::= X?       (Unknown place)
;;;           | _        (no place)
;;;           | SOC      (Source of Control)
;;;           | X_<n>    (Some place...)


;; USES: constants in constants.ss

#|

(define addrouting
  (lambda (expr)
    (match expr
	   [(,input-language (quote (program (props ,proptable ...) ,letexpr)))	    
    
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


    ;; Takes the 
    (define (process-varref used provided var)


    (define (process-bind binding)
      (match binding
	     [(,lhs ,heartbeat ,form ,memb ,rhs)
	      (guard (equal? form memb))
	      ,rhs]
	     [(,lhs ,heartbeat ,form ,memb ,rhs)
	      (ROUTE ,memb
	      
	      
	      
	     
    (define (process-let expr)
      (disp "processing let" expr)
      (match expr
	 [ (lazy-letrec ([process-bind -> binds*] ...) ,retvar)
	  `(lazy-letrec ,binds* ,retvar)]
	 [,other (error 'add-routing:process-let "bad lazy-letrec expression: ~s" other)]))
    
    `(add-routing-language (quote (program (props ,proptable ...) 
					   ,(process-let letexpr))))])))
	   

|#
