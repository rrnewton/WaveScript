;; [2004.07.26]  This assigns appropriate heartbeats to nodes in the graph.

;; Input language is Core plus edge classifications:

;;; <Pgm>  ::= (program (props <CatEntry>*) <Let>)
;;; <CatEntry>* ::= [<Name> <Prop>*]
;;; <Prop> ::= region | anchor | local | distributed | final | leaf
;;; <Let>  ::= (lazy-letrec (<Decl>*) <var>)
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= <Simple>
;;;          | (if <Simple> <Simple> <Simple>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <Simple>*)
;;; <Formalexp> ::= (<var>*)
;;; <Simple> ::= (quote <Lit>) | <Var>

;; Output language has a heartbeat annotation for some variable
;; entries (distributed ones only):
;; Heartbeats are in Hertz.

;;; <Decl> ::= (<var> <Exp>) 
;;;          | (<var> <Heartbeat> <Exp>) 
;;; <Heartbeat> ::= <Float>

;; The slow-pulse is used for region formation.
(define slow-pulse 1.0)
;; The fast-pulse is used for folding.
(define fast-pulse 10.0)

(define annotate-heartbeats
  (let ()
    (lambda (expr)
      (match expr
	     [(,input-language (quote (program (props ,proptable ...)
					       (lazy-letrec ,binds ,fin))))
	      (let ()
		;;; <Prop> ::= region | anchor | local | distributed | final | leaf
		(define (check-prop p s)
		  (let ((entry (assq s proptable)))
		    (if entry (memq p (cdr entry))
			(error 'pass10_deglobalize:check-prop
			       "This should not happen!  ~nName ~s has no entry in ~s."
			       s proptable))))

		
;; This constructs a tree of value-dependencies, and then walks it to
;; set the pulses.

		(define (process-let expr env)
		  (match expr
			 ;; [,lhs* ,[process-expr -> rhs*]]
		       [(lazy-letrec ,binds ,ret)
			
			;; [2004.07.26] This will only be *complete*
			;; for our very simple language.  Gotta face
			;; the possibility of uncompleteness here.
			;; Could memoize this for much more speed.
			(let ([dependency-graph
				 (let loop ([node (car (assq ret binds))])
				   (let ([deps (filter symbol? (cdr node))])
				     (cons (cons (car node) 
						 #f) ;; Initial heartbeat unlabeled
					   (map 
					    (lambda (s) (loop (car (assq s binds))))
					    deps
				     
			
			
			(map (lambda (lhs rhs)
			       (if (and (check-prop 'leaf)
					(check-prop 'distributed))

;; If we've just got a region on the leaf, like a circle formation,
;; then we give it the slow pulse, but gotta remember to give its
;; children faster pulses if necessary.


				   (if (check-prop 'region)
				       `[,lhs ,slow-pulse 

				       (if (check-prop 'fold)

			`(lazy-letrec ([,lhs* ,rhs*] ...) ,expr)]))
		
		'    (define (process-primapp prim args)
		       (cons prim args))
		     
     (define process-expr
       (lambda (expr)
	 (match expr
		[(quote ,const) `(quote ,const)]
		[,var (guard (symbol? var)) var]
		[(lambda ,formalexp ,expr)
		 (process-let expr (union formalexp env))]
		[(if ,test ,conseq ,altern)
	   `(if ,test ,conseq ,altern)]
		[(,prim ,rand* ...)
		 (guard (regiment-primitive? prim))
	   (process-primapp prim rand*)]
		[,unmatched
		 (error 'TEMPLATE "invalid syntax ~s" unmatched)])))
     

	      (set! proptable table)
	      	      
	      `(,input-language (quote (program ,body)))])))
)
