;; [2004.07.26]  This assigns appropriate heartbeats to nodes in the graph.

;; Right now it doesn't propogate information through the dataflow
;; graph, or do any fancy constraint solving.  It just assigns default
;; heartbeats to region formation operations and to folds.


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


;; Output language adds a frequency (heartbeat) value to each let
;; binding.  This value is either #f (if the value is local and has no
;; heartbeat), a floating point number expressing frequency in hertz,
;; or positive infinity (+inf.0) expressing that the value is
;; available as fast as you might want it.

;;; <Decl> ::= (<var> (<Annotation>*) <Exp>)
;;; <Annotation> ::= (heartbeat <Heartbeat>)
;;; <Heartbeat>  ::= <Float> | #f | +inf.0


(module add-heartbeats mzscheme
  (require "../../../plt/common.ss")
  (provide add-heartbeats)
  (chezimports)

(define add-heartbeats
  (let ()
    
    ;; [2005.12.19] letrec bindings are now [lhs type rhs]
    (define bind->lhs  car)
    (define bind->type cadr)
    (define bind->rhs  caddr)

    (define (simple? expr)
      (match expr
	[,num (guard (number? num)) #t]
	[,var (guard (symbol? var) (not (regiment-constant? var))) #t]
	[(quote ,const) (guard (or (simple-constant? const) (symbol? const))) #t]
	[,else #f]))

    (lambda (expr)
      (match expr
	[(,input-language (quote (program (props ,proptable ...) ,letexpr ,type)))
	 (let ()
	   (define (check-prop p s)
	     (let ((entry (assq s proptable)))
	       (if entry (memq p (cdr entry))
		   (error 'pass10_deglobalize:check-prop
			  "This should not happen!  ~nName ~s has no entry in ~s."
			  s proptable))))
	   		
	   ;; This constructs a tree of value-dependencies, and then walks it to
	   ;; set the pulses.

	   (define (derive-freqtable binds ret)	     

	     ;; [2004.07.26] This will only be *complete*
	     ;; for our very simple language.  Gotta face
	     ;; the possibility of uncompleteness here.
	     ;; Could memoize this for much more speed.
	     (let* ([freq-table (map (lambda (entry)
				       (cons (car entry) #f))
				     binds)]

		    ;; This is a nested list where the head of each
		    ;; entry is a freq-table entry (pair with name and
		    ;; freq).
		    [dependency-tree
		     ;; NOTE FIXME TODO: WONT HANDLE CIRCULAR DEPENDENCIES!!
		     (let loop ([node (assq ret binds)])
		       (if (not node)
			   (error 'add-heartbeats:derive-freqtable 
				  "could not find binding for '~s in ~s" ret binds)
		       (let ([deps (get-deps (bind->rhs node))])
			 (cons (assq (bind->lhs node) freq-table)
			       (map 
				(lambda (s) (loop (assq s binds)))
				deps)))))]

		    [reconcile
		     (lambda (newrate entry)
		       (set-cdr! entry (max newrate (cdr entry))))]
		    
		    [slow-prim?
		     (lambda (name expr)
		       (match expr
			      [,exp (guard (simple? exp)) #f]
			      [(lambda ,formalexp ,types ,expr) #f]
			      [(if ,test ,conseq ,altern) #f]
			      ;; All regiment constants are presumed to be "slow prims" for now.
			      [,prim (guard (regiment-constant? prim))
				     #t]				    

			      [(,prim ,rand* ...)				      
			       (guard (distributed-primitive? prim))
			       (or (check-prop 'region name)
				   (check-prop 'anchor name)) ]
			      [(,prim ,rand* ...)				      
			       (guard (regiment-primitive? prim)) #f]
			      [,else (error 'slow-prim? "invalid exp: ~s" expr)]))]

		    [fast-prim?
		     (lambda (name expr)
		       (match expr
			      [,exp (guard (simple? exp)) #f]
			      [(lambda ,formalexp ,types ,expr) #f]
			      [(if ,test ,conseq ,altern) #f]
			      [(,prim ,rand* ...)				      
			       (guard (distributed-primitive? prim))
			       (memq prim '(rmap rfold smap))
			       #t]
			      [(,prim ,rand* ...)				      
			       (guard (regiment-primitive? prim)) #f]
			      [,else (error 'fast-prim? "invalid exp: ~s" expr)]))]
		    )
	       
;	       (disp "got freq table")
;;	       (pp freq-table)
;	       (disp "got dep graph")
;;	       (parameterize ((print-graph #t))
;;			     (pp dependency-tree)) 
	       
	       ;; Now loop up that tree from the root and set those frequencies.
	       (for-each (lambda (freq-entry bind-entry)
			   (DEBUGMODE 
			    (if (not (eq? (bind->lhs freq-entry)
					  (bind->lhs bind-entry)))
				(error 'add-heartbeats:process-let
				       "Ryan, you did something wrong. bind list doesn't line up with freq-table.")))
			   (if (slow-prim? (bind->lhs freq-entry) ;; Edge name
					   (bind->rhs bind-entry)) ;; Generating Expression
					;(set-cdr! freq-entry +inf.0)
			       (set-cdr! freq-entry (default-slow-pulse))
			       (if (fast-prim? (bind->lhs freq-entry) (bind->rhs bind-entry))
				   (set-cdr! freq-entry (default-fast-pulse))))
			   )
			 freq-table
			 binds)

	       ;; Now the freq-table has some info in it.  But we need to move up and
	       ;; down the dependency-tree to propogate that information, so that
	       ;; all distributed names have a frequency.
	       
   	;		       (let treeloop ([node dependency-tree])
	;			 (if (null? (cdr node))
	;			     (reconcile default-slow-pulse (car node))				     			
	       
	       freq-table))

    (define process-let
      (lambda (expr)
        (match expr
	  [(lazy-letrec ([,lhs* ,type* ,[process-expr -> rhs*]] ...) ,fin)
	   (let* ([binds (map list lhs* type* rhs*)]
		  [freq-table (derive-freqtable binds fin)]
		  [newbinds (map (lambda (bind)
				   (match bind
					  [(,lhs ,type ,rhs)
					   `[,lhs ,type 
						  ([heartbeat ,(cdr (assq lhs freq-table))])
						  ,rhs]]))
				 binds)])
	     `(lazy-letrec ,newbinds ,fin))])))

    ;; Returns a list of the names of dependencies for an expression.  (Free vars)
    (define get-deps
      (lambda (expr)
        (match expr
	  [,var (guard (symbol? exp) (not (regiment-constant? exp)))
		exp]
	  [,simp (guard (simple? simp)) '()]
          [(lambda ,formalexp ,types ,bod) '()] ; lambdas have no free-vars
          [(if ,[test] ,[conseq] ,[altern])
	   (append test conseq altern)]
	  ;; Don't need to recur on rands:
          [,prim (guard (regiment-constant? prim)) '()]
          [(,prim ,[rand*] ...)
           (guard (regiment-primitive? prim))
	   (apply append rand*)]
          [,unmatched
	   (error 'get-deps "invalid syntax ~s" unmatched)])))

    (define process-expr
      (lambda (expr)
        (match expr
	  [,exp (guard (simple? exp)) exp]
          [(lambda ,formalexp ,types ,[process-let -> letexp])
	   `(lambda ,formalexp ,types ,letexp)]
	  ;; Don't need to recur on exprs:
          [(if ,test ,conseq ,altern)
	   `(if ,test ,conseq ,altern)]
          [,prim (guard (regiment-constant? prim)) prim]
	  ;; Don't need to recur on rands:
          [(,prim ,rand* ...)
           (guard (regiment-primitive? prim))
	   `(,prim ,rand* ...)]
          [,unmatched
	   (error 'add-heartbeat "invalid syntax ~s" unmatched)])))

    ;; Body of match case:    
    `(add-heartbeats-language
      (quote (program (props ,proptable ...)
		      ,(process-let letexpr)
		      ,type))))]))))

) ; End module