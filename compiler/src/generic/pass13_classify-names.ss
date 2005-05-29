;; NEED TO RENUMBER

; <TODO> -- Doesn't do anchors right now..

;;; Pass 09: classify-names

;;; This pass takes the simple top level progam, which is basically a
;;; dataflow graph, and categorizes its edges.

;;;   In particular, this pass builds an association list binding the
;;; following "properties" to names:
;;;   'distributed
;;;   'local
;;;   'region
;;;   'anchor
;;;   'final
;;;   'leaf
;; [2004.06.13] Added these too, might use them later:
;;;   'conditional
;;;   'function
;;;   '(alias-of <var>)

;;; Final implies that a value is the return value of the entire
;;; program.  Leaf implies that it's a value which must be jump
;;; started for eager evaluation to work (a value produced by a
;;; leaf-node primitive).
;;;   Distributed values involve the network, local values touch only
;;; one node.  Region and anchors are not specifically identified yet,
;;; but these are essentially types for the bindings.


;;; The input language from the last pass is the standard core language

;;; The only difference in the Output Language is that the program
;;; form includes an extra "catalogue" slot

;;; <Pgm>  ::= (program (props <CatEntry>*) <Let>)
;;; <CatEntry>* ::= [<Name> <Prop>*]
;;; <Prop> ::= region | anchor | local | distributed | final | leaf

(define (classify-names expr)
  
  ;; This table accumulates all the properties which become attached
  ;; to names:
  (define table '())
  ;; <TODO>, <TOOPTIMIZE> Ryan, rewrite this with a hash table for more speediness.

  ;; This accumulates a graph of dependencies:
  (define dependencies '())

  ;; Note, you can send this #f instead of a symbol to make it fizzle:
  (define add-prop! 
    (letrec ([all-memq (lambda (obs ls)
			 (if (null? obs) #t
			     (if (memq (car obs) ls)
				 (all-memq (cdr obs) ls)
				 #f)))]
	     [check-valid (lambda (name prop proplst)
			    (cond 
			     [(all-memq '(distributed local) proplst)
			      (error 'classify-names:add-prop! 
				     "var ~s cannot be both a distributed and local value: Cannot add ~s."
				     name prop)]
			     ; final and leaf?
			     ; anchor and region?
			     ))])
    (lambda (s p)
      (if s
	  (let ((entry (assq s table)))
					;	  (DEBUGMODE (if (not entry)
					;			 (error 'pass09_classify-names:add-prop!
;				"symbol ~s has no entry in table ~s" s table)))
	    (if entry 
		(if (not (memq p (cdr entry)))
		    (begin 
		      (set-cdr! entry (cons p (cdr entry)))
		      (check-valid s p (cdr entry))
		      ))
		(set! table (cons (list s p) table)))	   
	    )))))
    
  (define (get-props s)
    (let ((entry (assq s table)))
      (if entry (cdr entry) '())))

  (define (add-dependency! sym syms)
    (if sym
	(let ((entry (assq sym dependencies)))
	  (if entry
	      (set-cdr! entry (union (cdr entry) syms))
	      (set! dependencies (cons (cons sym syms) dependencies))))))
      
  ;;============================================================  
  
  (define (free-vars expr)
    (let loop ((env ()) (expr expr))
      (match expr	 
	     [,var (guard (symbol? var) (not (regiment-constant? var)))
		   (if (memq var env) '() (list var))]
	     [(quote ,x) '()]
	     [(,prim ,rand* ...) (regiment-primitive? prim)
	      (let ((frees (map (lambda (x) (loop env x)) rand*)))
		(apply append frees))]
	     [(lambda (,formals) ,expr)
	      (loop (append formals env) expr)]
	     [,else (error 'free-vars "not simple expression: ~s" expr)])))

  (define (process-let name expr env)
    (match expr
	   [(lazy-letrec ([,lhs* ,rhs*] ...) ,expr)
	    (for-each (lambda (lhs rhs)
			(process-expr lhs rhs env))
		      lhs* rhs*)
	    (process-expr name expr (union lhs* env))]))
  
  
  (define (simple-rand? expr)
    (match expr
	   [,var (guard (symbol? var) (not (regiment-constant? var))) 
		 #t]
	   [(quote ,const) (guard (constant? const)) #t]
	   [,else #f]))

  ;; This reconciles a variable with the type that it's expected to
  ;; have based on its usage context.  Basically it just tries to add
  ;; props and add-prop! raises an error if conflicting properties are
  ;; attached to a variable.  
  (define reconcile-type 
    (lambda (type name)
    (case type
      [(Region)
       (add-prop! name 'distributed)
       (add-prop! name 'area)
       (add-prop! name 'region)]
      
      [(Area)
       (add-prop! name 'distributed)
       (add-prop! name 'area)]

      [(Signal)
       (add-prop! name 'distributed)
       (add-prop! name 'signal)]
      
      [(Anchor)
       (add-prop! name 'distributed)
       (add-prop! name 'anchor)]

      ;; Does Node belong here?
      [(Location Reading Function Number Integer Float Bool List Node)
       (add-prop! name 'local)]

      ;; There's really not anything that we know about objects...
      [(Object)
       (add-prop! name 'unknown)]

      [(Event)
       (add-prop! name 'distributed)
       (add-prop! name 'event)
       ;(error 'classify-names:reconcile-type "unhandled type: ~s" type)
       ]

      [else (error 'classify-names:reconcile-type "invalid type: ~s" type)])))

  ;; This is a cheap (and definitely non-polymorphic) kind of type
  ;; inference for variables used as arguments to primitive functions.
  (define (type-inference-primapp prim args)
    (let ((entry (get-primitive-entry prim)))
      (for-each 
       (lambda (arg type)
	 (match arg
		[(quote ,const) (void)]
		[,var (guard (symbol? arg))		      
		      (reconcile-type type var)]
		[,other (error 'classify-names:type-inference-primapp
			       "primitive ~s should take only simple arguments: ~s" prim args)]))
       args (cadr entry))))

  (define process-expr
    (lambda (name expr env)
      (match expr
	     
	  ;; This means that the name is bound directly to a constant.
	  ;; A local binding:
          [(quote ,const) (add-prop! name 'local)]

	  ;; Here the name is bound directly to this other name, we
	  ;; temporarily add an alias, then at the end we replace it.
          [,var (guard (symbol? var) (not (regiment-constant? var)))
		;(add-dependency! name (list var))
		(add-prop! name `(alias-of ,var))
		]
		
          [(lambda ,formalexp ,expr)
	   ;; NOTE: No additional dependency for name... this is a closure.
	   ;; <TODO> CONSIDER THIS:
	   ;(process-let name expr (union formalexp env))
	   (add-prop! name 'function)
	   (add-prop! name 'local)
	   (process-let #f expr (union formalexp env))
	   
	   ]
 
	  ;; These are all simple.
	  [(if ,test ,conseq ,altern) 
	   ;; This is silly: <TODO> REMOVE
	   (add-prop! name 'conditional)
	   (add-dependency! name (union (free-vars test)
					(free-vars conseq)
					(free-vars altern)))]
	  
	  ;; BETTER HANDLE ALL CONSTANTS HERE
          [world 
	   (add-prop! name 'area)
	   (add-prop! name 'region)
	   (add-prop! name 'leaf)]
          [anchor 
	   (add-prop! name 'area)
	   (add-prop! name 'leaf)]


          [(,prim ,rand* ...)
           (guard (regiment-primitive? prim))
	   (add-dependency! name (apply union (map free-vars rand*)))
	   
	   ;; Process varrefs
	   (type-inference-primapp prim rand*)

	   ;; Process var binding:
	   (reconcile-type 
	    (get-primitive-return-type prim)
	    name)

	   '(cond 
	    [(distributed-primitive? prim)
	     (add-prop! name 'distributed)
	     (cond
	      [(eq? 'Region (get-primitive-return-type prim))
	       (add-prop! name 'area)
	       (add-prop! name 'region)]
	      [(eq? 'Area (get-primitive-return-type prim))
	       (add-prop! name 'area)]
	      [(eq? 'Signal (get-primitive-return-type prim))
	       (add-prop! name 'signal)]
	      [(eq? 'Anchor (get-primitive-return-type prim))
	       (add-prop! name 'anchor)])]
	    [(basic-primitive? prim) (add-prop! name 'local)]
	    [else (error 'classify-names.process-expr 
			 "This regiment primitive is neither basic nor distributed!:~s"
			 prim)])
	   ]
	   
          [,unmatched
	   (error 'classify-names "invalid syntax ~s" unmatched)])))

  ;;============================================================  
    
    (match expr
	   [(,input-language (quote (program (lazy-letrec (,binds ...) ,fin))))
	    (add-prop! fin 'final)	    
	    ;; This mutates the global list 'dependencies'.
	    (process-let #f `(lazy-letrec (,binds ...) ,fin) ())
	    ;; Now we label the leaves:
	    (for-each (lambda (node)
			(if (memq 'distributed (get-props (car node)))
			    (let ([these-deps 
				   (filter (lambda (dep)
					     (not (memq 'local (get-props dep))))
					   (cdr node))])			      
			      (if (null? these-deps) ;; No dependencies
				  (add-prop! (car node) 'leaf))
			      )))
		      dependencies)
	    
	    (let ((finaltable 
		   (cons 'props 
		     (map (lambda (entry)
			    ;; Well, there had better not be more than
			    ;; one alias-of's in there:
			    (DEBUGMODE
			     (if (not (<= (length (filter list? (cdr entry))) 1))
				 (error 'classify_names
					"makes no sense! entry is an `alias` of more than one variable!: ~s"
					entry)))
			    (cons (car entry)
				  ;; <TOOPTIMIZE>
				  (apply union
					 (map (lambda (prop)
					      (match prop
						     [(alias-of ,x) 
						      (disp "PROP IS X" x)
						      (cdr (assq x table))]
						     [,prop (list prop)]))
					      (cdr entry)))))
			  table))))

	    ;; <TODO> Need to write the little lnaguage stub.
	    `(classify-names-language
	      '(program ,finaltable (lazy-letrec (,binds ...) ,fin)))
	    )]
	   ))


(define these-tests 
  `(

    [(analyze-places '(add-places-language
		       '(program (props) (control-flow)
			       (lazy-letrec ([result 10 X? X? 3])
					    result))))
     unspecified]

  ))

(define test-this (default-unit-tester
		    "13: Classify-Names: to annotate simple variable properties"
		    these-tests))

(define test13 test-this)
(define tests13 these-tests)
(define test-classify-names test-this)
(define tests-classify-names these-tests)
