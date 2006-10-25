

; <TODO> -- Doesn't do anchors right now..

;;;; FIXME: NEED TO FIX THIS ACCORDING TO THE NEW TYPE SYSTEM.


;;; Pass 13: classify-names

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
  
  ;; This table accumulates all the properties which become attached to names:
  (define table '())
  ;; <TODO>, <TOOPTIMIZE> Ryan, rewrite this with a hash table for more speediness.

  ;; This accumulates a graph of dependencies:
  (define dependencies '())

  ;; Note, you can send this #f instead of a symbol to make it fizzle:
  (define add-props! 
    (letrec ([check-valid (lambda (name props)
			    (DEBUGASSERT (list? props))
			    (cond 
			     [(subset? '(distributed local) props)
			      (error 'classify-names:add-props! 
				     "var ~s cannot be both a distributed and local value: ~a"
				     name props)]
					; final and leaf?
					; anchor and region?
			     ))])
    (lambda (s p*)
      (DEBUGASSERT (andmap (lambda (p) (match p 
					 [,s (guard (symbol? s)) #t]
					 [(,s1 ,s2) (guard (symbol? s1) (symbol? s2)) #t]
					 [,else #f]))
			   p*))
      (if s
	  (let ((entry (assq s table)))
	    ;(inspect (vector p* entry (and entry (union p* (cdr entry))) table))
	    (if entry 
		(begin 
		  (set-cdr! entry (union p* (cdr entry)))
		  ;; Every time we change, check sanity:
		  (check-valid s (cdr entry)))
		(set! table (cons (cons s p*) table)))
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
      
  ;=============================================================  
  
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

  (define (simple-rand? expr)
    (match expr
	   [,var (guard (symbol? var) (not (regiment-constant? var))) 
		 #t]
	   [(quote ,const) (guard (or (constant? const) (symbol? const))) #t]
	   [,else #f]))

  ;; This reconciles a variable with the type that it's expected to
  ;; have based on its usage context.  Basically it just tries to add
  ;; props and add-props! raises an error if conflicting properties are
  ;; attached to a variable.  
  (define reconcile-type 
    (lambda (type name)
    (match type
      [Region
       (add-props! name '(distributed area region))]
      
      [(Area ,a)
       (add-props! name '(distributed area))]

      [(Signal ,a)
       (add-props! name '(distributed signal))]
      
      [Anchor
       (add-props! name '(distributed anchor))]

      ;; Does Node belong here?
      [,t (guard (memq t '(Location Reading Number Integer Float Bool Node Tuple)))
	  (add-props! name '(local))]

      ;; TEMP:
      [Object (add-props! name '())]

      ;; TEMP: These should have been static-elaborated away.
      [(List ,t)
       (add-props! name '(local))]
      
      [(Event ,a)
       (add-props! name '(distributed event))]

      [(quote ,a) (guard (symbol? a)) (add-props! name ())]
      [(,[a] ... -> ,[b])             (add-props! name ())]
      [#(,[t] ...)                    (add-props! name ())]

      [,else (error 'classify-names:reconcile-type "invalid type: ~s" type)])))

  
  ;; TODO: FIXME: Replace, this should be out-dated by the new type system.
  ;; This is a cheap (and definitely non-polymorphic) kind of type
  ;; inference for variables used as arguments to primitive functions.
  (define (type-inference-primapp prim args)
    (let ((entry (get-primitive-entry prim)))
      (for-each 
       (lambda (arg type)
	 (match arg
		[(quote ,const) (void)]
		[,var (guard (symbol? arg))	      
		      ;(disp "VAR: " var type)
		      (reconcile-type type var)]
		[,other (error 'classify-names:type-inference-primapp
			       "primitive ~s should take only simple arguments: ~s" prim other)]))
       args (fit-formals-to-args (cadr entry) args))))

  (define (process-let name expr env)
    (match expr
	   [(lazy-letrec ([,lhs* ,type* ,rhs*] ...) ,expr)
	    (if (not (symbol? expr)) (error 'classify-names "body of lazy-letrec should be a symbol!"))
	    (for-each (lambda (lhs rhs)
			(process-expr lhs rhs env))
		      lhs* rhs*)
	    (process-expr name expr (union lhs* env))]))

  (define process-expr
    (lambda (name expr env)
      (match expr
	     
	  ;; This means that the name is bound directly to a constant.
	  ;; A local binding:
          [(quote ,const) (add-props! name '(local))]

	  ;; Here the name is bound directly to this other name, we
	  ;; temporarily add an alias, then at the end we replace it.
          [,var (guard (symbol? var) (not (regiment-constant? var)))
		;(add-dependency! name (list var))
		(add-props! name `((alias-of ,var)))
		]
		
          [(lambda ,formalexp ,types (lazy-letrec ,binds ,bod))
	   ;; NOTE: No additional dependency for name... this is a closure.
	   ;; <TODO> CONSIDER THIS: It would let dependencies go through lambdas.
	   ;(process-let name expr (union formalexp env)) 
	   (add-props! name `(function local (returns ,bod)))
	   (let ((formals (get-formals formalexp)))
	     (for-each (lambda (v) (add-props! v '(formal)))
	       formals)
	     ;; Add a dependency on the formal params so the system
	     ;; doesn't think its got leaf nodes:
	     (for-each (lambda (bind)
			 (add-dependency! (car bind) formals))
	       binds)
	     (process-let #f `(lazy-letrec ,binds ,bod) (union formalexp env)))
	   ]
 
	  ;; These are all simple.
	  [(if ,test ,conseq ,altern)
	   ;; This is silly: <TODO> REMOVE
	   (add-props! name '(conditional))
	   (add-dependency! name (union (free-vars test)
					(free-vars conseq)
					(free-vars altern)))]

	  
	  ;; BETTER HANDLE ALL CONSTANTS HERE
          [world 
	   (add-props! name '(area region leaf))]
          [anchor 
	   (add-props! name '(area leaf))]

	  ;; Does nothing:
#;	  [(tuple ,args ...)
	   (add-dependency! name (apply union (map free-vars args)))]
	  [(tupref ,n ,m ,x)
	   (add-dependency! name (free-vars x))
	   (type-inference-primapp 'tupref (list `(quote ,n) `(quote ,m) x))
	   (reconcile-type 
	    (get-primitive-return-type 'tupref)
	    name)]

          [(,prim ,rand* ...)
           (guard (regiment-primitive? prim))
	   (add-dependency! name (apply union (map free-vars rand*)))
	   
	   ;; Process varrefs
	   (type-inference-primapp prim rand*)

	   ;; Process var binding:
	   (reconcile-type 
	    (get-primitive-return-type prim)
	    name)

	   #;
	   (cond 
	    [(distributed-primitive? prim)
	     (add-props! name '(distributed))
	     (cond
	      [(eq? 'Region (get-primitive-return-type prim))
	       (add-props! name '(area region))]
	      [(eq? 'Area (get-primitive-return-type prim))
	       (add-props! name '(area))]
	      [(eq? 'Signal (get-primitive-return-type prim))
	       (add-props! name '(signal))]
	      [(eq? 'Anchor (get-primitive-return-type prim))
	       (add-props! name '(anchor))])]
	    [(basic-primitive? prim) (add-props! name '(local))]
	    [else (error 'classify-names.process-expr 
			 "This regiment primitive is neither basic nor distributed!:~s"
			 prim)])
	   ]
	   
          [,unmatched
	   (error 'classify-names "invalid syntax ~s" unmatched)])))

  ;=============================================================  
    
    (match expr
	   [(,input-language (quote (program (lazy-letrec (,binds ...) ,fin) ,type)))
	    (add-props! fin '(final))
	    ;; This mutates the global list 'dependencies'.
	    (process-let #f `(lazy-letrec (,binds ...) ,fin) ())
	    ;; Now we label the leaves:
	    (for-each (lambda (node)
			(if (memq 'distributed (get-props (car node)))
			    (let ([these-deps
				   ;; Filter down to only those deps that count:
				   (filter (lambda (dep)
					     ;(disp dep (get-props dep))
					     (let ((props (get-props dep)))
					       (or (not (memq 'local props))
						   (memq 'formal props))))
				     (cdr node))])      
			      (if (null? these-deps) ;; No dependencies
				  (add-props! (car node) '(leaf)))
			      )))
		      dependencies)

	    ;(printf "Dependencies:\n")
	    ;(pp dependencies)
	    
	    (let ((finaltable 
		   (cons 'props 
		     (map (lambda (entry)
			    ;; Well, there had better not be more than
			    ;; one alias-of's in there:
			    (DEBUGMODE
			     (if (not (<= (length (filter (lambda (ls) (match ls [(alias-of ,v) #t] [,_ #f]))
						    (cdr entry))) 1))
				 (error 'classify_names
					"makes no sense! entry is an `alias` of more than one variable!: ~s"
					entry)))
			    (cons (car entry)
				  ;; <TOOPTIMIZE>
				  (apply union
					 (map (lambda (prop)
					      (match prop
						     [(alias-of ,x) 
						      ;(disp "PROP IS X" x)
						      (cdr (assq x table))]
						     [,prop (list prop)]))
					      (cdr entry)))))
			  table))))

	    ;; <TODO> Need to write the little lnaguage stub.
	    `(classify-names-language
	      '(program ,finaltable (lazy-letrec (,binds ...) ,fin) ,type))
	    )]
	   ))


(define these-tests 
  `(



  ))

(define test-this (default-unit-tester
		    "13: Classify-Names: to annotate simple variable properties"
		    these-tests))

(define test13 test-this)
(define tests13 these-tests)
(define test-classify-names test-this)
(define tests-classify-names these-tests)
