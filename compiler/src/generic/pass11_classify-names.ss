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
  (define (add-prop! s p)
    (if s
	(let ((entry (assq s table)))
;	  (DEBUGMODE (if (not entry)
;			 (error 'pass09_classify-names:add-prop!
;				"symbol ~s has no entry in table ~s" s table)))
	  (if entry 
	      (if (not (memq p (cdr entry)))
		  (set-cdr! entry (cons p (cdr entry))))
	      (set! table (cons (list s p) table))))))

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
	     [,var (guard (symbol? var)) (if (memq var env) '() (list var))]   
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
	   [,var (guard (symbol? var)) #t]
	   [(quote ,const) (guard (constant? const)) #t]
	   [,else #f]))

  (define process-expr
    (lambda (name expr env)
      (match expr
	     
	  ;; This means that the name is bound directly to a constant.
	  ;; A local binding:
          [(quote ,const) (add-prop! name 'local)]

	  ;; Here the name is bound directly to this other name, we
	  ;; temporarily add an alias, then at the end we replace it.
          [,var (guard (symbol? var)) 
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
	  
          [(,prim ,rand* ...)
           (guard (regiment-primitive? prim))
	   (add-dependency! name (apply union (map free-vars rand*)))
	   
	   (cond 
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
	   (error 'verify-core "invalid syntax ~s" unmatched)])))

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
						     [(alias-of ,x) (cdr (assq x table))]
						     [,prop (list prop)]))
					      (cdr entry)))))
			  table))))

	    ;; <TODO> Need to write the little lnaguage stub.
	    `(classify-names-language
	      '(program ,finaltable (lazy-letrec (,binds ...) ,fin)))
	    )]
	   ))
