

;;;; .title WaveScript EmitC Version TWO
;;;; .author Ryan Newton

;;;; This uses the generic C-generation libary (c_generator.ss).
;;;; Unlike the first version (emit-c.ss) this version does not rely on:

;;;;  (1) C++ features (templates etc)
;;;;  (2) XStream classes (Sigseg, etc)
;;;;  (3) Boost smart pointers

;;;; It produces "lower level" C code than the first version, meant to
;;;; be used with a custom garbage collector.

(module emit-c2 mzscheme 
  (require  "../../../plt/common.ss"
	    (all-except (lib "list.ss") sort sort! filter)
	    (all-except "nominalize-types.ss" test-this these-tests)
	    "convert-sums-to-tuples.ss"
	    "../../compiler_components/c_generator.ss" )
  (provide emit-c2)
  (chezprovide )  
  (chezimports (except helpers test-this these-tests))

;; These are just for sanity checking.  Disjoint types keep mix-ups from happening.
(reg:define-struct (lines text))
(reg:define-struct (expression text))
(define (append-lines ls) (make-lines (apply append (map lines-text ls))))
(define (idk x) (ASSERT expression? x) x)




(define incr-local-refcount 0)
(define decr-local-refcount 0)

(define incr-heap-refcount 0)
(define decr-heap-refcount 0)


(define (Type ty) 
  (match ty))

(define Var symbol->string)

;; This generates the "emit" code that connects the control flow
;; between one box and another.
(define (Linkage name val) 
  99)

    ;; Generates code for an emit.  (curried)
    ;; .param down*   A list of sinks (names of functions) to emit to.
    ;;                These can also be (NUM NAME) pairs for indexed ports.
    (define (Emit down*)
      ;;(ASSERT (not (null? down*)))
      (lambda (expr)
	;; Just call each of the sites with the argument.
	(obj 'make-let `(["emitted" ,expr])
		  (apply make-seq
			 (append 
			  (map (lambda (down) 
				 (cond 
				  [(eq? down 'BASE) `("baseSink emitted")]
				  ;; If we're emitting *to* a unionN, we include the index tag.
				  [(pair? down)
				   (ASSERT fixnum? (car  down))
				   (ASSERT symbol? (cadr down))
				   
				   (ASSERT (= (length down) 2))
				   (list (Var (cadr down))
					 "("(obj 'Const (car down))", emitted )")]
				  [else 
				   (ASSERT symbol? down)
				   `(,(Var down)" emitted")]))
			    down*)
			  '("()")))
		  )))

(define (ConstBind cb)
  (match cb
    [(,[Var -> v] ,[Type -> t] ,rhs)
     (Value rhs (lambda (x) `(,v ,t ,x)))]
    [,oth (error 'ConstBind "Bad ConstBind, got ~s" oth)]) )

(define (Effect xp emitter)
  (match xp
    [(emit ,[Simple -> x])
     (Linkage ??? x)
     
     ]
    ))

;; The continuation k is invoked on a piece of text representing the return expression.
;; k is expected to return text of the form "lines" that stores away this result.
(trace-define (Value xp emitter k)
  (match xp))

;; .returns code body (lines) and state decls (lines) 
(define (Source xp)
  (match xp
    [((name ,nm) (output-type ,ty) (code ,cd)  (outgoing ,down* ...))
     (match cd 
       [(timer ,annot ',rate)
	(ASSERT integer? rate)

	;; For the basic backend, we simply call the downstream
	;; operator when we ourselves are asked for data (invoked).
	(values 
	 (apply append-lines 
		(map (lambda (down) (Linkage down (Value '(tuple) id)))	   
		  down*))
	 
	 ())
	
	;(inspect 'woot)
	])]))

;; .returns top-level decls (lines)
(define (Operator op)
  (match op
    [(iterate (name ,name) 
	      (output-type ,ty)
	      (code (iterate ,annot ,itercode ,_))
	      (incoming ,up)
	      (outgoing ,down* ...))
     
     (match itercode
       [(let ([,lhs* ,ty* ,rhs*]) (lambda (,v ,vq) (,vty (VQueue ,outty)) ,bod))	
	(values 

	 (make-fun-decl name			
			(Value bod (lambda (x) (make-lines `("return ",(expression-text x)";\n"))))
	  )
	 
	 )])
     ]))

(define-pass emit-c2
  [Expr (lambda (xp fallthru) (Value xp (lambda (_) "")))]
  [Program 
   (lambda (prog Expr)
     	 (match prog
	   [(,lang '(graph (const ,[ConstBind -> cb*] ...)
			   (init  ,[Effect -> init*] ...)
			   (sources ,[Source -> src*  state1**] ...)
			   (operators ,[Operator -> oper* state2**] ...)
			   (sink ,base ,basetype)
			   ,_))
	    
	    ;; state** ?? 

	    (map line-text (list cb* init* src* oper* ))]

	   [,other ;; Otherwise it's an invalid program.
	    (error 'emit-c2 "ERROR: bad top-level WS program: ~s" other)]))]
  ) ;; End pass




) ;; End module

