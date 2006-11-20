
;;;; .title WaveScript Nominalize Types
;;;; .author Ryan Newton

;;;; This pass replaces the ML-like algebraic types and replaces them with suitable C-types.


;;;; TODO: Make grammar enforcing no tuprefs.  ESPECIALLY since there is no typechecking after this.



  ;; This gets set later in a different scope.
  (define bindings-fun 'uninit)

  ;; This looks up a variable's tuple type in the tupdef bindings.
  ;; I can't push it down deeper because currently the define-pass
  ;; macro only works at top level.
  (define-pass convert-types [Bindings (lambda args (apply bindings-fun args))])



(module wavescript_nominalize-types  mzscheme 
  (require "helpers.ss")
  (provide nominalize-types test-this test-nominalize-types)
  (chezprovide )
  (chezimports (except helpers test-this these-tests)
	       (except reg_core_generic_traverse test-this these-tests))

  ;; Does this type have a known size?
  ;; Matters for making tuples into structs.
  (define (known-size? t)
    (define (id x) x)
    (match t
      [,simple (guard (symbol? simple)) #t]
      ;; TODO: FIXME: Use the type alias table, don't check for Region/Anchor directly:
      [(,qt ,v) (guard (memq qt '(quote NUM)) (symbol? v)) #f]
      [(,qt (,v . ,[t])) (guard (memq qt '(quote NUM)) (symbol? v)) t]
      ;; This shouldn't be in a tuple anyway:
      [(,[arg] ... -> ,[ret]) #f]

      [(Struct ,name)         #t]
      ;; These are pointers, doesn't matter what the type inside is.
      [(Sigseg ,_)            #t]
      [(Array  ,_)            #t]
      [(List   ,_)            #t]
      [(,C ,[t] ...) (guard (symbol? C)) (andmap id t)]
      [#(,[t] ...) (andmap id t)]
      [,else #f]))


  (define nominalize-types
    (let ()
      ;; The generic traversal returns an intermediate value of type
      ;; #(Expr TypeDefs) where TypeDefs is list of [Name [StructFieldType ...]] pairs.
      (reg:define-struct (result expr tydefs))

      ;; The fixed names of fields.
      (define field-names 
	(map (lambda (n) (string->symbol (format "fld~s" (fx+ 1 n))))
	  (iota MAX_TUPLE_SIZE)))

      ;; An association list accumulating new struct types.
      ;; 
      (define struct-table '())
      
      ;; Must be an exported type.
      (define (convert-type ty tupdefs)
	(match ty
	  [,s (guard (symbol? s)) s]
	  ;; Allowing polymorphic types for list... damn null lists.
	  [(List ',_) `(List ',_)]
	  [(,qt ,v) (guard (memq qt '(quote NUM)))
	   (error 'nominalize-types:convert-type
		  "should not have polymorphic type: ~s" ty)]
	  [(,[arg] ... -> ,[ret]) `(,@arg -> ,ret)]
	  [(,C ,[t] ...) (guard (symbol? C)) `(,C ,@t)]
	  [#(,t* ...)
	   ;; Do the lookup on the *pre*converted type:
	   ;; It's ok if there are duplicates in the tupdefs, this will get the first.
	   (match (assoc t* tupdefs)
	     [#f (error 'nominalize-types:convert-type
			"cannot find tuple type ~s in tuple defs:\n ~s" (list->vector t*) tupdefs)]
	     [(,types ,flds ,structname) `(Struct ,structname)])]
	  [,else (error 'nominalize-types:convert-type "unmatched type: ~s" else)]))
      
           
      (define (add-new-tydef def lst)
	(let ([ty* (car def)])
	  (DEBUGASSERT (andmap type? ty*))
	  (if (assoc ty* lst)
	      lst
	      (cons def lst)
	      )))      
      (define (append-tydefs2 defs1 defs2)
	(if (null? defs1) defs2
	    (add-new-tydef (car defs1)
			   (append-tydefs (cdr defs1) defs2))))
      (define (append-tydefs . args)
	(if (null? args) '()
	    (append-tydefs2 (car args)
			    (apply append-tydefs (cdr args)))
	    ))

      (define (insert-struct-names expr tupdefs)
	(core-generic-traverse
	 (lambda (x fallthru)
	   (match x
	     [(assert-type ,t ,[e])
	      `(assert-type ,(convert-type t tupdefs) ,e)
	      ]
	     [(make-struct ,ty* ,[args] ...)
	      `(make-struct ,(last (assoc ty* tupdefs)) ,@args)]
	     [,oth (fallthru oth)])
	   )
	 (lambda (ls k) (apply k ls))
	 expr))

      ;; We avoid the boilerplate by defining this as a "generic traversal"
      (define (collect-tupdefs expr tenv)
	(core-generic-traverse/types
	 ;; Driver
	 (lambda (expr tenv loop)
	   ;; Everything returned must be an intermediate result.
	   (ASSERT result?
	    (match expr
	     ;; Tuple statements HAVE to be type annotated now.
	     [(tuple ,arg* ...)
	      (printf "TUPLE!! ~s\n" arg*)	 
	      (let ([type (recover-type `(tuple . ,arg*) tenv)]
		    [newtype (unique-name 'tuptyp)])
		(match type
		  [#(,argtypes ...)
		   (unless (andmap known-size? argtypes)
		     (error 'nominalize-types
			    "there should not remain polymorphic tuples of this sort after static-elab: ~s" 
			    type))
		   (let* ([results (map (lambda (x) (collect-tupdefs x tenv)) arg*) ]
			  [args (map result-expr results)]
			  [tydefs (apply append-tydefs (map result-tydefs results))]
			  [mytypes (map (lambda (arg) (recover-type arg tenv)) arg*)]
			  [newdefs (add-new-tydef
				    (list mytypes
					  (list-head field-names (length arg*))
					  newtype)
				    tydefs)]
			  ;; Get the name back out... might not be the one we put in.
			  ;[type-name (last (ASSERT (assoc mytypes newdefs)))]
			  )		    
		     (make-result 
		      ;; We replace mytypse with an actual name in a second pass:
		      `(make-struct ,mytypes ,args ...)
		      ;; Append a new typedef to the existing.
		      ;; Don't convert types for this entry:
		      newdefs)
		     )]))]
	     
	     [(unionList ,ls)
	      (let ([tupletype (recover-type `(unionList ,ls) tenv)]
		    [newstruct (unique-name 'unionlst_tuptyp)])
		(match tupletype
		  [(Signal #(Int ,argtype))
		   (unless (known-size? argtype)
		     (error 'nominalize-types
			    "there should not remain polymorphic tuples of this sort after static-elab: ~s" 
			    tupletype))
		   (let ([result (collect-tupdefs ls tenv)])
		     (make-result 
		      ;; Annotate the primapp with the struct-type.
		      `(unionList ,newstruct ,(result-expr result))
		      (add-new-tydef
		       `((Int ,argtype) ,(list-head field-names 2) ,newstruct)
		       (result-tydefs result))
		      ))]))]

	     ;; tuprefs are simple:
	     [(tupref ,i ,len ,[result])
	      (make-result `(struct-ref ,(result-expr result) ,(list-ref field-names i))
			   (result-tydefs result))]
	     
	     ;; DEBUGGING
	     #;
	     [(iterate ,f ,s)
	      (let ([result (loop `(iterate ,f ,s) tenv)])
		(match (result-expr result)
		  [(iterate ,f ,s) 
			 (printf "SOURCE: ~s\n" s)
			 
			 ])
		result)
	      ]

	     [,other (loop other tenv)]
	     )
		   ))
	 ;; Fuser
	 (lambda (ls k)
;	   (printf "FUSING: ~s\n\n" ls)
	   (make-result (apply k (map result-expr ls))
			(apply append-tydefs (map result-tydefs ls))))
	 expr tenv
	 ))

      ;; TODO: REMOVE DUPLICATE STRUCT DEFS THAT HAVE THE SAME TYPES
      (define (remove-redundant tupdefs)
	;; FIXME FIXME FIXME 
	tupdefs)


      ;; What structs are used within a type?
      (define (type->structs t)
	(match t
	  [,s (guard (symbol? s)) '()]
	  [(,qt ,v) (guard (memq qt '(quote NUM)) (symbol? v)) '()]
	  [(,qt (,v . ,[ls])) (guard (memq qt '(quote NUM)) (symbol? v)) ls]
	  [(,[arg*] ... -> ,[ret]) (apply append ret arg*)]
	  [(Struct ,name) (list name)]
	  [(,C ,[ls*] ...) (guard (symbol? C)) (apply append ls*)]
	  [#(,ls* ...) (error 'type->structs "shouldn't be any tuple types left! ~s" 
			      (list->vector ls*))]
	  [,oth (error 'type->structs "unrecognized type: ~s" oth)]))
      
      ;; Topological sort on struct-defs
      (trace-define (sort-defs defs)	
	(let ([edges (map (lambda (def)
			    (match def
			      [(,name [,fld* ,ty*] ...)
			       (cons name (apply append (map type->structs ty*)))
			       ]))
		       defs)])
	  (map (lambda (name)
		 (assq name defs))
	    (reverse (tsort edges)))))

      ;; Main body:
      (lambda (prog) 
	(match prog 
	  [(,lang '(program ,body ,type))
	   (let* ([result (collect-tupdefs body (empty-tenv))]
		  ;[newbod (result-expr result)]
		  [tupdefs (remove-redundant (result-tydefs result))]
		  [newbod (insert-struct-names (result-expr result) tupdefs)]
		  )
	     
	     (define (do-bindings vars types exprs reconstr exprfun) 
	       (reconstr vars (map (lambda (t) (convert-type t tupdefs)) types) 
			 (map exprfun exprs)))

	     (set! bindings-fun do-bindings)
	     
	     (match (convert-types `(,lang '(program ,newbod ,type)))
	       [(,lang '(program ,body ,type))
		;; Running the type-checker/inferencer isn't going to work on this output any longer:
		`(nominalize-types-language
		  '(program ,body		       
		     ;; We stick the type definitions here:
		     (struct-defs 
		      ,@(sort-defs
			 (map (match-lambda ((,types ,flds ,name))
				`(,name ,@(map list flds 
					       (map (lambda (t) (convert-type t tupdefs)) 
						 types))))
			   tupdefs)))
		     ,(convert-type type tupdefs)))])
	     )]))))

  (define these-tests  
    `(
      ["remove all those tuprefs"
       (deep-assq 'tupref
		  (nominalize-types '(type-print/show-language
			   '(program
				(let ([s1_1 (Signal (Sigseg Int))
					    (audioFile '"./countup.raw" '4096 '0)])
				  (let ([s2_2 (Signal #(Int Int))
					      (iterate
					       (lambda (w_3)
						 ((Sigseg Int))
						 (let ([VIRTQUEUE_4 (VQueue #(Int Int)) (virtqueue)])
						   (begin
						     (emit VIRTQUEUE_4 (tuple (width w_3) (start w_3)))
						     VIRTQUEUE_4)))
					       s1_1)])
				    (let ([s3_5 (Signal #(Int Int Float))
						(iterate
						 (lambda (pattmp_6)
						   (#(Int Int))
						   (let ([x_7 Int (tupref 0 2 pattmp_6)])
						     (let ([y_8 Int (tupref 1 2 pattmp_6)])
						       (let ([VIRTQUEUE_9 (VQueue #(Int Int Float)) (virtqueue)])
							 (begin
							   (emit VIRTQUEUE_9 (tuple y_8 x_7 '3.0))
							   VIRTQUEUE_9)))))
						 s2_2)])
				      s3_5)))
			      (Signal #(Int Int Float))))))
       #f]

      ["tuples of tuples"
       (nominalize-types '(type-print/show-language
			   '(program
				(tuple 1 (tuple 2 3))
			      (Signal #(Int #(Int Int))))))
       (nominalize-types-language
	'(program
	     (make-struct tuptyp_5 1 (make-struct tuptyp_6 2 3))
	   (struct-defs
	    (tuptyp_5 (fld1 Int) (fld2 (Struct tuptyp_6)))
	    (tuptyp_6 (fld1 Int) (fld2 Int)))
	   (Signal (Struct tuptyp_5))))
       ]
      

      ))
  (define test-this (default-unit-tester "" these-tests))
  (define test-nominalize-types test-this)

) ;; End module.





