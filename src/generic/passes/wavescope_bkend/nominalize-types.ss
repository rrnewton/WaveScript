
;;;; .title WaveScript Nominalize Types
;;;; .author Ryan Newton

;;;; This pass replaces the ML-like algebraic types and replaces them with suitable C-types.



  ;; This gets set later in a different scope.
  (define bindings-fun 'uninit)

  ;; This looks up a variable's tuple type in the tupdef bindings.
  ;; I can't push it down deeper because currently the define-pass
  ;; macro only works at top level.
  (define-pass convert-types [Bindings bindings-fun])



(module wavescript_nominalize-types  mzscheme 
  (require "helpers.ss")
  (provide nominalize-types test-this test-nominalize-types)
  (chezprovide )
  (chezimports (except helpers test-this these-tests)
	       (except reg_core_generic_traverse test-this these-tests))

  (define nominalize-types
    (let ()
      ;; The generic traversal returns an intermediate value of type
      ;; #(Expr TypeDefs) where TypeDefs is list of [Name [StructFieldType ...]] pairs.
      (reg:define-struct (result expr tydefs))

      ;; The fixed names of fields.
      (define field-names 
	(map (lambda (n) (string->symbol (format "fld~s" n)))
	  (iota MAX_TUPLE_SIZE)))

      ;; An association list accumulating new struct types.
      ;; 
      (define struct-table '())
           
      ;; We avoid the boilerplate by defining this as a "generic traversal"
      (define collect-tupdefs
	(core-generic-traverse/types
	 ;; Driver
	 (lambda (expr tenv loop)
	   (match expr
	     ;; Tuple statements HAVE to be type annotated now.
	     [(tuple ,arg* ...)
	      (printf "TUPLE!! ~s\n" arg*)	 
	      (let ([type (recover-type `(tuple . ,arg*) tenv)]
		    [newtype (unique-name 'tuptyp)])
		(if (polymorphic-type? argtypes)
		    (error 'nominalize-types
			   "there should not remain polymorphic tuples after static-elab: ~s" type))
		(match type
		  [#(,argtypes ...)
		   (let* ([results (map (lambda (x) (collect-tupdefs x tenv)) arg*) ]
			  [args (map result-expr results)]
			  [tydefs (apply append (map result-tydefs args))])
		     (make-result 
		      `(make-struct ,newtype ,args ...)
		      `((,(map (lambda (arg) (recover-type arg tenv)) arg*)
			 ,(list-head field-names (length arg*))
			 ,newtype
			 )
			. ,tydefs))
		     )]))]

	     ;; tuprefs are simple:
	     [(tupref ,i ,len ,[result])
	      (make-result `(struct-ref ,(result-expr result) ,(list-ref field-names i))
			   (result-tydefs))]

	     [,other (loop other tenv)]
	     ))
	 ;; Fuser
	 (lambda (ls k)
	   (printf "FUSING: ~s\n\n" ls)
	   (make-result (apply k (map result-expr ls))
			(apply append (map result-tydefs ls))))
	 ))

      ;; TODO: REMOVE DUPLICATE STRUCT DEFS THAT HAVE THE SAME TYPES
      (define (remove-redundant tupdefs)
	tupdefs)

      ;; Main body:
      (lambda (prog) 
	(match prog 
	  [(,lang '(program ,body ,type))
	   (let* ([result (collect-tupdefs body)]
		  [newbod (result-expr result)]
		  [tupdefs (remove-redundant (result-tydefs result))])
	    
	     ;; Must be an exported type.
	     (define (convert-type ty)
	       (match ty
		 [,s (guard (symbol? s)) s]
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
		    [(,types ,flds ,structname) structname])]
		 [,else (error 'nominalize-types:convert-type "unmatched type: ~s" else)]))

	     (define (do-bindings vars types exprs reconstr exprfun) 
	       (reconstr vars (map convert-type types) (map exprfun exprs)))

	     (set! bindings-fun do-bindings)

	     (inspect tupdefs)
	     
	     (match (convert-types `(,lang '(program ,newbod ,type)))
	       [(,lang '(program ,body ,type))
		;; Running the type-checker/inferencer isn't going to work on this output any longer:
		`(nominalize-types-language
		  '(program ,body		       
		     ;; We stick the type definitions here:
		     (struct-defs ,@(map (match-lambda (,types ,flds ,name)
					   `(,name (map list flds types)))
				      tupdefs))
		     ,type))])
	     )]))))

  (define these-tests  `())
  (define test-this (default-unit-tester "" these-tests))
  (define test-nominalize-types test-this)

) ;; End module.





