
;;;; .title WaveScript Nominalize Types
;;;; .author Ryan Newton

;;;; This pass replaces the ML-like algebraic types with suitable C-types (structs).
;;;; <br><br>

;;;; It COULD simply follow the methodology of coining a new tuple
;;;; type whenever there is demand for one.  <br><br>
;;;;
;;;; But that seems sloppy.  *Instead* we take the approach where we
;;;; first collect tuple types from all the places they 
;;;; *originate*.  (Tuple expressions and certain primitives.)
;;;;   Because of the closed-world property that we get from
;;;; whole-program compilation, these should be the only places
;;;; tuples can come from. <br><br>

;;;; TODO: Make grammar enforcing no tuprefs.  ESPECIALLY since there
;;;; is no typechecking after this.

;;;; <br><br>
;;;; [2007.05.27] Moving towards ONLY collecting tuple-types from
;;;; explicit type annotations, NOT from tuple-forming statements
;;;; themselves.  This critically depends on the program being
;;;; flattened by remove-complex-opera*.

;============================================================

(module nominalize-types  mzscheme 
  (require "../../../plt/common.ss"
	   (all-except "../../util/tsort.ss" test-this these-tests))
  (provide nominalize-types test-this test-nominalize-types standard-struct-field-names
	    convert-types)
  (chezprovide )
  (chezimports (except helpers                   test-this these-tests)
	       (except reg_core_generic_traverse test-this these-tests)
	       (except tsort                     test-this these-tests))


  ;; ============================================================
  ;;; Constants, typedefs, and global variables

  ;; The fixed names of fields.
  (define standard-struct-field-names 
    (map (lambda (n) (string->symbol (format "fld~s" (fx+ 1 n))))
      (iota MAX_TUPLE_SIZE)))


  ;; The generic traversal returns an intermediate value of type
  ;; #(Expr TypeDefs) where TypeDefs is list of [Name [StructFieldType ...]] pairs.
  (reg:define-struct (result expr tydefs))
  
  ;; An association list accumulating new struct types.
  ;; 
  (define struct-table '())

  ;; This gets bound later in a different scope.
  (define tupdefs 'uninit)
  
  ;; ============================================================
  ;;; Helpers:

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
      [(Pointer   ,_)         #t]
      [(ExclusivePointer  ,_) #t]
      [(,C ,[t] ...) (guard (symbol? C)) (andmap id t)]
      [#(,[t] ...) (andmap id t)]
      [,else #f]))


  ;; Topological sort on struct-defs
  (define (sort-defs defs) 
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
	[,s (guard (string? s)) '()]
	[,oth (error 'type->structs "unrecognized type: ~s" oth)]))
    (let ([edges (map (lambda (def)
			(match def
			  [(,name [,fld* ,ty*] ...)
			   (cons name (apply append (map type->structs ty*)))
			   ]))
		   defs)])
      (map (lambda (name)
	     (assq name defs))
	(reverse (tsort edges)))))
    
  ; ========================================
  ;;; ADT for typedef collections

  ;; Keep them sets rather than converting them to sets at the
  ;; end.  The idea is that we'll see a relatively small number of
  ;; tuple types, but we'll see them all over the place.
  (define (add-new-tydef def lst)
    (let ([ty* (car def)])
      (DEBUGASSERT (andmap type? ty*))
      (if (assoc ty* lst)
	  lst
	  (cons def lst)
	  )))      

  ;; Union collections of typedefs.
  (define (append-tydefs . args)
    (if (null? args) '()
	(append-tydefs2 (car args)
			(apply append-tydefs (cdr args)))
	))
  (define (append-tydefs2 defs1 defs2)
    (if (null? defs1) defs2
	(add-new-tydef (car defs1)
		       (append-tydefs (cdr defs1) defs2))))

  ;; Introduce a new typedef.
  (define (make-new-typedef argtypes)
    (if  (null? argtypes) (error 'nominalize-types:make-new-typedefs 
				 "no typedefs allowed for unit type."))
    (unless (andmap known-size? argtypes)
      (error 'nominalize-types
	     "there should not remain tuples of unknown size (probably polymorphic) after static-elab: ~s" 
	     argtypes))
    (DEBUGASSERT (andmap (lambda (t) (not (polymorphic-type? t))) argtypes))    
    ;; Return a new typedef:
    (list argtypes
	  (list-head standard-struct-field-names (length argtypes))
	  (unique-name 'tuptyp)))

  ;; TODO: REMOVE DUPLICATE STRUCT DEFS THAT HAVE THE SAME TYPES
  (define (remove-redundant tupdefs)
    ;; FIXME FIXME FIXME 
    tupdefs)



  ;; ============================================================
  ;;; First pass -- Collect tupdefs from the program:

#;
  ;; Collect tupdefs from an entire list of expressions.
  (define (collect-tupdefs-from-list exprList tenv)
    (let ([results (map (lambda (x) (collect-tupdefs-from-expr x tenv)) exprList)])
      (values (map result-expr results)
	      (apply append-tydefs (map result-tydefs results)))))

  ;; Collect from type all its contained tuples:
  (define (collect-from-type ty)
    (match ty
      [,s (guard (symbol? s)) '()]
      ;; Allowing polymorphic types for list... damn null lists.
      [(List ',_) '()]
      [(,qt ,v) (guard (memq qt '(quote NUM)))
       (error 'nominalize-types:collect-from-type
	      "should not have polymorphic type: ~s" ty)]
      [(,[arg*] ... -> ,[ret]) (apply append ret arg*)]
      [(,C ,[t*] ...) (guard (symbol? C)) (apply append t*)]
      [#() '()]
      [#(,t* ...)
       (cons (make-new-typedef t*)
	     (apply append (map collect-from-type t*)))]
      [,s (guard (string? s)) '()]
      [,else (error 'nominalize-types:collect-from-type "unmatched type: ~s" else)]))


#;

  ;; A first pass to collect tuple type defs & convert tuple/tupref terms.
  ;; Doesn't process the type of every term, but processes all the
  ;; places it needs to observe all used tuple types.
  (define (collect-tupdefs-from-expr expr tenv)
    ;; We avoid the boilerplate by defining this as a "generic traversal"
    (core-generic-traverse/types
     ;; Driver
     (lambda (expr tenv loop)
       ;; Everything returned must be an intermediate result.
       (ASSERT result?
	       (match expr
		 [(tuple) (make-result '(tuple) '())]

		 [(tuple ,arg* ...)
		  ;; INEFFICIENT: (as are all uses of recover-type...)
		  (let ([type (recover-type `(tuple . ,arg*) tenv)])
		    (match type
		      [#(,argtypes ...)
		       (mvlet ([(args tydefs) (collect-tupdefs-from-list arg* tenv)])
			 (make-result 
			  ;; We replace argtypes with an actual name in a second pass:
			  `(make-struct ,argtypes ,args ...)
			  ;; Append a new typedef to the existing.
			  ;; Don't convert types for this entry:
			  (add-new-tydef (make-new-typedef argtypes)
					 tydefs)))]))]

		 ;; DAMMIT: special case for zip2 because it currently uses its own tuple type.
		 [(zip2 ,[res1] ,[res2])
		  (let ([defs (append-tydefs (result-tydefs res1) (result-tydefs res2))]
			[ty1 (match (recover-type (result-expr res1) tenv)
			       [(Stream ,t) t])]
			[ty2 (match (recover-type (result-expr res2) tenv)
			       [(Stream ,t) t])]
			)
		    (printf "SPECIAL CASE ZIP2: ~s"   `((,ty1 ,ty2) (_first _second) (EXT Zip2)))
		    
		    (make-result
		     `(zip2 ,(result-expr res1) ,(result-expr res2))		 
		     (add-new-tydef
		      ;; Special tuple for zip.
		      `((,ty1 ,ty2) (_first _second) (EXT Zip2)) ;; Special NAME field.		  
		      defs)
		     ))]

		 [(assert-type ,t ,[e])
		  (make-result `(assert-type ,t ,(result-expr e))
			       (append-tydefs (collect-from-type t)
					      (result-tydefs e)))]

#;
		 ;; Going to go ahead and add any types that are found in let bindings.
		 [(let ([,v* ,ty* ,[e*]] ...) ,body)
		  (let ([body (collect-tupdefs-from-expr body (tenv-extend tenv v* ty*))]
			[defs1* (map collect-from-type ty*)]
			[defs2* (map result-tydefs e*)]
			[e* (map result-expr e*)])
		    (make-result `(let ([,v* ,ty* ,e*] ...) ,(result-expr body))
				 (apply append-tydefs 
					 (append defs1* defs2* (list (result-tydefs body)))))
		    )]

		 ;; TODO: Should do this generically for all binding forms...
		 

		 #;
		 [(return ,[e])
		  (make-result `(return ,(result-expr e))
			       (result-tydefs e))]

		 ;; TODO: THIS SHOULD JUST USE collect-from-type:
		 ;; This produces new tuples.
		 [(unionN ,e* ...)
		  (let ([type (recover-type `(unionN ,e* ...) tenv)])
		    (mvlet ([(args tydefs) (collect-tupdefs-from-list e* tenv)])		 
		      (make-result
		       ;; Assert the type so that it gets converted.
		       `(assert-type ,type (unionN ,@args))
		       (append-tydefs (collect-from-type type) tydefs))))]

		 ;; tuprefs are simple:
		 [(tupref ,i ,len ,[result])
		  (make-result `(struct-ref ,(result-expr result) ,(list-ref standard-struct-field-names i))
			       (result-tydefs result))]

		 [,other (loop other tenv)]
		 )
	       ))
     ;; Fuser
     (lambda (ls k)
       (make-result (apply k (map result-expr ls))
		    (apply append-tydefs (map result-tydefs ls))))
     expr tenv
     ))

  (define-pass collect-tupdefs
    ;; Expressions just bottom out in null-lists of tydefs.
    [Expr (lambda (x fallthru) 
	    (match x
	      [,s (guard (symbol? s)) ()]
	      [',c                    ()]
	      [,oth (fallthru oth)]))]
    [Fuser (lambda (ls k) (apply append-tydefs ls))]
    [Bindings 
      (lambda (vars types exprs reconstr exprfun)
	(apply append-tydefs 
	       (append (map exprfun exprs)
		       (map collect-from-type types))))]
    [Program (lambda (prog ExprFun)
	       (match prog
		 [(,lang '(program ,body ,meta* ... ,toptype)) 
		  (append-tydefs2 (collect-from-type toptype)
				  (ExprFun body))]))])


  ;; ============================================================
  ;;; Second pass -- change tuple construction to struct-construction.

  (define-pass convert-tuples 
   [Expr/Types (lambda (x tenv fallthru)
     (match x 
       [(tuple) '(tuple)]
       [(tuple ,[arg*] ...)
	;; INEFFICIENT: (as are all uses of recover-type...)
	;; Since the program is flattened we can grab this at the let-binding...
	(match (recover-type `(tuple . ,arg*) tenv)
	  [#(,argtypes ...)
	   `(make-struct ,(last (ASSERT (assoc argtypes tupdefs))) ,@arg*)])]
       ;; tuprefs are simple:
       [(tupref ,i ,len ,[x])
	`(struct-ref ,x ,(list-ref standard-struct-field-names i))]
       [,oth (fallthru oth tenv)]
       ))])

  ;; ============================================================
  ;;; Third pass -- go back through and insert struct names where
  ;;; struct's are built.  Also convert the types to use their nominal
  ;;; counterparts.
 
  ;; This looks up a variable's tuple type in the tupdef bindings.
  (define-pass convert-types 
      ;; This depends on TUPDEFS:
      (define (bindings-fun vars types exprs reconstr exprfun) 
	(reconstr vars (map (lambda (t) (convert-type t tupdefs)) types) 
		  (map exprfun exprs)))
      [Expr (lambda (x fallthr)
	      (match x 
#;
		[(make-struct ,ty* ,[args] ...)
		 `(make-struct ,(last (assoc ty* tupdefs)) ,@args)]

		;; We handle ascription (assert-type) specially.  It is not
		;; caught by the "Binding" form.
		[(assert-type ,t ,[e])
		 `(assert-type ,(convert-type t tupdefs) ,e)]
		[,oth (fallthr oth)]))]
      [Bindings bindings-fun])

  ;; This goes through and replaces tuples with Struct types.
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

      [#() #()]
      [#(,t* ...)
       ;; Do the lookup on the *pre*converted type:
       ;; It's ok if there are duplicates in the tupdefs, this will get the first.
       (match (assoc t* tupdefs)
	 [#f (error 'nominalize-types:convert-type
		    "cannot find tuple type ~s in tuple defs:\n ~s" (list->vector t*) tupdefs)]
	 [(,types ,flds ,structname) `(Struct ,structname)])]
      [,s (guard (string? s)) s]
      [,else (error 'nominalize-types:convert-type "unmatched type: ~s" else)]))




  ;; ============================================================
  ;;; Main Entry Point

  ;; This is the main entry point.
  ;; It combines the micro-passes above in succession:
  ;;  (1) collect-tupdefs
  ;;  (2) convert-tuples
  ;;  (3) convert-types
  (define (nominalize-types prog)
    (match prog 
      [(,lang '(program ,body ,meta* ... ,type))
       (fluid-let ([tupdefs (collect-tupdefs prog)])
	 (match (convert-types (convert-tuples prog)) ;; Uses tupdefs!!
	   [(,lang '(program ,body ,meta* ... ,type))
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
		 ,meta* ... ;; The _other_ metadata.
		 ,(convert-type type tupdefs)))]))]))

  ;; ============================================================
  ;;; Unit Tests.

  (define-testing these-tests  
    `(
      ["remove all those tuprefs"
       (deep-assq 'tupref
		  (nominalize-types '(type-print/show-language
			   '(program
				(let ([s1_1 (Stream (Sigseg Int))
					    (audioFile '"./countup.raw" '4096 '0)])
				  (let ([s2_2 (Stream #(Int Int))
					      (iterate
					       (lambda (w_3 VIRTQUEUE_4)
						 ((Sigseg Int) (VQueue #(Int Int)))
						 (begin
						   (emit VIRTQUEUE_4 (tuple (width w_3) (start w_3)))
						   VIRTQUEUE_4))
					       s1_1)])
				    (let ([s3_5 (Stream #(Int Int Float))
						(iterate
						 (lambda (pattmp_6 VIRTQUEUE_9)
						   (#(Int Int) (VQueue #(Int Int Float)))
						   (let ([x_7 Int (tupref 0 2 pattmp_6)])
						     (let ([y_8 Int (tupref 1 2 pattmp_6)])
						       (begin
							 (emit VIRTQUEUE_9 (tuple y_8 x_7 '3.0))
							 VIRTQUEUE_9))))
						 s2_2)])
				      s3_5)))
			      (Stream #(Int Int Float))))))
       #f]

      ["tuples of tuples"
       (reunique-names  ;; <- This kinda thing makes me wish I used de Bruijin indices.
	(nominalize-types
	 '(type-print/show-language
	   '(program
		(let ((x #(Int Int) (tuple 2 3)))
		  (tuple 1 x))
	      (Stream #(Int #(Int Int)))))))
       (nominalize-types-language
	'(program
	     (let ([x (Struct tuptyp) (make-struct tuptyp 2 3)])
	       (make-struct tuptyp_1 1 x))
	   (struct-defs
	    (tuptyp (fld1 Int) (fld2 Int))
	    (tuptyp_1 (fld1 Int) (fld2 (Struct tuptyp))))
	   (Stream (Struct tuptyp_1))))]
      

      ))
  (define-testing test-this (default-unit-tester "nominalize types pass" these-tests))
  (define test-nominalize-types test-this)

) ;; End module.

