
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
  (provide nominalize-types 
	   test-this test-nominalize-types standard-struct-field-names
	   convert-types collect-tupdefs)
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

	;; Unit is OK:
	[#() '()]
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


  ;; Collect from type all its contained tuples:
  (define (collect-from-type ty)
    (match ty
      [,s (guard (symbol? s)) '()]

      ;; HACK: Allowing polymorphic types for list... damn null lists.
      [(List ',_) '()]

      ;; This is a special addition for these last few passes... (AKA: A *HACK*)
      ;[(ByteArray ,n) '()]

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


  (define-pass collect-tupdefs
    (define (Expr x fallthru) 
      (match x
	[(assert-type   ,[collect-from-type -> t] ,[e])  (append-tydefs2 t e)]
	[(cast-variant-to-parent ,tc ,[collect-from-type -> t] ,[e])  (append-tydefs2 t e)]
	[,form (guard (binding-form? form))
	       (let ([scoped (binding-form->scoped-exprs form)]
		     [types (binding-form->types form)]
		     [others (binding-form->unscoped-exprs form)])
		 (apply append-tydefs		 
			(append (map collect-from-type types)
				(map (lambda (x) (Expr x fallthru)) others)
				(map (lambda (x) (Expr x fallthru)) scoped))))]
	[,oth (fallthru oth)]))
    ;; Expressions just bottom out in null-lists of tydefs.
    [Expr Expr]
    [Fuser (lambda (ls k) (apply append-tydefs ls))]
    [Program (lambda (prog ExprFun)
	       (match prog
		 [(,lang '(program ,[ExprFun -> body] ,meta* ... 
				   ,[collect-from-type -> toptype]))
		  (append-tydefs2 toptype body)
		  ]))])


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
	   (printf "ERK argtypes ~s and tupdefs ~s\n" argtypes tupdefs)
	   `(make-struct ,(last (ASSERT (assoc argtypes tupdefs))) ,@arg*)])]
       ;; tuprefs are simple:
       [(tupref ,i ,len ,[x])
	`(struct-ref ,x ,(list-ref standard-struct-field-names i))]

       [(cast-variant-to-parent ,tc ,t ,[e]) `(cast-variant-to-parent ,tc ,t ,e)]
       [,oth (fallthru oth tenv)]
       ))])

  ;; ============================================================
  ;;; Third pass -- go back through and insert struct names where
  ;;; struct's are built.  Also convert the types to use their nominal
  ;;; counterparts.
 
  ;; This looks up a variable's tuple type in the tupdef bindings.
  (define-pass convert-types 
    [Expr (lambda (x fallthr)
	    (match x 
	      ;; We handle ascription (assert-type) specially.  It is not
	      ;; caught by the "Binding" form.
	      [(assert-type ,t ,[e])    `(assert-type   ,(convert-type t tupdefs) ,e)]
	      [(cast-variant-to-parent ,tc ,t ,[e])  `(cast-variant-to-parent ,tc ,(convert-type t tupdefs) ,e)]
	      [,oth (fallthr oth)]))]
    [Bindings 
     (lambda (vars types exprs reconstr exprfun) 
       ;; This depends on TUPDEFS:
       (reconstr vars (map (lambda (t) (convert-type t tupdefs)) types) 
		 (map exprfun exprs)))])

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
	 (define unions (or (assq 'union-types meta*) '(union-types)))
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
		 (union-types 
		  ,@(map (lambda (x)			   
			   (match x 
			     [(,name (,tc* ,[(lambda (t) (convert-type t tupdefs)) -> ty*]) ...)
			      `(,name ,@(map list tc* ty*))]))
		      (cdr unions)))
		 ,(remq unions meta*) ... ;; The _other_ metadata.
		 ,(convert-type type tupdefs)))]))]))

  ;; ============================================================
  ;;; Unit Tests.

  (define-testing these-tests  
    `(
      ["remove all those tuprefs"
       (deep-assq 'tupref
	  (nominalize-types

 '(type-print/show-language
    '(program
      (let ([s0_0 (Stream #0()) (timer '3.0)])
       (let ([s1_1 (Stream (Sigseg Int))
              (readFile '"./countup.raw" '"" s0_0)])
        (let ([s2_2 (Stream #(Int Int64))
               (iterate
                (lambda (w_3 VIRTQUEUE_4)
                 ((Sigseg Int) (VQueue #(Int Int64)))
                 (begin
                  (emit VIRTQUEUE_4 (tuple (width w_3) (start w_3)))
                  VIRTQUEUE_4))
                s1_1)])
         (let ([s3_5 (Stream #3(Int64 Int Float))
                (iterate
                 (lambda (pattmp_6 VIRTQUEUE_9)
                  (#(Int Int64) (VQueue #3(Int64 Int Float)))
                  (let ([x_7 Int (tupref 0 2 pattmp_6)])
                   (let ([y_8 Int64 (tupref 1 2 pattmp_6)])
                    (begin
                     (emit VIRTQUEUE_9 (tuple y_8 x_7 '3.0))
                     VIRTQUEUE_9))))
                 s2_2)])
          s3_5))))
      (Stream #3(Int Int Float))))

#;

 '(type-print/show-language
			   '(program
				(let ([s0_0 (Stream #()) (timer '3.0)])
				(let ([s1_1 (Stream (Sigseg Int))
					    (readFile '"./countup.raw" '"" s0_0)])
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
				      s3_5))))
			      (Stream #(Int Int Float))))))
       #f]

      ["tuples of tuples"
       (reunique-names  ;; <- This kinda thing makes me wish I used de Bruijin indices.
	(nominalize-types
	 '(type-print/show-language
	   '(program
		(let ((x #(Int Int) (tuple 2 3)))
		  (tuple 1 x))
	      (union-types)
	      (Stream #(Int #(Int Int)))))))
       (nominalize-types-language
	'(program
	     (let ([x (Struct tuptyp) (make-struct tuptyp 2 3)])
	       (make-struct tuptyp_1 1 x))
	   (struct-defs
	    (tuptyp (fld1 Int) (fld2 Int))
	    (tuptyp_1 (fld1 Int) (fld2 (Struct tuptyp))))
	   (union-types)
	   (Stream (Struct tuptyp_1))))]

      ["collect tupdefs"
       (reunique-names
	(,collect-tupdefs 
	 '(lang '(program (assert-type #(Int Int) (tuple '3 '4)) (Sum Foo)))))
       (((Int Int) (fld1 fld2) tuptyp))]
      
      ))
  (define-testing test-this (default-unit-tester "nominalize types pass" these-tests))
  (define test-nominalize-types test-this)

) ;; End module.

