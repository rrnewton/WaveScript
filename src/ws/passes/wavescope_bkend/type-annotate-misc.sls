#!r6rs

;; This adds some extra type annotations that we will need when generating code.
;; (After nominalize-types we can no longer use recover-type.)

(library (ws passes wavescope_bkend type-annotate-misc)
  (export type-annotate-misc
	  type-annotate-misc-grammar
	  )
  (import (rnrs) (ws common) 
	  (ws passes normalize_query ws-lift-let))

;; UNFINISHED, should reflect constraints on annotated prims:
  (define type-annotate-misc-grammar

    (let* ([hitforapp #f]
	   [filtered 
	    (filter (lambda (x) 
		      (match x
			[(Const . ,_) #f]
			[(AppConstructs ('foreign-app . ,_)) (set! hitforapp #t) #f]
			[,else #t]))
	      ws-lift-let-grammar)])
      (unless hitforapp (error 'type-annotate-misc-grammar 
			       "failed to remove foreign-app production from previous pass's grammar."))
    ;; Add symbols to the literal grammar (for dataFile)
    ;; Remove 'Const' and make everything 'ComplexConst' (again)
    (append `((Datum ,symbol?)
	      (Const ComplexConst)
	      (ComplexConst ('__foreign        Const Const ComplexDatum))
	      (ComplexConst ('__foreign_source Const Const ComplexDatum))
	      (AppConstructs ('foreign-app Const ('assert-type Type Var) Simple ...))
	      )
	    filtered)))

;; Adds types to various primitives for code generation.
(define-pass type-annotate-misc
    
    ;(define annotated-prims '(print show cons hashtable seg_get))
    (define annotate-outside-prims 
      '(hashtable prim_window List:append List:reverse cons
		  Array:make Array:makeUNSAFE
		  unionN
		  ptrToArray
		  ;; Being lenient with these and not requiring direct annotation from the start.
		  foreign_source

		  ; readFile
		  ))

    (define annotate-first-arg 
      '(List:append List:length List:ref seg_get toArray
		    print show __show_ARRAY 
		    Array:ref Array:set Array:length
		    
		    car ;; [2008.03.22] Adding

		    < <= > >= max min
		    
		    = wsequal?
		    joinsegs subseg width toSigseg toArray timebase start end seg_get
		    ))
    

    (define (not-poly? ty) (not (polymorphic-type? ty)))
    (define (wrap ty x)
      (ASSERT "shouldnt find polymorohism at this point"  not-poly? ty)
      `(assert-type ,ty ,x))

    (define (maybewrap x tenv)
      (match x
	[(assert-type ,ty ,_) x]
	[,_ 
	 (define recovered (recover-type x tenv))
	 (DEBUGASSERT (not (polymorphic-type? recovered)))
	 (wrap recovered x)]))
    
    (define (process-expr x tenv fallthru)
;      (printf "PE: \n")
      (match x

	;; These primitives need their assert-types on the INSIDE:
       
	[(List:make ,[n] ,[init])  `(List:make ,n ,(maybewrap init tenv))]
	[(set! ,v ,[x])  `(set! ,v ,(maybewrap x tenv))]
	
	;; TODO, FIXME: THIS IS A HACKISH AND BROKEN APPROACH:
	
       	;; Here we catch these primitives where they're bound and just
	;; use the pre-computed type.  We wrap that assert-type
	;; construct around the OUTSIDE of the form.
	[,frm (guard (binding-form? frm))	      
	 ;; After we're all done we run the fallthru function to complete processing.
	 ;;
	 ;; NOTE: we put the whole thing back through fallthru because
	 ;; we don't NECESSARILY know whether "other"s are in the
	 ;; scope of the variables or not.
	 (fallthru 
	  (mvlet ([(vars types rhs* other k) (binding-form-visit-knowncode frm)])	    
	    (k vars types 
	       (map (lambda (type rhs) ;rhs may be #f for 'unavailable'
		      (if (and (pair? rhs) (memq (car rhs) annotate-outside-prims))		       
			  (wrap type rhs)
			  rhs))
		 types rhs*)
	       other))
	  tenv)]
		
	[(,annfirst ,[x] ,[y*] ...) (guard (memq annfirst annotate-first-arg))
	 `(,annfirst ,(maybewrap x tenv)  . ,y*)]

	;; Generically handle all the annotate-outside forms:
	;; Wouldn't need this if the program were flattened so that
	;; the above binding-form case caught everything.
#;
	[(,annprim ,[e*] ...) (guard (memq annprim annotate-outside-prims))
	 (let ([exp `(,annprim . ,e*)])
	   ,(maybewrap exp tenv))]

	;; Tag the applications too:	
	[(foreign-app ',realname ,rator ,[arg*] ...)
;	 (ASSERT symbol? rator) ;; [2007.10.26] Not sure why this would need to be the case...
	 `(foreign-app ',realname
		       ,(maybewrap rator tenv)
		       ,@arg*)]

	;; Anything already in assert form is covered.
	;; [2007.01.24] Commenting:
	;;[(assert-type ,t ,e) `(assert-type ,t ,(fallthru e tenv))]


#;	
	;; Safety nets:
	[(,missed ,_ ...) (guard (memq missed '(dataFile readFile)))
	 (error 'type-annotate-misc "uncaught '~s' call: ~s" missed (cons missed _))]

	;; For now it's an error for this stuff to occur otherwise.
	;;
	;; Can't assert this now because currently we put the whole
	;; thing back through 'fallthru' in the above binding form case.
#;
	[(,annprim ,e* ...) (guard (or (memq annprim annotate-outside-prims) (memq annprim annotate-first-arg)))
	 (error 'type-annotate-misc "was supposed to catch this prim at a binding site: ~s"
		`(,annprim . ,e*))]

	[,other (fallthru other tenv)]))


  [Expr/Types process-expr]

  ;; TEMP FIXME!!!
  ;[OutputGrammar type-annotate-misc-grammar]
)


) ; End module
