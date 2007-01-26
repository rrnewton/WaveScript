
;; This adds some extra type annotations that we will need when generating code.
;; (After nominalize-types we can no longer use recover-type.)

(module type-annotate-misc mzscheme 
  (require  "../../../plt/common.ss" )
  (provide type-annotate-misc)
  (chezimports)

;; Adds types to various primitives for code generation.
(define-pass type-annotate-misc

    ;(define annotated-prims '(print show cons hashtable seg-get))
    (define annotate-outside-prims '(hashtable window))

    (define (process-expr x tenv fallthru)
;      (printf "PE: \n")
      (match x

	;; These primitives need their assert-types on the INSIDE:
	[(print ,[e])
	 `(print (assert-type ,(recover-type e tenv) ,e))]
	[(show ,[e]) 
	 `(show (assert-type ,(recover-type e tenv)  ,e))]
	[(cons ,[a] ,[b])
	 `(assert-type (List ,(recover-type a tenv)) (cons ,a ,b))]
	[(equal? ,[a] ,[b])
	 `(equal? (assert-type ,(recover-type a tenv) ,a) ,b)]
	[(seg-get ,[seg] ,[ind])
	 `(seg-get (assert-type ,(recover-type seg tenv) ,seg) ,ind)]

	
       	;; Here we catch these primitives where they're bound and just
	;; use the pre-computed type.  We wrap that assert-type
	;; construct around the OUTSIDE of the form.
	[,frm (guard (binding-form? frm))	      
	 ;; After we're all done we run the fallthru function to complete processing.
	 (fallthru 
	  (mvlet ([(vars types rhs* other k) (binding-form-visit-knowncode frm)])	    
	    (k vars types 
	      (map (lambda (type rhs) ;rhs may be #f for 'unavailable'
		    (if (and (pair? rhs) (memq (car rhs) annotate-outside-prims))		       
			`(assert-type ,type ,rhs)
			rhs))
	       types rhs*)
	     other))
	  tenv ;(tenv-extend tenv vars types)
	  )]

	;; This needs an explicit annotation to run with wsint.
	[(assert-type (Signal ,t) (dataFile ,[f] ,[m] ,[r]))
	 (match t
	   [#(,t* ...)  `(assert-type (Signal ,t) (__dataFile ,f ,m ,r ',t*))]
	   [,t   	`(assert-type (Signal ,t) (__dataFile ,f ,m ,r ',(list t)))])]

	;; Anything already in assert form is covered.
	;; [2007.01.24] Commenting:
	;[(assert-type ,t ,e) `(assert-type ,t ,(fallthru e tenv))]

;; Removing this error condition, even after we annotate above, we
;; still come by "hashtable" again.
#;
	;; For now it's an error for this stuff to occur otherwise.
	[(,annprim ,e* ...) (guard (memq annprim annotated-prims))
	 (error 'type-annotate-misc "was supposed to catch this prim at a binding site: ~s"
		`(,annprim . ,e*))]
	[,other (fallthru other tenv)]))


  [Expr/Types process-expr])


) ; End module
