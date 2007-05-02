
;; This adds some extra type annotations that we will need when generating code.
;; (After nominalize-types we can no longer use recover-type.)

(module type-annotate-misc mzscheme 
  (require  "../../../plt/common.ss" 
	    "../normalize_query/ws-lift-let.ss")
  (provide type-annotate-misc
	   type-annotate-misc-grammar
	   )
  (chezimports)

;; UNFINISHED, should reflect constraints on annotated prims:
  (define type-annotate-misc-grammar
    ;; Add symbols to the literal grammar (for dataFile)
    ;; Remove 'Const' and make everything 'ComplexConst' (again)
    (append `((Datum ,symbol?)
	      (Const ComplexConst)
	      (ComplexConst ('__foreign Const Const ComplexDatum))
	      (Value ('foreign-app Const ('assert-type Type Var) Simple ...))
	      )
	  (filter (lambda (x) 
		    (match x
		      [(Const . ,_) #f]
		      [(,prod ('foreign-app . ,_)) #f]
		      [,else #t]))
	    ws-lift-let-grammar)))

;; Adds types to various primitives for code generation.
(define-pass type-annotate-misc
    
    ;(define annotated-prims '(print show cons hashtable seg-get))
    (define annotate-outside-prims 
      '(hashtable prim_window List:append List:reverse cons
		  Array:make Array:makeUNSAFE
		  ))

    (define annotate-first-arg 
      '(List:append List:length List:ref print show equal? seg-get toArray
		    Array:ref Array:set Array:length

		    joinsegs subseg width toSigseg toArray timebase start end seg-get
		    ))

    (define (process-expr x tenv fallthru)
;      (printf "PE: \n")
      (match x

	;; These primitives need their assert-types on the INSIDE:

	[(List:make ,[n] ,[init])
	 `(List:make ,n (assert-type ,(recover-type init tenv) ,init))]

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
			`(assert-type ,type ,rhs)
			rhs))
	       types rhs*)
	      other))
	  tenv
	  )]
	
	[(,annfirst ,[x] ,[y*] ...) (guard (memq annfirst annotate-first-arg))
	 `(,annfirst (assert-type ,(recover-type x tenv) ,x)  . ,y*)]

	;; Generically handle all the annotate-outside forms:
	;; Wouldn't need this if the program were flattened so that
	;; the above binding-form case caught everything.
#;
	[(,annprim ,[e*] ...) (guard (memq annprim annotate-outside-prims))
	 (let ([exp `(,annprim . ,e*)])
	   (assert-type ,(recover-type exp tenv)
			,exp))]
	
	;; This needs to explicitly pass the types as argument to run with wsint.
	[(assert-type (Stream ,t) (dataFile ,[f] ,[m] ,[rate] ,[rep]))
	 (let ([types (match t [#(,t* ...)  t*]  [,t   	(list t)])])
	   `(__readFile ,f ,m ,rep ,rate '0 '0 '0 ',types)
	   )]

	;; This needs the type tagged on also:
	[(assert-type ,T (foreign ,[name] ,[file]))
	 `(__foreign ,name ,file ',T)]
	;; Tag the applications too:
	[(foreign-app ',realname ,rator ,[arg*] ...)
	 (ASSERT symbol? rator)
	 `(foreign-app ',realname
		       (assert-type ,(recover-type rator tenv) ,rator)
		       ,@arg*)]
		
	;; Move this to another file:
	[(assert-type (Stream ,t) (readFile ,[fn] ',str))
	 (ASSERT string? str)
	 ;; Defaults:
	 (let* ([mode "text"]
		[repeats 0]
		[rate 1000] ;; A khz... this is arbitrary.
		[winsize 1] ;; Another meaningless default.
		[skipbytes 0]
		[offset 0]
		[p (open-input-string str)]
		[params (let loop ([x (read p)])
			  (if (eof-object? x) '()
			      (cons x (loop (read p)))))]		
		[pairs (match params
			 [() '()]
			 [(,a ,b . ,[tl]) (cons (list a b) tl)]
			 [,oth (error 'readFile "invalid parameter string to readFile primitive: ~s" str)])]
		[num (lambda (n) 
		       (if (integer? n) n			   
			   (error 'readFile "expected numeric parameter, got: ~s" n)))]
		[types (match t
			 [#(,t* ...)  t*]
			 [(Sigseg ,[t]) t]
			 [,t   	(list t)])])
	   (for-each (match-lambda ((,flag ,val))
		       (case flag
			 [(mode:) (set! mode (case val 
					       [(text) "text"]
					       [(binary) "binary"]
					       [else (error 'readFile "unsupported mode: ~s" val)]))]
			 [(repeats:)   (set! repeats (num val))]
			 [(rate:)      (set! rate (num val))]
			 [(skipbytes:) (set! skipbytes (num val))]
			 [(offset:)    (set! offset  (num val))]
			 [(window:)    (set! winsize (num val))]
			 [else (error 'readFile "unknown option flag \"~s\"\n Valid flags are: ~s\n" 
				      flag 
				      '(mode: repeats: rate: skipbytes: offset: window:))])
		       ) pairs)
	   (when (equal? mode "text")
	     (unless (= offset 0)
	       (error 'readFile "doesn't support 'offset:' option in conjunction with text mode"))
	     (unless (= skipbytes 0)
	       (error 'readFile "doesn't support 'skipbytes:' option in conjunction with text mode")))
	   
	   ;; If we're not producing a sigseg, we must set the winsize to zero:
	   (match t
	     [(Sigseg ,t) (void)]
	     [,else (set! winsize 0)])
	   `(__readFile ,fn ',mode ',repeats ',rate ',skipbytes ',offset ',winsize ',types)
	   )]

	;; Anything already in assert form is covered.
	;; [2007.01.24] Commenting:
	;;[(assert-type ,t ,e) `(assert-type ,t ,(fallthru e tenv))]

	
	;; Safety nets:
	[(dataFile ,_ ...) (error 'type-annotate-misc "uncaught 'dataFile' call: ~s" `(dataFile ,_ ...))]

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
  [OutputGrammar type-annotate-misc-grammar])


) ; End module
