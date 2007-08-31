

;;;; This is a pass to verify that the elaborated program has the
;;;; right structure.  Mainly it checks for the absense of certain
;;;; not-allowed things.  (Like applications of user functions.)

(module verify-elaborated mzscheme
  (require "../../../plt/common.ss"
	   "../normalize_source/remove-unquoted-constant.ss"
	   "../static_elaborate/degeneralize-arithmetic.ss"
	   )
  (provide verify-elaborated verify-elaborated-grammar)
  (chezimports )

  (define verify-elaborated-grammar degeneralize-arithmetic-grammar)

;; Verifies that there are no polymorphic types left on the programs variable bindings.
;; Also verifies that there are no disallowed applications.
;; FIXME: [2007.08.13] Easing this up... need to work on it more though.
(define-pass verify-elaborated
    [OutputGrammar verify-elaborated-grammar]

    ;; [2007.01.25] Changing this to be stricter: no remaining
    ;; polymorphism in these types:
#;    (define (verify-type t) (not (polymorphic-type? t)))

    ;; UNFINISHED:
    ;; This verifies that tuple types are not polymorphic.
    (define (verify-type t)
      (define (id x) x)
      (match t
	[,s (guard (symbol? s)) #t]
	[(quote ,v)          #t]
	[(quote (,v . ,[t]))  t]

	;; [2007.08.13] DANGER: THINK ABOUT THIS SOME MORE.
	;; At the least a numeric type doesn't contain a Stream!
	[(NUM ,_)            #t]

	[(,[arg] ... -> ,[ret]) (and ret (andmap id  arg))]
	
	;; Areas can contain streams:
	[(Area ,[t]) t]

	[(,C ,t* ...) (guard (symbol? C)) 
	 (or (andmap verify-stream-free t*)
	     ;(inspect t*)
	     (error 'verify-type
		    "elaboration didn't succeed in getting all (potential) stream types free from other type constructors:\n  ~s"
		    `(,C . ,t*)))]
	[#(,t* ...) 
	 (and ;(not (polymorphic-type? (list->vector t*)))
	      (or (andmap verify-stream-free t*)
		  (error 'verify-type
			 "elaboration didn't succeed in getting this stream type free from this tuple:\n  ~s"
			 (list->vector t*))))]
	[,else #f]))

  (define (verify-stream-free t)
    (match t
      [(Stream ,t) #f]
      [,s (guard (symbol? s)) #t]
;      [(NUM ,_)               #t]
      [(,qt ,v) (guard (memq qt '(quote NUM)) (symbol? v)) #t]
      [(,qt (,v . ,[t])) (guard (memq qt '(quote NUM)) (symbol? v)) t]
      [(,[arg] ... -> ,[ret]) (and ret (andmap id  arg))]
      [(,C ,t* ...) (guard (symbol? C)) (andmap verify-stream-free t*)]
      [,s (guard (string? s)) #t]
      [#(,t* ...) (and (andmap verify-stream-free t*)
		       ;(not (polymorphic-type? (list->vector t*)))
		       )]
      ;[,else #f]
      ))

  ;; Mutable state:
  (define inside-iterate #f)

  (define process-expr 
    (lambda (expr tenv fallthrough)
       (match expr
	 [(app ,[rator] ,[rand*] ...)		     
	  (let ([type (recover-type rator tenv)])
	    (if ;(or (deep-assq 'Stream type) (deep-assq 'Region type))
	     ;(distributed-type? type)	
	     #t ;; <-- Harshest version: no functions at all.
	     (error 'verify-elaborated
		    ;"post-elaboration expression should not contain arrow types containing monads.\n  Type: ~s\n  Rator: ~s\n"
		    "~a~a  Type: ~s\n\n  Rator: ~a\n  Location: ~a\n"
		    "post-elaboration expression should (currently) not contain function applications at all.\n"
		    "This should have inlined...\n"
		    ;"This probably means that you have a "
		    type 
		    ;; Approximate location:
		    (get-snippet rator)
		    (get-location rator)))
	    `(app ,rator ,rand* ...))]


	 ;; This is insufficiently precise, because it can allow
	 ;; naughty things, for example, in the RHS of the state
	 ;; bindings.
	 [(iterate ,letorlamb ,[src])
	  (fluid-let ([inside-iterate #t])
	    `(iterate ,(process-expr letorlamb tenv fallthrough) ,src))]
	 
	 ;; TODO: disallow lambdas except as arguments to iterate and select higher order prims.
	 

	 ;; [2007.08.02] Allowing this now:
#;
	 [(,higher ,[x*] ...)
	  (guard (assq higher higher-order-primitives))
	  (unless inside-iterate
	    (error 'verify-elaborated "didn't elaborate far enough. \n~s~a ~s"
		   higher
		   " is not allowed after elaboration, except inside iterate.\n" 
		   `(,higher . x*)))
	  `(,higher . ,x*)]
	 [(vector ,_ ...) (error 'verify-elaborated "didn't elaborate far enough. vector is not allowed after elaboration.")]
	 ;[(tuple ,_ ...)  (error 'verify-elaborated "didn't elaborate far enough. tuple is not allowed after elaboration.")]

	 [(,foreign ',name ',files)
	  (guard (memq foreign '(foreign foreign_box foreign_source)))
	  (unless (and (string? name) (andmap string? files))
	    (error 'verify-regiment "\"~s\" construct can only be used with strings, not: ~s and ~s" 
		   foreign name files))
	  `(,foreign ',name ',files)]

	 ;; Run verification on the types:
	 [,form (guard (binding-form? form))
		(for-each (lambda (t)
			    (unless (verify-type t)
			      (error 'verify-elaborated 
				     "type is not valid post-elaboration: ~s" t)))
		  (binding-form->types form))
		(fallthrough form tenv)]	 
	 
	 [(,genop ,args ...)
	  (guard (assq genop generic-arith-primitives))
	  (error 'verify-elaborated 
		 "shouldn't have generic arithmetic after static-elaborate: ~s"
		 `(,genop . ,args))]

	 ;; [2007.07.08] TEMPORARY: FIXME
	 ;; Ditching source location at this point... the rest of the compiler just isn't ready for it yet.
	 [(src-pos ,_ ,[e]) e]	 

	 [,other (fallthrough other tenv)])))

  [Expr/Types process-expr]
  
  ;; TODO: FIXME VERIFY THAT THERE ARE *NO* POLYMORPHIC TYPES LEFT:
  ;[Bindings ]

  )

) ; End module
