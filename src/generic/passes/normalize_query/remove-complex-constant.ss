
;;;; Pass: Remove Complex Constant
;;;; .author Ryan Newton

;;;; TODO: we *should* lift array values that don't escape and aren't
;;;; mutated.  We should also combine any identical list or array
;;;; values that we do lift.  These are major changes, however.

;;;; remove-complex-constant arranges for complex constant values to
;;;; be built only once, when the program begins, by wrapping each
;;;; program that contains complex constants in a let expression
;;;; binding temporary variables to the expressions that create the
;;;; complex constants and rewriting the original quote expressions
;;;; as references to these temporaries.  For example,


(module remove-complex-constant mzscheme  
  (require "../../../plt/common.ss"
           "reduce-primitives.ss")
  (provide remove-complex-constant remove-complex-constant-grammar) 
  (chezimports)
  
;; TODO: FILL THIS IN:
(define remove-complex-constant-grammar reduce-primitives-grammar)

(define-pass remove-complex-constant

  ;; Returns expression and constant binds (inside a vector):  
  (define (do-datum datum ty)
    (let-values ([(exp type mutable?) (datum->code datum ty)])
      (if (or mutable? (simple-expr? exp))
	  (vector exp ())
	  ;; Immutable things get pulled upward:
	  (let ([tmp (unique-name "tmplft")])
	    (vector tmp `((,tmp ,type ,exp)))))))

  ;; Returns expression and constant binds (inside a vector):
  (define (Expr x fallthrough)
    (match x 	    
      [(assert-type ,ty (quote ,datum))
       (match (do-datum datum ty)
	 [#(,e ,cb*) (vector `(assert-type ,ty ,e) cb*)])]

      ;; Catch the type assertion at the binding site?
      ;;[(let )]

      ;; This is not very safe now that we have more numeric types than Scheme representations:
      [(quote ,datum)   (do-datum datum #f)]

      ;; Don't lift out these complex constants!
      [(foreign ',name ',files) (vector `(foreign ',name ',files) ())]
      [(foreign_source ',name ',files) (vector `(foreign_source ',name ',files) ())]      

      [(lambda ,formals ,types ,[result])
       (match result
	 [#(,body ,body-b*) 
	  ;;(vector `(lambda ,formals ,body) body-b*)
	  ;; [2005.12.08] Modifying this so it doesn't (yet) lift them all the way up to the top.
	  (vector `(lambda ,formals ,types (letrec ,body-b* ,body)) ())]
	 )]
      
      ;; [2008.03.28] This handles things like "timer", utilize available type info:
      [(,prim ,arg* ...) (guard (regiment-primitive? prim))	     
       (define tmp
	 (map-prim-w-types 
	  (lambda (arg ty)
	    (match arg
	      [(quote ,datum) 
	       ;; We want to use extra info, but we don't want to *weaken* the info we have.
	       (do-datum datum (types-compat? ty (type-const datum)))]
	      [(annotations . ,_) ;; Hack, the stream operators will have these fake arguments.
	       (vector arg '())]
	      [,oth (Expr oth fallthrough)]))
	  prim arg*))
       (define _args (map (lambda (v) (vector-ref v 0)) tmp))
       (define cb    (apply append (map (lambda (v) (vector-ref v 1)) tmp)))
       (vector (cons prim _args) cb)]

      [,other (fallthrough other)]))
    
  ;; Returns vector of two things: new expr and list of const binds
  [Expr Expr]

  [Fuser (lambda (results k)
         (match results
           [(#(,exps ,binds) ...) (vector (apply k exps) (apply append binds))]
           [,other (error 'remove-complex-constant:process-expr 
                          "bad intermediate result: ~s" other)]))]

  [Program 
   (lambda (prog process-expr)
     (match prog
       [(,input-language (quote (program ,body ,meta* ... ,type)))
        (let-match ([#(,body ,body-b*) (process-expr body)])
          (if (null? body-b*)
              `(remove-complex-constant-language
                '(program ,body ,meta* ... ,type))
              `(remove-complex-constant-language
                '(program 
                     (letrec ,body-b* ,body)
                   ,meta* ...  ,type))))]))]

  ;; Returns (1) Expr (2) Type (3) Mutable?
  (define datum->code
    (let* ([pow32 (expt 2 32)]
	   [pow31 (expt 2 31)]
	   [convert-to-signed
	    (lambda (n)
	      (if (< n pow31) n
		  (- (- pow32 n))))])
      (lambda (orig origty)
	;(when (and (not origty) (not (string? orig)) (not (boolean? orig)))
	;  (printf " INFERRING TYPE OF CONST: ~s\n" orig))
	;; Empty tenv is ok, it's just a constant:
	;(ASSERT origty)
	(let loop ([x orig] 
		   ;[type (or origty (recover-type `',orig (empty-tenv)))]
		   ;[type (recover-type `',orig (empty-tenv))]
		   ;[type (type-const orig)]
		   [type (or origty (export-type (type-const orig)))])
	  (match type

	   [(List ,elt-t)
	    ;; Respect the invariant that nulls have type assertions?
	    (if (null? x)
		;; LAME: the regiment part of the backend doesn't know how to handle these assert-types
		(values ''() type #f)
		;; Really mutability is a function of the type, not the value.  This is a bit silly.
		(let-values ([(e1 t1 mu1?) (loop (car x) elt-t)]
			     [(e2 t2 mu2?) (loop (cdr x) type)])
		  (values `(cons ,e1 ,e2) type (or mu1? mu2?))))]
	   	   
	   ;; Vectors are mutable and can't be lifted to the top with our current semantics.
	   ;; This generates awfully verbose code:
	   [(Array ,elt-ty)
	    (cond
	     
	     [(= 0 (vector-length x))
	      ;(values `(assert-type ,type Array:null) type #f)
	      (values 'Array:null type #f)
	      ]

	     ;; If they're all equal, reduce to 
	     [(and (all-equal? (vector->list x))
		   (not (type-containing-mutable? elt-ty)))
	      (when (>= (regiment-verbosity) 3)
		(printf " ** Note: Found compile-time vector with constant contents.\n"))
	      ;`(Array:make ,(vector-length x) (assert-type ,elt-ty ,(vector-ref x 1)))
	      (values	       
	       (if (eq? 'BOTTOM (vector-ref x 0))
		   ;; HACK: Special case, an array of BOTTOM turns back into Array:makeUNSAFE
		   (begin
		     (when (>= (regiment-verbosity) 3)
		       (printf "   ** Hack: turning Array of bottom symbols back into Array:makeUNSAFE.\n"))
		     `(Array:makeUNSAFE ',(vector-length x)))
		   `(Array:make ',(vector-length x) ,(first-value (datum->code (vector-ref x 0) elt-ty))))
	       type
	        #t)]

	     ;; For the new C backends we're going to keep array constants:
	     [(and ;(wsc2-variant-mode? (compiler-invocation-mode))
	       ;; Note, currently [2008.04.08] this only works for wstiny:
	           (eq? (compiler-invocation-mode) 'wavescript-compiler-nesc)
		   (scalar-type? elt-ty)  ;; But just for arrays of scalars for now.
		   )
	      (values `',x type #t)]
	     
	     ;; Otherwise generate some verbose code to fill the array:
	     [else (values
		    (let ([tmp (unique-name "tmparr")])
		      `(let ([,tmp ,type (Array:makeUNSAFE ',(vector-length x))])
			 (begin 
			   ,@(list-build 
			      (vector-length x)
			      (lambda (i) 
				;; Because of Array:makeUNSAFE at metatime, we might end up with BOTTOMs here:
				(if (eq? 'BOTTOM (vector-ref x i))
				    ;; We needn't fill anything in, generate a NOOP:
				    '(tuple)
				    `(Array:set ,tmp ',i ,(first-value (datum->code (vector-ref x i) elt-ty))))))
			   ,tmp)))
		    type #t)])]

	   ;; This is kind of pointless... we turn tuples back into code.
	   [,vec (guard (vector? vec))
	    (let* ([anymuts? #f]
		   [expr `(tuple ,@(map (lambda (x ty) 
					  (let-values ([(e ty mut?) (loop x ty)])
					    (when mut? (set! anymuts? #t))
					    e))
				     (tuple-fields x) (vector->list type) ))])
	      (values expr type anymuts?))]
	   
	   ;; This is only for nullseg:
	   [(Sigseg ,elt-t)
	    (ASSERT (fx= 0 (vector-length (sigseg-vec x))))
	    (values 'nullseg type #f)]
	   
	   ;[Int (inspect x) (values `',x type #f)]

	   ;; HACK: if we don't have the type available, we make it generic again.
	   [(NUM ,_) 
	    ;(unless (fixnum? x) (printf "GINT : ~s\n" x))
	    (values `(gint ',x) type #f)]
	   #;
	   [,othernum (guard (memq othernum num-types))  
		      (printf "NUM TYPE, wrapping: ~s ~s\n" othernum x)
		      (values `(assert-type ,othernum ',x) type #f)]
	   

	   ;; [2008.03.28] No type info at all, restart and use
	   ;; type-const which will give us *something*
	   [(quote ,_) 	    
	    (if (eq? x 'BOTTOM)
		(values ''BOTTOM type #f)
		(loop x (export-type (type-const x))))]

	   ;; [2006.10.14] Umm we shouldn't be supporting symbols:
	   [,_ (guard (symbol? x))  (values `',x type #f)]

	   [String (values `',x type #f)]
	   
	   ;; Opt: should move this case up:
	   ;; Anything else doesn't need an assert-type:
	   [,_ (guard (simple-constant? x)) 
	       (ASSERT scalar-type? type)
	       (values `',x type #f)]
	   [else (error 'datum->code "unhandled quoted constant: ~s" x)]
	   )))))
#;
  (define negate-datum
    (lambda (datum)
        (cond
	 [(number? datum) (process-expr `(quote ,(- datum)))]
	 [else (error 'remove-complex-constant.negate-datum
		      "cannot negate non-numeric datum: ~s" datum)])))
  )


) ; End module