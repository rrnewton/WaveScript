
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
  
  (define (do-datum datum ty)
    (let-values ([(exp type mutable?) (datum->code datum ty)])
      (if (or mutable? (simple-expr? exp))
	  (vector exp ())
	  (let ([tmp (unique-name 'tmp)])
	    (vector tmp `((,tmp ,type ,exp))))
	  )))
    
  ;; Returns vector of two things: new expr and list of const binds
  [Expr (lambda (x fallthrough)
	  (match x 	    
	    [(assert-type ,ty (quote ,datum))
	     (match (do-datum datum ty)
	       [#(,e ,cb*) (vector `(assert-type ,ty ,e) cb*)])]
	    [(quote ,datum)   (do-datum datum #f)]
	  
	  ;; Don't lift out these complex constants!
	  [(foreign ',name ',files) (vector `(foreign ',name ',files) ())]

          [(lambda ,formals ,types ,[result])
	   (match result
	     [#(,body ,body-b*) 
	      ;;(vector `(lambda ,formals ,body) body-b*)
	      ;; [2005.12.08] Modifying this so it doesn't (yet) lift them all the way up to the top.
	      (vector `(lambda ,formals ,types (letrec ,body-b* ,body)) ())]
	     )]
	  [,other (fallthrough other)]))]

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

  ;; Works just for lists right now.
  (define datum->code
    (let* ([pow32 (expt 2 32)]
	   [pow31 (expt 2 31)]
	   [convert-to-signed
	    (lambda (n)
	      (if (< n pow31) n
		  (- (- pow32 n))))])
      (lambda (orig origty)
	;; Empty tenv is ok, it's just a constant:
	(let loop ([x orig] [type (or origty (recover-type `',orig (empty-tenv)))])
	  (cond		     

	   [(pair? x)
	    (match type
	      [(List ,elt-t)
	       ;; Really mutability is a function of the type, not the value.  This is a bit silly.
	       (let-values ([(e1 t1 mu1?) (loop (car x) elt-t)]
			    [(e2 t2 mu2?) (loop (cdr x) type)])
		 (values `(cons ,e1 ,e2) type (or mu1? mu2?)))])]
	   
	   ;; Respect the invariant that nulls have type assertions:
	   [(null? x) 
	    ;; LAME: the regiment part of the backend doesn't know how to handle these assert-types
	    (if (memq (compiler-invocation-mode)  '(wavescript-simulator wavescript-compiler-cpp wavescript-compiler-caml))
		(begin
		  (ASSERT type)
		  (ASSERT (compose not polymorphic-type?) type)
		  (values `(assert-type ,type '()) type #f))
		(values ''() type #f))]

	   ;; Vectors are mutable and can't be lifted to the top.
	   [(vector? x) ;(ASSERT type)
	    (values
	     (let ([tmp (unique-name 'tmparr)])
	      `(let ([,tmp ,type (Array:makeUNSAFE ',(vector-length x))])
		 (begin 
		   ,@(list-build 
		      (vector-length x)
		      (lambda (i) 
			`(Array:set ,tmp ',i ,(first-value (datum->code (vector-ref x i)))))
		      )
		   ,tmp)))
	     type #t)]

	   [(simple-constant? x) (values `',x type #f)]
	   ;; [2006.10.14] Umm we shouldn't be supporting symbols:
	   [(symbol? x)  (values `',x type #f)]

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