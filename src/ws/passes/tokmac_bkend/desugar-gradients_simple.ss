;;;; .title Desugar gradients... simply!  (pass23_desugar-gradients_simple.ss)

;;;; [2006.01.14] <br>

;;;; This implementation is greatly simplified from the original
;;;; desugar-gradients.  It uses a separate linked library for all of
;;;; the data return/aggregation code.  All that "up"-stuff.
;;;;   <br> <br>

;;;; It's further simplified by the fact that it no longer has to
;;;; generate extra token handlers as it traverses the program.  Thus
;;;; the traversal is simpler and uses tml-generic-traverse.


; ----------------------------------------------------------------------
;;; Main program

;; This is the compiler pass itself.
(define desugar-gradients_simple
  (build-compiler-pass
   'desugar-gradients
   `(input)
   `(output ) ;(grammar ,foo PassInput))
  (let ()

    ;; Every unique to/via pair needs a constant number assigned to it.
    (define get-aggr-ID
      (let ([aggrID-counter 0]
	    [aggrID-table '()])
	(lambda (to via)
	  (let ([key (cons to via)])
	    (let loop ([table aggrID-table])
	      (cond
	       [(null? table)
		(set! aggrID-table
		      (cons `[(,to . ,via) ,aggrID-counter] aggrID-table))
		(set! aggrID-counter (add1 aggrID-counter))
		(sub1 aggrID-counter)]
	       [(equal? (car table) key) (cadar table)]
	       [else (loop (cdr table))]))))))

    (define (token->tokname t)
      (match t
	[(tok ,t ,e) t]))

    ;; This is a front-end that takes an expression processor (looping
    ;; function) and wraps it so as to expect a token expression in
    ;; this context and to not touch the token expression directly.
    ;; (The loop only gets to touch sub-expressions.)
    (define (statictok loop)
      (lambda (tk)
	(match tk
	       [(tok ,t ,n) (guard (number? n))  `(tok ,t ,n)]
	       [(tok ,t ,[loop -> e])        `(tok ,t ,e)]
	       [,other (error 'statictok "this is not a token: ~a" other)])))
	    
    (define process-expr
      (lambda (env tokens this-token tainted)
	(letrec ([loop 
	  (lambda (expr)

	    (tml-generic-traverse
	     ; Driver, Fuser, Expression:
	     (lambda (expr kont)
	       (match expr

	     ;; This is "dynamic" context so no tainted names are allowed!
	     ;; Basically gradient bearing token handlers are second class!
	     [(tok ,t ,n) (guard (number? n))
	      (if #f ;(memq t tainted) ; Allowing -[2005.11.23]
		  (error 'desugar_gradients:process-expr "dynamic token ref to tainted token!: ~a" t)
		  `(tok ,t ,n))]
	     [(tok ,t ,[e])                
	      (if #f ;(memq t tainted) ; Allowing -[2005.11.23]
		  (error 'desugar_gradients:process-expr "dynamic token ref to tainted token!: ~a" t)
		  `(tok ,t ,e))]
	     
	     [(gemit ,[(statictok loop) -> tok] ,[args*] ...)
	      (let ((ver (unique-name 'ver)))
		`(let-stored ([,ver 0])
		   ;; Increment persistent version counter:
		   (set! ,ver (+ 1 ,ver))
		   ,@(DEBUG_GRADIENTS `(dbg "~a: Emitting tok %d ver ~a" (my-id) ',tok ,ver))
		   (call ,tok ',NO_PARENT (my-id) 0 ,ver ,@args*)
		   ))]

	     ;; TODO: This doesn't cache or pass any arguments on to the grelayed tokhand!!!!
	     [(grelay (tok ,t ,n) ,[arg*] ...) (guard (number? n))
	      (if (eq? this-token t)
		  `(bcast (tok ,t ,n) ,@(add-grad-args-to arg* `((my-id) ,ORIGIN_ARG (+ 1 ,HOPCOUNT_ARG) ,VERSION_ARG)))
		  `(bcast (tok ,t ,n)
			  ,@(add-grad-args-to 
			     arg*
			     `((my-id)
			       (ext-ref (tok ,t ,n) ,STORED_ORIGIN_ARG)
			       (+ 1 (ext-ref (tok ,t ,n) ,STORED_HOPCOUNT_ARG))
			       (ext-ref (tok ,t ,n) ,STORED_VERSION_ARG))
			     )))]
	     [(grelay (tok ,t ,[e]) ,[arg*] ...)
	      (if (eq? this-token t)
		  `(bcast (tok ,t ,e) ,@(add-grad-args-to arg* `((my-id) ,ORIGIN_ARG (+ 1 ,HOPCOUNT_ARG) ,VERSION_ARG)))
		  (let ([num (unique-name 'n)])
		    `(let ([,num ,e])
		       (bcast (tok ,t ,num)
			      ,@(add-grad-args-to
				 arg*
				 `((my-id)
				   (ext-ref (tok ,t ,num) ,STORED_ORIGIN_ARG)
				   (+ 1 (ext-ref (tok ,t ,num) ,STORED_HOPCOUNT_ARG))
				   (ext-ref (tok ,t ,num) ,STORED_VERSION_ARG))
				 )))))]
	     [(grelay ,other ...)
	      (error 'desugar-gradients
		     "bad grelay form: ~s" `(grelay ,other ...))]


	     ;; Gdist uses standard units of 10 rather than 1.
	     [(gdist ,tok) `(* 10 ,(loop `(ghopcount ,tok)))]
	     ;; Uses the current version rather than the stored one if its available.
	     [(ghopcount ,[(statictok loop) -> tok])
		      (if (eq? (token->tokname tok) this-token)
			  `(if (eq? ',LOCALCALL ,HOPCOUNT_ARG)
			       ,STORED_HOPCOUNT_ARG
			       ,HOPCOUNT_ARG)
			  `(ext-ref ,tok ,STORED_HOPCOUNT_ARG))]
	     [(gparent ,[(statictok loop) -> tok])
		      (if (eq? (token->tokname tok) this-token)
			  `(if (eq? ',LOCALCALL ,HOPCOUNT_ARG)
			       ,STORED_PARENT_ARG
			       ,PARENT_ARG)
			  `(ext-ref ,tok ,STORED_PARENT_ARG))]
	     [(gorigin ,[(statictok loop) -> tok])
		      (if (eq? (token->tokname tok) this-token)
			  `(if (eq? ',LOCALCALL ,HOPCOUNT_ARG)
			       ,STORED_ORIGIN_ARG
			       ,ORIGIN_ARG)
			  `(ext-ref ,tok ,STORED_ORIGIN_ARG))]
	     [(gversion ,[(statictok loop) -> tok])
		      (if (eq? (token->tokname tok) this-token)
			  `(if (eq? ',LOCALCALL ,HOPCOUNT_ARG)
			       ,STORED_VERSION_ARG
			       ,VERSION_ARG)
			  `(ext-ref ,tok ,STORED_VERSION_ARG))]
	     
	     ;; TODO: THIS WILL NEED TO GET MUCH MORE COMPLEX:
	     ;; I'm assuming this chunk of code will get hit with regular frequency.
	     ;; However, we don't *know* that frequency.  Do we have enough info to aggregate?
	     ;; Well, for now we just do the epoch-skewed thing.  Each time we're called we send 
	     ;; up our old results and start aggregating new ones.
	     [(greturn ,[expr]            ;; Value
		      (to (tok ,to ,toind))
		      (via (tok ,via ,[viaind_expr]))
		      (seed ,[seed_exp])
		      (aggr ,aggr))
	      (if (not (or (equal? toind ''0) (equal? toind '0)))
		  (error 'desugar-gradients_simple
			 "This gradient implementation does not allow to-tok indices that are not zero: (tok ~a ~a)"
			   to toind))
	      (let ([aggr_ID (unique-name 'aggr_ID)]
		    [static_ID (unique-name 'static_ID)])
		
	       ;; Return value: the generated code for the return statement.
	       ;; This will call the return-handler on the local node,
	       ;; which will result in a message sent up to the  parent.
	       ;; This expression is expected to be executed regularly, to drive a regular 
	       ;; aggregation process.  It doesn't yet work for return-once 
	       ;; Each aggregation is unique based on its to, via, and aggr arguments:
	       (let ([viaind_tmp (unique-name 'viaind)])
		 `(let ((,static_ID ',(get-aggr-ID to via)))
		    (let ((,viaind_tmp ,viaind_expr))
		      (let ([,aggr_ID (begin 
					,@(COMMENT "This aggregation identifier incorporates the TO and VIA components:")
					(+ (* ,MAX_SUBTOK ,static_ID) ,viaind_tmp))])
			,(make-begin 
			  (append 
			   (COMMENT "Call the appropriate local return 'server'")
			   `(

			     (call (tok GRAD:return-handler ,aggr_ID)
				  ',NULL_ID   ; destid - UNUSED b/c this is a local call
				  ',RHLOCAL   ; flag
				  ,expr       ; val
				  (tok ,to 0)   ; totok
				  (tok ,via ,viaind_tmp) ; viatok
				  ,aggr       ; aggrtok
				  ,seed_exp   ; seedval
				  ))))))))
	       )]
	      

	     ;; Timed call to gradient bearing token.  (Duplicate code!  
	     ;; Separate case just because the token is in a different place.)
	     [(timed-call ,[time] (tok ,t ,[e]) ,[args*] ...)
	      (guard (memq t tainted))
	      `(timed-call ,time (tok ,t ,e)
			   ,@(add-grad-args-to
			      args*
			      `('#f ;; gparent
				'#f ;; gorigin
				',LOCALCALL   ;; ghopcount
				'#f ;; gversion				   
				)))]
	     ;; This is a non-gradient call to a gradient-bearing token:
	     [(,call-style (tok ,t ,[e]) ,[args*] ...)
	      (guard (memq t tainted) (memq call-style '(call subcall bcast call-fast)))
	      `(,call-style (tok ,t ,e)
			    ,@(add-grad-args-to 
			       args*
			       `('#f ;; gparent
				 '#f ;; gorigin
				 ',LOCALCALL   ;; hopcount -- LOCAL CALLS HAVE HOPCOUNT "LOCALCALL"
				 '#f ;; gversion
				 )))]

	     [(,prim ,args ...)
	      (guard (or ;(begin (disp "Prim: " prim args) #f)
			 (token-machine-primitive? prim)
			 (basic-primitive? prim))
		     )
	      ;; Treat token args differently, they don't count as escaped usages.
	      (let ([newargs		       
		     (map-prim-w-types 
		      (lambda (arg type)
			  (match (cons type arg)
			    [(Token . (tok ,tok ,[loop -> e]))
			     `(tok ,tok ,e)]
			    [(,other . ,[loop -> e])
			     e]))
		      prim args)])
		`(,prim ,newargs ...))]

	     ;; Otherwise fall through to the auto-walker:
	     [,otherwise (kont otherwise)]))

	     ;; Fuser:
	     (lambda (ls k) (apply k ls))
	     ;; Expression:
	     expr))])
	  ;; Return value for process-expr:
	  loop)))

    (define process-tokbind 
	(lambda (env tokens tainted)
	  (lambda (tokbind)
	    (mvlet ([(tok id args stored bindings body) (destructure-tokbind tokbind)])
 	       (let ([newbod ((process-expr env tokens tok tainted) body)])
		 (if (not (memq tok tainted))
		       ;; Just return the plain old token handler:
		       `(,tok ,id ,args (stored ,@stored) ,newbod)
		       ;; In this case make it a gradient token:
		       `(,tok ,id ,(add-grad-args-to args `(,PARENT_ARG ,ORIGIN_ARG ,HOPCOUNT_ARG ,VERSION_ARG))
			 ;; Don't use let-stored for these, it incurs extra overhead:
			 (stored ;[call-count 0]
			   ;; Pay attention to whether gradient stored vars come *before* or after user vars:
			   [,STORED_PARENT_ARG   '#f]
			   [,STORED_ORIGIN_ARG   '#f]
			   [,STORED_HOPCOUNT_ARG '#f]
			   [,STORED_VERSION_ARG  '#f]
			   ,@stored 
			   )
			 
			 ,@(DEBUG_GRADIENTS
			    `(if (not (eq? ',LOCALCALL ,HOPCOUNT_ARG))
				 (dbg "%d.%d: Gradientized token firing: ~a<~a> with gradargs (~a ~a ~a ~a) and stored (~a ~a ~a ~a) and real args ~a"
				      (my-clock) (my-id) ',tok ,id
				      ,PARENT_ARG ,ORIGIN_ARG ,HOPCOUNT_ARG ,VERSION_ARG 
				      ,STORED_PARENT_ARG ,STORED_ORIGIN_ARG ,STORED_HOPCOUNT_ARG ,STORED_VERSION_ARG
				      (list ,@args))))
			 
			 ;; Here we decide whether or not to accept the token:
			 (if (or 
			      (begin ,@(COMMENT "Local calls have special hopcount (usually 0), accept those:")
				     (eq? ',LOCALCALL ,HOPCOUNT_ARG))                  ;; Local calls we accept
			      (begin 
				,@(COMMENT "First time received we definitely run:")
				(not ,STORED_HOPCOUNT_ARG))   ;; First time we definitely accept
			      (> ,VERSION_ARG ,STORED_VERSION_ARG) ;; Newer version we accept
			      (and (= ,VERSION_ARG ,STORED_VERSION_ARG) ;; Smaller hopcounts we accept
				   (< ,HOPCOUNT_ARG ,STORED_HOPCOUNT_ARG)))
			     ,(make-begin 
				`(,@(COMMENT "The gradient-tagged message is accepted, handler fires.")
				  				  
				  (highlight-edge ,PARENT_ARG (rgb 0 0 150) 1000)

				;; If the msg is accepted we run our code:
				;; It can get to both the current version of 
				;; the gradient parameters and the "stored" version from last time:
				  ,newbod
				;; And then store these gradient parameters for next time:
				;; (Unless it was a local call, in which case there's nothing to store.)
				  (if (not (eq? ',LOCALCALL ,HOPCOUNT_ARG))
				      (begin 
					,@(COMMENT "If it's not a local message, set stored gradient info:")
					(set! ,STORED_PARENT_ARG ,PARENT_ARG)
					(set! ,STORED_ORIGIN_ARG ,ORIGIN_ARG)
					(set! ,STORED_HOPCOUNT_ARG ,HOPCOUNT_ARG)
					(set! ,STORED_VERSION_ARG ,VERSION_ARG)))))
			     ;; Otherwise, fizzle
			     (begin ,@(COMMENT "Gradient message fizzles.") (void))
				
			     ))
		       ))))))
    
    ;; Does a simple tree-walk to look for the occurence of a (greturn ...) form.
    (define (contains-greturn? tb)
      (mvlet ([(_ __ ___ ____ _____ body) (destructure-tokbind tb)])
	(tml-generic-traverse
	 ;; driver, fuser, expression
	 (lambda (x loop) 
	   (match x
	     [(greturn ,_ ...) #t]
	     [,x (loop x)]))
	 (lambda (ls _) (ormap (lambda (x) x) ls))
	 body)))
    
    ;; Main body of desugar-gradient
    (lambda (prog)
      (match prog
	[(,lang '(program (bindings ,constbinds ...) 
			  (nodepgm (tokens ,toks ...))
			  (emittoks ,tainted ...)))
	 ;; If there are any greturns in the program, we link in the aggregation-code.
	 (let ([linkedcode
		(if (ormap contains-greturn? toks)
		    (match (mvlet (((prog _) 
				    (read-regiment-source-file 
				     (** (WAVESCRIPTD) "/src/linked_lib/gradient_lib.tm"))))
			     prog)
		      [(tokens ,gradtoks ...) gradtoks])
		    '())])

	   (let ([processtb (process-tokbind (map car constbinds) toks tainted)])
	     (match toks
	       [(,[processtb -> toks] ...)
		`(desugar-gradient-lang
		  '(program (bindings ,constbinds ...)
		     (nodepgm (tokens ,@(append toks linkedcode)))))]))
	   )]))
    )))
    
