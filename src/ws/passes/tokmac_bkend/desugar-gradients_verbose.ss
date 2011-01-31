;;;; .title Desugar gradients... verbosely (pass23_desugar-gradients_verbose.ss)

;;;; [2005.02.25] <br>
;;;; (This is the original desugar-gradients pass, in all its hairy complexity.)
;;;; <br><br>

;  FIXME: TODO!!!! REWRITE ACCUMULATOR TO USE A FIXED SIZE VECTOR!!!
;  TODO!!!! REFACTOR TO USE TML-GENERIC-TRAVERSE
;  TODO!!!! Make version that uses estimated link quality.

;  CHECK FOR REFERENCES TO GLOBAL-TREE!!!

;  TODO: heuristic optimization: if there are no emits to unknown
;  tokens, then we needn't be conservative about first class token refs.


;;;;  This pass will convert TM's with gradients to TM's without them.
;;;;  It represents one particular (and fragile) algorithm for gradient implementation.
;;;; <br><br>

;;;;  It proceeds by adding FOUR additional arguments to every token handler:
;;;;    gradient parent, gradient origin node, hop-count, gradient version, 
;;;; <br><br>

;;;;  NOTE: Requires another CLEANUP (cleanup-token-machine) after this pass executes.
;;;;  (It uses shorthand such as "and" and "or" syntax.
;;;; <br><br>

;;;;  NOTE: For now token emission, relaying, distance checking, and
;;;;        returning are all restricted to statically specified tokens
;;;;        (but the subtok indices may be dynamically computed). <br>
;;;;  This just makes it easier for me to determine which handlers need
;;;;  extra gradient arguments and which don't.  
;;;; <br><br>

;;;;  NOTE: For now we require that seed expressions be statically computable and deterministic!
;;;; <br><br>

;;;;  TODO: Two aggregations should merge with eachother even if they
;;;;  don't come from the same return statement!!  They just need to use
;;;;  the same tree, same destination, and same aggregator!
;;;; <br><br>

;  Input grammar:

;    <Pgm> := (program (bindings <Cbind>*) <NodePgm>)
;    <NodePgm> := (nodepgm (tokens <TokBinding>*))
;       NOTE: tokens will inclide a binding for SOC-start and node-start
;    <Cbind> := (<var> <Exp>)
;       NOTE: This expressions will be statically calculable -- constants.
;    <TokBinding> := (<TokName> <SubtokId> (<Var> ...) (bindings <Cbind>*) (stored <Stored>) <Expr>)
;    <TokName>   := <Symbol> 
;    <SubtokId>  := <Number>
;    <Token>     := (tok <Tokname> <Int>)
;    <DynToken>  := <Token>    | (tok <Tokname> <Expr>)
;       NOTE: Either the whole token reference or just the sub-index can be dynamic.
;    <Expr>      := (quote <Constant>)
;                  | <Var>
;                  | <DynToken>
;                  | (set! <Var> <Expr>)
;                  | (ext-ref <Token> <Var>)
;                  | (ext-set! <Token> <Var> <Expr>)
;       NOTE: These are static token refs for now.
;                  | (begin <Expr> ...)
;                  | (let ((<Symbol> <Expr>)) <Expr>)
;                  | (if <Expr> <Expr> <Expr>)
;                  | (subcall <DynToken> <Expr>...)
;                  | (<Prim> <Expr> ...)
;                  | (<Expr> ...)
;                  | (leds <Red|Yellow|Green> <On|Off|Toggle>)
;                  | <GExpr>
;   <GExpr>      := (gemit <DynToken> <Expr> ...)
;                  | (grelay <DynToken>)                    ; NEED TO ADD RELAY ARGS!
;                  | (greturn <Expr> (to <DynToken>) (via <DynToken>) (seed <Expr>) (aggr <PlainTok>))
;                  | (greturn <Expr> (to <DynToken>) (via <DynToken>) (seed <Expr>) (aggr #f))
;                  | (gdist <DynToken>)

;    <Prim> := <BasicPrim> 
;             | call | subcall | timed-call | bcast
;             | is_scheduled | deschedule | is_present | evict

;   Output Grammar:

;   No more GExpr

; ----------------------------------------------------------------------
; Changes

; [2006.01.12] Putting the introduced arguments at the end of the list
; instead of the beginning.  This is because I'm now using indices for
; dynamic ext-ref/set!.


; ----------------------------------------------------------------------
;;; Main program

;; This is the compiler pass itself.
(define desugar-gradients_verbose
  (build-compiler-pass
   'desugar-gradients
   `(input)
   `(output ) ;(grammar ,foo PassInput))
  (let ()

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
	       [(tok ,t ,n) (guard (number? n))  (values () `(tok ,t ,n))]
	       [(tok ,t ,[loop -> etb e])        (values etb `(tok ,t ,e))]
	       [,other (error 'statictok "this is not a token: ~a" other)])))

    ;; The usual expression processor.
    (define process-expr
      (lambda (env tokens this-token tainted)
	(letrec ([loop 
	  (lambda (expr)
	    (match expr
;	     [,x (guard (begin (printf  "PEXPmatch ~s\n" x) #f)) 3]

	     [(quote ,const) (values () `(quote ,const))]
             ;; Only for recurring on tokens:
	     [,num (guard (number? num))  (values () num)]
	     [,var (guard (symbol? var))  (values () var)]
	     [(set! ,var ,[etb e])        (values etb  `(set! ,var ,e))]
	     ;; [2006.01.12] Loosening statictok restriction on ext-ref/set!
	     [(ext-ref ,[loop  -> ttb t] ,v)       (values ttb `(ext-ref ,t ,v))]
	     [(ext-set! ,[loop -> ttb t] ,v ,[e2tb e2]) (values (append ttb e2tb)
						       `(ext-set! ,t ,v ,e2))]
	     ;; This is "dynamic" context so no tainted names are allowed!
	     ;; Basically gradient bearing token handlers are second class!
	     [(tok ,t ,n) (guard (number? n))
	      (if #f ;(memq t tainted) ; Allowing -[2005.11.23]
		  (error 'desugar_gradients:process-expr "dynamic token ref to tainted token!: ~a" t)
		  (values () `(tok ,t ,n)))]
	     [(tok ,t ,[etb e])                
	      (if #f ;(memq t tainted) ; Allowing -[2005.11.23]
		  (error 'desugar_gradients:process-expr "dynamic token ref to tainted token!: ~a" t)
		  (values etb `(tok ,t ,e)))]
	     [(begin ,[tb* expr*] ...)         (values (apply append tb*) `(begin ,expr* ...))]
	     [(if ,[ttb test] ,[ctb conseq] ,[atb altern]) 
	      (values (append ttb ctb atb) 
		      `(if ,test ,conseq ,altern))]
	     [(let ([,lhs ,[rtb rhs]]) ,[btb body])
	      (values (append rtb btb)
		      `(let ([,lhs ,rhs]) ,body))]

	     [(let-stored ([,lhs ,[rtb rhs]]) ,[btb body])
	      (values (append rtb btb)
		      `(let ([,lhs ,rhs]) ,body))]
	     
	     [(gemit ,[(statictok loop) -> ttb tok] ,[atb* args*] ...)
	      (values (apply append ttb atb*)
	      (let ((ver (unique-name 'ver))
		    (emitargs (map unique-name (make-list (length args*) 'emitargs))))
	      `(let-stored ([,ver 0])
		;; Set our gparent flag to NULL_ID, indicating this node is the root of this tree.
		;(ext-set! ,tok ,STORED_PARENT_ARG ',NULL_ID)
			   ;; DOESNT WORK ^^^ Might not be there.
		;; Increment persistent version counter:
		(set! ,ver (+ 1 ,ver))
		,@(DEBUG_GRADIENTS `(dbg "~a: Emitting tok %d ver ~a" (my-id) ',tok ,ver))
		
		;; TEMP: mark emissions:
		;(light-node 0 100 100)

		(let* ,(map list emitargs args*)
		  ;; Arguments: Parent, Origin, Hopcount, Version, realargs
		  (call ,tok ,@(add-grad-args-to emitargs `(',NO_PARENT (my-id) 0 ,ver)))
		  
		  ;; [2006.01.12] Removing this broadcast, the callee does this via relay.
;		  (bcast ,tok (my-id) (my-id) 1 ,ver ,@emitargs)
		  ))))]
	     
	     ;; TODO: This doesn't cache or pass any arguments on to the grelayed tokhand!!!!
	     [(grelay (tok ,t ,n) ,[atb* arg*] ...) (guard (number? n))
	      (values (apply append atb*)
	      (if (eq? this-token t)
		  `(bcast (tok ,t ,n) ,@(add-grad-args-to arg* `((my-id) ,ORIGIN_ARG (+ 1 ,HOPCOUNT_ARG) ,VERSION_ARG)))
		  `(bcast (tok ,t ,n)
			  ,@(add-grad-args-to 
			     arg*
			     `((my-id)
			       (ext-ref (tok ,t ,n) ,STORED_ORIGIN_ARG)
			       (+ 1 (ext-ref (tok ,t ,n) ,STORED_HOPCOUNT_ARG))
			       (ext-ref (tok ,t ,n) ,STORED_VERSION_ARG))
			     ))))]
	     [(grelay (tok ,t ,[etb e]) ,[atb* arg*] ...)
	      (values (apply append etb atb*)
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
				 ))))))]
	     [(grelay ,other ...)
	      (error 'desugar-gradients
		     "bad grelay form: ~s" `(grelay ,other ...))]

	     ;; Gdist uses standard units of 10 rather than 1.
	     [(gdist ,tok) 
	      (mvlet ([(ttb expr) (loop `(ghopcount ,tok))])
		(values ttb `(* 10 ,expr)))]
	     ;; Uses the current version rather than the stored one if its available.	     
	     [(ghopcount ,[(statictok loop) -> ttb tok])
	      (values ttb
		      (if (eq? (token->tokname tok) this-token)
			  `(if (eq? ',LOCALCALL ,HOPCOUNT_ARG)
			       ,STORED_HOPCOUNT_ARG
			       ,HOPCOUNT_ARG)
			  `(ext-ref ,tok ,STORED_HOPCOUNT_ARG)))]
	     [(gparent ,[(statictok loop) -> ttb tok])
	      (values ttb
		      (if (eq? (token->tokname tok) this-token)
			  `(if (eq? ',LOCALCALL ,HOPCOUNT_ARG)
			       ,STORED_PARENT_ARG
			       ,PARENT_ARG)
			  `(ext-ref ,tok ,STORED_PARENT_ARG)))]
	     [(gorigin ,[(statictok loop) -> ttb tok])
	      (values ttb
		      (if (eq? (token->tokname tok) this-token)
			  `(if (eq? ',LOCALCALL ,HOPCOUNT_ARG)
			       ,STORED_ORIGIN_ARG
			       ,ORIGIN_ARG)
			  `(ext-ref ,tok ,STORED_ORIGIN_ARG)))]
	     [(gversion ,[(statictok loop) -> ttb tok])
	      (values ttb
		      (if (eq? (token->tokname tok) this-token)
			  `(if (eq? ',LOCALCALL ,HOPCOUNT_ARG)
			       ,STORED_VERSION_ARG
			       ,VERSION_ARG)
			  `(ext-ref ,tok ,STORED_VERSION_ARG)))]	    
	     
	     ;; TODO: THIS WILL NEED TO GET MUCH MORE COMPLEX:
	     ;; I'm assuming this chunk of code will get hit with regular frequency.
	     ;; However, we don't *know* that frequency.  Do we have enough info to aggregate?
	     ;; Well, for now we just do the epoch-skewed thing.  Each time we're called we send 
	     ;; up our old results and start aggregating new ones.
	     [(greturn ,[etb expr]            ;; Value
		      (to (tok ,to ,[ttb toind_expr]))
		      (via (tok ,via ,[vtb viaind_expr]))
		      (seed ,[stb seed_exp])
		      (aggr ,aggr)) 
	      (let ([aggr_ID (unique-name 'aggr_ID)]
		    [acc (unique-name 'acc)]
		    [oldacc (unique-name 'oldacc)]
		    [stored_seed (unique-name 'stored_seed)]
		    [parent_pointer (unique-name 'parent_pointer)]
		    [return-handler (unique-name 'greturn-handler)]
		    [return-timeout-handler (unique-name 'greturn-timeout-handler)]
		    [return-aggr-and-send (unique-name 'greturn-aggrsend-handler)]
		    )

	      (values 
	       ;; First return value: 

	       ;;  Two new token handlers.  These are the return handlers.  
	       ;;  One for storing state (aggregation accumulator), and one 
	       ;;  for timer events for this aggregation.
	       `(
		 ;; Invoke the timeout handler from node-start:
		 ;; ASSUMES STATIC TOIND VIAIND:
;; [2005.10.10] DONT NEED THIS, JUST MAKE SURE THAT ANY GRETURN ACTIVITY SETS TIMER.
; 		 [node-start () 
; 		    (let ((retid 
; 			   ',(if (not (and (integer? viaind_expr)
; 					   (integer? toind_expr)))
; 				 (error 'desugar-gradient "not allowed to have dynamic viaind/toind: ~a/~a\n"
; 					viaind_expr toind_expr)
; 				 (+ (* MAX_SUBTOK toind_expr) viaind_expr))))
; 		      ,@(DEBUG_GRADIENTS
; 			 `(dbg "%d.%d: Setting gradient aggr/up-send time-out timer, retid %d." (my-clock) (my-id) retid))
; 		      (token-deschedule (tok ,return-timeout-handler retid))
; 		      (timed-call ,DEFAULT_RHSEND (tok ,return-timeout-handler retid) ,toind_expr ,viaind_expr))]

		 ;; First the timeout handler.  When it fires it does the aggregation and sends it up the tree.
	         ;; We only use this when there's an aggregator, otherwise return vals go straight up!
		 ,@(if aggr
		 `([,return-timeout-handler retid (toind viaind) (stored)
		   ;; Do the aggregate-and-send:
		   ,@(DEBUG_GRADIENTS
		      `(dbg "%d.%d  Time-out fired!" (my-clock) (my-id)))

		   (call (tok ,return-aggr-and-send retid) toind viaind)

		   ,@(COMMENT "Reset the default time-out timer, if there's anything left to aggregate")
		   
		   (if (and (token-present? (tok ,return-handler retid))
			    (not (equal? (ext-ref (tok ,return-handler retid) ,acc)
					 ;(ext-ref (tok ,return-handler retid) ,stored_seed)
					 ,seed_exp
					 )))
		       (begin 
			 ,@(DEBUG_GRADIENTS
			    `(dbg "%d.%d: Reset timer again." (my-clock) (my-id)))
			 (token-deschedule (tok ,return-timeout-handler retid))
			 (timed-call ,DEFAULT_RHSEND (tok ,return-timeout-handler retid) toind viaind))
		       (begin ,@(DEBUG_GRADIENTS
				 `(dbg "%d.%d: Nothing in acc, stop time-out timer." (my-clock) (my-id)))))
		   ]) ())

		 [,return-aggr-and-send retid (toind viaind) (stored)
                   ;; First thing first we check to see if the data token exists.
		   ;; If not there is no point in firing.
		   (if (not (token-present? (tok ,return-handler retid)))
		       (void) ;; fizzle
		     ;; Otherwise it's time to aggregate!
		     ;; NOTE: This is cheating!!  I'm using pointers and heap allocation here.  Not part of the strict model!
		     (let ([,oldacc (ext-ref (tok ,return-handler retid) ,acc)])
		       (if ,(if aggr ''#t 
				`(or (not (null? ,oldacc))
				     (begin 
				       (dbg "~a.~a: WARNING: Trying to return-aggr-and-send with a null accumulator on non-aggr greturn!"
					    (my-clock) (my-id))
				       #f)
				     ))
			   (begin 
			     ,@(DEBUG_GRADIENTS
				`(dbg "%d.%d: Aggr-and-send: ~a" (my-clock) (my-id) ,oldacc))

			     ;; Reset the accumulator, doesn't matter if there's no aggregator:		       
			     (ext-set! (tok ,return-handler retid) ,acc ,(if aggr seed_exp `(cdr ,oldacc)))
			     
		       ;; Next, do we have the via token?
		       (if (not (token-present? (tok ,via viaind)))
			   ;; NOTE: FIZZLE SEMANTICS.
			   ;; That is, if we get a local return before the trees there.  Then we just fizzle.
			   ;; One could imagine buffering here, but that gets complex.
			   (begin 
			     ,@(DEBUG_GRADIENTS
				`(dbg "Warning: Didn't have the via token %d at node %d (FIZZLE)" ',via (my-id))))
			   ;; Now we look at the via tree for this aggregation. Have we reached the root of the tree?	
			   (let ((,parent_pointer (ext-ref (tok ,via viaind) ,STORED_PARENT_ARG)))
			     (if (not ,parent_pointer)
				 (begin ,@(DEBUG_GRADIENTS `(dbg "ERROR: fell off the via tree: %d at node %d" ',via (my-id)))
					(void))
				 (if (eq? ',NO_PARENT ,parent_pointer)
				     (begin
				       ,@(COMMENT "Reached tree root, calling to token.")
				       ,@(DEBUG_GRADIENTS 
					  `(dbg "~a.~a: At ROOT of tree, invoking ~a<~a> with ~a" 
						(my-clock) (my-id) ',to toind 
						,(if aggr oldacc `(car ,oldacc))))
				       (call (tok ,to toind) ,(if aggr oldacc `(car ,oldacc)))
				       )
				     ;; Otherwise, send it on up to the parent:
				     ;; TODO: Should use "send_to" form here, but haven't implemented yet:
				     (begin 
				       ,@(DEBUG_GRADIENTS
					  `(dbg "%d.%d: Returning up tree %d, parent %d to %d acc %d" 
						(my-clock) (my-id) ',via ,parent_pointer
						'(tok ,return-handler retid) ,oldacc))

					;TODO: (if (simalpha-visualize-gradients)
				       (highlight-edge ,parent_pointer (rgb 200 0 0))
				       
				       (bcast (tok ,return-handler retid)
					      ,parent_pointer ;; destid
					      ',RHREMOTE        ;; flag
					      ,(if aggr oldacc `(car ,oldacc)) ;; val
					      toind viaind  ;; toind, viaind
					      ))))))
		       ;; If we're in non-aggregation mode, we just keep sending whatever we've got:
		       ,@(if aggr ()
			     `((if (and (not (null? (cdr ,oldacc)))
					(not (token-scheduled? (tok ,return-aggr-and-send retid))))
				   (begin 
				     ,@(DEBUG_GRADIENTS
					`(dbg "%d.%d: Still have stuff left, continue aggregating..." (my-clock) (my-id)))
				     (call (tok ,return-aggr-and-send retid) toind viaind)))))
		       ))))
		   ]
		;; Now the data handler.
		;; When called locally, this triggers aggregation.
		;; When called remotely, this builds up the accumulator.
		;; If there is no aggregation operator (fold function) provided, it still aggregates; it
		;; simply builds *lists* of results that are passed up to parents.  (i.e. cons is default aggregator)
		[,return-handler retid (destid flag val toind viaind)
		   ;; retid is used as a unique identifier to keep different aggregations from overlapping
		   ;; destid is ID number of node that the message is bound for (??????)
		   ;; flag indicates which type of invocation this is
		   ;; val is the value itself
                   ;; toind is the subtoken index of the "to" token
		   ;; viaind is the subtoken index of the "via" token

;; FIXME TODO: Must check destid before running stuff....
;(if (not (= destid (my-id)))
;    (printf  "WARNING gradient-aggregation: destid ~a not equal myid ~a\n" destid (my-id)))
				 
		   ;; Must be initialized with the seed value before aggregation begins.
		   ;; If there is no aggregator, the accumulator stores just a single value,
		   ;; each returned datum is handled/transmitted seperately.
		   (stored 
;		    ,@(if aggr `([,stored_seed ,seed_exp]) ())
		    [,acc ,(if aggr seed_exp ''())])
		   
		   ,@(DEBUG_GRADIENTS
		      `(if (or (eq? flag ',RHLOCAL)
			       (eq? flag ',RHSEND)
			       (eq? flag ',RHINIT)
			       (and (eq? flag ',RHREMOTE) (or (= destid ',NULL_ID) (= destid (my-id)))))
			   (dbg '"~a.~a: Return Handler<~a>: args (~a ~a ~a to:~a.~a via:~a.~a) prnt:~a timeout?:~a aggrsend?:~a acc: ~a"
				(my-clock) (my-id) retid destid flag val ',to toind ',via viaind 
				(ext-ref (tok ,via viaind) ,STORED_PARENT_ARG)
				,(if aggr `(token-scheduled? (tok ,return-timeout-handler retid)) ''NA)
				(token-scheduled? (tok ,return-aggr-and-send retid))
				,acc)
			   (void)))
		       
		   ;; While the potential subcall is hapenning below this return_handler very well may be called again.
		   ;; FIXME: This is dangerous.
		   ;; One of the values may get lost.
		   ;; We need something like a lock on the ACC variable.
		   (if (eq? flag ',RHLOCAL)
		       ;; When we get the local value, we lump it together:
		       (begin
			 (set! ,acc ,(if aggr `(subcall ,aggr val ,acc) ;; [2005.11.03] Making direct for now.
					      `(cons val ,acc)))
			 ;; Now kill the scheduled timer token if there is one, and set a new timer.
			 ;; (Don't bother with the if, because default semantics for deschedule 
			 ;; is to fizzle if its not there.)
			   ;(if (token-scheduled? (tok ,return-timeout-handler retid))

			 ;; Do aggregation right now:
			 (call (tok ,return-aggr-and-send retid) toind viaind)

			 ,@(COMMENT "Reset the default time-out timer")
			 ,@(if aggr
			    `((begin 
			       (token-deschedule (tok ,return-timeout-handler retid));)
			       ,@(DEBUG_GRADIENTS
				  `(dbg "%d.%d: Setting time-out!!" (my-clock) (my-id)))
			       (timed-call ,DEFAULT_RHSEND (tok ,return-timeout-handler retid) toind viaind)
			       ,@(DEBUG_GRADIENTS
				  `(dbg "%d.%d: Time-out set: %d" 
					(my-clock) (my-id) (token-scheduled? (tok ,return-timeout-handler retid))))))
			    ())
			 )
		       
		       ;; Otherwise, flag = RHREMOTE
		       ;; If called remotely, we only proceed if we are the intended destination.
		       (if (not (or (= destid ',NULL_ID) (= destid (my-id))))
			     ;(DEBUG_GRADIENTS (dbg '"  CANCELED, not destination."))
			     (void) ;; TODO: FIXME OPTIMIZATION: Might want to evict self here -- wasted space on useless tokens.
			     ;; Now we simply accumulate and wait for the local call.
			     (begin 
			       (set! ,acc ,(if aggr `(subcall ,aggr val ,acc) ;; [2005.11.03] Making direct for now
					       `(cons val ,acc)))

			       ,(if aggr 
				 `(begin 
				    ,@(COMMENT "Now we don't reset the timer, but we ENSURE that it's set:")
				    (if (not (token-scheduled? (tok ,return-timeout-handler retid)))
					(timed-call ,DEFAULT_RHSEND (tok ,return-timeout-handler retid) toind viaind)))
				 `(begin
				    ,@(COMMENT "For non-aggregated returns we just send up immediately.")
				    (call (tok ,return-aggr-and-send retid) toind viaind))
			       )))
		       )]
		;; And finally add the new return handler(s) to the other bindings:
		,@(append etb ttb vtb stb))

	       ;; Second return value: the generated code for the return statement.
	       ;; This will call the return-handler on the local node,
	       ;; which will result in a message sent up to the  parent.
	       ;; This expression is expected to be executed regularly, to drive a regular 
	       ;; aggregation process.  It doesn't yet work for return-once 
	       ;; Each aggregation is unique based on its to, via, and aggr arguments:
	       (let ([toind_tmp (unique-name 'toind)]
		     [viaind_tmp (unique-name 'viaind)])
		 `(let ((,toind_tmp ,toind_expr))
		    (let ((,viaind_tmp ,viaind_expr))
		      (let ([,aggr_ID (begin 
					,@(COMMENT "This aggregation identifier incorporates the TO and VIA components:")
					(+ (* ,MAX_SUBTOK ,toind_tmp) ,viaind_tmp))])
			,(make-begin 
			  (append 
			   (COMMENT "Call the appropriate local return 'server'")
			   `((call (tok ,return-handler ,aggr_ID) 
				  (my-id)
				  ',RHLOCAL ;; flag
				  ,expr ,toind_tmp ,viaind_tmp))))))))
	       ))]
	      
	     [(return ,[etb e]) (values etb `(return ,e))]
	     [(leds ,what ,which)                      
	      (values () `(leds ,what ,which))]


	     ;; OTHERWISE, let it fall through to the prim case.
	     ;; Call to non-gradient bearing token:
;	     [(,call-style ,[ttb tok] ,[atb* args*] ...) 
;	      (guard (memq call-style '(call subcall bcast)))
;	      (values (apply append ttb atb*)
;		      `(call ,tok ,args* ...))]
	     ;; Same here:
	     [(timed-call ,[ttb time] (tok ,t ,[etb e]) ,[atb* args*] ...)
	      (guard (memq t tainted))
	      (values (apply append ttb etb atb*)
		      `(timed-call ,time (tok ,t ,e)
				   ,@(add-grad-args-to
				      args*
				      `('#f ;; gparent
					'#f ;; gorigin
					',LOCALCALL   ;; ghopcount
					'#f ;; gversion				   
					))))]
	     ;; OTHERWISE, let it fall through to the prim case.
;	     [(timed-call ,[ttb time] ,[ttb2 tok] ,[atb* args*] ...) 
;	      (values (apply append ttb ttb2 atb*)
;		      `(timed-call ,time ,tok ,args* ...))]

	     ;; This is a non-gradient call to a gradient-bearing token:
	     [(,call-style (tok ,t ,[etb e]) ,[atb* args*] ...)
	      (guard (memq t tainted) (memq call-style '(call subcall bcast call-fast)))
	      (values (apply append etb atb*)
		      `(,call-style (tok ,t ,e)
				    ,@(add-grad-args-to 
				       args*
				       `('#f ;; gparent
					 '#f ;; gorigin
					 ',LOCALCALL   ;; hopcount -- LOCAL CALLS HAVE HOPCOUNT "LOCALCALL"
					 '#f ;; gversion
					 ))))]

	     [(,prim ,args ...)
	      (guard (or ;(begin (disp "Prim: " prim args) #f)
			 (token-machine-primitive? prim)
			 (basic-primitive? prim))
		     )
; 	      (DEBUG_GRADIENTS
; 	       (if (memq prim '(call subcall bcast))
; 		   (error 'desugar-gradients:process-expr
; 			  "Somehow this call form fell through to prim case: ~a" `(,prim ,@args))))
	      ;; Treat token args differently:	      
	      (let ([newtbs '()])
		(let ([newargs		       
		       (map-prim-w-types 
			(lambda (arg type)
			  (match (cons type arg)
			    [(Token . (tok ,tok ,[loop -> tb* e]))
			     (set! newtbs (append tb* newtbs))
			     `(tok ,tok ,e)]
			    [(,other . ,[loop -> tb* e])
			     (set! newtbs (append tb* newtbs))
			     e]))
			prim args)])
		  
		  (values newtbs `(,prim ,newargs ...))
		  ))]

	     ; TEMPORARY, We allow arbitrary other applications too!
	     [(app ,[rtb rator] ,[rtb* rands] ...)
	      (warning 'desugar-gradient
		       "arbitrary application of rator: ~s" rator)
	      (values (apply append rtb rtb*)
		      `(app ,rator ,rands ...))]

	     [,otherwise
	      (error 'desugar-gradient:process-expr 
		     "bad expression: ~s" otherwise)]
	     ))])

	  loop)))

    ;; The usual token handler processor.
    (define process-tokbind 
	(lambda (env tokens tainted)
	  (lambda (tokbind)
	    (mvlet ([(tok id args stored bindings body) (destructure-tokbind tokbind)])
 	       (mvlet ([(newtoks newbod) ((process-expr env tokens tok tainted) body)])
		 (values newtoks
		   (if (not (memq tok tainted))
		       ;; Just return the plain old token handler:
		       `(,tok ,id ,args (stored ,@stored) ,newbod)
		       ;; In this case make it a gradient token:
		       `(,tok ,id ,(add-grad-args-to args `(,PARENT_ARG ,ORIGIN_ARG ,HOPCOUNT_ARG ,VERSION_ARG))
			 ;; Don't use let-stored for these, it incurs extra overhead:
			 (stored ;[call-count 0]
			  ,@stored 
			  ;; We're adding these to the end to not mess up possible index-based references.
			  [,STORED_PARENT_ARG   '#f]
			  [,STORED_ORIGIN_ARG   '#f]
			  [,STORED_HOPCOUNT_ARG '#f]
			  [,STORED_VERSION_ARG  '#f]
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
		       )))))))
    
    ;; Main body of desugar-gradient
    (lambda (prog)
      (match prog
	[(,lang '(program (bindings ,constbinds ...) 
			  (nodepgm (tokens ,toks ...))
			  (emittoks ,tainted ...)))
	 (let ([processtb (process-tokbind (map car constbinds) toks tainted)])
	   (match toks
	     [(,[processtb -> newtoks toks] ...)
	      `(desugar-gradient-lang
		'(program (bindings ,constbinds ...)
			  (nodepgm (tokens ,@(apply append toks newtoks)))))]))
	 ]))
    )))


