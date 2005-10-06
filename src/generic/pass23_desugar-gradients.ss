;; TODO!!!! REWRITE ACCUMULATOR TO USE A FIXED SIZE VECTOR!!!



;; CHECK FOR REFERENCES TO GLOBAL-TREE!!!

;; TODO: heuristic optimization: if there are no emits to unknown
;; tokens, then we needn't be conservative about first class token refs.




;; [2005.02.25]
;; This pass will convert TM's with gradients to TM's without them.
;; It represents one particular (and fragile) algorithm for gradient implementation.

;; It proceeds by adding FOUR additional arguments to every token handler:
;;   gradient parent, gradient origin node, hop-count, gradient version, 


;; NOTE: Requires another CLEANUP (cleanup-token-machine) after this pass executes.
;; (It uses shorthand such as "and" and "or" syntax.

;; NOTE: For now token emission, relaying, distance checking, and
;;       returning are all restricted to statically specified tokens
;;       (but the subtok indices may be dynamically computed).
;; This just makes it easier for me to determine which handlers need
;; extra gradient arguments and which don't.  

;; NOTE: For now we require that seed expressions be statically computable and deterministic!

;; TODO: Two aggregations should merge with eachother even if they
;; don't come from the same return statement!!  They just need to use
;; the same tree, same destination, and same aggregator!


;; Input grammar:

;;;  <Pgm> ::= (program (bindings <Cbind>*) <NodePgm>)
;;;  <NodePgm> ::= (nodepgm (tokens <TokBinding>*))
;       NOTE: tokens will inclide a binding for SOC-start and node-start
;;;  <Cbind> ::= (<var> <Exp>)
;       NOTE: This expressions will be statically calculable -- constants.
;;;  <TokBinding> ::= (<TokName> <SubtokId> (<Var> ...) (bindings <Cbind>*) (stored <Stored>) <Expr>)
;;;  <TokName>   ::= <Symbol> 
;;;  <SubtokId>  ::= <Number>
;;;  <Token>     ::= (tok <Tokname> <Int>)
;;;  <DynToken>  ::= <Token>    | (tok <Tokname> <Expr>)
;;;     NOTE: Either the whole token reference or just the sub-index can be dynamic.
;;;  <Expr>      ::= (quote <Constant>)
;;;                | <Var>
;;;                | <DynToken>
;;;                | (set! <Var> <Expr>)
;;;                | (ext-ref <Token> <Var>)
;;;                | (ext-set! <Token> <Var> <Expr>)
;       NOTE: These are static token refs for now.
;;;                | (begin <Expr> ...)
;;;                | (let ((<Symbol> <Expr>)) <Expr>)
;;;                | (if <Expr> <Expr> <Expr>)
;;;                | (subcall <DynToken> <Expr>...)
;;;                | (<Prim> <Expr> ...)
;;;                | (<Expr> ...)
;;;                | (leds <Red|Yellow|Green> <On|Off|Toggle>)
;;;                | <GExpr>
;;; <GExpr>      ::= (gemit <DynToken> <Expr> ...)
;;;                | (grelay <DynToken>) ;; NEED TO ADD RELAY ARGS!
;;;                | (greturn <Expr> (to <DynToken>) (via <DynToken>) (seed <Expr>) (aggr <PlainTok>))
;;;                | (greturn <Expr> (to <DynToken>) (via <DynToken>) (seed <Expr>) (aggr #f))
;;;                | (gdist <DynToken>)

;;;  <Prim> ::= <BasicPrim> 
;;;           | call | subcall | timed-call | bcast
;;;           | is_scheduled | deschedule | is_present | evict

;;; Output Grammar:

;;; No more GExpr


(define desugar-gradients
  (let ()

    ;; This is a very error prone pass, I'm optionally including a bunch of debugging print statements.
    ;(IFDEBUG
       (define-syntax DEBUG_GRADIENTS (syntax-rules () [(_ expr ...) (list expr ...)])) ;; ON
      ;(define-syntax DEBUG_GRADIENTS (syntax-rules () [(_ expr ...) ()]))              ;; OFF
    ;)


    (define PARENT_ARG 'g_parent)
    (define ORIGIN_ARG 'g_origin)
    (define HOPCOUNT_ARG 'g_hopcount)
    (define VERSION_ARG 'g_version)

    (define STORED_PARENT_ARG 'stored_g_parent) 
    (define STORED_ORIGIN_ARG 'stored_g_origin)
    (define STORED_HOPCOUNT_ARG 'stored_g_hopcount)
    (define STORED_VERSION_ARG 'stored_g_version)

    ;; This signifies that we're at the root of the tree.
    (define NO_PARENT 'noparent)

    ;; Value for the g_hopcount argument to indicate that it's a local (non-gradient) invocation.
    (define LOCALCALL 'nongrad-invoke)
    
    ;; Call flags:
;    (define RHINIT 111)
;    (define RHLOCAL 222)
;    (define RHREMOTE 333)
;    (define RHTIMEOUT 444)
    (define RHINIT 'rhinit)
    (define RHLOCAL 'rhlocal)
    (define RHREMOTE 'rhremote)
    (define RHTIMEOUT 'rhtimeout)

    ;; Default return-handler timeout:
    ;; Won't hold buffered values forever...
    (define DEFAULT_RHTIMEOUT 1000)



    (define (token->tokname t)
      (match t
	[(tok ,t ,e) t]))



;; Replacing this the verbose case with a simple generic-traversal: 
;; FIXME: NOT DONE YET
; (define find-emittoks 
;   (letrec ([do-primitive
; 	    ;; We don't count direct references to tokens in primitive 
; 	    ;; arguments as tainted.
; 	    (lambda (prim args loopk)
; 	      (apply append
; 		     (map-prim-w-types 
; 		      (lambda (arg type)
; 			(match (cons type arg)
; 			       [(Token . (tok ,tok ,[loopk -> e])) e]
; 			       [(,other . ,[loopk -> e]) e]))
; 		      prim args)))])

;   (lambda (expr)
;     (tml-generic-traverse
;      (lambda (x k)
;        (match x

;          ;; "Direct call":  Not allowing dynamic gemit's for now:
; 	 [(gemit (tok ,t ,[e]) ,[args*] ...)  (cons t (apply append e args*))]
; 	     ;; Indirect gemit call... could consider restricting these.
; 	 [(gemit ,[e] ,[args*] ...)
; 	  (error 'pass23_desugar-gradients "not allowing dynamically targetted emits atm.")
; 					;(apply append e args*)
; 	  ]
; 	 ;; Also allowing dynamic grelays and gdists.  
; 	 ;; These don't matter as much because I'm basing 
; 	 [(grelay (tok ,t ,[e])) e]
; 	 [(gdist (tok ,t ,[e])) e]
; 	 [(gparent (tok ,t ,[e])) e]
; 	 [(gorigin (tok ,t ,[e])) e]
; 	 [(ghopcount (tok ,t ,[e])) e]
; 	 [(gversion (tok ,t ,[e])) e]

; 	 ;; The to's and the vias are static! Aggr has no subtok index! 
; 	 [(greturn ,[expr] (to (tok ,t ,tn)) (via (tok ,v ,vn)) (seed ,[seed_val]) (aggr ,a))
; 	  (append expr seed_val)]
  

    (define find-emittoks
      (letrec ([tok-allowed-loop 
		(lambda (expr)
		  (match expr
			 [(tok ,t ,[main-loop -> e]) e]
			 [,e (main-loop e)]))]
	       [do-primitive
		(lambda (prim args)
		  (apply append
		  (map-prim-w-types 
		   (lambda (arg type)
		     (match (cons type arg)
		       [(Token . (tok ,tok ,[main-loop -> e])) e]
		       [(,other . ,[main-loop -> e]) e]))
		   prim args)))]
	       [main-loop
		(lambda (expr)
		  (match expr
	     ;[,x (guard (begin (printf "FindEmitToks matching: ~a~n" x) #f)) 3]
	     [(quote ,_) '()]
	     [,num (guard (number? num)) '()]
	     [,var (guard (symbol? var)) '()]
	     [(set! ,var ,[e]) e]
	     [(ext-ref (tok ,t ,[e]) ,v) e]
	     [(ext-set (tok ,t ,[e]) ,v ,[e2]) (append e e2)]
	     ;; If we ever have a first class reference to a token name, it is potentially tainted.
	     ;; This is a conservative estimate:
	     [(tok ,t ,n) (guard (number? n)) (list t)]
	     [(tok ,t ,[e]) (cons t e)]
	     [(begin ,[exprs] ...) (apply append exprs)]
	     [(if ,[exprs] ...) (apply append exprs)]
	     [(let ([,_ ,[rhs]]) ,[body])	(append body rhs)]

	     ;; "Direct call":  Not allowing dynamic gemit's for now:
	     [(gemit (tok ,t ,[e]) ,[args*] ...)  (cons t (apply append e args*))]
	     ;; Indirect gemit call... could consider restricting these.
	     [(gemit ,[e] ,[args*] ...)
	      (error 'pass23_desugar-gradients "not allowing dynamically targetted emits atm.")
	      ;(apply append e args*)
	      ]
	     ;; Also allowing dynamic grelays and gdists.  
	     ;; These don't matter as much because I'm basing 
	     [(grelay (tok ,t ,[e])) e]
	     [(gdist (tok ,t ,[e])) e]
	     [(gparent (tok ,t ,[e])) e]
	     [(gorigin (tok ,t ,[e])) e]
	     [(ghopcount (tok ,t ,[e])) e]
	     [(gversion (tok ,t ,[e])) e]

	     ;; The to's and the vias are static! Aggr has no subtok index!
	     [(greturn ,[expr] (to (tok ,t ,tn)) (via (tok ,v ,vn)) (seed ,[seed_val]) (aggr ,a))
	      ;; The via requires that a tree be there, and hence it be gradientized.
	      (cons v (append expr seed_val))]

	     [(leds ,what ,which) '()]

	     ;; Static calls are allowed:
	     [(call (tok ,t ,[e]) ,[args*] ...) (apply append e args*)]
	     ;; Anything more dynamic makes us think the operand is potentially emitted.

	     [(return ,[e])  e]

#|	     [(call ,[args*] ...) (apply append args*)]
	     [(subcall (tok ,t ,[e]) ,[args*] ...) (apply append e args*)]
	     [(subcall ,[args*] ...) (apply append args*)]
	     [(bcast (tok ,t ,[e]) ,[args*] ...) (apply append e args*)]
	     [(bcast ,[args*] ...) (apply append args*)]
	     [(timed-call ,[time] (tok ,t ,[e]) ,[args*] ...) (apply append time e args*)]
	     [(timed-call ,[args*] ...) 
	      ;(disp "TIMED call to dynamic tokens...")
	      (apply append args*)]
|#	     
	     ;; All primitives that can take tokenss
	     [(,prim ,args* ...)
	      (guard (or (token-machine-primitive? prim)
			 (basic-primitive? prim)))
	      (do-primitive prim args*)
	      ]

	     [(app ,[rator] ,[rands] ...) (apply append rator rands)]
	     [,otherwise
	      (error 'desugar-gradient:find-emittoks
		     "bad expression: ~s" otherwise)]
	     ))])

	main-loop))


    (define (statictok loop)
      (lambda (tk)
	(match tk
	       [(tok ,t ,n) (guard (number? n))  (values () `(tok ,t ,n))]
	       [(tok ,t ,[loop -> etb e])                (values etb `(tok ,t ,e))]
	       [,other (error 'statictok "this is not a token: ~a" other)])))

	    
    (define process-expr
      (lambda (env tokens this-token tainted)
	(letrec ([loop 
	  (lambda (expr)
	  (match expr
	     [(quote ,const) (values () `(quote ,const))]
             ;; Only for recurring on tokens:
	     [,num (guard (number? num))  (values () num)]
	     [,var (guard (symbol? var))  (values () var)]
	     [(set! ,var ,[etb e])        (values etb  `(set! ,var ,e))]
	     [(ext-ref ,[ttb t] ,v)       (values ttb `(ext-ref ,t ,v))]
	     [(ext-set ,[ttb t] ,v ,[e2]) (values ttb `(ext-set ,t ,v ,e2))]
	     ;; This is "dynamic" context so no tainted names are allowed!
	     ;; Basically gradient bearing token handlers are second class!
	     [(tok ,t ,n) (guard (number? n))
	      (if (memq t tainted) 
		  (error 'desugar_gradients:process-expr "dynamic token ref to tainted token!: ~a" t)
		  (values () `(tok ,t ,n)))]
	     [(tok ,t ,[etb e])                
	      (if (memq t tainted) 
		  (error 'desugar_gradients:process-expr "dynamic token ref to tainted token!: ~a" t)
		  (values etb `(tok ,t ,e)))]
	     [(begin ,[tb* expr*] ...)         (values (apply append tb*) `(begin ,expr* ...))]
	     [(if ,[ttb test] ,[ctb conseq] ,[atb altern]) 
	      (values (append ttb ctb atb) 
		      `(if ,test ,conseq ,altern))]
	     [(let ([,lhs ,[rtb rhs]]) ,[btb body])
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
		(let* ,(map list emitargs args*)
		  ;; Arguments: Parent, Origin, Hopcount, Version, realargs
		  (call ,tok ',NO_PARENT (my-id) 0 ,ver ,@emitargs)
		  (bcast ,tok (my-id) (my-id) 1 ,ver ,@emitargs)
		  ))))]

	     ;; TODO: This doesn't cache or pass any arguments on to the grelayed tokhand!!!!
	     [(grelay (tok ,t ,n)) (guard (number? n))
	      (values ()
	      (if (eq? this-token t)
		  `(bcast (tok ,t ,n) (my-id) ,ORIGIN_ARG (+ 1 ,HOPCOUNT_ARG) ,VERSION_ARG)
		  `(bcast (tok ,t ,n)
			  (my-id)
			  (ext-ref (tok ,t ,n) ,STORED_ORIGIN_ARG)
			  (+ 1 (ext-ref (tok ,t ,n) ,STORED_HOPCOUNT_ARG))
			  (ext-ref (tok ,t ,n) ,STORED_VERSION_ARG))))]
	     [(grelay (tok ,t ,[etb e]))
	      (values etb
	      (if (eq? this-token t)
		  `(bcast (tok ,t ,e) (my-id) ,ORIGIN_ARG (+ 1 ,HOPCOUNT_ARG) ,VERSION_ARG)
		  (let ([num (unique-name 'n)])
		    `(let ([,num ,e])
		       `(bcast (tok ,t ,num)
			       (my-id)
			       (ext-ref (tok ,t ,num) ,STORED_ORIGIN_ARG)
			       (+ 1 (ext-ref (tok ,t ,num) ,STORED_HOPCOUNT_ARG))
			       (ext-ref (tok ,t ,num) ,STORED_VERSION_ARG))))))]
	     ;; Uses the current version rather than the stored one if its available.
	     [(gdist ,[(statictok loop) -> ttb tok])
	      (values ttb
		      (if (eq? (token->tokname tok) this-token)
			  ;; In this case we're inside the handler currently:
			  ;; Choose based on whether it's a real gradient call, or just local:
			  `(if (eq? ',LOCALCALL ,HOPCOUNT_ARG)
			       ,STORED_HOPCOUNT_ARG
			       ,HOPCOUNT_ARG)
;			  HOPCOUNT_ARG
			  `(ext-ref ,tok ,STORED_HOPCOUNT_ARG)))]
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
		    [parent_pointer (unique-name 'parent_pointer)]
		    [return-handler (unique-name 'greturn-handler)]
		    [return-timeout-handler (unique-name 'greturn-timeout-handler)]
		    )

	      (values 
	       ;; First return value: 

	       ;;  Two new token handlers.  These are the return handlers.  
	       ;;  One for storing state (aggregation accumulator), and one 
	       ;;  for timer events on for this aggregation.
	       `(
		 ;; First the timer handler.  When it fires it does the aggregation and sends it up the tree.
		 [,return-timeout-handler retid (toind viaind) (stored)
		   ;; First thing first we check to see if the data token exists.
		   ;; If not there is no point in firing.
		   (if (not (token-present? (tok ,return-handler retid)))
		       (void) ;; fizzle
		     ;; Otherwise it's time to aggregate!
		     ;; NOTE: This is cheating!!  I'm using pointers and heap allocation here.  Not part of the strict model!
		     (let ([,oldacc (ext-ref (tok ,return-handler retid) ,acc)])
		       ;; Reset the accumulator:
		       (ext-set! (tok ,return-handler retid) ,acc ,(if aggr seed_exp ''()))
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
				     ;; Now, if we're the destination we need to call the 'to' token.
				     (begin
				       ,@(DEBUG_GRADIENTS 
					  `(dbg "~a: At ROOT of tree, invoking ~a<~a> with ~a" (my-id) ',to toind ,oldacc))
				       (call (tok ,to toind) ,oldacc))
				     ;; Otherwise, send it on up to the parent:
				     ;; TODO: Should use "send_to" form here, but haven't implemented yet:
				     (begin 
				       ,@(DEBUG_GRADIENTS
					  `(dbg "%d.%d: Returning up tree %d, parent %d to %d acc %d" 
						(my-clock) (my-id) ',via ,parent_pointer
						'(tok ,return-handler retid) ,oldacc))
				       (bcast (tok ,return-handler retid)
					      ,parent_pointer ;; destid
					      ',RHREMOTE        ;; flag
					      ,oldacc ;; val
					      toind viaind  ;; toind, viaind
					      ))))))))		   
		   '"Reset the default time-out timer"
		   (token-deschedule (tok ,return-timeout-handler retid));)
		   (timed-call ,DEFAULT_RHTIMEOUT (tok ,return-timeout-handler retid) toind viaind)
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
		   (stored [,acc ,(if (equal? aggr '#f)
				      ''() ;; Use lists as our default aggregation.
				      seed_exp)])
		   
		   ,@(DEBUG_GRADIENTS
		      `(if (or (eq? flag ',RHLOCAL)
			       (eq? flag ',RHTIMEOUT)
			       (eq? flag ',RHINIT)
			       (and (eq? flag ',RHREMOTE) (or (= destid ',NULL_ID) (= destid (my-id)))))
			   (dbg '"~a.~a: Return Handler<~a>: args (~a ~a ~a to:~a.~a via:~a.~a) stored acc: ~a"
				(my-clock) (my-id) retid destid flag val ',to toind ',via viaind ,acc)
			   (void)))
		       
		   ;; While the potential subcall is hapenning below this return_handler very well may be called again.
		   ;; FIXME: This is dangerous.
		   ;; One of the values may get lost.
		   ;; We need something like a lock on the ACC variable.
		   (if (eq? flag ',RHLOCAL)
		       ;; When we get the local value, we lump it together:
		       (begin
			   (set! ,acc ,(if aggr `(subcall ,aggr val ,acc)
					   `(cons val ,acc)))
			   ;; Now kill the scheduled timer token if there is one, and set a new timer.
			   ;; (Don't bother with the if, because default semantics for deschedule 
			   ;; is to fizzle if its not there.)
			   ;(if (token-scheduled? (tok ,return-timeout-handler retid))
			   '"Reset the default time-out timer"
			   (token-deschedule (tok ,return-timeout-handler retid));)
			   (timed-call ,DEFAULT_RHTIMEOUT (tok ,return-timeout-handler retid) toind viaind)
			   )
		       
		       ;; Otherwise, flag = RHREMOTE
		       ;; If called remotely, we only proceed if we are the intended destination.
		       (if (not (or (= destid ',NULL_ID) (= destid (my-id))))
			   (begin
					;(DEBUG_GRADIENTS (dbg '"  CANCELED, not destination."))
			     (void)) ;; TODO: FIXME OPTIMIZATION: Might want to evict self here -- wasted space on useless tokens.
			   ;; Now we simply accumulate and wait for the local call.
			   (set! ,acc ,(if aggr `(subcall ,aggr val ,acc)
					   `(append val ,acc))))
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
		      (let ([,aggr_ID (begin '"This aggregation identifier incorporates the TO and VIA components:"
					     (+ (* ,MAX_SUBTOK ,toind_tmp) ,viaind_tmp))])
			(begin '"Call the appropriate local return 'server'"
			       (call (tok ,return-handler ,aggr_ID) 
				     (my-id)
				     ',RHLOCAL ;; flag
				     ,expr ,toind_tmp ,viaind_tmp))))))
	       ))]
	      
	     [(return ,[etb e]) (values etb `(return ,e))]

	     ;; This is a non-gradient call to a gradient-bearing token:
	     [(,call-style (tok ,t ,[etb e]) ,[atb* args*] ...)
	      (guard (memq t tainted) (memq call-style '(call subcall bcast)))
	      (values (apply append etb atb*)
		      `(,call-style (tok ,t e)
				    '#f ;; gparent
				    '#f ;; gorigin
				    ',LOCALCALL   ;; hopcount -- LOCAL CALLS HAVE HOPCOUNT "LOCALCALL"
				    '#f ;; gversion
			     ,args* ...))]

	     
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
				   '#f ;; gparent
				   '#f ;; gorigin
				   ',LOCALCALL   ;; ghopcount
				   '#f ;; gversion
				   ,args* ...))]
	     ;; OTHERWISE, let it fall through to the prim case.
;	     [(timed-call ,[ttb time] ,[ttb2 tok] ,[atb* args*] ...) 
;	      (values (apply append ttb ttb2 atb*)
;		      `(timed-call ,time ,tok ,args* ...))]

	     [(leds ,what ,which)                      
	      (values () `(leds ,what ,which))]
	     [(,prim ,[rtb* rands] ...)
	      (guard (or (token-machine-primitive? prim)
			 (basic-primitive? prim)))
	      (values (apply append rtb*) `(,prim ,rands ...))]
	     ;;; TEMPORARY, We allow arbitrary other applications too!
	     [(app ,[rtb rator] ,[rtb* rands] ...)
	      (warning 'desugar-gradient
		       "arbitrary application of rator: ~s" rator)
	      (values (apply append rtb rtb*)
		      `(,rator ,rands ...))]

	     [,otherwise
	      (error 'desugar-gradient:process-expr 
		     "bad expression: ~s" otherwise)]
	     ))])

	  loop)))
    

    (define process-tokbind 
	(lambda (env tokens tainted)
	  (lambda (tokbind)
	    (mvlet ([(tok id args stored bindings body) (destructure-tokbind tokbind)])
 	       (mvlet ([(newtoks newbod) ((process-expr env tokens tok tainted) body)])
		      (values newtoks
			      (if (memq tok tainted)				  
				  `(,tok ,id (,PARENT_ARG ,ORIGIN_ARG ,HOPCOUNT_ARG ,VERSION_ARG ,@args)

					 ;; Don't use let-stored for these, it incurs extra overhead:
					 (stored ;[call-count 0]
						      [,STORED_PARENT_ARG   '#f]
						      [,STORED_ORIGIN_ARG   '#f]
						      [,STORED_HOPCOUNT_ARG '#f]
						      [,STORED_VERSION_ARG  '#f]
						      ,@stored)

	    ,@(DEBUG_GRADIENTS
	       `(if (not (eq? ',LOCALCALL ,HOPCOUNT_ARG))
		    (dbg "%d.%d: Gradientized token firing: ~a<~a> with gradargs (~a ~a ~a ~a) and stored (~a ~a ~a ~a) and real args ~a"
			 (my-clock) (my-id) ',tok ,id
			 ,PARENT_ARG ,ORIGIN_ARG ,HOPCOUNT_ARG ,VERSION_ARG 
			 ,STORED_PARENT_ARG ,STORED_ORIGIN_ARG ,STORED_HOPCOUNT_ARG ,STORED_VERSION_ARG
			 (list ,@args))))

					    ;; Here we decide whether or not to accept the token:
					    (if (or 
						    (begin '"Local calls have special hopcount (usually 0), accept those:"
							   (eq? ',LOCALCALL ,HOPCOUNT_ARG))                  ;; Local calls we accept
						    (begin '"First time received we definitely run:" 
							   (not ,STORED_HOPCOUNT_ARG))   ;; First time we definitely accept
						    (> ,VERSION_ARG ,STORED_VERSION_ARG) ;; Newer version we accept
						    (and (= ,VERSION_ARG ,STORED_VERSION_ARG) ;; Smaller hopcounts we accept
							 (< ,HOPCOUNT_ARG ,STORED_HOPCOUNT_ARG)))
						,(make-begin 
						  (list
						   '"The gradient-tagged message is accepted, handler fires."
						  ;; If the msg is accepted we run our code:
						  ;; It can get to both the current version of 
						  ;; the gradient parameters and the "stored" version from last time:
						  newbod
						  ;; And then store these gradient parameters for next time:
						  ;; (Unless it was a local call, in which case there's nothing to store.)
						  `(if (not (eq? ',LOCALCALL ,HOPCOUNT_ARG))
						       (begin '"If it's not a local message, set stored gradient info:"
							 (set! ,STORED_PARENT_ARG ,PARENT_ARG)
							 (set! ,STORED_ORIGIN_ARG ,ORIGIN_ARG)
							 (set! ,STORED_HOPCOUNT_ARG ,HOPCOUNT_ARG)
							 (set! ,STORED_VERSION_ARG ,VERSION_ARG)))))
						;; Otherwise, fizzle
						(begin '"Gradient message fizzles." (void))
						))
				  `(,tok ,id ,args ,newbod))))))))

    (define findall-emittoks
      (lambda (tbs)
	(if (null? tbs) '()
	    (mvlet ([(_ __ ___ ____ _____ body) (destructure-tokbind (car tbs))])
		   (append (find-emittoks body)
			   (findall-emittoks (cdr tbs)))))))

    ;; Main body of desugar-gradient
    (lambda (prog)
      (match prog
	[(,lang '(program (bindings ,constbinds ...) 
			  (nodepgm (tokens ,toks ...))))

	 (let ([tainted (findall-emittoks toks)])
	   (disp "TAINTED: " tainted)
	 (let ([processtb (process-tokbind (map car constbinds) toks tainted)])
	   (match toks
	     [(,[processtb -> newtoks toks] ...)
	      `(desugar-gradient-lang
		'(program (bindings ,constbinds ...)
			  (nodepgm (tokens ,@(apply append toks newtoks)))))])))]))
    ))



(define these-tests
  `(
    
    ["Put an empty test through." 
     (desugar-gradients
      '(cleanup-token-machine-lang
	'(program
	  (bindings )
	  (nodepgm (tokens) ))))
     (desugar-gradient-lang
      '(program (bindings) (nodepgm (tokens))))]


    ["Make sure it gets all the gradient calls out.." 
     (let ((x (desugar-gradients
      '(foolang '(program (bindings) 
        (nodepgm
	 (tokens
	  (node-start subtok_ind () (stored) (void))
	  (SOC-start subtok_ind () (stored) (call (tok f 0)))
	  (f subtok_ind () (stored) (gemit (tok g 0) '3))
	  (g subtok_ind
	     (x)
	     (stored)
	     (if (< (gdist (tok g subtok_ind)) '3)
		 (grelay (tok g subtok_ind))
                (greturn (gdist (tok g subtok_ind))
			(to (tok h 0))
			(via (tok g 0))
			(seed '#f)
			(aggr #f))))
	  (h subtok_ind (v) (stored) (dbg '"Got val: %d\\n" v)))))))))
       (list (deep-assq 'gemit x)
	     (deep-assq 'grelay x)
	     (deep-assq 'ggreturn x)
	     (deep-assq 'gdist x)))
     (#f #f #f #f)]
		        
))


(define test-this (default-unit-tester
		    "23: Desugar-Gradient: convert gradient commands to plain token machine code." 
		    these-tests))


(define test23 test-this)
(define tests23 these-tests)
(define test-desugar-gradients test-this)
(define tests-desugar-gradients these-tests)
