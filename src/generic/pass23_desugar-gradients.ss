
;; CHECK FOR REFERENCES TO GLOBAL-TREE!!!



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
;;; <GExpr>      ::= (emit <DynToken> <Expr> ...)
;;;                | (relay <DynToken>) ;; NEED TO ADD RELAY ARGS!
;;;                | (return <Expr> (to <DynToken>) (via <DynToken>) (seed <Expr>) (aggr <PlainTok>))
;;;                | (dist <DynToken>)

;;;  <Prim> ::= <BasicPrim> 
;;;           | call | subcall | timed-call | bcast
;;;           | is_scheduled | deschedule | is_present | evict

;;; Output Grammar:

;;; No more GExpr


(define desugar-gradients
  (let ()
    (define PARENT_ARG 'g_parent)
    (define ORIGIN_ARG 'g_origin)
    (define HOPCOUNT_ARG 'g_hopcount)
    (define VERSION_ARG 'g_version)

    (define STORED_PARENT_ARG 'stored_g_parent)
    (define STORED_ORIGIN_ARG 'stored_g_origin)
    (define STORED_HOPCOUNT_ARG 'stored_g_hopcount)
    (define STORED_VERSION_ARG 'stored_g_version)
    
    ;; Call flags:
    (define INIT 111)
    (define LOCAL 222)
    (define REMOTE 333)

    (define (token->tokname t)
      (match t
	[(tok ,t ,e) t]))

    (define (find-emittoks expr)
      (match expr
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

	     ;; "Direct call":  Not allowing dynamic emit's for now:
	     [(emit (tok ,t ,[e]) ,[args*] ...)  (cons t (apply append e args*))]
	     ;; Indirect emit call... could consider restricting these.
					;[(emit ,[e] ,[args*] ...) (apply append e args*)]

	     ;; Also allowing dynamic relays and dists.  
	     ;; These don't matter as much because I'm basing 
	     [(relay (tok ,t ,[e])) e]
	     [(dist (tok ,t ,[e])) e]
	     ;; The to's and the vias are static! Aggr has no subtok index!
	     [(return ,[expr] (to (tok ,t ,tn)) (via (tok ,v ,vn)) (seed ,[seed_val]) (aggr ,a))
	      (append expr seed_val)]

	     [(leds ,what ,which) '()]
	     [(call ,[args*] ...) (apply append args*)]
	     [(subcall ,[args*] ...) (apply append args*)]
	     [(bcast ,[args*] ...) (apply append args*)]
	     [(timed-call ,[args*] ...) (apply append args*)]
					;	 [(activate ,_ ,[args*] ...) (apply append args*)]
	     
	     [(,[rator] ,[rands] ...) (apply append rator rands)]

	     [,otherwise
	      (error 'desugar-gradient:find-emittoks
		     "bad expression: ~s" otherwise)]
	     ))

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
	     [,num (guard (number? num)) (values () num)]
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

	     [(emit ,[(statictok loop) -> ttb tok] ,[atb* args*] ...)
	      (values (apply append ttb atb*)
	      (let ((ver (unique-name 'ver)))
	      `(let-stored ([,ver 0])
		;; Set our parent flag to NULL_ID, indicating this node is the root of this tree.
		(ext-set! ,tok ,STORED_PARENT_ARG ',NULL_ID)
		;; Increment persistent version counter:
		(set! ,ver (+ 1 ,ver))
		(dbg "Emitting %d from %d\n" tok (my-id))
		(bcast ,tok (my-id) 1 ,ver ,@args*))))]

	     ;; TODO: This doesn't cache or pass any arguments on to the relayed tokhand!!!!
	     [(relay (tok ,t ,n)) (guard (number? n))
	      (values ()
	      (if (eq? this-token t)
		  `(bcast (tok ,t ,n) (my-id) ,ORIGIN_ARG (+ 1 ,HOPCOUNT_ARG) ,VERSION_ARG)
		  `(bcast (tok ,t ,n)
			  (my-id)
			  (ext-ref (tok ,t ,n) ,STORED_ORIGIN_ARG)
			  (+ 1 (ext-ref (tok ,t ,n) ,STORED_HOPCOUNT_ARG))
			  (ext-ref (tok ,t ,n) ,STORED_VERSION_ARG))))]
	     [(relay (tok ,t ,[etb e]))
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
	     [(dist ,[(statictok loop) -> ttb tok])
	      (values ttb
		      (if (eq? (token->tokname tok) this-token)
			  HOPCOUNT_ARG ;; In this case we're inside the handler currently.
			  `(ext-ref ,tok STORED_HOPCOUNT_ARG)))]
	     
	     ;; TODO: THIS WILL NEED TO GET MUCH MORE COMPLEX:
	     ;; I'm assuming this chunk of code will get hit with regular frequency.
	     ;; However, we don't *know* that frequency.  Do we have enough info to aggregate?
	     ;; Well, for now we just do the epoch-skewed thing.  Each time we're called we send 
	     ;; up our old results and start aggregating new ones.
	     [(return ,[etb expr]            ;; Value
		      (to (tok ,to ,[ttb toind]))
		      (via (tok ,via ,[vtb viaind]))
		      (seed ,[stb seed_exp])
		      (aggr ,aggr)) 
	      (let ([aggr_ID (unique-name 'aggr_ID)]
		    [acc (unique-name 'acc)]
		    [oldacc (unique-name 'oldacc)]
		    [return-handler (unique-name 'return-handler)])
	      (values 
	       ;; First return value: a new handler for the aggregation object
	       ;; corresponding to this particular return statment.
	       ;; When called locally, this sends the aggregate to the parent, and resets the acc.
	       ;; When called remotely, this builds up the aggregation accumulator.
	       ;; If there is no aggregation operator provided, it does the same thing except 
	       ;; simply builds lists of results that are passed up to parents.
	       (cons `[,return-handler retid (destid flag val toind viaind)
			;; Must be initialized with the seed value before aggregation begins.
		        (stored [,acc ,(if (equal? aggr '#f)
					  '()
					  seed_exp)])

			;; TODO: we need to have a facility for intermediate nodes on the tree to NOT 
			;; be part of the aggregation.  This will probably amount to another "mode flag" 
			;; -- a version of LOCAL that just ignores "val".  But we'll still need to set up 
			;; a timed call to fire the return-handler with this INTERMEDIARY flag.

			;; TODO: we need to activate a timer on every node that receives return messages.
			;; This timer will insure that *every* return value gets returned at some point.
			;; LOCAL calls which force the aggregation will reset the timer.

		       ;; First thing is to check the flag to see if we're being called locally or remotely.
		       ;; But before that we'll bind some functions for our aggregator and seed.
		       ;; If there is not a user-provided aggregator, the implicit aggregator forms a list of all results.
		       ,(let ([fold (lambda (ac)
				      (if aggr
					  `(subcall ,aggr val ,ac)
					  `(cons val ,ac)))]
			      [theseed (if aggr seed_exp ''())]
			      [parent_pointer (unique-name 'parent_pointer)])
			`(if (= flag ',LOCAL) ;; When we get the local value, we lump together and send upwards.
			     (let ([,oldacc ,acc])
			       ,@(DEBUGMODE `((dbg "Returning locally at %d val %d" (my-id) ,(fold oldacc))))

			       ;; Reset the accumulator:
			       (set! ,acc ,theseed)
			       ;; While the potential subcall is hapenning below this return_handler very well may be called again.
			       ;; But we just reset the acc, so any calls from this moment on will be in the next epoch.
			       ;; Now we look at the via tree for this aggregation. Have we reached the root of the tree?
			       ,@(DEBUGMODE 
				  (if (not (is_present? (tok ,via viaind)))
				      `((dbg "ERROR: fell off the via tree: %d at node %d\n" ,via (my-id))))
				  
				  TODO FINISISIHIHSISHI)

			       (let ((,parent_pointer (ext-ref (tok ,via viaind) ,STORED_PARENT_ARG)))
				 (if (not ,parent_pointer)
				     ,@(DEBUGMODE `((dbg "ERROR: fell off the via tree: %d at node %d\n" ,via (my-id))))
				     (if (= ',NULL_ID ,parent_pointer)
					 ;; Now, if we're the destination we need to call the 'to' token.
					 (call (tok ,to toind) ,(fold oldacc))
					 ;; Otherwise, send it on up to the parent:
					 ;; TODO: Should use "send_to" form here, but haven't implemented yet:
					 (begin 
					   ,@(DEBUGMODE
					      `((dbg "Returning up tree from %d to %d val %d" 
						 (my-id) '(tok ,return-handler retid) ,(fold oldacc))))
					 (bcast (tok ,return-handler retid)
						,parent_pointer ;; destid
						',REMOTE        ;; flag
						,(fold oldacc) ;; val
						,toind ,viaind  ;; toind, viaind
						))))))

			     ;; Otherwise, flag = REMOTE
			     ;; If called remotely, we only proceed if we are the intended destination.
			     (if (not (or (= destid ',NULL_ID) (= destid (my-id))))
				 (void) ;; Might want to evict self here -- wasted space on useless tokens.
				 ;; Now we simply accumulate and wait for the local call.
				 (set! ,acc ,(fold acc)))))]
		     (append etb ttb vtb stb))

	       ;; Second return value: the generated code for the return statement.
	       ;; This will call the return-handler on the local node,
	       ;; which will result in a message sent up to the  parent.
	       ;; This expression is expected to be executed regularly, to drive a regular 
	       ;; aggregation process.  It doesn't yet work for return-once 
	       ;; Each aggregation is unique based on its to, via, and aggr arguments:
		 `(let ([,aggr_ID (+ (* ,MAX_SUBTOK ,toind) ,viaind)])
		    (call (tok ,return-handler ,aggr_ID) 
			  (my-id)
			  ',LOCAL ;; flag
			  ,expr ,toind ,viaind))))]

	     ;; This is a local call to a gradient-bearing token:
	     [(,call-style (tok ,t ,[etb e]) ,[atb* args*] ...)
	      (guard (memq t tainted) (memq call-style '(call subcall bcast)))
	      (values (apply append etb atb*)
		      `(call (tok ,t e)
			     '#f ;; parent
			     '#f ;; origin
			     0   ;; hopcount
			     '#f ;; version
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
				   '#f ;; parent
				   '#f ;; origin
				   0   ;; hopcount
				   '#f ;; version
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
	     [(,[rtb rator] ,[rtb* rands] ...)
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
						      [,STORED_VERSION_ARG  '#f])
					    ;; Here we decide whether or not to accept the token:
					    (if (or (not ,STORED_HOPCOUNT_ARG)           ;; First time we definitely accept
						    (= 0 ,HOPCOUNT_ARG)                   ;; Local calls we accept
						    (> ,VERSION_ARG ,STORED_VERSION_ARG) ;; Newer version we accept
						    (and (= ,VERSION_ARG ,STORED_VERSION_ARG) ;; Smaller hopcounts we accept
							 (< ,HOPCOUNT_ARG ,STORED_HOPCOUNT_ARG)))
						,(make-begin 
						  (list
						  ;; If the msg is accepted we run our code:
						  ;; It can get to both the current version of 
						  ;; the gradient parameters and the "stored" version from last time:
						  newbod
						  ;; And then store these gradient parameters for next time:
						  ;; (Unless it was a local call, in which case there's nothing to store.)
						  `(if (not (= ,HOPCOUNT_ARG 0))
						       (begin 
							 (set! ,STORED_PARENT_ARG ,PARENT_ARG)
							 (set! ,STORED_ORIGIN_ARG ,ORIGIN_ARG)
							 (set! ,STORED_HOPCOUNT_ARG ,HOPCOUNT_ARG)
							 (set! ,STORED_VERSION_ARG ,VERSION_ARG)))))
						;; Otherwise, fizzle
						(void)
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
			  (nodepgm ,toks)))
	 (let ([tainted (findall-emittoks (cdr toks))])
	   (disp "EMITTOKS:" tainted)
	 (let ([processtb (process-tokbind (map car constbinds) toks tainted)])
	   (match toks
	     [(tokens ,[processtb -> newtoks toks] ...)
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
	  (socpgm (bindings ) )
	  (nodepgm (tokens) (startup ) ))))
     (desugar-gradients-lang
      '(program
	(bindings )
	(socpgm (bindings ) )
	(nodepgm (tokens) (startup ) ))) ]

    ["Make sure it gets all the gradient calls out.." 
     (desugar-gradients
      '(cleanup-token-machine-lang
	'(program
	  (bindings )
	  (socpgm (bindings ) (call f))
	  (nodepgm (tokens
		    (f () 
		       (emit g '3))
		    (g (x) (if (< (dist) '3) 
			       (relay)
			       (return (dist) (to h) (via g) (seed '#f) (aggr #f))))
		    (h (v) (dbg '"Got val: %d\\n" v))
		    )
		   (startup ) ))))
     ,(lambda (x) #t) ]
    

))


(define test-this (default-unit-tester
		    "Pass 18d cps-tokmac: use CPS on blocking calls."
		    these-tests))


(define test23 test-this)
(define tests23 these-tests)
(define test-desugar-gradients test-this)
(define tests-desugar-gradients these-tests)
