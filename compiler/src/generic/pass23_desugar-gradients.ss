;; [2005.02.25]
;; This pass will convert TM's with gradients to TM's without them.
;; It represents one particular (and fragile) algorithm for gradient implementation.

;; It proceeds by adding FOUR additional arguments to every token handler:
;;   gradient parent, gradient origin node, hop-count, gradient version, 

;; NOTE: For now token emission, relaying, distance checking, and returning are all 
;;       restricted to statically specified tokens (but the subtok indices may be 
;;       dynamically computed).
;; This just makes it easier for me to determine which handlers need
;; extra gradient arguments and which don't.  

;; NOTE: Requires another CLEANUP (cleanup-token-machine) after this pass executes.

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
;;;           | call | subcall | timed_call
;;;           | is_scheduled | deschedule | is_present | evict

;;; Output Grammar:

;;; No more GExpr


(define (find-emittoks expr)
  (match expr
	 [(quote ,_) '()]
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
	 [(return ,[expr] (to (tok ,t)) (via (tok ,v)) (seed ,[seed_val]) (aggr (tok ,a)))
	  (append expr seed_val)]

	 [(leds ,what ,which) '()]
	 [(call ,[args*] ...) (apply append args*)]
	 [(timed-call ,[args*] ...) (apply append args*)]
;	 [(activate ,_ ,[args*] ...) (apply append args*)]
	 
	 [(,[rator] ,[rands] ...) (apply append rator rands)]

	 [,otherwise
	  (error 'desugar-gradient:process-expr 
		 "bad expression: ~s" otherwise)]
	 ))


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

    (define (token->tokname t)
      (match t
	[(tok ,t ,e) t]))
	    
    (define process-expr
      (lambda (env tokens this-token)
	(lambda (expr)
	  (match expr
	     [(quote ,const) (values () `(quote ,const))]
             ;; Only for recurring on tokens:
	     [,num (guard (number? ,num)) (values () num)]
	     [,var (guard (symbol? var))  (values () var)]
	     [(set! ,var ,[etb e])        (values etb  `(set! ,var ,e))]
	     [(ext-ref ,[ttb t] ,v)       (values ttb `(ext-ref ,t ,v))]
	     [(ext-set ,[ttb t] ,v ,[e2]) (values ttb `(ext-set ,t ,v ,e2))]
	     ;; If we ever have a first class reference to a token name, it is potentially tainted.
	     ;; This is a conservative estimate:
	     [(tok ,t ,n) (guard (number? n))  (values () `(tok ,t ,n))]
	     [(tok ,t ,[etb e])                (values etb `(tok ,t ,e))]
	     [(begin ,[tb* expr*] ...)         (values (apply append tb*) `(begin ,expr* ...))]
	     [(if ,[ttb test] ,[ctb conseq] ,[atb altern]) 
	      (values (append ttb ctb atb) 
		      `(if ,test ,conseq ,altern))]
	     [(let ([,lhs ,[rtb rhs]]) ,[btb body])
	      (values (append rtb btb)
		      `(let ([,lhs ,rhs]) ,body))]

	     [(emit ,[ttb tok] ,[atb* args*] ...)
	      (values (apply append ttb atb*)
	      `(let-stored ([ver 0])
		(set! ver (+ 1 ver))
		(bcast ,tok (my-id) 1 ver ,@args*)))]

	     ;; TODO: This doesn't cache or pass any arguments on to the relayed tokhand!!!!
	     [(relay (tok ,t ,n)) (guard (number? n))
	      (values ()
	      (if (eq? this-token t)
		  `(bcast (tok ,t ,n) (my-id) ,STORED_ORIGIN_ARG (+ 1 ,STORED_HOPCOUNT_ARG) ,STORED_VERSION_ARG)
		  `(bcast (tok ,t ,n)
			  (my-id)
			  (ext-ref (tok ,t ,n) ,STORED_ORIGIN_ARG)
			  (+ 1 (ext-ref (tok ,t ,n) ,STORED_HOPCOUNT_ARG))
			  (ext-ref (tok ,t ,n) ,STORED_VERSION_ARG))))]
	     [(relay (tok ,t ,[etb e]))
	      (values etb
	      (if (eq? this-token t)
		  `(bcast (tok ,t ,e) (my-id) ,STORED_ORIGIN_ARG (+ 1 ,STORED_HOPCOUNT_ARG) ,STORED_VERSION_ARG)
		  (let ([num (unique-name 'n)])
		    `(let ([,num ,e])
		       `(bcast (tok ,t ,num)
			       (my-id)
			       (ext-ref (tok ,t ,num) ,STORED_ORIGIN_ARG)
			       (+ 1 (ext-ref (tok ,t ,num) ,STORED_HOPCOUNT_ARG))
			       (ext-ref (tok ,t ,num) ,STORED_VERSION_ARG))))))]
	     ;; Uses the current version rather than the stored one if its available.
	     [(dist ,[ttb tok]) 
	      (values ttb
		      (if (eq? (token->tokname tok) this-token)
			  HOPCOUNT_ARG ;; In this case we're inside the handler currently.
			  (ext-ref ,tok STORED_HOPCOUNT_ARG)))]
	     
	     ;; TODO: THIS WILL NEED TO GET MUCH MORE COMPLEX:
	     ;; I'm assuming this chunk of code will get hit with regular frequency.
	     ;; However, we don't *know* that frequency.  Do we have enough info to aggregate?
	     ;; Well, for now we just do the epoch-skewed thing.  Each time we're called we send 
	     ;; up our old results and start aggregating new ones.
	     [(return ,[etb expr]            ;; Value
		      (to (tok ,to ,[ttb toind]))
		      (via (tok ,via ,[vtb viaind]))
		      (seed ,[stb seed_exp])
		      (aggr (tok ,aggr 0)))
	      (let ([aggr_ID (unique-name 'aggr_ID)]
		    [return-handler (unique-name 'return-handler)])
	      (values 
	       ;; First return a new handler for the aggregation object corresponding to this particular return statment.
	       ;; When called locally, this sends the aggregate to the parent, and resets the acc.
	       ;; When called remotely, this builds up the aggregation accumulator.
	       (cons `[,return-handler retid (flag val toind viaind)
		        (let-stored ([acc ,seed_val])
 		     ;; Must be initialized with the seed value before aggregation begins.
;		     (if (= flag ,INIT)
;			 (set! acc val)
		     (if (= flag ,LOCAL) ;; This is the sign to send to upward:
			 (let ([oldacc acc])
			   (set! acc ,seed)
			   ;; Now, if we're the destination we need to call the 'to' token.
			   (if (= (my-id) (ext-ref (tok ,via viaind) ,STORED_G_ORIGIN))
			       (call (tok ,to toind) (subcall (tok ,aggr 0) val oldacc))
			       ;; While the subcall is hapenning this return_handler very well may be called again.
			       ;; But we just reset the acc, so any calls from this moment on will be in the next epoch.
			       (send_to (ext-ref (tok ,via viaind) ,STORED_G_PARENT)
					(tok return-handler_395 retid)
					(subcall (tok ,aggr 0) val oldacc))))
			 ;; Otherwise we simply accumulate and wait.
			 (set! acc (subcall (tok ,aggr 0) val acc))))]
		     (append etb ttb vtb stb))		     

	       ;; Second, return the generated code for the return statement.
	       ;; Each aggregation is unique based on its to, via, and aggr arguments.
		 `(let ([,aggr_ID (+ (* ,MAX_SUBTOK ,toind) ,viaind)])
		    ;; Just call off to the appropriate local aggregator:
		    (call (tok ,return-handler ,aggr_ID) expr))))]

	     	   
	     [(call ,[ttb tok] ,[atb* args*] ...)                    
	      (values (apply append ttb atb*)
		      `(call ,tok ,args* ...))]
	     [(timed-call ,[ttb time] ,[ttb2 tok] ,[atb* args*] ...) 
	      (values (apply append ttb ttb2 atb*)
		      `(timed-call ,time ,tok ,args* ...))]
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
	     ))))

    (define process-tokbind 
	(lambda (env tokens)
	  (lambda (tokbind)
	    (mvlet ([(tok id args stored bindings body) (destructure-tokbind tokbind)])
	      (mvlet ([(newtoks newbod) ((process-expr env tokens tok) body)])
		     (values newtoks
		     `(,tok ,id (,PARENT_ARG ,ORIGIN_ARG ,HOPCOUNT_ARG ,VERSION_ARG ,args ...)
			    (let-stored ([STORED_PARENT_ARG '#f]
					 [STORED_ORIGIN_ARG '#f]
					 [STORED_HOPCOUNT_ARG '#f]
					 [STORED_VERSION_ARG '#f])
			      newbod))))))))

    ;; Main body of desugar-gradient
    (lambda (prog)
      (match prog
	[(,lang '(program (bindings ,constbinds ...) 
			  (nodepgm ,toks)))	 
	 (let ([processtb (process-tokbind (map car constbinds) toks)])
	   (match toks
	     [(tokens ,[processtb -> newtoks toks] ...)
	      `(desugar-gradient-lang
		'(program (bindings ,constbinds ...)
			  (nodepgm (tokens ,@(append toks newtoks)))))]))]))
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
