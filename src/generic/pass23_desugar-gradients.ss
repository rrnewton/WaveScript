;; [2005.02.25]
;; This pass will convert TM's with gradients to TM's without them.
;; It represents one particular (and fragile) algorithm for gradient implementation.

;; It proceeds by adding three arguments to every token handler:
;;   gradient origin node, hop-count, gradient version, 

;; NOTE: For now token emission, relaying, distance checking, and returning are all 
;;       restricted to statically specified tokens (but the subtok indices may be 
;;       dynamically computed).

;; This just makes it easier for me to determine which handlers need
;; extra gradient arguments and which don't.  

;; Input grammar:

;;;  <Pgm> ::= (program (bindings <Cbind>*) <NodePgm>)
;;;  <NodePgm> ::= (nodepgm (tokens <TokBinding>*))
;       NOTE: tokens will inclide a binding for SOC-start and node-start
;;;  <Cbind> ::= (<var> <Exp>)
;       NOTE: This expressions will be statically calculable -- constants.
;;;  <TokBinding> ::= (<TokName> <SubtokId> (<Var> ...) (bindings <Cbind>*) (stored <Stored>) <Expr>)
;;;  <TokName>   ::= <Symbol> 
;;;  <SubtokId>  ::= <Symbol>
;;;  <PlainTok>  ::= (tok <TokName>)
;;;  <Token>     ::= <PlainTok> | (tok <Tokname> <Int>)
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
	 [(ext-ref (tok ,t ,[e]) ,v) e]
	 [(ext-set (tok ,t ,[e]) ,v ,e2) (append e e2)]
	 ;; If we ever have a first class reference to a token name, it is potentially tainted.
	 ;; This is a conservative estimate:
	 [(tok ,t ,e) (cons t e)]
	 [(begin ,[exprs] ...) (apply append exprs)]
	 [(if ,[exprs] ...) (apply append exprs)]
	 [(let ( (,_ ,[rhs]) ...) ,[body])	(apply append body rhs)]

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
	    
    ;; Process an expression in value context.
    (define process-expr
      (lambda (env tokens this-token)
	(lambda (expr)
	  (match expr
	     [(quote ,const) `(quote ,const)]
	     [,var (guard (symbol? var)) var]

	     [(begin ,[exprs] ...) `(begin ,exprs ...)]
	     [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
	     [(let* ( (,lhs ,[rhs]) ...) ,[body])
	      `(let*  ([,lhs ,rhs] ...)	,body)]

	     [(emit ,tok ,[args*] ...)
	      `(let-stored ([ver 0])
		(set! ver (+ 1 ver))
		(bcast ,tok (my-id) 1 ver))]

	     [(relay) ;((process-expr env tokens this-token) `(relay ,this-token))]	      
	      `(bcast ,this-token stored_g_origin (+ 1 stored_g_hopcount) stored_g_version)]
	     [(relay ,tok)
	      ;`(relay ,tok)
	      (if (eq? tok this-token)
		  ((process-expr env tokens this-token) `(relay))
		  (error 'desugar-gradients:process-expr
			 "Can't handle relaying *other* tokens yet: ~a" `(relay ,tok)))]

	     ;; Should this be the stored version, or the current version if its available??
	     ;; Should all args be lifted?  Then we can tell if we even have a newer version.
	     [(dist) 'stored_g_hopcount]
	     [(dist ,tok) 
	      (if (eq? tok this-token)
		  ((process-expr env tokens this-token) `(dist))
		  (error 'desugar-gradients:process-expr
			 "Can't handle dist from *other* tokens yet: ~a" `(dist)))]

	     [(return ,[expr]            ;; Value
		      (to ,memb)         ;; To
		      (via ,parent)      ;; Via
		      (seed ,[seed_val]) ;; With seed
		      (aggr ,aggr)) ;; Aggregator 	      
	      `(return ,expr 
		       (to ,memb) 
		       (via ,parent) 
		       (seed ,seed_val) 
		       (aggr ,aggr))]

	     	   
	     [(,call-style ,tok ,[args*] ...)
	      (guard (memq call-style '(emit call activate)))
	      `(,call-style ,tok ,args* ...)]

	     [(timed-call ,time ,tok ,[args*] ...)
	      `(timed-call ,time ,tok ,args* ...)]

	     [(leds ,what ,which) `(leds ,what ,which)]

	     [(,prim ,[rands] ...)
	      (guard (or (token-machine-primitive? prim)
			 (basic-primitive? prim)))
	      `(,prim ,rands ...)]
	     ;;; TEMPORARY, We allow arbitrary other applications too!
	     [(,rator ,[rands] ...)
	      (warning 'desugar-gradient
		       "arbitrary application of rator: ~s" rator)
	      `(,rator ,rands ...)]

	     [,otherwise
	      (error 'desugar-gradient:process-expr 
		     "bad expression: ~s" otherwise)]
	     ))))

    (define process-tokbind 
	(lambda (env tokens)
	  (lambda (tokbind)
	    (match tokbind
		   [(,tok ,args ,expr)
		    `(,tok (g_origin g_hopcount g_version ,args ...)
			   (let-stored ([g_origin #f]
					[g_hopcount #f]
					[g_version #f])
				       ,((process-expr env tokens tok) expr)))
		    ]
		   ))))


    ;; Main body of desugar-gradient
    (lambda (prog)
      (match prog
	[(,lang '(program (bindings ,constbinds ...)
			  (socpgm (bindings ,socbinds ...) 
				  ,socstmts ...)
			  (nodepgm (tokens ,nodetoks ...)
				   (startup ,starttoks ...))))
	 

	 (let* ([toks (map car nodetoks)]
		[newsocstmts
		 (map (process-expr (append (map car constbinds) (map car socbinds)) toks #f)
		      socstmts)]
		[newtoks
		 (map (process-tokbind (map car constbinds) toks)
		      nodetoks)])
	   
	   `(desugar-gradients-lang
	     '(program (bindings ,constbinds ...)
		       (socpgm (bindings ,socbinds ...) 
			       ,newsocstmts ...)
		       (nodepgm (tokens ,newtoks ...)
				(startup ,starttoks ...)))))]))))


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
