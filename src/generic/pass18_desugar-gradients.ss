;; [2005.02.25]
;; This pass will convert TM's with gradients to TM's without them.
;; It represents one particular (and fragile) algorithm for gradient implementation.

;; It proceeds by adding three arguments to every token handler:
;;   gradient origin node, hop-count, gradient version, 


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
	      (bcast ,this-token stored_g_origin (+ 1 stored_g_hopcount) stored_g_version)]
	     [(relay ,tok)
	      ;`(relay ,tok)
	      (if (eq? tok this-token)
		  ((process-expr env tokens this-token) `(relay))
		  (error 'desugar-gradients:process-expr
			 "Can't handle relaying *other* tokens yet: ~a" `(relay ,tok)))]

	     ;; Should this be the stored version, or the current version if its available??
	     ;; Should all args be lifted?  Then we can tell if we even have a newer version.
	     [(dist) (stored g_hopcount)]
	     [(dist ,tok) 
	      (if (eq? tok this-token)
		  ((process-expr env tokens this-token) `(dist))
		  (error 'desugar-gradients:process-expr
			 "Can't handle dist from *other* tokens yet: ~a" `(dist)))]

	     [(return ,[expr]            ;; Value
		      (to ,memb)         ;; To
		      (via ,parent)      ;; Via
		      (seed ,[seed_vals] ...) ;; With seed
		      (aggr ,rator_toks ...)) ;; Aggregator 	      
	      `(return ,expr 
		       (to ,memb) 
		       (via ,parent) 
		       (seed ,seed) 
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
	      (warning 'cleanup-token-machine
		       "arbitrary application of rator: ~s" rator)
	      `(,rator ,rands ...)]	     

	     [,otherwise
	      (error 'cleanup-token-machine:process-expr 
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


    ;; Main body of cleanup-token-machine
    (lambda (prog)
      (match prog
	[(,lang '(program (bindings ,constbinds ...)
			  (socpgm (bindings ,socbinds ...) 
				  ,socstmts ...)
			  (nodepgm (tokens ,nodetoks ...)
				   (startup ,starttoks ...))))
	 (let ([newtoks ;newstmts)
		(let ([processs (process-effect (map car socbinds) (map car nodetoks) #f)])
		  (foldl (lambda (stmt)
;			   (mvlet ([(newstmt newtok) (process stmt)])
			   0000
			   )))])
	   000 ;; TODO TODO TODO
	)]))))



(define these-tests
  `(
    
    ["Put an empty test through." 
     (cps-tokmac
      '(cleanup-token-machine-lang
	'(program
	  (bindings )
	  (socpgm (bindings ) )
	  (nodepgm (tokens) (startup ) ))))
     (cleanup-token-machine-lang
      '(program
	(bindings )
	(socpgm (bindings ) )
	(nodepgm (tokens) (startup ) ))) ]

))


(define test-this (default-unit-tester
		    "Pass 18d cps-tokmac: use CPS on blocking calls."
		    these-tests))


(define test18d test-this)
(define tests18d these-tests)
