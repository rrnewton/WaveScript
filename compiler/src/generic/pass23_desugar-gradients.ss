;; [2005.02.25]
;; This pass will convert TM's with gradients to TM's without them.
;; It represents one particular (and fragile) algorithm for gradient implementation.

;; It proceeds by adding three arguments to every token handler:
;;   gradient origin node, hop-count, gradient version, 

    (define (find-emittoks expr)
       (match expr
 	     [(quote ,_) '()]
 	     [,var (guard (symbol? var)) '()]
 	     [(begin ,[exprs] ...) (apply append exprs)]
 	     [(if ,[exprs] ...) (apply append exprs)]
 	     [(let* ( (,_ ,[rhs]) ...) ,[body])	(apply append body rhs)]
 	     [(emit ,tok ,[args*] ...)	(cons tok (apply append args*))]

 	     [(relay ,_ ...) '()]
 	     [(dist ,_ ...) '()]
 	     [(leds ,what ,which) '()]

 	     [(return ,[expr] (to ,t) (via ,v) (seed ,[seed_val]) (aggr ,a))
 	      (append expr seed_val)]
	     	   
	     [(call ,_ ,[args*] ...) (apply append args*)]
 	     [(activate ,_ ,[args*] ...) (apply append args*)]

 	     [(timed-call ,_ ,__ ,[args*] ...) (apply append args*)]
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
