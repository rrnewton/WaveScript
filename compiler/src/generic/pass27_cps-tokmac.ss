;; [2005.02.10]

;; cps-tokmac
;; This pass does the CPS transformation for the token machine.

;; INIT message = 0
;; CALL message = 1

(define cps-tokmac
  (let ()
    (define INIT 0)
    (define CALL 0)

    ;; Process an expression in effect context.
    '(define process-effect
      (lambda (env tokens this-token)
	(lambda (stmt)
	  (match expr
	     [,const (guard (constant? const))
		     `(quote ,const)]
	     [(quote ,const) `(quote ,const)]
	     [,var (guard (symbol? var)) var]
	     [(begin ,[(process-effect env tokens this-token) -> x] ,[y]) x]
	     [(if ,[(process-value env tokens this-token) -> test]
		  ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
	     ;; IF VALUES ARE NOT USED THEN DON'T LABEL WORRY ABOUT THEM.
	     [(let* ( (,lhs ,[rhs]) ...) ,[bodies] ...)
	      `(let*  ([,lhs ,rhs] ...)	,(make-begin bodies))]

	     ;; HERE NEED TO DO CPS:
	     [(,call-style ,tok ,[args*] ...)
	      (guard (memq call-style '(emit call activate)))
	      `(,call-style ,tok ,args* ...)]
	     [(timed-call ,time ,tok ,[args*] ...)
	      `(timed-call ,time ,tok ,args* ...)]

	     [(relay) '(relay)]
	     [(relay ,tok) `(relay ,tok)]
	     [(dist ,tok) `(dist ,tok)]	     
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

    (define (subst-freevars e)
      (let ([fvs (free-vars e)])
	
	
      
    
    ;; Process an expression in value context.
    (define process-value 
      (lambda (env tokens this-token)
	(lambda (expr pvk)
	  (match expr
	     [,const (guard (constant? const)) `(quote ,const)]
	     [(quote ,const) `(quote ,const)]
	     [,var (guard (symbol? var)) var]

	     [(begin ,[(process-effect env tokens this-token) -> x] ,[y]) x]
	     [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
	     [(let* ( (,lhs ,[rhs]) ...) ,[bodies] ...)
	      `(let*  ([,lhs ,rhs] ...)	,(make-begin bodies))]

	     
	     [(subcall ,tok ,[args*] ...)
	      (let ((k (new-cont-name)))
		(values 
		 ;; Return expression:
		 `(begin 
		    (call ,k ,INIT ???)
		    (call ,tok ,k ,args* ...))
		 ;; Tainted tokens:
		 (list tok)
		 ;; New token handler, 
		 (list 
		  `(,k (flag vec) 
		       ,(subst-freevars (pvk `(void)))))))]
	     

	     [(,call-style ,tok ,[args*] ...)
	      (guard (memq call-style '(emit call activate)))
	      `(,call-style ,tok ,args* ...)]

	     [(timed-call ,time ,tok ,[args*] ...)
	      `(timed-call ,time ,tok ,args* ...)]

	     [(relay) '(relay)]
	     [(relay ,tok) `(relay ,tok)]
	     [(dist ,tok) `(dist ,tok)]	     
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
;		    `(,tok ,args ,((process-value env tokens tok) expr))
		    `(,tok ,args ,expr)
		    ]
		   ))))


    (lambda (prog)
      (match prog
	[(,lang '(program (bindings ,constbinds ...)
			  (socpgm (bindings ,socbinds ...)
				  ,socstmts ...)
			  (nodepgm (tokens ,nodetoks ...)
				   (startup ,starttoks ...))))

'	 (let ([newtoks ;newstmts)
		(let ([processs (process-effect (map car socbinds) (map car nodetoks) #f)])
		  (foldl (lambda (stmt)
;			   (mvlet ([(newstmt newtok) (process stmt)])
			   0000
			   )))])
	   000 ;; TODO TODO TODO
	)


	 prog


]))))



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
		    "Pass cps-tokmac: use CPS on blocking calls."
		    these-tests))


(define test27 test-this)
(define tests27 these-tests)
(define test-cps-tokmac test-this)
(define tests-cps-tokmac these-tests)
