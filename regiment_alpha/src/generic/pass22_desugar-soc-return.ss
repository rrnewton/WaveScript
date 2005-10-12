
;; [2005.04.20]
;; Soc-return's are a strange beast.

;; [2005.10.02] I was just doing this in cleanup-tokmac, but I'm going to move it here.


(define desugar-soc-return
  (let ()

  (define (process-expr expr)
       (tml-generic-traverse
	;; Driver:
	(lambda (x k)
	  (match x
	     ;; For now this is just syntactic sugar for routing on the global tree:   
	     ;; return-retry indi
	     [(soc-return ,[k -> x])
;	      (loop `(return-retry ,x (to (tok SOC-return-handler 0)) (via (tok global-tree 0))))]
	      (let ([socretval (unique-name 'socretval)])
		`(let ([,socretval ,x])
		   (if (= (my-id) ',BASE_ID)
		       (begin 
			 ,@(DEBUGMODE `(dbg '"Soc return on basenode, returning directly: %d" ,socretval))
			 (call (tok SOC-return-handler 0) ,socretval))
		       (greturn ,socretval 
				(to (tok SOC-return-handler 0)) 
				(via (tok global-tree 0))
				(seed '#f)
				(aggr #f)
				))))]
	     ;; Sending to subtok 1 indicates that we're finished.
;	     [(soc-return-finished ,x)
;	      (loop `(return ,x (to (tok SOC-return-handler 1)) (via (tok global-tree 0))))]
	     
	     [,other (k other)]))
	;; Fuser:
	(lambda (ls k) (apply k ls))
	;; Expression:
	expr))

  (define (process-tokbind tb)
    (mvlet ([(tok id args stored constbinds body) (destructure-tokbind tb)])
       `[,tok ,id ,args (stored ,@stored) 
	      ,(process-expr body)]))

  (lambda (prog)
    (match prog
      [(,lang '(program (bindings ,constbinds ...) 
			(nodepgm (tokens ,[process-tokbind -> toks] ...))))
      `(,lang '(program (bindings ,constbinds ...)
			(nodepgm (tokens ,toks ...))))]))))




(define these-tests
  `(
    ,@(let ((randomprog
	     '(cleanup-token-machine-lang
	      '(program
		(bindings)
		(nodepgm
		 (tokens
		  (node-start subtok_ind () (stored) (void))
		  (SOC-start subtok_ind () (stored)
		   (begin
		     (printf '"~a " (token-scheduled? (tok tok1 0)))
		     (timed-call 500 (tok tok1 0))
		     (timed-call 100 (tok check 0))
		     (timed-call 800 (tok check 0))))
		  (tok1 subtok_ind () (stored) (printf '"tok1 "))
		  (check subtok_ind () (stored)
		   (printf '"~a " (token-scheduled? (tok tok1 0))))))))))
       
      `(
	["Just make sure we get the same thing back for a prog without soc-return:"
	 (desugar-soc-return ',randomprog)
	 ,randomprog]
	))   

    ))

(define test-this (default-unit-tester
		    "22: Desugar-Soc-Return: convert return-to-base statements to use gradients"
		    these-tests))

(define test22 test-this)
(define tests22 these-tests)

