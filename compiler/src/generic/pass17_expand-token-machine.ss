
;;; [2004.10.22] 

;; This pass expands out some token machine "macros" that I take for granted.

;; For now this means flood & elect-leader

(define expand-token-machine
  (let ()

;; This removes duplicates among the token bindings.
;; <TOOPTIMIZE> Would use a hash-table here for speed.
;; :: TokenBindings -> TokenBindings
(define (remove-duplicate-tokbinds tbinds)
;  (disp "Removing token dups" tbinds)
  (let loop ((ls tbinds))
    (if (null? ls) '()
	(let* ([tok (caar ls)]
	       [dups (filter (lambda (entry) (eq? tok (car entry))) (cdr ls))])
	  (if (null? dups)
	      (cons (car ls) (loop (cdr ls)))	      	      
	      (let* ([all-handlers (cons (car ls) dups)]
		     [formals* (map cadr all-handlers)]
		     [body* (map caddr all-handlers)]
		     )
;		(disp "GOT DUPS " tok formals*)
		(DEBUGMODE
		 (if (not (apply myequal? formals*))
		     (error 'simulator_nought:remove-duplicate-tokbinds
			    "handlers for token ~s don't all take the same arguments: ~s" 
			    tok formals*))
		 )
		(let ([mergedhandler
		       `[,tok ,(car formals*)
			      (begin ,@(randomize-list body*)				      
				     'multiple-bindings-for-token)
;		             (begin ,@body* 'multiple-bindings-for-token)
			     ]])
;		  (disp "MERGED:" mergedhandler)
		(cons mergedhandler
		      (loop (filter (lambda (entry) (not (eq? tok (car entry)))) (cdr ls)))))
		  ))))))	      

    (define process-expr 
      (lambda (env tokens)
	(lambda (stmt)
	  (define-syntax check-tok
	    (syntax-rules ()
	      [(_ call tok) 
	       (if (not (memq tok tokens))
		   (warning 'cleanup-token-machine
			    "~s to unknown token: ~s" call tok))]))
      (match stmt
	     [,const (guard (constant? const))
		     `(quote ,const)]
	     [(quote ,const) `(quote ,const)]
	     [,var (guard (symbol? var))
		   (DEBUGMODE 
		    (cond 
		     [(memq var env) var]
		     [(memq var tokens) 			
			(warning 'cleanup-token-machine
				 "reference to token name: ~s" var)]
		     [(token-machine-primitive? var) var]
		     [else (warning 'cleanup-token-machine
				    "unbound variable reference: ~s" var)]))
		   var]

	     [(begin ,[x]) x]
	     [(begin ,[xs] ...)
	      (make-begin xs)]	      

	     [(if ,[test] ,[conseq] ,[altern])
	      `(if ,test ,conseq ,altern)]
	     [(if ,test ,conseq)
	      ((process-expr env tokens) `(if ,test ,conseq (void)))]

	     [(let* ( (,lhs ,[rhs]) ...) ,bodies ...)
	      `(let*  ([,lhs ,rhs] ...)
		 ,(make-begin bodies))]

	     [(,call-style ,tok ,[args*] ...)
	      (guard (memq call-style '(emit call activate)))
	      (check-tok call-style tok)
	      `(,call-style ,tok ,args* ...)]
	     [(timed-call ,time ,tok ,[args*] ...)
	      (check-tok 'timed-call tok)
	      `(timed-call ,time ,tok ,args* ...)]
	     [(relay) '(relay)]
	     [(relay ,tok) 
	      (check-tok 'relay tok)
	      `(relay ,tok)]

	     [(return ,[expr]            ;; Value
		      (to ,memb)         ;; To
		      (via ,parent)      ;; Via
		      (seed ,[seed_val]) ;; With seed
		      (aggr ,rator_tok)) ;; Aggregator 	      
	      (check-tok 'return-to memb)
	      (check-tok 'return-via parent)
	      (check-tok 'return-aggr rator_tok)
	      `(return ,expr (to ,memb) (via ,parent) 
		       (seed ,seed_val) (aggr ,rator_tok))]

	     [(return ,stuff ...)
	      (error 'cleanup-token-machine
		     "process-expr: bad syntax for return: ~s" `(return ,stuff ...))]

	     [(,prim ,[rands] ...)
	      (guard (or (token-machine-primitive? prim)
			 (basic-primitive? prim)))
	      `(,prim ,rands ...)]

;	     [(,kwd ,stuff ...)
;	      (guard (base-keyword? kwd))
;	      (error 'cleanup-token-machine:process-expr 
;		     "keyword expression not allowed: ~s" stmt)]
	     
	     ;;; TEMPORARY, We allow arbitrary other applications too!
	     [(,rator ,[rands] ...)
	      (warning 'cleanup-token-machine
		       "arbitrary application of rator: ~s" rator)
	      
	      (DEBUGMODE 
	       (if (or (not (symbol? rator)) (not (memq rator env)))
		   (warning 'cleanup-token-machine
			    "unbound rator: ~s" rator)))
	      `(,rator ,rands ...)]
	     [,otherwise
	      (error 'cleanup-token-machine:process-expr 
		     "bad expression: ~s" otherwise)]
	     ))))
    
    (define process-tokbind 
      (lambda (env tokens)
	(lambda (tokbind)
					;	  (disp "process-tokbind" tokbind)
	  (match tokbind
	     [(,tok ,args ,[(process-expr (append args env) tokens) -> expr*] ...)
	      `(,tok ,args ,(make-begin expr*))]
      ))))
	    

    ;; Main body of cleanup-token-machine
    (lambda (prog)
      (match prog
        [(deglobalize-lang '(program (bindings ,constbinds ...)
				(socpgm (bindings ,socbinds ...) 
					,socstmts ...)
				(nodepgm (tokens ,nodetoks ...)
					 (startup ,starttoks ...))))
	 (let ([nodup-binds (remove-duplicate-tokbinds nodetoks)])
	   `(cleanup-token-machine-lang
	     '(program (bindings ,constbinds ...)
		       (socpgm (bindings ,socbinds ...) 
			       ,socstmts ...)
		       (nodepgm (tokens ,(map (process-tokbind (map car constbinds)
							       (map car nodup-binds))
							       nodup-binds) ...)
				(startup ,starttoks ...))))
	   )]
	[,other (error 'cleanup-token-machine "bad input: ~s" prog)]))))