
;;; NOTE: for the moment I AM ALLOWING USAGE OF ARBITRARY SCHEME FUNCTIONS HERE!



;;; [2004.06.28] Pass: Cleanup Token Machine

;;; This pass:
;;;   (*) Regularizes the syntax of token machines.  Right now what it
;;;   does is include implicit begins,

;;;   (*) Collapses duplicate tokens with the same name into one token.

;;;   (*) In the future it might expand out primitive applications
;;;   that are just shorthands.  (For example, (dist) could become
;;;   (dist <this-token>))



;;; Input and Output language:
;;; <Statement*> is fairly unrestricted.

;;;  <Pgm> ::= (program (bindings <Decl>*) <SOCPgm> <NodePgm>)
;;;  <SOCPgm> ::= <Statement*>
;;;  <NodePgm> ::= (nodepgm <Entry> (bindings <Decl>*) (tokens <TokBinding>*))
;;;  <Entry>  ::= <Token>
;;;  <Decl> ::= (<var> <Exp>)
;;;  <TokBinding> ::= (<Token>  <Code>*)
;;; <TODO> DECIDE ON LOCAL BINDINGS:
;;;  <TokBinding> ::= (<Token> (bindings <Decl>*) <Code>*)

;;;  <Code> ::= <Statement>*
;;;  <Statement>  ::= <BasicStuff?>
;;;                | (emit <Token> <Simple>*)
;;;                | (relay <Token>)
;;;                | (dist <Token>)
;;;                | (return <Token> <Simple>)
;;;                | <Macro> 
;;;  <Macro> ::= (flood <Token>)
;;;            | (elect-leader <Token> [<Token>])  ;; <TODO> optional second argument.. decider
;;;  <Simple> ::= (quote <Lit>) | <Var>

;;;  <Token> ::= <Symbol> | ...???
;;;  <Exp>  ::= ???


;;; DEPENDS: make-begin


(define cleanup-token-machine
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

(define (process-binding bind)
      (let ([newbind 
	     (match bind
		    [(,sym ,[process-expr -> exp])
		     `(,sym ,exp)]
		    [,else (error 'cleanup-token-machine:process-binding 
				  "invalid constant binding: " bind)])])
	(DEBUGMODE ;; Constant binds are simple and should not change!
	 (if (not (equal? bind newbind))
	     (error 'cleanup-token-machine:process-binding 
		    "constant binding should not have changed!~n ~s to ~s."
		    bind newbind)))
	newbind))
	      

    (define process-expr 
      (lambda (env)
	(lambda (stmt)
      (match stmt
	     [(quote ,const) `(quote ,const)]
	     [,var (guard (symbol? var))
		   (DEBUGMODE 
		    (if (not (memq var env))
			(warning 'cleanup-token-machine
				 "unbound variable: ~s" var)))
		   var]
	     [(,prim ,[rands] ...)
	      (guard (token-machine-primitive? prim))
	      `(,prim ,rands ...)]
;	     [(,kwd ,stuff ...)
;	      (guard (base-keyword? kwd))
;	      (error 'cleanup-token-machine:process-expr 
;		     "keyword expression not allowed: ~s" stmt)]
	     
	     ;;; TEMPORARY, We allow arbitrary other applications too!
	     [(,rator ,[rands] ...)
	      (DEBUGMODE 
	       (if (or (not (symbol? rator)) (not (memq rator env)))
		   (warning 'cleanup-token-machine
			    "unbound rator: ~s" rator)))
	      `(,rator ,rands ...)]
	     [,otherwise
	      (error 'cleanup-token-machine:process-expr 
		     "bad expression: ~s" stmt)]
	     ))))
    
    (define process-tokbind 
      (lambda (env)
	(lambda (tokbind)
	  (match tokbind
	     [(,tok ,args ,[process-expr -> expr*] ...)
	      `(,tok ,args (make-begin expr*))]
      ))))
	    

    ;; Main body of cleanup-token-machine
    (lambda (prog)
      (match prog
        [(deglobalize-lang '(program (bindings ,nodebinds ...)
				(socpgm (bindings ,socbinds ...) 
					,socstmts ...)
				(nodepgm (tokens ,nodetoks ...)
					 (startup ,starttoks ...))))
	 (let ([nodup-binds (remove-duplicate-tokbinds nodetoks)])	   
	   `(cleanup-token-machine-lang
	     '(program (bindings ,nodebinds ...)
		       (socpgm (bindings ,socbinds ...) 
			       ,socstmts ...)
		       (nodepgm (tokens ,nodup-binds ...)
				(startup ,starttoks ...))))
	   )]
	[,other (error 'cleanup-token-machine "bad input: ~s" prog)]))))


;	 (let* ([initenv (map car bindings)]
;		[socenv (append (map car socbinds) initenv)]
;		[tokenv (map car nodetoks)]
;		[nodebinds (map (process-bind initenv) nodebinds)]
;		[socbinds  (map (process-bind socenv) socbinds)]
;		[socstmts (map (process-expr socenv)


;       	 `(,input-lang
;	   '(program (bindings ,nodebinds ...)
;		     (socpgm (bindings ,socbinds ...) ,(make-begin socstmts))
;		     (nodepgm (tokens ,nodetoks ...) 
;			      (startup ,starttoks ...))))
	   

(define these-tests
  `(
    
    ["Put an empty test through." 
     (cleanup-token-machine 
      '(deglobalize-lang 
	'(program
	  (bindings )
	  (socpgm (bindings ) )
	  (nodepgm (tokens) (startup ) ))))
     (cleanup-token-machine-lang
      '(program
	(bindings )
	(socpgm (bindings ) )
	(nodepgm (tokens) (startup ) ))) ]


    ["Now collapse two tokens of the same name"
     (cleanup-token-machine 
      '(deglobalize-lang 
	'(program
	  (bindings )
	  (socpgm (bindings ) (emit tok1))
	  (nodepgm
	   (tokens
	    [tok1 () (fun1)]
	    [tok1 () (fun2)])
	   (startup )
	   ))))

     ,(lambda (p)
	(match p 
	  [(cleanup-token-machine-lang
	    '(program (bindings )
		      (socpgm (bindings ) (emit tok1))
		      (nodepgm (tokens [tok1 () (begin ,bodies ...)]) (startup ))))
	   (and (member '(fun1) bodies)
		(member '(fun2) bodies))]))
    ]))


(define test-this (default-unit-tester
		    "Pass 14 cleanup-token-machine: regularize token machine"
		    these-tests))


(define test14 test-this)
(define tests14 these-tests)