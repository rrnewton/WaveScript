
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
		     [thunks (map (lambda (bod) `(lambda () ,bod)) body*)])
;		(disp "GOT DUPS " tok formals*)
		(DEBUGMODE
		 (if (not (apply myequal? formals*))
		     (error 'simulator_nought:remove-duplicate-tokbinds
			    "handlers for token ~s don't all take the same arguments: ~s" 
			    tok formals*))
		 )
		(let ([mergedhandler
		       `[,tok ,(car formals*)
			      (begin (for-each 
				      (lambda (th) (th))
				      (randomize-list ,(cons 'list thunks)))
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
	(DEBUG ;; Constant binds are simple and should not change!
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
		   (DEBUG 
		    (if (not (memq var env))
			(warning 'cleanup-token-machine
				 "unbound variable: ~s" var)))
		   var]
	     [(,prim ,[rands] ...)
	      (guard (token-machine-primitive? prim))
	      `(,prim ,rands ...)]
	     [(,kwd ,stuff ...)
	      (guard (base-keyword? kwd))
	      (error 'cleanup-token-machine:process-expr 
		     "keyword expression not allowed: ~s" stmt)]
	     
	     ;;; TEMPORARY, We allow arbitrary other applications too!
	     [(,rator ,[rands] ...)
	      (DEBUG 
	       (if (or (not (symbol? rator)) (not (memq rator env)))
		   (warning 'cleanup-token-machine
			    "unbound rator: ~s" var)))
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
	    

    (lambda (prog)
      (match prog
        [(,input-lang '(program (bindings ,nodebinds ...)
				(socpgm (bindings ,socbinds ...) 
					,socstmts ...)
				(nodepgm (tokens ,nodetoks ...) 
					 (startup ,starttoks ...))))
	 (let* ([initenv (map car bindings)]
		[socenv (append (map car socbinds) initenv)]
		[tokenv (map car nodetoks)]
		[nodebinds (map (process-bind initenv) nodebinds)]
		[socbinds  (map (process-bind socenv) socbinds)]
		[socstmts (map (process-expr socenv)


       	 `(,input-lang
	   '(program (bindings ,nodebinds ...)
		     (socpgm (bindings ,socbinds ...) ,(make-begin socstmts))
		     (nodepgm (tokens ,nodetoks ...) 
			      (startup ,starttoks ...))))]))))
	   



'	 (,input-lang '(program (bindings ,[process-binding -> nodebinds] ...)
				(socpgm (bindings ,[process-binding -> socbinds] ...) 
					,[process-expr -> socstmts] ...)
				(nodepgm (tokens ,[process-tokbind -> nodetoks] ...) 
					 (startup ,starttoks ...))))