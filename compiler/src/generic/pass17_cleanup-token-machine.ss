
;;; NOTE: for the moment I AM ALLOWING USAGE OF ARBITRARY SCHEME FUNCTIONS HERE!

;;; [2004.06.28] Pass: Cleanup Token Machine
;===============================================================================

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
;;;          <TODO> DECIDE ON LOCAL BINDINGS:
;;;  <TokBinding> ::= (<Token> (bindings <Decl>*) <Code>*)

;;;  <Code> ::= <Statement>*
;;;  <Statement>  ::= <BasicStuff?>
;;;                | (emit <Token> <Simple>*)
;;;                | (relay <Token>)
;;;                | (dist <Token>)
;;;                | (return <Token> <Simple>)
;;;                | (leds <Red|Yellow|Green> <On|Off|Toggle>)
;;;                | <Macro> 
;;;  <Macro> ::= (flood <Token>)
;;;            | (elect-leader <Token> [<Token>])  ;; <TODO> optional second argument.. decider
;;;  <Simple> ::= (quote <Lit>) | <Var>

;;;  <Token> ::= <Symbol> | ...???
;;;  <Exp>  ::= ???

;; NOTE: introduces (void) primitive.

;;; DEPENDS: make-begin

;===============================================================================

;;; [2004.10.22]  Now this also will expand out flood/elect-leader.
;;; This pass is starting to do way too much work...

(define cleanup-token-machine
  (let ()

    (define make-begin
      (lambda (expr*)
	(match (match `(begin ,@expr*)
		      [(begin ,[expr*] ...) (apply append expr*)]
		      [,expr (list expr)])
	       [(,x) x]
	       [(,x ,x* ...) `(begin ,x ,x* ...)])))
    
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

	     [(leds ,what ,which) `(leds ,what ,which)]

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
	    
    (define decode 
      (lambda (stuff)
	(let ([bindings '()]
	      [socbindings '()]
	      [socpgm '()]
	      [nodetoks '()]
	      [node-startup '()])
	  (let loop ((ls stuff))
	    (if (null? ls)
		(let ((result `(deglobalize-lang
				'(program (bindings ,@bindings)
					  (socpgm (bindings ,@socbindings) ,@socpgm)
					  (nodepgm (tokens ,@nodetoks)
						   (startup ,@node-startup))))))
		  (printf "cleanup-token-machine: Desugaring to: ~n")
		  (pp result)
		  result)
		(begin 
		  (match (car ls)
			 [(bindings ,x ...) (set! bindings x)]
			 [(socbinds ,x ...) (set! socbindings x)]
			 [(socpgm (bindings ,b ...) ,x ...)  
			  (set! socbindings b)
			  (set! socpgm x)]
			 [(socpgm ,x ...)   (set! socpgm x)]			 
			 [(nodepgm (tokens ,x ...) (startup ,s ...))
			  (set! nodetoks x)
			  (set! node-startup s)]
			 [(nodetoks ,x ...) (set! nodetoks (append x nodetoks))]
			 [(tokens ,x ...) (set! nodetoks (append x nodetoks))]
			 [(startup ,x ...) (set! node-startup x)]
			 [,other (error 
				  'cleanup-token-machine:decode
				  "this isn't part of a valid token machine program! ~n ~a"
				  other)])
		  (loop (cdr ls))))))))

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
	;; Cleanup-token-machine is a real lenient pass.  
	;; It will take the token machines in other forms, such as
	;; without the language annotation:
	[(program (bindings ,constbinds ...)
		  (socpgm (bindings ,socbinds ...) 
			  ,socstmts ...)
		  (nodepgm (tokens ,nodetoks ...)
			   (startup ,starttoks ...)))
	 (cleanup-token-machine `(deglobalize-lang ',prog))]

	['(program ,stuff ...) (cleanup-token-machine (decode stuff))]
	[(program ,stuff ...) (cleanup-token-machine (decode stuff))]
	[,stuff (cleanup-token-machine (decode (list stuff)))]
	))))


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
		    "Pass 17 cleanup-token-machine: regularize token machine"
		    these-tests))


(define test17 test-this)
(define tests17 these-tests)
