
;;; NOTE: for the moment I AM ALLOWING USAGE OF ARBITRARY SCHEME FUNCTIONS HERE!

;;; [2004.06.28] Pass: Cleanup Token Machine
;===============================================================================

;; [2005.03.25] I'm making this pass pretty permissive so that I can
;; use it from multiple places.  It's permissive on its input grammar,
;; but strict with its output grammar.


;;; This pass:
;;;   (*) Regularizes the syntax of token machines.  Right now what it
;;;   does is include implicit begins,

;;;   (*) Collapses duplicate tokens with the same name into one token.

;;;   (*) Lift's the "socpgm" into a token handler called "SOC-start"
;;;   The grammar of the output has no socpgm.  We also insure that
;;;   the output has some SOC-start and node-start handler, even if
;;;   they are empty.

;;    (*) Throws away the "SOC bindings".  These become just the
;;    handler-local bindings for "SOC-start".
;; NOTE: [2005.03.25] Getting rid of node-local constant binds.
;; Whatever, let them all be global.


;;;   (*) It expands out some primitive applications
;;;   that are just shorthands.  (For example, (dist) becomes
;;;   (dist <this-token>)    ;;; TODO: not finished...
;;;   It also expands some syntaxes (and, or).

;;;   (*) Should be IDEMPOTENT.  
;;;   Can run it multiple times or inbetween different passes.

;;;   (*) RENAME-VARS SHOULD RUN BEFORE THIS PASS.  It depends on
;;;   merging token handlers without collisions between stored vars
;;;   and constant bindings.





;;; INPUT GRAMMAR:

;; Extremely messy, ad-hoc and lenient.


;;; OUTPUT GRAMMAR:

;;;  <Pgm> ::= (program (bindings <Cbind>*) <NodePgm>)
;;;  <NodePgm> ::= (nodepgm (tokens <TokBinding>*))
;       NOTE: tokens will inclide a binding for SOC-start and node-start
;;;  <Cbind> ::= (<var> <Exp>)
;       NOTE: These expressions will be statically calculable -- constants.
;;;  <TokBinding> ::= (<TokName> <SubtokId> (<Var> ...) (bindings <Cbind>*) (stored <Stored>) <Expr>)
;;;  <TokName>   ::= <Symbol> 
;;;  <SubtokId>  ::= <Integer>
;;;  <Token>     ::= (tok <Tokname> <Int>)
;;;  <DynToken>  ::= <Token>    | (tok <Tokname> <Expr>)
;;;     NOTE: Either the whole token reference or just the sub-index can be dynamic.
;;;  <Expr>      ::= (quote <Constant>)
;;;                | <Var>
;;;                | <DynToken>
;;;                | (set! <Var> <Expr>)
;;;                | (ext-ref <DynToken> <Var>)
;;;                | (ext-set! <DynToken> <Var> <Expr>)
;       NOTE: These are static token refs for now.
;;;                | (begin <Expr> ...)
;;;                | (let ((<Symbol> <Expr>)) <Expr>)
;;;                | (if <Expr> <Expr> <Expr>)
;;;                | (subcall <DynToken> <Expr>...)
;;;                | (<Prim> <Expr> ...)
;;;                | (<Expr> ...)
;;;                | (leds <Red|Yellow|Green> <On|Off|Toggle>)
;;;                | <GExpr>
;;;                | <Sugar> 
;;;  <GExpr>     ::= (emit <DynToken> <Expr> ...)
;;;                | (return <Expr> (to <DynToken>) (via <DynToken>) (seed <Expr>) (aggr <Token>))
;;;                | (relay <DynToken>) ;; NEED TO ADD RELAY ARGS!
;;;                | (dist <DynToken>)
;;;  <Sugar>     ::= (flood <Expr>)
;;;                | (elect-leader <Token> [<Token>])
                     ;; <TODO> optional second argument.. decider
;;;  <Prim> ::= <BasicPrim> 
;;;           | call | subcall | timed-call | bcast
;;;           | is_scheduled | deschedule | is_present | evict



;===============================================================================

;;; [2004.10.22]  Now this also will expand out flood/elect-leader.
;;; This pass is starting to do way too much work...

;; [2005.03.28]  Moving destructure-tokbind to helpers.ss

(define cleanup-token-machine
  (let ()

    ;; Uses constants DEFAULT_SUBTOK and DEFAULT_SUBTOK_VAR

    (define (handler->formals tb)
      (mvlet ([(tok id args stored bindings body) (destructure-tokbind tb)])
	     args))

    (define (handler->body tb)
      (mvlet ([(tok id args stored bindings body) (destructure-tokbind tb)])
	     body))

    ;; [2005.03.27] Adding a somewhat unsafe hack (assumes all variables
    ;; renamed) that makes this do the right thing when it's given a begin
    ;; expression as opposed to a list.
    (define make-begin
      (lambda (expr*)
	(match (match `(begin ,@expr*)
		      ;; This means we messed up:
		      [(begin begin ,[expr*] ...) (apply append expr*)]
		      [(begin ,[expr*] ...) (apply append expr*)]
		      [,expr (list expr)])
	       [() (void)]
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
			 [formals* (map handler->formals all-handlers)]
			 [body* (map handler->body all-handlers)]
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
				  ,(make-begin (snoc ''multiple-bindings-for-token
						     (randomize-list body*)))]
;		             (begin ,@body* 'multiple-bindings-for-token)
			     ])
;		  (disp "MERGED:" mergedhandler)
		      (cons mergedhandler
			    (loop (filter (lambda (entry) (not (eq? tok (car entry)))) (cdr ls)))))
		    ))))))

    (define process-expr 
      ;(trace-lambda cleanuptokmac:procexp 
      (lambda (env tokens this-token this-subtok)
	(lambda (stmt)
	  (define-syntax check-tok
	    (syntax-rules ()
	      [(_ call tok)
	       (let ((name (match tok
				  [,s (guard (symbol? tok)) s]
				  [(tok ,tok) tok]
				  [(tok ,tok ,_.) tok])))
		 (if (not (memq name tokens))
		     (if (regiment-verbose)
			 (warning 'cleanup-token-machine
				  "~s to unknown token: ~s" call tok))))]))
	  (define (tokname? t) (memq t tokens))
	  (match stmt
	     [,const (guard (constant? const))
		     `(quote ,const)]
	     [(quote ,const) `(quote ,const)]

	     [,t (guard (symbol? t) (memq t tokens))
		 `(tok ,t ,DEFAULT_SUBTOK)]
	     [(tok ,t) `(tok ,t ,DEFAULT_SUBTOK)]
	     ;; Static form
	     [(tok ,t ,n) (guard (number? n)) `(tok ,t ,n)]
	     ;; Dynamic form
	     [(tok ,t ,[e]) `(tok ,t ,e)]

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

	     [(and) ''#t]
	     [(and ,[a]) a]
	     [(and ,[a] ,b ...)
	      `(if ,a ,((process-expr env tokens this-token this-subtok)
			`(and ,b ...))
		   '#f)]
	     [(or) ''#f]
	     [(or ,[a]) a]
	     [(or ,[a] ,b ...)
	      `(if ,a '#t
		   ,((process-expr env tokens this-token this-subtok)
			`(and ,b ...)))]

	     [(if ,[test] ,[conseq] ,[altern])
	      `(if ,test ,conseq ,altern)]
	     [(if ,test ,conseq)
	      ((process-expr env tokens this-token this-subtok) 
	       `(if ,test ,conseq (void)))]

	     [(let ([,lhs ,[rhs]]) ,bodies ...)
	      `(let ([,lhs ,rhs])
		 (make-begin (map (process-expr (cons lhs env) tokens this-token this-subtok) bodies)))]

	     ;; Here we have letrec style binding.  Probably shouldn't.
	     [(let-stored ([,lhs* ,rhs*] ...) ,bodies ...)
	      (let ([newenv (append lhs* env)])
		(let ([loop (process-expr newenv tokens this-token this-subtok)])
		  `(let-stored ([,lhs* ,(map loop rhs*)] ...)
			       @(make-begin (map loop bodies)))))]

	     ;; TODO: expand away let*
	     [(let* () ,[bodies] ...)  (make-begin bodies)]
	     [(let* ( [,l1 ,r1] ,rest ...) ,bodies ...)
	      ((process-expr env tokens this-token this-subtok)
	       `(let ([,l1 ,r1])
		  (let* ,rest ,bodies ...)))]

	     [(,call-style ,tok ,[args*] ...)
	      (guard (memq call-style '(emit call activate bcast))) ;; Activate should be desugared at some point!
	      (check-tok call-style tok)	     
	      `(,call-style ,(if (tokname? tok)
				 `(tok ,tok ,DEFAULT_SUBTOK)
;				 `(tok ,tok)
				 tok)
			    ,args* ...)]
	     [(timed-call ,time ,tok ,[args*] ...)
	      (check-tok 'timed-call tok)
	      `(timed-call ,time ,(if (tokname? tok)
				      `(tok ,tok ,DEFAULT_SUBTOK)
				      tok)
			   ,args* ...)]
	     [(relay) `(relay (tok ,this-token ,this-subtok))]
	     [(relay ,tok) (guard (tokname? tok))
	      (check-tok 'relay tok)
	      ;; There is some question here as to whether we should
	      ;; default to this-subtok or to Zero subtoken index.
	      `(relay (tok ,tok ,this-subtok))]
	     [(relay (tok ,t ,[e])) (check-tok 'relay t)
	      `(relay (tok ,t ,e))]
	     
	     ;; Expand this out to refer to the precise token...
	     [(dist) `(dist ,this-token)]
	     
	     [(return ,[expr]            ;; Value
		      (to ,memb)         ;; To
		      (via ,parent)      ;; Via
		      (seed ,[seed_vals] ...) ;; With seed
		      (aggr ,rator_toks ...)) ;; Aggregator 	      
	      (check-tok 'return-to memb)
	      (check-tok 'return-via parent)

	      (let ((seed (if (null? seed_vals) #f
			      (car seed_vals)))
		    (aggr (if (null? rator_toks) #f
			      (car rator_toks))))
		(if aggr (check-tok 'return-aggr aggr))
		`(return ,expr 
			 (to ,memb) 
			 (via ,parent) 
			 (seed ,seed) 
			 (aggr ,aggr)))]

	     [(return ,thing ,stuff ...)
	      (if (or (not (andmap pair? stuff))
		      (not (set? (map car stuff)))
		      (not (subset? (map car stuff)
				    '(to via seed aggr))))
		  (error 'cleanup-token-machine
			 "process-expr: bad syntax for return: ~s" `(return ,stuff ...)))
	      ((process-expr env tokens this-token this-subtok)
	       `(return ,thing 
			,(assq 'to stuff)
			,(if (assq 'via stuff)
			     (assq 'via stuff)
			     `(via ,this-token))
			,(if (assq 'seed stuff)
			     (assq 'seed stuff)
			     '(seed '#f))
			,(if (assq 'aggr stuff)
			     (assq 'aggr stuff)
			     '(aggr #f))))]

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
	     [(,[rator] ,[rands] ...)
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
	  (mvlet ([(tok id args stored bindings body) (destructure-tokbind tokbind)])
		 (disp "PROCESSING TOKBIND" args tokbind)

		 `(,tok ,id ,args (stored ,@stored) ;(bindings ,@bindings)
			,((process-expr (append args stored bindings env) tokens tok id) body))))))
	    
    (define decode 
      (lambda (stuff)
	(let ([bindings '()]
	      [socbindings '()]
	      [socpgm '()]
	      [nodetoks '()]
	      [node-startup '()])
	  (let loop ((ls stuff))
	    (disp "Decoding:" (map car bindings) (map car socbindings)
		  socpgm (map car nodetoks) node-startup)
	    (disp "Decoding from" ls)
		 
	    (if (null? ls)
		(let ((result `(deglobalize-lang
				'(program (bindings ,@bindings)
					  (socpgm (bindings ,@socbindings) ,@socpgm)
					  (nodepgm (tokens ,@nodetoks)
						   (startup ,@node-startup))))))
;		  (printf "cleanup-token-machine: Desugaring to: ~n")
;		  (pp result)
		  result)
		(begin 
		  (match (car ls)
			 [(bindings ,x ...) (set! bindings x)]
			 [(socbinds ,x ...) (set! socbindings x)]
			 [(socpgm (bindings ,b ...) ,x ...)  
			  (set! socbindings b)
			  (set! socpgm x)]
			 [(socpgm ,x ...)   (set! socpgm x)]
			 [(nodepgm (tokens ,x ...))
			  (set! nodetoks x)]
			 [(nodepgm (tokens ,x ...) (startup ,s ...))
			  (set! nodetoks x)
			  (set! node-startup s)]
			 [(nodetoks ,x ...) (set! nodetoks (append x nodetoks))]
			 [(tokens ,x ...) (set! nodetoks (append x nodetoks))]
			 [(startup ,x ...) (set! node-startup x)]
			 ;; TEMP: for my own convenience, I allow other expressions to 
			 ;; transform into node-startup code:
			 [,expr (set! nodetoks
				      (cons `[node-start () ,expr] nodetoks))]
			 [,other (error 
				  'cleanup-token-machine:decode
				  "this isn't part of a valid token machine program! ~n ~a"
				  other)])
		  (loop (cdr ls))))))))

    (define (cleanup socconsts nodeconsts socpgm tokens nodestartups)
      (if (not (null? nodestartups))
	  (set! tokens
		(cons `[node-start () ,@(map (lambda (t) `(call ,t)) nodestartups)]
		      tokens)))
      (if (not (assq 'node-start tokens))
	  (set! tokens (cons `[node-start () (void)] tokens)))
      (if (not (assq 'SOC-start tokens))
	  (set! tokens (cons `[SOC-start () (void)] tokens)))
      (if socpgm
	  (set! tokens (cons `[SOC-start () ,(make-begin socpgm)] tokens)))
      ;; This is a little weird.  It depends on the duplicate handler
      ;; merging below.  We push the constbinds into *one* SOC-start
      ;; handler, and if there are others, the bindings get to them
      ;; too during the merge.
; NEVERMIND, no more local const binds:
;      (if (not (null? socconsts))
;	  (set! tokens (cons `[SOC-start () (bindings ,@socconsts (void))] tokens)))
      ;; Instead, we merge together all binds:

      (let ([allconsts (append socconsts nodeconsts
			       (foldl (lambda (th acc)
					(mvlet ([(tok id args stored bindings body) 
						 (destructure-tokbind th)])
					       (append bindings acc)))
				      '() tokens))]
	    ;; Duplicate elimination happens before other processing of token handlers:
	    [nodup-toks (remove-duplicate-tokbinds tokens)])
	`(cleanup-token-machine-lang
	     '(program (bindings ,@nodeconsts)
		       (nodepgm (tokens
				 ,@(map (process-tokbind (map car allconsts)
							 (map car nodup-toks))
					nodup-toks)))))
	))

    ;; Main body of cleanup-token-machine
    (lambda (prog)
      (match prog
        [(,lang '(program (bindings ,constbinds ...)
			  (socpgm (bindings ,socbinds ...) 
				  ,socstmts ...)
			  (nodepgm (tokens ,nodetoks ...)
				   (startup ,starttoks ...))))
	 
	 (disp "Lang" lang
	       "constbinds" (map car constbinds)
	       "socbinds" (map car socbinds)
	       "socstmts" socstmts
	       "tokens" (map car nodetoks)
	       "starttoks" starttoks)

	 (cleanup socbinds constbinds 
                  (if (null? socstmts) #f `(begin ,@socstmts))
                  nodetoks starttoks)]
	[(,lang '(program ,stuff ...)) (cleanup-token-machine (decode stuff))]
	;; Cleanup-token-machine is a real lenient pass.  
	;; It will take the token machines in other forms, such as
	;; without the language annotation:
	[(program (bindings ,constbinds ...)
		  (socpgm (bindings ,socbinds ...)
			  ,socstmts ...)
		  (nodepgm (tokens ,nodetoks ...)
			   (startup ,starttoks ...)))
	 (cleanup-token-machine `(UNKNOWNLANG ',prog))]

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
        (bindings)
        (nodepgm
         (tokens
          (soc-start #f () (stored) (void))
          (node-start #f () (stored) (void))))))]

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
		      (nodepgm (tokens ,toks ...))))
           (let* ([tok1 (assq 'tok1 toks)]
                  [body (rac tok1)])
	   (and (deep-member? '(fun1) body)
		(deep-member? '(fun2) body)))]))]
    
    ["Test of return normalization #1"
     (cleanup-token-machine 
      '(deglobalize-lang 
	'(program
	  (bindings ) (socpgm (bindings ) (emit tok1))
	  (nodepgm
	   (tokens
	    [tok1 () (return 3 (to tok2) )]
	    [tok2 (x) 3])
	   (startup )
	   ))))
     (cleanup-token-machine-lang
      '(program
        (bindings)
        (nodepgm
         (tokens
          (soc-start  #f  ()      (stored)
                      (begin begin (emit tok1) (void) 'multiple-bindings-for-token))
          (node-start #f () (stored) (void))
          (tok1  #f    ()  (stored)
                 (return '3 (to tok2) (via tok1) (seed '#f) (aggr #f)))
          (tok2 #f (x) (stored) '3)))))
     ]
    
))



(define test-this (default-unit-tester
		    "Pass cleanup-token-machine: regularize token machine"
		    these-tests))


(define test21 test-this)
(define tests21 these-tests)
(define test-cleanup-token-machine test-this)
(define tests-cleanup-token-machine these-tests)
