
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


;;;   (*) In the future it might expand out primitive applications
;;;   that are just shorthands.  (For example, (dist) could become
;;;   (dist <this-token>))

;;;   (*) Should be IDEMPOTENT.

;;;   (*) RENAME-VARS SHOULD RUN BEFORE THIS PASS.  It depends on
;;;   merging token handlers without collisions between stored vars
;;;   and constant bindings.







;;; TODO FIX UUP:
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


    (define (destructure-tokbind tbind)  
      (define (process-stored s)
	(match s
	       [(,v ,e) `(,v ,e)]
	       [,v `(,v '#f)]))
      (define (process-bods x)
	(match x
	       [((stored ,s ...) (bindings ,b ...) ,bods ...)
		(values (map process-stored s)
			b
			(make-begin `((begin ,bods ...))))]
	       [((bindings ,b ...) (stored ,s ...) ,bods ...)
		(values (map process-stored s)
			b
	       (make-begin `((begin ,bods ...))))]
	       [((stored ,s ...) ,bods ...)
		(values (map process-stored s)
			'()
			(make-begin `((begin ,bods ...))))]
	       [((bindings ,b ...) ,bods ...)
		(values '() b
			(make-begin `((begin ,bods ...))))]
	       [,bods 
		(values '() '()
			(make-begin `((begin ,bods ...))))]))
      (match tbind
	     [(,t (,a ...) ,bds ...)
	      (mvlet ([(stored bindings body) (process-bods bds)])
		     (values t #f a stored bindings body))]
	     [(,t ,i (,a ...) ,bds ...)
	      (mvlet ([(stored bindings body) (process-bods bds)])
		     (values t i a stored bindings body))]
	     [,other (error 'destructure-tokbind "bad tokbind: ~a" other)]))


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
      ;(trace-lambda cleanuptokmac:procexp 
      (lambda (env tokens this-token)
	(lambda (stmt)
	  (define-syntax check-tok
	    (syntax-rules ()
	      [(_ call tok) 
	       (if (not (memq tok tokens))
		   (if (regiment-verbose)
		       (warning 'cleanup-token-machine
				"~s to unknown token: ~s" call tok))
		   )]))
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
	      ((process-expr env tokens this-token) `(if ,test ,conseq (void)))]

	     [(let* ( (,lhs ,[rhs]) ...) ,bodies ...)
	      (let ([newbinds 
		     (let loop ([env env] [prs (map list lhs rhs)])
		       (if (null? prs) '()
			   (let ([lhs (caar prs)] [rhs (cadar prs)])
			     (let ([newenv (cons lhs env)])
			       (cons (list lhs ((process-expr newenv tokens this-token) rhs))
				     (loop newenv (cdr prs)))))))]
		    [newbods (map (process-expr (append lhs env) tokens this-token) bodies)])
	      `(let*  ,newbinds ,(make-begin newbods)))]

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
	      ((process-expr env tokens this-token)
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
	  (mvlet ([(tok id args stored bindings body) (destructure-tokbind tokbind)])
		 `(,tok ,id ,args (stored ,@stored) ;(bindings ,@bindings)
			,((process-expr (append args env) tokens tok) body))))))
	    
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
	  (set! tokens (cons `[SOC-start () (begin ,@socpgm)] tokens)))
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
	 (cleanup socbinds constbinds 
                  (if (null? socstmts) #f `(begin ,@socstmts))
                  nodetoks starttoks)]
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
