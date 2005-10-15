


;; [2005.09.26]
;; This compiler has grown large enough..

;; [2005.09.26] This is limited right now.
(define (check-grammar expr grammar . initialprod)
  ;; expr is an sexpression
  ;; grammar is just a list of productions
  (define allvariants (list->set (map car grammar)))
  (define (cut-grammar p) (filter (lambda (prod) (eq? (car prod) p)) grammar))

  (call/cc (lambda (failedcheck)

  ;; Keeps track of the places where we fail, might be able to give some feedback.
  (define fail-points '())
  ;; This keeps track of how deep we are, for purposes of deciding which failure to report.
  (define current-depth 0)
  (define failure-stack '())

  (define (add-failure x p k)
    (set! failure-stack (cons (list current-depth x p k)
			      failure-stack)))
  (define (clear-failures)
    (printf "  Clearing stack: ~s\n" failure-stack)
    (set! failure-stack '()))

  (define-syntax goingdeeper
    (syntax-rules ()
      [(_ e) 
       (begin 
	 ;(clear-failures)
	 (fluid-let ((current-depth (add1 current-depth))) e))]))

  ;(define (fail) (failedcheck #f)) ;; Default fail function, for jumping out.
  (define failfun (lambda (x p k) 
;		 (printf "Failing1: ~a ~a depth ~a\n" x p current-depth)
		 (add-failure x p k)
		 ;; Signal failure and keep trying
		 (k #f)))

  (define-syntax fail
    (syntax-rules ()
      [(_ x p k)
       (failfun x p k)]))

  ;; This just goes through the grammar in order until it hits a match.
  (define (scangrammar expr prods k)
    ;(define (fail) (scangrammar expr (cdr prods)))
    ;(printf "Scanning ~a against ~a\n" expr (map car prods))
    (if (null? prods) (fail expr prods k)
	(match (car prods)
	  [(,lhs ,rhs)
	   (checkmatch expr rhs
		       (lambda (check)
			 (if check 
			     (k `(,lhs ,@check))
			     (scangrammar expr (cdr prods) k))))])))

  ;; This sees if an expression matches a given production.
  (define (checkmatch expr prod k)
    ;; For this function failing means returning #f:
    (fluid-let ((failfun (lambda (x p k) 
;			(printf "Failing2: ~a ~a depth ~a\n" x p current-depth)
			(add-failure x p k)
			;; Signal failure and keep trying
			(k #f))))
    (match (list expr prod)
;		 [(,lhs ,type) (guard (basic-type? type))
;		  (if (check-basic expr type) #t
;		      (fail))]
      [(,x ,fun) (guard (procedure? fun))
	    (if (fun x) 
		(k fun) ;'<fun>
		(fail x fun k))]
      [(,x (quote ,sym)) ;(guard (atom? x))
       (if (eq? x sym) 
	   (k `(quote ,sym))
	   (fail x `(quote ,sym) k))]
      [(,x (,p* ...)) ;; A list production
       (if (not (list? x))
	   (fail x p* k)
	   (goingdeeper (matchlist x p* k)))]
      [(,x ,p) (guard (memq p allvariants))
           (scangrammar x (cut-grammar p) k)]
      [(,_ ,p) (guard (symbol? p))
       (error check-grammar "This is production-symbol is not bound: ~a" p)]
      )))

  ;; This is for compound productions that have some structure to 'em.
  (define (matchlist ls p* k)
     ;; For this function any failure means the whole list failed:
     (call/cc (lambda (failedlist)
     (fluid-let ((failfun (lambda (x p k)
;			(printf "Failing3: ~a ~a depth ~a\n" x p current-depth)
			(add-failure x p k)
			;; No backtracking here (lists are linear), exit the whole list.
			;(failedlist #f)
			;; Scratch that, now all fail's are in tail pos:
			;; Signal failure and keep trying:
			(k #f)
			)))
     (let listloop ((ls ls) (p* p*) (k k))
     (match (list ls p*)
	    [(() ()) (k ())]
	    [(,x ()) (fail x () k)]
	    [(() (,_ ,v)) (guard (eq? v '...)) (k ())] ;; If we ran out on a "..." that's ok.s
	    [(() ,x) (fail () x k)]
	    
	    [((,x ,lsnew ...)
	      (,fun ,p*new ...))
	     (guard (procedure? fun))
	     (if (fun x)
		 (goingdeeper (listloop lsnew p*new (lambda (e) (if e (k (cons fun e)) (k #f)))))
		 (fail x fun k))]

	    [((,x ,lsnew ...)
	      ((quote ,p) ,p*new ...))
	     ;(guard (atom? x))
	     (if (eq? x p)
		 (goingdeeper (listloop lsnew p*new (lambda (e) (if e (k (cons `(quote ,p) e)) (k #f)))))
		 (fail x `(quote ,p) k))]

	    ;; Here we greedily eat up everything when we have a "..." ("*" BNF notation):
	    [((,x ,lsnew ...)
	      (,p ,elipses ,p*new ...)) (guard (eq? elipses '...))
	      (goingdeeper (checkmatch x p
		 (lambda (check)
		   (if check
		       (goingdeeper (listloop lsnew p* (lambda (e) (if e (k (cons check e)) (k #f)))))
		       (goingdeeper (listloop ls p*new k))))))]

	    ;; Named production:
	    [((,x ,lsnew ...)
	      (,p ,p*new ...))
	     (guard (memq p allvariants))
	     (scangrammar x (cut-grammar p)
			  (lambda (scan)
			    (if scan
				(goingdeeper (listloop lsnew p*new (lambda (e) (if e (k (cons scan e)) (k #f)))))
				(fail x p k))))]
	    [((,x ,lsnew ...)
	      (,p ,p*new ...))
	     (guard (symbol? p))
	     (error check-grammar "This is production-symbol is not bound: ~a" p)]

	    ;; [sub]List pattern:
	    [((,x ,lsnew ...)
	      ((,subp* ...) ,p*new ...))
	     (if (not (list? x))
		 (fail x subp* k)
		 (goingdeeper (listloop x subp*
					(lambda (sub)
					  (if sub
					      (listloop lsnew p*new (lambda (e) (if e (k (cons sub e)) (k #f))))
					      (fail x subp* k))))))]
	    ))))))

  (set-top-level-value! 'checkmatch checkmatch)
  (set-top-level-value! 'matchlist matchlist)
  (set-top-level-value! 'scangrammar scangrammar)


  (let ((result 
	 (scangrammar expr (if (null? initialprod)
;			 (if (assq 'PassInput grammar)
;			     (begin ;; This is cheesy but convenient for me:
;			       (printf "Defaulting to using 'PassInput as starting production for grammar check.\n")
;			       (cut-grammar 'PassInput))
			     ;; Otherwise we allow a match against any production in the grammar:
			 grammar
			 (cut-grammar (car initialprod)))
		(lambda (e) e))))
    (if (not result)
	(begin (set-top-level-value! 'failure-stack failure-stack)
	       (printf "Grammar check failed.  Stored failure trace in global var \"failure-stack\"\n")
	       (printf "Run (analyze-grammar-failure failure-stack) to see what went wrong.\n")
	       #f)
	result)
   ))))

(define (analyze-grammar-failure grammar-failure)
  ;; Could do something more sophisticated here in the future involving the depth:
  (let ((max-size 0)
	(winner 'UNINTIALIZED-ERROR-IN-ANALYZE-GRAMMAR-FAILURE))
    (for-each (lambda (ls)
   		(match ls 
		  [(,d ,x ,p ,k)
		   (let ((reconstructed (k 'FAIL)))
		     (let ((size (count-nodes reconstructed)))
		       (when (> size max-size)
			 (set! max-size size)
			 (set! winner (list d x p reconstructed)))))]))
	      grammar-failure)
    (match winner
      [(,d ,x ,p ,context)
       (printf "Most likely failed parsing: \n")
       (printf "Failure depth ~a\n Expression ~a did not satisfy ~a\n Context:\n   " d x p)
       (parameterize ((pretty-standard-indent 5)
		      (pretty-initial-indent 5))
	 (pretty-print context))]
      )))
	      

;; ==================================================================
;; This is the constructor for compiler passes.  It takes the main
;; function that does the real work of the compiler, and then wraps it
;; with some extra debugging code.

;; Todo, add invariant-procedures as well as grammars:
(define (build-compiler-pass name input-spec output-spec transform)  
  (match (list input-spec output-spec)
    [((input ,instuff ...) (output ,outstuff ...))
     (lambda (prog)
       (let ([ingram (assq 'grammar instuff)]
	     [outgram (assq 'grammar outstuff)])
	 ;; Check input grammar:
	 (DEBUGMODE ;; When we're not in debugmode we don't waste cycles on this.
	  (match ingram
	   [#f (void)]
	   ;; The optional initial production may or may not be supplied:
	   [(grammar ,gram ,optional_initialprod ...)
	    (or (apply check-grammar prog gram optional_initialprod)
		(error 'build-compiler-pass "Bad input to pass: \n ~a" prog))]))
	 (let ((result (transform prog)))
	   (DEBUGMODE
	    (if (regiment-verbose) 
		(printf "~a: Got result, checking output grammar...\n" name))
	    ;; Check output grammar:	   
	    (match outgram
	     [#f (void)]
	     ;; The optional initial production may or may not be supplied:
	     [(grammar ,gram ,optional_initialprod ...)
	      (or (apply check-grammar result gram optional_initialprod)
		  (begin (pretty-print result) #f)
		  (error 'build-compiler-pass "Bad pass output from ~a, failed grammar: \n ~a" name prog))])
	    (if (regiment-verbose)
		(printf "~a: Output grammar passed.\n" name)))
	   result
	   )))]))



		  
;; ======================================================================

;;;  <Sugar>     ::= (flood <Expr>)
;;;                | (elect-leader <Token> [<Token>])

;;;  <Prim> ::= <BasicPrim> 
;;;           |  | timed-call | bcast
;;;           | is_scheduled | deschedule | is_present | evict
(define basic_tml_grammar
  (let ()
    (define (is-var? x) (and (symbol? x) (not (token-machine-keyword? x))))
  `(
    [PassInput (Lang ('quote Program))]
    [Lang ,symbol?]
    ;; The bindings must be "constant" in the sense that their expressions are statically evaluatable:
    [Program ('program ('bindings (Var Expr) ...) NodePgm)]
    ;       NOTE: tokens will inclide a binding for SOC-start and node-start:
    [NodePgm ('nodepgm ('tokens TokBinding ...))] 
    [TokBinding (TokName Var ;; subtokid
			 (Var ...) 
			 ; ('bindings Cbind ...) ; Got rid of these local constant bindings
			 ('stored (Var Expr) ...)
			 Expr)]
    [Expr Var]
    [Expr Num] ;; Allow unquoted?
    [Expr Const]
    [Expr Token] ;; NOTE: Either the whole token reference or just the sub-index can be dynamic.
    [Expr ('set! Var Expr)]
    [Expr ('ext-ref Token Var)]
    [Expr ('ext-set! Token Var Expr)]

;       NOTE: These are static token refs for now.
    [Expr ('begin Expr ...)]
    ;[Expr ('let ([Var Expr] ...) Expr)]
    [Expr ('let ([Var Expr]) Expr)]
    [Expr ('if Expr Expr Expr)]
    [Expr ('leds LedColor LedState)]    
    [LedColor 'Red]
    [LedColor 'Yellow]
    [LedColor 'Green]
    [LedState 'On]
    [LedState 'Off]
    [LedState 'Toggle]   

;; Should scratch this and explicitely enforce argument count in grammar:
    [Expr (Prim Expr ...)]
    ,@(map (lambda (entry) `[Prim (quote ,(car entry))]) 
	   token-machine-primitives)
    [Expr ('app Expr ...)]
    [Expr ('call Token Expr ...)]

    [Expr ('dbg DebugArg ...)] ;; Debug Args can "cheat" and go outside the scope of TML
    [DebugArg Expr]
    [DebugArg ('quote DebugArgConstData)]
    [DebugArgConstData ,atom?]
    [DebugArgConstData (DebugArgConstData ...)]
    
;; These are now just primitives:  
;; But still need to remember to subtract them when the grammar shrinks.
;    [Expr ('return Expr)]
;    [Expr ('subcall DynToken Expr ...)]

    [Num ,integer?]
    [Var ,is-var?]
    [Token StaticToken]
    [Token DynToken]
    [StaticToken ('tok TokName ,integer?)]
    [DynToken ('tok TokName Expr)]
    [TokName ,symbol?]
;    [Cbind (Var Const)] ; NOTE: These expressions will be statically calculable -- constants.
    [Const ('quote ,atom?)]

    )))

(define tml_gradient_grammar
  `([GExpr ('gemit Token Expr ...)]
    [GExpr ('grelay Token Expr ...)]
    [GExpr ('greturn Expr
		     ('to Token)
		     ('via Token)
		     ('seed Expr)
		     ('aggr TokenOrFalse)
		     )]
    [TokenOrFalse Token]
    [TokenOrFalse '#f]
    [GExpr ('gdist Token)]
    [GExpr ('gparent Token)]
    [GExpr ('gorigin Token)]
    [GExpr ('ghopcount Token)]
    [GExpr ('gversion Token)]
    ))

(define tml_letstored_grammar
;  `([LetStored ('let-stored ([Var Expr] ...) Expr)]))
  `([LetStored ('let-stored ([Var Expr]) Expr)])) ;; Restricting let-stored further.

(define full_but_clean_tml
  `(,@ basic_tml_grammar
    [Expr GExpr]
    ,@ tml_gradient_grammar
    [Expr LetStored]
    ,@ tml_letstored_grammar
    ;; Allowing this for now:
    [Expr ('lambda (Var ...) Expr)]
 ))


;;;  <GExpr>     ::= (gemit <DynToken> <Expr> ...)
;;;                | (greturn <Expr> (to <DynToken>) (via <DynToken>) (seed <Expr>) (aggr <Token>))
;;;                | (grelay <DynToken>) ;; NEED TO ADD RELAY ARGS!
;;;                | (gdist <DynToken>)
;;;                | (gparent <DynToken>)
;;;                | (gorigin <DynToken>)
;;;                | (ghopcount <DynToken>)
;;;                | (gversion <DynToken>)


;;;  <Pgm> ::= (program (bindings <Decl>*) <SOCPgm> <NodePgm>)
;;;  <SOCPgm> ::= <Statement*>
;;;  <NodePgm> ::= (nodepgm <Entry> (bindings <Decl>*) (tokens <TokBinding>*))
;;;;;;;  <Entry>  ::= <Token>
;;;  <Decl> ::= (<var> <Exp>)
;;;  <TokBinding> ::= (<Token>  <Code>*)
;;; <TODO> DECIDE ON LOCAL BINDINGS:
;;;  <TokBinding> ::= (<Token> (bindings <Decl>*) <Code>*)

;;;  <Code> ::= <Statement>*
;;;  <Statement>  ::= <BasicStuff?>
;;;                | <GExpr>
;;;                | <Macro> 
;;;  <GExpr>     ::= (gemit <DynToken> <Expr> ...)
;;;                | (greturn <Expr> (to <DynToken>) (via <DynToken>) (seed <Expr>) (aggr <Token>))
;;;                | (grelay <DynToken>) ;; NEED TO ADD RELAY ARGS!
;;;                | (gdist <DynToken>)
;;;                | (gparent <DynToken>)
;;;                | (gorigin <DynToken>)
;;;                | (ghopcount <DynToken>)
;;;                | (gversion <DynToken>)
;;;  <Macro> ::= (flood <Token>)
;;;            | (elect-leader <Token> [<Token>])  ;; <TODO> optional second argument.. decider
;;;  <Simple> ::= (quote <Lit>) | <Var>

;;;  <Token> ::= <Symbol> | ...???
;;;  <Exp>  ::= ???


; (define deglobalize_output_grammar
;   (make-grammar 'code
;    `([code (statement ...)]
;      [statement basic_tml]
;      [statement gexpr]

; (define (basic-type? t) (memq t '(symbol)))
; (define (check-basic e t) 
;   (case t
;     [(symbol) (symbol? e)]
;     [(
  
	      


(define these-tests
  `([(check-grammar '(set! foo 3) basic_tml_grammar 'Expr) ,list?]
    [(check-grammar '(ext-set! (tok foo 3) storedvar 4) basic_tml_grammar 'Expr) ,list?]
    [(check-grammar '(let ((x 4)) (let ((y 5)) 3)) basic_tml_grammar 'Expr) ,list?]
    [(check-grammar '(nodepgm (tokens)) basic_tml_grammar) ,list?]
    [(car (check-grammar '(tok1 subind () (stored) 333) basic_tml_grammar)) TokBinding]
    [(check-grammar '(program (bindings) (nodepgm (tokens))) basic_tml_grammar) ,list?]
    [(check-grammar '(program (bindings (x '3)) (nodepgm (tokens))) basic_tml_grammar) ,list?]
    [(check-grammar '(program (bindings) (nodepgm (tokens
						   (tok1 subind () (stored) 333)
						   ))) basic_tml_grammar)
     ,list?]
    ["Testing elipses followed by something"
     (check-grammar '(testfoo a b c 3) 
		    `([Test ('testfoo Var ... Num)]
		      [Num ,integer?]
		      [Var ,symbol?]))
     ,list?]

    ["Run check on example output of cleanup-token-machine: "
     (check-grammar '(program
		      (bindings)
		      (nodepgm
		       (tokens
			(SOC-start subtok_ind () (stored) (void))
			(node-start subtok_ind () (stored) (printf '"woot"))))) basic_tml_grammar)
     ,list?]

    ))

(define test-this (default-unit-tester
		    "grammar_checker: this is my by-hand grammar checker for pass input/output"
		    these-tests))
(define test-grammar test-this)
(define tests-grammar these-tests)

