
;;;; .title Grammar Checker

;;;; This module allows me to write down and check grammars for the
;;;; mini-languages that occur between passes.

; [2005.10.15] For decent (medium) size programs analyze-grammar-failure is working *very* slowly.

; [2005.09.26]
; This compiler has grown large enough I'm going to check the output
; of passes to make sure they actually fit their announced grammars.
; This also forces me to make first class representations of grammars.


(module grammar_checker mzscheme
  (require (lib "include.ss")
	   "../plt/iu-match.ss"
	   "../generic/constants.ss" ;; For DEBUGMODE
           "../plt/prim_defs.ss"
	   (all-except "../plt/helpers.ss" test-this these-tests)
;	   (all-except "../plt/regiment_helpers.ss" test-this these-tests)
           )
  (provide 
         check-grammar
	 analyze-grammar-failure 
         
         ;; Predifined Grammars
	 initial_regiment_grammar
	 ; elaborated_regiment_grammar
         basic_tml_grammar
;         tml_gradient_grammar
;         tml_letstored_grammar
         full_but_clean_tml
         
;         these-tests test-this
;         test-grammar tests-grammar
         )

  (chezimports ;constants
               (except helpers   test-this these-tests)
;	       (except regiment_helpers   test-this these-tests)
	       )
; ======================================================================

;;; Main grammar checking entry points

;; [2005.09.26] This is limited right now.
(define (check-grammar origexpr grammar . initialprod)
  ;; .param origexpr is an sexpression
  ;; .param grammar is just a list of productions
  ;; .param initialprod indicates which production to use to start checking
  (define allvariants (list->set (map car grammar)))
  (define (cut-grammar p) (filter (lambda (prod) (eq? (car prod) p)) grammar))

  ;; This keeps track of how deep we are, for purposes of deciding which failure to report.
  (define current-depth 0)
  ;; Keeps track of the places where we fail, might be able to give some feedback.
  (define failure-stack '())

  (define (add-failure x p k)
    (set! failure-stack (cons (list current-depth x p k)
			      failure-stack)))
  (define-syntax goingdeeper
    (syntax-rules ()
      [(_ e) (fluid-let ((current-depth (add1 current-depth))) e)]))
  (define-syntax fail
    (syntax-rules ()
      [(_ x p k)
       (begin
;	 (printf "Failing: ~a ~a depth ~a\n" x p current-depth)
	 (add-failure x p k)
	 ;; Signal failure and keep trying
	 (k #f))]))

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
    (match (list expr prod)
;		 [(,lhs ,type) (guard (basic-type? type))
;		  (if (check-basic expr type) #t
;		      (fail))]
      [(,x ,fun) (guard (procedure? fun))
	    (if (fun x)
		(if (atom? x)
		    (k (cons fun x))
		    (k fun)) ;'<fun>
		(fail x fun k))]
      [(,x (quote ,sym)) ;(guard (atom? x))
       (if (eq? x sym) 
	   (k `(quote ,sym))
	   (fail x `(quote ,sym) k))]
      [(,x (,p* ...)) ;; A list production
       (if (not (list? x))
	   (fail x p* k)
	   (goingdeeper (matchlist x p* k)))]
      [(,x #(,p* ...)) ;; A vector production, just convert to list
       (if (not (vector? x))
	   (fail x (list->vector p*) k)
	   (goingdeeper (matchlist (vector->list x) p* 
				   ;; Add to the continuation to convert back to vector
				   (lambda (x) (k (list->vector x))))))]
      [(,x ,p) (guard (memq p allvariants)) ;; A production-name
           (scangrammar x (cut-grammar p) k)]
      [(,_ ,p) (guard (symbol? p))
       (error check-grammar "This is production-symbol is not bound: ~a" p)]
      [(,_ ,p)
       (error check-grammar "Unknown kind of production in the grammar: ~a" p)]
      ))

  ;; This is for compound productions that have some structure to 'em.
  (define (matchlist ls p* k)
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
		 (goingdeeper (listloop lsnew p*new 
					(lambda (e) (if e (k (cons (if (atom? x) (cons fun x) fun) e))
							(k #f)))))
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
	    [,other (error 'check-grammar:match-list "unmatched sexp/pattern pair: ~a\n" other)]
	    )))

  (let ((result 
	 (scangrammar origexpr (if (null? initialprod)
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
	       (set-top-level-value! 'current-parse origexpr)
	       (printf "Grammar check failed.  Stored failure trace in global var \"failure-stack\", and program in \"current-parse\"\n")
	       (printf "Run (analyze-grammar-failure failure-stack) to see what went wrong.\n")
	       #f)
	result)
   ))

;; This function allows one to try to figure out where the problem was
;; when a grammar check fails.  This is a total hack and operates
;; according to an arbitrary heuristic I made up.  At some point I
;; should take a more disciplined approach to this grammar checking
;; business.
(define (analyze-grammar-failure grammar-failure)

    (let* ((count (length grammar-failure))
	   (progressbar (display-progress-meter count)))
      (printf "\nAnalyzing ~a failure scenarios.\n" count)
      (let ((winner-list
	     (filter id 
	       (map (lambda (ls)
		      (match ls
			[(,d ,x ,p ,k)
			 (let ((reconstructed (k 'FAIL)))
			   (if reconstructed
			       (let ((failcount (length (deep-all-matches (lambda (x) (eq? x 'FAIL)) reconstructed))))
					;(printf "Failcount ~a\n" failcount)
				 (if (= 0 failcount)
					;#f
				     ;; This is not a failure!
				     (error 'analyze-grammar-failure 
					    "Correct Parsing!\n ~s \n Correct Parsing! \n"
					    reconstructed)
				     ;; This is my random scoring heuristic:
				     ;; We want a large context and a small expression:
				     (let ((score (- (count-nodes reconstructed)
							(* 30 (count-nodes x)))
						  ;failcount
						     ))
				       (progressbar)
				       `(,score ,d ,x ,p ,reconstructed))))
			       (begin (progressbar)
				      ;; We couldn't do anything at all with it, failed reconstruct:
				      ;; TODO: FIXME: WHEN DOES THIS HAPPEN, WHY?
				      #f))
			   )]))
		 grammar-failure))))

	;; First we sort minimizing depth:
	;; Weird, but this seems heuristically to work ok:
;; [2005.10.26] Disabling again... it's hard to say with these heuristics:
;	(set! winner-list (sort! (lambda (x y) (< (cadr x) (cadr y))) winner-list))
	
	(printf "\nMost likely failed parsing: \n")
	(set! winner-list (sort! (lambda (x y) (> (car x) (car y))) winner-list))
	
;	(pretty-print winner-list)

	(let userloop ((i 0) (ls winner-list) (past ()))
	  (if (null? ls) (printf "\n No more grammar failures to analyze.\n")
	      (match (car ls)
		[(,score ,d ,x ,p ,context)
		 (printf "\n~a: At failure depth ~a, expression below did not satisfy ~a (context score ~a)\n    " i d p score)
		 (parameterize (;(pretty-standard-indent 5)  ;; <- This didn't have the desired effect 
				;(pretty-initial-indent 5)   ;; And isn't in plt anyway...
				(print-level 3)
				(print-length 5))
		   (pretty-print x))
		 (let menuloop ()
		   (printf "  Press n(next), p(previous), c(failure context), q(quit): ")
		   (let readloop ()
		     (case (read)
		       [(q) (void)]
		       [(n) 
			(if (null? ls)
			    (begin (printf "No more failure scenarios.")
				   (userloop i ls past))
			    (userloop (add1 i) (cdr ls) (cons (car ls) past)))]
		       [(p) 
			(if (null? past)
			    (begin (printf "No more previous failure scenarios.\n")
				   (userloop i ls past))
			    (userloop (sub1 i) (cons (car past) ls) (cdr past)))]
		       [(c) 
			(printf "\nContext:\n")
			(parameterize (;(pretty-standard-indent 5)
				       ;(pretty-initial-indent 5)
                                       )
			  (pretty-print context))
			(menuloop)]
		       [else (printf "\nInvalid input.\n") (readloop)]
		       )))]))))))


; =================================================================================

#|
;; Trying again: no backtrack
(define (check-grammar2 origexpr grammar . initialprod)
  ;; expr is an sexpression
  ;; grammar is just a list of productions
  (define allvariants (list->set (map car grammar)))
  (define (cut-grammar p) (filter (lambda (prod) (eq? (car prod) p)) grammar))

  ;; This just goes through the grammar in order until it hits a match.
  (define (scangrammar expr prods)
    (if (null? prods) 'FAIL
	(match (car prods)
	  [(,lhs ,rhs)
	   (let ((check (checkmatch expr rhs)))
	     (if check
		 `(,lhs ,@check)
		 (scangrammar expr (cdr prods))))])))

;; FIXME: FINISH BELOW

  ;; This sees if an expression matches a given production.
  (define (checkmatch expr prod k)
    (match (list expr prod)
;		 [(,lhs ,type) (guard (basic-type? type))
;		  (if (check-basic expr type) #t
;		      (fail))]
      [(,x ,fun) (guard (procedure? fun))
	    (if (fun x)
		(if (atom? x)
		    (k (cons fun x))
		    (k fun)) ;'<fun>
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
      ))

  ;; This is for compound productions that have some structure to 'em.
  (define (matchlist ls p* k)
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
		 (goingdeeper (listloop lsnew p*new 
					(lambda (e) (if e (k (cons (if (atom? x) (cons fun x) fun) e))
							(k #f)))))
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
	    )))

  (let ((result 
	 (scangrammar origexpr (if (null? initialprod)
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
	       (set-top-level-value! 'current-parse origexpr)
	       (printf "Grammar check failed.  Stored failure trace in global var \"failure-stack\", and program in \"current-parse\"\n")
	       (printf "Run (analyze-grammar-failure failure-stack) to see what went wrong.\n")
	       #f)
	result)
   ))
|#


; =======================================================================
;;; Regiment Grammars.

;; This is the type grammar supported by the Regiment source language.
(define type_grammar
  (let ()
    `(
      [Type 'Integer]
      [Type 'Float]
      [Type 'Bool]
      
      [Type 'Node]
      [Type 'Anchor]
      [Type 'Region]
      [Type ('Signal Type)]
      [Type ('Area Type)]
      [Type ('Event Type)]
      
      [Type ('List Type)]
      [Type #(Type ...)] ;; Tuples
      [Type (Type ... '-> Type)]

      ;; Also type vars:
      [Type ('quote ,symbol?)]
      )))

(define base_regiment_forms
  (let ()
    (define (is-var? x) (and (symbol? x) (not (regiment-keyword? x))))
  `(
    [PassInput (Lang ('quote Program))]
    [Lang ,symbol?]
    ;; The bindings must be "constant" in the sense that their expressions are statically evaluatable:
    [Program ('program Expr Type)]

    [Expr Var]
    [Expr Int]   
    [Expr Float] 
    [Expr ('quote Datum)]
    [Expr ('if Expr Expr Expr)]
    [Expr ('letrec ([Var Type Expr] ...) Expr)]
    [Expr ('tuple Expr ...)]
    [Expr ('tupref Int Int Expr)]
    [Expr (Prim Expr ...)]

    ,@(map (lambda (entry) `[Prim (quote ,(car entry))])
	   ;; Remove dbg from the list... we handle that special:
	regiment-primitives)

    [Int ,integer?]
    [Float ,flonum?]
    [Var ,is-var?]
    ;[QuotedDatum ,atom?]
    ;[QuotedDatum ,list?]
    [Datum ,atom?]
    [Datum (Datum ...)] ;; Quite flexible here, any sort of thing is a const.

    ,@type_grammar
    )))

;; This is the grammar output from verify-regiment.
(define initial_regiment_grammar
  `( ,@base_regiment_forms
     ;; These are forms only valid for the meta-language (pre-elaboration)
     [Expr ('lambda (Var ...) (Type ...) Expr)]
     [Expr (Expr ...)]  ;; Application.  Should make this labeled.
    ))

;; This is the grammar for the output of static-elaborate
;; UNFINISHED:
(define elaborated_regiment_grammar
  ;; TODO, make check-grammar optionally take a procedure which is given a sub-checker.
  (lambda (subcheck)
    `( ,@base_regiment_forms
       [Expr ('lambda . ValidLambda)]
       ;; Lambda's are no longer allowed to have free-vars.
       [ValidLambda ,(lambda (ls)
		       (match ls
			 [((,v* ...) (,t* ...) ,e)
			  (and (subcheck e 'Expr)
			       (= (length v*) (length t*))
			       ;; No free vars allowed!!
			       (subset? ('TODOfree-vars e) v*))]
			 [,else #f]))]
       )))
		  
; =======================================================================

;;; Token Machine Grammars

;  <Sugar>     := (flood <Expr>)
;                | (elect-leader <Token> [<Token>])

;  <Prim> := <BasicPrim> 
;           |  | timed-call | bcast
;           | is_scheduled | deschedule | is_present | evict
;; This is the baseline TML grammar.
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
    [Expr ('leds LedState LedColor)]
    [LedColor 'red]
    ;[LedColor 'yellow]
    [LedColor 'blue]
    [LedColor 'green]
    [LedState 'on]
    [LedState 'off]
    [LedState 'toggle]

;; Should scratch this and explicitely enforce argument count in grammar:
    [Expr (Prim Expr ...)]
    ,@(map (lambda (entry) `[Prim (quote ,(car entry))]) 
	   ;; Remove dbg from the list... we handle that special:
	   (assq-remove-all 'dbg token-machine-primitives))
    [Expr ('app Expr ...)]
    ;; These are PRIMS:
;    [Expr ('call Token Expr ...)]
;    [Expr ('call-fast Token Expr ...)]
;    [Expr ('bcast Token Expr ...)]

    [Expr ('dbg ('quote ,string?) DebugArg ...)] ;; Debug Args can "cheat" and go outside the scope of TML
    [DebugArg Expr]
    [DebugArg ('quote DebugArgConstData)]
    [DebugArgConstData ,atom?]
    [DebugArgConstData (DebugArgConstData ...)]

    ;; 
    ;; This is dangerous, but I allow arbitrary debug statements to put hidden in "BLACKBOX" syntax.
    ;; This are absolutely opaque both to the grammar checker and subsequent passes' transformations.
    [Expr ('BLACKBOX ,(lambda (_) #t))]

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

;; This is a "mix-in" for adding gradients to the TML grammar.
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

;; This is another "mix-in" for the let-stored form.
(define tml_letstored_grammar
;  `([LetStored ('let-stored ([Var Expr] ...) Expr)]))
  `([LetStored ('let-stored ([Var Expr]) Expr)])) ;; Restricting let-stored further.

;; This is the frequently used "full TML" grammar.  For example, this
;; characterizes the output cleanup-token-machines.
(define full_but_clean_tml
  `(,@ basic_tml_grammar
    [Expr GExpr]
    ,@ tml_gradient_grammar
    [Expr LetStored]
    ,@ tml_letstored_grammar
    ;; Allowing this for now:
    [Expr ('lambda (Var ...) Expr)]
    [Expr ('kcall Expr Expr)]
 ))


;    <GExpr>     := (gemit <DynToken> <Expr> ...)
;                  | (greturn <Expr> (to <DynToken>) (via <DynToken>) (seed <Expr>) (aggr <Token>))
;                  | (grelay <DynToken>) ;; NEED TO ADD RELAY ARGS!
;                  | (gdist <DynToken>)
;                  | (gparent <DynToken>)
;                  | (gorigin <DynToken>)
;                  | (ghopcount <DynToken>)
;                  | (gversion <DynToken>)


;    <Pgm> := (program (bindings <Decl>*) <SOCPgm> <NodePgm>)
;    <SOCPgm> := <Statement*>
;    <NodePgm> := (nodepgm <Entry> (bindings <Decl>*) (tokens <TokBinding>*))
;  ;  ;  <Entry>  := <Token>
;    <Decl> := (<var> <Exp>)
;    <TokBinding> := (<Token>  <Code>*)
;   <TODO> DECIDE ON LOCAL BINDINGS:
;    <TokBinding> := (<Token> (bindings <Decl>*) <Code>*)

;    <Code> := <Statement>*
;    <Statement>  := <BasicStuff?>
;                  | <GExpr>
;                  | <Macro> 
;    <GExpr>     := (gemit <DynToken> <Expr> ...)
;                  | (greturn <Expr> (to <DynToken>) (via <DynToken>) (seed <Expr>) (aggr <Token>))
;                  | (grelay <DynToken>) ;; NEED TO ADD RELAY ARGS!
;                  | (gdist <DynToken>)
;                  | (gparent <DynToken>)
;                  | (gorigin <DynToken>)
;                  | (ghopcount <DynToken>)
;                  | (gversion <DynToken>)
;    <Macro> := (flood <Token>)
;              | (elect-leader <Token> [<Token>])  ;; <TODO> optional second argument.. decider
;    <Simple> := (quote <Lit>) | <Var>

;    <Token> := <Symbol> | ...???
;    <Exp>  := ???


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
  

; =======================================================================
;;; Unit tests.

;; Unit tests.
(define these-tests
  `([(check-grammar '(set! foo 3) basic_tml_grammar 'Expr) ,list?]
    [(check-grammar '(ext-set! (tok foo 3) storedvar 4) basic_tml_grammar 'Expr) ,list?]
    [(check-grammar '(let ((x 4)) (let ((y 5)) 3)) basic_tml_grammar 'Expr)   ,list?]
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


    #; 
    ["Test a failed grammar check."
     (check-grammar '(dbg "test" '1 '2 '3) basic_tml_grammar 'Expr)
     ????]

    ))

(define test-this (default-unit-tester
		    "grammar_checker: this is my by-hand grammar checker for pass input/output"
		    these-tests))

;; Unit tester.
(define test-grammar test-this)
(define tests-grammar these-tests)

) ;; End module.
