
;; [2005.09.26]
;; This compiler has grown large enough..


;; [2005.09.26] This is limited right now.
(define (check-grammar expr grammar)
  ;; expr is an sexpression
  ;; grammar is just a list of productions
  (define allvariants (list->set (map car grammar)))
  (define (cut-grammar p) (filter (lambda (prod) (eq? (car prod) p)) grammar))

  (call/cc (lambda (failedcheck)

  ;(define (fail) (failedcheck #f)) ;; Default fail function, for jumping out.
  (define (fail) #f)

  (define (scangrammar expr prods)
    ;(define (fail) (scangrammar expr (cdr prods)))
    ;(printf "Scanning ~a against ~a\n" expr (map car prods))
    (if (null? prods) (fail)
	(match (car prods)
	  [(,lhs ,rhs)
	   (let ((check (checkmatch expr rhs)))
	     (if check `(,lhs ,@check)
		 (scangrammar expr (cdr prods))))])))

  (define (checkmatch expr prod)
    ;; For this function failing means returning #f:
    (fluid-let ((fail (lambda () #f)))
    (match (list expr prod)
;		 [(,lhs ,type) (guard (basic-type? type))
;		  (if (check-basic expr type) #t
;		      (fail))]
      [(,x ,fun) (guard (procedure? fun))
	    (if (fun x) fun ;'<fun>
		(fail))]
      [(,x ',sym) (if (eq? x sym) sym #f)]
      [(,x (,p* ...)) ;; A list production
       (if (not (list? x))
	   #f
	   (matchlist x p*))]
      [(,x ,p) (guard (memq p allvariants))
	 (scangrammar x (cut-grammar p))]
      [(,_ ,p) (guard (symbol? p))
       (error check-grammar "This is production-symbol is not bound: ~a" p)]
      )))

  (define (matchlist ls p*)
     ;; For this function any failure means the whole list failed:
     (call/cc (lambda (failedlist)
     (fluid-let ((fail (lambda () (failedlist #f))))
     (let listloop ((ls ls) (p* p*))
     (match (list ls p*)
	    [(() ()) ()]
	    [(,_ ()) (fail)]
	    [(() (,_ ,v)) (guard (eq? v '...)) ()] ;; If we ran out on a "..." that's ok.s
	    [(() ,_) (fail)]
	    
	    [((,x ,lsnew ...)
	      (,fun ,p*new ...))
	     (guard (procedure? fun))
	     (if (fun x)
		 (cons fun (listloop lsnew p*new))
		 (fail))]

	    [((,x ,lsnew ...)
	      ((quote ,p) ,p*new ...))
	     (guard (symbol? x))
	     (if (eq? x p)
		 (cons `(quote ,p) (listloop lsnew p*new))
		 (fail))]

	    ;; Here we greedily eat up everything when we have a "..." or "*" BNF notation:
	    [((,x ,lsnew ...)
	      (,p ,v ,p*new ...)) (guard (eq? v '...))
	     (let ((check (checkmatch x p)))
	       (if check
		   (cons check (listloop lsnew p*))
		   (listloop ls p*new)))]

	    ;; Named production:
	    [((,x ,lsnew ...)
	      (,p ,p*new ...))
	     (guard (memq p allvariants))
	     (let ((scan (scangrammar x (cut-grammar p))))
	       (if scan
		   (cons scan (listloop lsnew p*new))
		   (fail)))]	     
	    [((,x ,lsnew ...)
	      (,p ,p*new ...))
	     (guard (symbol? p))
	     (error check-grammar "This is production-symbol is not bound: ~a" p)]

	    ;; [sub]List pattern:
	    [((,x ,lsnew ...)
	      ((,subp* ...) ,p*new ...))
	     (if (not (list? x))
		 (fail)
	     (let ((sub (listloop x subp*)))
	       (if sub
		   (cons sub (listloop lsnew p*new))
		   (fail))))]
	    ))))))

   (scangrammar expr grammar))))
		  


;;;                | <Sugar> 

;;;  <Sugar>     ::= (flood <Expr>)
;;;                | (elect-leader <Token> [<Token>])
                     ;; <TODO> optional second argument.. decider
;;;  <Prim> ::= <BasicPrim> 
;;;           | call | subcall | timed-call | bcast
;;;           | is_scheduled | deschedule | is_present | evict
(define basic_tml_grammar
  `([PassInput (Lang ('quote Program))]
    [Lang ,symbol?]
    [Program ('program ('bindings Cbind ...) NodePgm)]
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
    [Expr DynToken] ;; NOTE: Either the whole token reference or just the sub-index can be dynamic.
    [Expr ('set! Var Expr)]
    [Expr ('ext-ref DynToken Var)]
    [Expr ('ext-set! DynToken Var Expr)]

;       NOTE: These are static token refs for now.
    [Expr ('begin Expr ...)]
    [Expr ('let ([Var Expr] ...) Expr)]
    [Expr ('if Expr Expr Expr)]
    [Expr ('subcall DynToken Expr ...)]
    [Expr ('return Expr)]
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


    [Num ,integer?]
    [Var ,(lambda (x) (and (symbol? x) (not (token-machine-keyword? x))))]
    [DynToken Token]
    [DynToken ('tok TokName Expr)]
    [Token ('tok TokName Expr)]
    [TokName ,symbol?]
    [Cbind (Var Const)] ; NOTE: These expressions will be statically calculable -- constants.
    [Const ('quote ,atom?)]
    ))

(define tml_gradient_grammar
  `([GExpr ('gemit DynToken Expr)]
    [GExpr ('greturn Expr
		     ('to DynToken)
		     ('via DynToken)
		     ('seed Expr)
		     ('aggr Token))]
    [GExpr ('gdist DynToken)]
    [GExpr ('gparent DynToken)]
    [GExpr ('gorigin DynToken)]
    [GExpr ('ghopcount DynToken)]
    [GExpr ('gversion DynToken)]
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
  `([(check-grammar '(set! foo 3) basic_tml_grammar) ,list?]
    [(check-grammar '(ext-set! (tok foo 3) storedvar 4) basic_tml_grammar) ,list?]
    [(check-grammar '(let ((x 4) (y 5)) 3) basic_tml_grammar) ,list?]
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

    