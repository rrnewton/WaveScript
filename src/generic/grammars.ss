;;;; Tree Grammars for Regiment and TML

;;;; Currently 'include'd into grammar_checker.ss

; =======================================================================
;;; Regiment Grammars.

;; This is the type grammar supported by the Regiment source language.
(define type_grammar
  (let ()
    `(
      [Type 'Integer]
      [Type 'Float]
      [Type 'Complex]
      [Type 'Bool]
      
      [Type 'Node]
      [Type 'Anchor]
      [Type 'Region]
      [Type ('Signal Type)]
      [Type ('Area Type)]
      [Type ('Event Type)]

      ;; TODO: NEED TO ALLOW FOR USER DEFINED TYPE CONSTRUCTORS
      [Type ('Array Type)]
      [Type ('VQueue Type)]
      [Type ('Sigseg Type)]
      
      [Type ('List Type)]
      [Type #(Type ...)] ;; Tuples
      [Type (Type ... '-> Type)]
      [Type ('-> Type)]

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
    [Expr String] 
    [Expr ('quote Datum)]
    [Expr ('if Expr Expr Expr)]
    [Expr ('letrec ([Var Type Expr] ...) Expr)]
    [Expr ('tuple Expr ...)]
    [Expr ('tupref Int Int Expr)]
    [Expr (Prim Expr ...)]

    ;; Adding side-effects for WaveScript.
    [Expr ('begin Expr ...)]
    [Expr ('set! Var Expr)]

    ,@(map (lambda (entry) `[Prim (quote ,(car entry))])
	   ;; Remove dbg from the list... we handle that special:
	regiment-primitives)

    [Int ,integer?]
    [Float ,flonum?]
    [String ,string?]

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
    [Expr ('ext-ref ExtTok ExtInd)]
    [Expr ('ext-set! ExtTok ExtInd Expr)]

    [ExtTok Token]
    [ExtInd Var]

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
    ;; [2006.05.03] DebugArg's can contain any constants they want:
    [DebugArg ('quote ,(lambda (_) #t))]
    [DebugArg Expr]
;    [DebugArg ('quote DebugArgConstData)]
;    [DebugArgConstData ,atom?]
;    [DebugArgConstData (DebugArgConstData ...)]

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

    ;; This breaks the non-backtracking system by having a non-unique head:
    ;[Token StaticToken]
    ;[StaticToken ('tok TokName ,integer?)]

    [Token DynToken]
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

       ;; For now the reference index can be either a variable name or a NUMBER:
       ;; Not sure if I'm going to keep this option around, it allows
       ;; referencing the stored variables within a token object by *position*.     
       [ExtInd Num]
       ;; Further, we allow arbitrary expressions to compute the token name.
       [ExtTok Expr]

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
  

