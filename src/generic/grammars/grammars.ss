;;;; Tree Grammars for Regiment and TML

;;;; Currently 'include'd into grammar_checker.ss


;;;; WARNING.  Do not refactor these grammars without being VERY
;;;; careful.  The grammars for subsequent passes depend on
;;;; programmatically modifying these basic grammars.

; =======================================================================
;;; Regiment Grammars.

;; This is the type grammar supported by the Regiment source language.
(define type_grammar
  (let ()
    (define (valid-typevar-symbol? s)
      (let ([str (symbol->string s)])
	(and (> (string-length str) 0)
	     (char-lower-case? (string-ref str 0)))))
    `(
      [Type 'Int]
      [Type 'Float]
      [Type 'Complex]
      [Type 'Bool]
      [Type 'String]
      
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
      
      [Type ('HashTable Type)]
      [Type ('List Type)]
      [Type #(Type ...)] ;; Tuples
      [Type (Type ... '-> Type)]
      [Type ('-> Type)]

      ;; Also type vars:
      [Type ('quote ,valid-typevar-symbol?)]
      ;; And NUM variables:
      [Type ('NUM ,valid-typevar-symbol?)]
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

    [Expr Const]
    [Const Int]
    [Const Float]
    [Const String]
    [Const ('quote Datum)]
;    [Const ()]

    [Expr ('if Expr Expr Expr)]
    [Expr ('letrec ([LHS Type Expr] ...) Expr)]
    [Expr ('let ([LHS Type Expr] ...) Expr)]
    [Expr ('tuple Expr ...)]
    [Expr ('tupref Int Int Expr)]

    [Expr (Prim Expr ...)]

    ;; This is a type assertion that constrains the type of an expression:
    [Expr ('assert-type Type Expr)]

    ;; Adding side-effects for WaveScript.
    [Expr ('begin Expr ...)]
    [Expr ('set! Var Expr)]
    [Expr ('for (Var Expr Expr) Expr)]

    ;; Include an entry for each primitive.
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

    ;; At first LHS can be any pattern:
    [LHS Pattern]
    [Pattern Var]
    [Pattern #(Pattern ...)]

    ,@type_grammar
    )))

;; This is the main grammar (minus a few sugars, see below)
(define initial_regiment_grammar
  `( ,@base_regiment_forms
     ;; These are forms only valid for the meta-language (pre-elaboration)
     [Expr ('lambda (LHS ...) (Type ...) Expr)]
     [Expr ('app Expr ...)]  ;; Application.  Should make this labeled.     
    ))
;; The rest of the grammars are defined in the individual pass files.

;; This is the grammar consumed by verify-regiment
(define sugared_regiment_grammar
  `(,@ initial_regiment_grammar
       [Expr ('let* ([Var Type Expr] ...) Expr)]
       [Expr ('or Expr ...)]
       [Expr ('and Expr ...)]
       )
  )
		  
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
  

