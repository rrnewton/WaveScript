

;; Input Grammar:

;;;  <Pgm> ::= (program (bindings <Cbind>*) <NodePgm>)
;;;  <NodePgm> ::= (nodepgm (tokens <TokBinding>*))
;       NOTE: tokens will inclide a binding for SOC-start and node-start
;;;  <Cbind> ::= (<var> <Exp>)
;       NOTE: This expressions will be statically calculable -- constants.
;;;  <TokBinding> ::= (<TokName> <SubtokId> (<Var> ...) (bindings <Cbind>*) (stored <Stored>) <Expr>)
;;;  <TokName>   ::= <Symbol> 
;;;  <SubtokId>  ::= <Number>
;;;  <Token>     ::= (tok <TokName>) | (tok <Tokname> <Int>)
;;;  <DynToken>  ::= (tok <Tokname> <Expr>) | <Token>
;;;     NOTE: Either the whole token reference or just the sub-index can be dynamic.
;;;  <Expr>      ::= (quote <Constant>)
;;;                | <Var>
;;;                | <DynToken>
;;;                | (set! <Var> <Expr>)
;;;                | (ext-ref <Token> <Var>)
;;;                | (ext-set! <Token> <Var> <Expr>)
;       NOTE: These are static token refs for now.
;;;                | (begin <Expr> ...)
;;;                | (let ((<Symbol> <Expr>)) <Expr>)
;;;                | (if <Expr> <Expr> <Expr>)
;;;                | (subcall <DynToken> <Expr>...)
;;;                | (<Prim> <Expr> ...)
;;;                | (<Expr> ...)
;;;                | <Sugar> 
;;;                | (leds <Red|Yellow|Green> <On|Off|Toggle>)
;;;  <Prim> ::= <BasicPrim> 
;;;           | call | subcall | timed_call
;;;           | is_scheduled | deschedule | is_present | evict
;;;  <Sugar>  ::= (flood <Expr>)
;;;           | (elect-leader <Token> [<Token>])
              ;; <TODO> optional second argument.. decider


;;;  <GExpr>     ::= (emit <DynToken> <Expr> ...)
;;;                | (return <Expr> (to <DynToken>) (via <DynToken>) (seed <Expr>) (aggr <Token>))
;;;                | (relay <DynToken>) ;; NEED TO ADD RELAY ARGS!
;;;                | (dist <DynToken>)


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






    (define process-expr 
      (lambda (env tokens this-token this-subtok)
	(lambda (expr)
	  ;(define (tokname? t) (memq t tokens))
	  (match expr
	     [(quote ,const) (values () `(quote ,const))]
	     [,num (guard (number? num)) (values () num)]

	     [(tok ,t ,n) (guard (number? n)) `(tok ,t ,n)]
	     [(tok ,t ,[e]) (guard (number? n)) `(tok ,t ,e)]
	     [,var (guard (symbol? var)) var]
	     [(begin ,[xs] ...) `(begin ,xs ...)]
	     [(if ,[test] ,[conseq] ,[altern])
	      `(if ,test ,conseq ,altern)]
	     [(let ([,lhs ,[rhs]]) ,body)
	      `(let ([,lhs ,rhs])		 
		 ,((process-expr (cons lhs env) tokens this-token this-subtok) body))]
	     [(,call-style ,[args*] ...)
	      (guard (memq call-style '(call timed_call)))
	      `(,call-style ,args* ...)]

	     [(let-stored ([,lhs ,[rhs]]) ,body)
	      `(let ([lhs (void)])
		 (if ____
		     (set! lhs ,rhs))
		 ,((process-expr (cons lhs env) tokens this-token this-subtok) body))]
		 
	     [(leds ,what ,which) `(leds ,what ,which)]
	     [(,prim ,[rands] ...)
	      (guard (or (token-machine-primitive? prim)
			 (basic-primitive? prim)))
	      `(,prim ,rands ...)]
	     ;;; TEMPORARY, We allow arbitrary other applications too!
	     [(,[rator] ,[rands] ...)
	      (warning 'FOOBAR-pass
		       "arbitrary application of rator: ~s" rator)	      
	      `(,rator ,rands ...)]
	     [,otherwise
	      (error 'cleanup-token-machine:process-expr 
		     "bad expression: ~s" otherwise)]
	     ))))


