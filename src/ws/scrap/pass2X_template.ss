
;;;  <Pgm> ::= (program (bindings <Cbind>*) <NodePgm>)
;;;  <NodePgm> ::= (nodepgm (tokens <TokBinding>*))
;       NOTE: tokens will inclide a binding for SOC-start and node-start
;;;  <Cbind> ::= (<var> <Exp>)
;       NOTE: These expressions will be statically calculable -- constants.
;;;  <TokBinding> ::= (<TokName> <SubtokId> (<Var> ...) (bindings <Cbind>*) (stored <Stored>) <Expr>)
;;;  <TokName>   ::= <Symbol> 
;;;  <SubtokId>  ::= <Int>
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
;;;           | call | subcall | timed_call
;;;           | is_scheduled | deschedule | is_present | evict



    (define process-expr 
      (lambda (env tokens this-token this-subtok)
	(lambda (expr)
	  ;(define (tokname? t) (memq t tokens))
	  (match expr
	     [(quote ,const) `(quote ,const)]
	     ;; This is for recurring on tokens:
	     [,num (guard (number? num)) num]
	     [(tok ,t ,n) (guard (number? n)) `(tok ,t ,n)]
	     [(tok ,t ,[e])  `(tok ,t ,e)]
	     [(ext-ref ,t ,v) `(ext-ref ,t ,v)]
	     [(ext-set! ,t ,v ,[x]) `(ext-set! ,t ,v ,x)]
	     [,var (guard (symbol? var)) var]
	     [(set! ,v ,[x]) `(set! ,v ,x)]
	     [(begin ,[xs] ...) `(begin ,xs ...)]
	     [(if ,[test] ,[conseq] ,[altern])
	      `(if ,test ,conseq ,altern)]
	     [(let ([,lhs ,[rhs]]) ,body)
	      `(let ([,lhs ,rhs])		 
		 ,((process-expr (cons lhs env) tokens this-token this-subtok) body))]
	     [(,call-style ,[args*] ...)
	      (guard (memq call-style '(emit call timed-call)))
	      `(,call-style ,args* ...)]
	     [(relay (tok ,t ,[e])) `(relay (tok ,t ,e))]	    
	     [(dist (tok ,t ,[e]))  `(dist (tok ,t ,e))]
	     [(return ,[expr]    
		      (to (tok ,to ,[toe]))
		      (via (tok ,via ,[viae]))
		      (seed ,[seed_val])
		      (aggr (tok ,aggr 0)))
	      `(return ,expr
		      (to (tok ,to ,toe))
		      (via (tok ,via ,viae))
		      (seed ,seed_val)
		      (aggr (tok ,aggr 0)))]
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
	      (error 'FOOBAR-pass:process-expr 
		     "bad expression: ~s" otherwise)]
	     ))))
