

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


(define desugar-let-stored
  (let ()

(define (free-vars e)
  (match e
    [,const (guard (constant? const)) '()]
    [(quote ,const) '()]
    [,var (guard (symbol? var)) (list var)]
    [(tok ,tok ,[expr]) expr]
    [(ext-ref ,tok ,var) '()]
    [(ext-set! ,tok ,var ,[expr]) expr]
    [(begin ,[xs] ...) (apply append xs)]
    [(if ,[test] ,[conseq] ,[altern]) (append test conseq altern)]
    [(let ( (,lhs ,[rhs])) ,[body])
     (append rhs (remq lhs body))]
    [(leds ,what ,which) '()]
    [(,prim ,[rands] ...) 
	 (guard (or (token-machine-primitive? prim)
		    (basic-primitive? prim)))     
	 rands]
    [(,[rator] ,[rands] ...) `(apply append rator rands)]
    [,otherwise
     (error 'cps-tokmac:freevars 
	    "bad expression: ~s" otherwise)]))


(define process-expr 
  (lambda (env )
    (lambda (expr)
      (match expr
	[(quote ,const)                    (values () `(quote ,const))]
	[,num (guard (number? num))        (values () num)]
	[(tok ,t ,n) (guard (number? n))   (values () `(tok ,t ,n))]
	[(tok ,t ,[e])                     (values () `(tok ,t ,e))]
	[,var (guard (symbol? var))        (values () var)]
	[(begin ,[st xs] ...)                 
	 (values (apply append st) `(begin ,xs ...))]
	[(if ,[test] ,[conseq] ,[altern])  
	 (values (append test conseq altern) 
		 `(if ,test ,conseq ,altern))]
	[(let ([,lhs ,[rst rhs]]) ,body)
	 (mvlet ([(bst newbod) ((process-expr (cons lhs env) ) body)])
	    (values (append bst rst)
		    `(let ([,lhs ,rhs]) ,newbod)))]
	[(,call-style ,[st* args*] ...)
	 (guard (memq call-style '(call timed_call)))
	 (values (apply append st*)
		 `(,call-style ,args* ...))]

	;; The semantics of let-stored are that the first time the
	;; expression is executed (and only the first), the RHS is
	;; evaluated and stored.
	[(let-stored ([,lhs ,[rst rhs]]) ,body)
	 `(let ([lhs (void)])
	    (if ____
		(set! lhs ,rhs))
	    ,((process-expr (cons lhs env)) body))]
	
	[(leds ,what ,which) (values () `(leds ,what ,which))]
	[(,prim ,[rst* rands] ...)
	 (guard (or (token-machine-primitive? prim)
		    (basic-primitive? prim)))
	  (values (apply append rst*)
		  `(,prim ,rands ...))]
;;;;;; FINISH:

	[(,[rst1 rator] ,[rst* rands] ...)
	 (warning 'FOOBAR-pass
		  "arbitrary application of rator: ~s" rator)	      
	 `(,rator ,rands ...)]

	[,otherwise
	 (error 'cleanup-token-machine:process-expr 
		"bad expression: ~s" otherwise)]
	))))

(define (process-tokbind tb)
  (mvlet ([(tok id args stored constbinds body) (destructure-tokbind tb)])
    (mvlet ([(newstored newbod) ((process-expr (map car constbinds)) body)])
       `[,tok ,id ,args (stored ,@stored ,@newstored) ,newbod])))


(lambda (prog)
  (match prog
    [(,lang '(program (bindings ,constbinds ...)
		      (nodepgm (tokens ,[process-tokbind -> toks] ...))))
     `(,lang '(program (bindings ,constbinds ...)
		       (nodepgm (tokens ,toks ...))))]))
))
     

     
     
     

