
;;; Pass 10: deglobalize
;;; April 2004
;===============================================================================

;;; This pass represents the biggest jump in the compiler.  It
;;; transforms my simplified global language into a local program to
;;; be run in each node in the sensor network.


;;; Input grammar:

;;; <Pgm>  ::= <Let>
;;; <Let>  ::= (lazy-letrec (<Decl>*) <var>)
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= <Simple>
;;;          | (if <Simple> <Simple> <Simple>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <Simple>*)
;;; <Formalexp> ::= (<var>*)
;;; <Simple> ::= (quote <Lit>) | <Var>

;;; Output grammar:
;;; We introduce a simple imperative language here.
;;;

;;;  <Pgm> ::= (program <SOCPgm> <NodePgm>)
;;;  <SOCPgm> ::= <Statement*>
;;;  <NodePgm> ::= (<SymBinding>*)
;;;  <SymBinding> ::= (<SymPattern> <Code>)
;;;  <Code> ::= <Statement>*
;;;  <Statement> ::= <Stuff> | (broadcast <Sym> <SymPattern>)


(define deglobalize
  (let ()
    (define (simple? x) 
      (match x
          [(quote ,imm) #t]
          [,var (guard (symbol? var)) #t]
	  [,otherwise #f]))
    (define process-letrec
      (lambda (expr)
        (match expr
          [(lazy-letrec ([,lhs* ,rhs*] ...) ,body)1
	   (if (simple? body)
	       `(lazy-letrec ([,lhs* ,rhs*] ...) ,body)
	       (let ([main (unique-name 'result)])
					;(code-name 'main)]) ;; Old version used code-name for labels...
		 `(lazy-letrec ([,lhs* ,rhs*] ...
				[,main ,body])  ;(lambda () ,body))])
			       ,main)))])))

    (define process-expr
      (lambda (expr)
        (match expr
	  [,x (guard (simple? x)) x]
          [(if ,[test] ,[conseq] ,[altern])
	   `(if ,test ,conseq ,altern)]
	  [(lambda ,formalexp ,[process-letrec -> body])
	   `(lambda ,formalexp ,body)]
          [(,prim ,[rand*] ...) (guard (regiment-primitive? prim))
	   `(,prim ,rand* ...)]
          [,unmatched
            (error 'lift-letrec "invalid expression: ~s"
                   unmatched)])))

    (lambda (prog)
      (match prog
        [(,input-language (quote (program ,[process-letrec -> body])))
	 ;; This pass uses the same language as the prior pass, lift-letrec
	 `(,input-language '(program ,body))]))))
