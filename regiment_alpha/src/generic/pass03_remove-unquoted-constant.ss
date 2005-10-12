;;; Pass 2: remove-unquoted-constant

;;; This pass replaces unquoted constants with equivalent quoted
;;; expressions, e.g., -17 => '17 and #t => '#t.

;;; The input language is the same as the output language of Pass 3.

;;; Output from this pass is in the same language, except that
;;; there are no unquoted constants:

;;; OUTPUT LANG:

;;; <Pgm>  ::= (<language-name> (quote (program <Exp>)))
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= 
;;;            (quote <datum>)
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (lambda <Formalexp> <Exp>)
;;;          | (letrec (<Decl>*) <Exp>)
;;;          | (<primitive> <Exp>*)
;;; <Formalexp> ::= (<var>*)

;;; The implementation requires constant? and scheme-primitive? from
;;; helpers.ss.

;;; We construct begin expressions directly, without make-begin, since
;;; we can't introduce any new nested begin expressions and assume
;;; that the input does not contain them either.

(define remove-unquoted-constant
  (let ()
    (define process-expr
      (lambda (expr)
        (match expr
          [,const
            (guard (constant? const))
            `(quote ,const)]
          [(quote ,datum) `(quote ,datum)]
          [,var (guard (symbol? var)) var]
          [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(lambda ,formals ,[body])  `(lambda ,formals ,body)]
          [(letrec ([,lhs* ,[rhs*]] ...) ,[body]) `(letrec ([,lhs* ,rhs*] ...) ,body)]
          [(,prim ,[rand*] ...)
           (guard (regiment-primitive? prim))
           `(,prim ,rand* ...)]
	  ;; Adding normal applications because the static elaborator will get rid of them.
	  [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
          [,unmatched
            (error 'remove-unquoted-constant "invalid expression: ~s"
                   unmatched)])))
    (lambda (expr)
      (match expr
	[(,input-language (quote (program ,body)))
	 (let ([body (process-expr body)])
	   `(,input-language '(program  ,body)))]))))

