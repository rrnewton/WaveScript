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


(define remove-unquoted-constant-grammar
  initial_regiment_grammar)


(define-pass remove-unquoted-constant
  [OutputGrammar remove-unquoted-constant-grammar]
  [Expr (lambda (x fallthrough)
	  (match x
	    [,const (guard (constant? const))
		    `(quote ,const)]	      
	    [,other (fallthrough other)]))]
  [Program (lambda (prog Expr)
	     (unique-name-counter 0)
	     (match prog
	       [(,input-language (quote (program ,body ,type)))
		`(remove-unquoted-constant-language 
		  '(program ,(Expr body) ,type))]))])
