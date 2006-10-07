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
  (build-compiler-pass ;; This wraps the main function with extra debugging
   'rename-var
   `(input)
   `(output) 
   (let ()
     (define process-expr
       (lambda (expr)
	 (core-generic-traverse
	  (lambda (expr fallthrough)
	    (match expr
	      [,const (guard (constant? const))
		      `(quote ,const)]	      
	      [,other (fallthrough other)]))
	  (lambda (ls k) (apply k ls)) ;; fuser
	  expr)))
     ;; Main pass body:
     (lambda (expr)
       (unique-name-counter 0)
       (match expr
	 [(,input-language (quote (program ,body ,type)))
	  (let ([body (process-expr body)])
	    `(,input-language '(program ,body ,type)))])))))



