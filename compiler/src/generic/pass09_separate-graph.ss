;;; Pass 09: separate-graph

;;; This pass takes the simple top level progam, which is basically a
;;; dataflow graph, and segregates its edges into anchor edges
;;; (corresponding to anchorship events/tasks), region edges
;;; (corresponding to region membership events/tasks), and other
;;; edges.

;;; Note that the let's *within* lambdas should at this point all be
;;; *other* edges rather than anchor or region edges:

;;; The input language from the last pass is:

;;; <Pgm>  ::= (program <Let>)
;;; <Let>  ::= (lazy-letrec (<Decl>*) <var>)
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= <Simple>
;;;          | (if <Simple> <Simple> <Simple>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <Simple>*)
;;; <Formalexp> ::= (<var>*)
;;; <Simple> ::= (quote <Lit>) | <Var>

;;; And the Output Language is:

;;; <Pgm>  ::= (program (<AnchorDecl>*) (<RegionDecl*>) (<Decl*>) <var>)
;;; <Decl> ::= (<var> <Exp>)
;;; <AnchorDecl> ::= (<var> <Exp>)
;;; <RegionDecl> ::= (<var> <Exp>)
;;; <Let>  ::= (lazy-letrec (<Decl>*) <var>)
;;; <Exp>  ::= <Simple>
;;;          | (if <Simple> <Simple> <Simple>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <Simple>*)
;;; <Formalexp> ::= (<var>*)
;;; <Simple> ::= (quote <Lit>) | <Var>


(define verify-core 
  (let ()


    (lambda (expr)
      (match expr
	     [(,input-language (quote (program ,body)))
	      (process-let body '())])
      )))


