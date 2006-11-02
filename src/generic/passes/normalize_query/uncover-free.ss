;;; Pass 06: uncover-free

;;; [2004.04.24] NOTE this pass has been resurrected for my new
;;; compiler.  Right now I'm allowing *no* free
;;; variables. *Simplicity* is the essence.

;;;------------------------------

;;; This pass wraps the body of each lambda expression in a free form
;;; that lists the free variables of the lambda expression.  For
;;; example:
;;;
;;; (let ([x.2 '3] [y.1 '4])
;;;   (letrec ([f.3 (lambda (z.5) (* y.1 z.5))])
;;;     (letrec ([g.4 (lambda () (f.3 (+ x.2 y.1)))])
;;;       (g.4))))
;;;
;;; becomes
;;;
;;; (let ([x.2 '3] [y.1 '4])
;;;   (letrec ([f.3 (lambda (z.5) (free (y.1) (* y.1 z.5)))])
;;;     (letrec ([g.4 (lambda ()
;;;                     (free (f.3 x.2 y.1)
;;;                       (f.3 (+ x.2 y.1))))])
;;;       (g.4))))

;;; The input language is the same as the output language of Pass 10.

;;; The output language differs in that each lambda body is wrapped
;;; in a free form.

;;; NOTE: NO FREE VARS YOU MIGHT NOTICE!

;;; <Pgm>  ::= (<language-name> (quote (program <Exp>)))
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= 
;;;            (quote <imm>)
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (lambda <Formalexp> (free () <Exp>))  
;;;          | (letrec (<Decl>*) <Exp>)
;;;          | (<primitive> <Exp>*)
;;; <Formalexp> ::= (<var>*)


;; There may be multiple versions of uncover-free with different
;; input/output grammars.  Factoring out the transformer functions here.
;; This gets the first shot at the expression.
(define (uncover-free-Expr x fallthrough)
  (match x
    [,v (guard (symbol? v) (not (regiment-constant? v)))
	(vector v `(,v))]
    ;; Now all the binding forms:
    [,other (fallthrough other)]  
    ))
  ;; This catches all binding forms.
(define (uncover-free-Bindings vars types exprs   ;; PLUS ANNOTS?
		 reconstr exprfun ;fallthrough
		 )
     (let* ([results (map exprfun exprs)]
	    [newresults (map (match-lambda (#(,expr ,free))
			       (vector `(free ,free ,expr)
				       (difference free vars)))
			  results)]
	    [free* (map (lambda (v) (vector-ref v 1)) results)]
	    [free (apply union free*)])

       (reconstr vars types newresults)
#;       
       (vector (reconstr vars types newresults) ;; PLUS ANNOTS?
	       (difference free vars))

       ))
(define (uncover-free-Fuser ls k)
  (match ls
    [(#(,expr* ,free*) ...)
;     (pretty-print `(fusing ,expr* ,free*))
     (vector (apply k expr*)
	     (apply union free*)	       
	     )]))
(define (uncover-free-Program p Expr)
  (match p 
    [(,lang '(program ,[Expr -> result] ,type))
     (match result 
       [#(,bod ,free)
	(ASSERT null? free)
	`(uncover-free-language 
	  '(program ,bod ,type))])]))

;; This first version of uncover free is ran directly after
;; remove-complex-constant and before lift-letrec.
(define uncover-free-grammar1
  (cons `[Expr ('free (Var ...) Expr)]
	remove-complex-constant-grammar))

;; [2006.10.31] rewriting with define-pass
;; Assumes variable names are unique.
(define-pass uncover-free    
  [OutputGrammar uncover-free-grammar1]
  [Expr     uncover-free-Expr]
  [Bindings uncover-free-Bindings]
  [Fuser    uncover-free-Fuser]
  [Program  uncover-free-Program]
  )

