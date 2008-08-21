#!r6rs

;;; Pass 06: uncover-free

;;; [2004.04.24] NOTE this pass has been resurrected for my new
;;; compiler.  Right now I'm allowing *no* free
;;; variables in lambda. *Simplicity* is the essence.

;;;------------------------------

(library (ws passes normalize_query uncover-free)
  (export uncover-free uncover-free-grammar1)
  (import (except (rnrs (6)) error) (ws common) 
	  (ws passes normalize_query remove-complex-constant))  

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
    [(,lang '(program ,[Expr -> result] ,meta* ... ,type))
     (match result 
       [#(,bod ,free)
	(ASSERT null? free)
	`(uncover-free-language 
	  '(program ,bod ,meta* ... ,type))])]))

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

) ; End module
