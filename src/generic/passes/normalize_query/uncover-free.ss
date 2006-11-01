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
	    [rhs* (map (lambda (v)
			 `(free ,(vector-ref v 1)
				,(vector-ref v 0)))
		    results)]
	    [free* (map (lambda (v) (vector-ref v 1)) results)]
	    [free (apply union free*)])
       (vector (reconstr vars types rhs*) ;; PLUS ANNOTS?
	       (difference free vars)))     )
(define (uncover-free-Fuser ls k)
  (match ls
    [(#(,expr* ,free*) ...)
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

#;
(define uncover-free
  (let ()

    (define process-lambdaclause
      (lambda (formalexp types body)
        (mvlet ([(body body-free*) (process-expr body)])
          (let ((free* (difference body-free* (get-formals formalexp))))
            (values
	     `(,formalexp ,types (free ,free* ,body))
	     free*)))))
    (define process-expr
      (lambda (expr)
        (match expr
          [(quote ,imm) (values `(quote ,imm) '())]
          [,var (guard (symbol? var) (not (regiment-constant? var)))
		(values var (list var))]

	  [(lambda ,formalexp ,types ,body) 
           (mvlet ([(clause free*) (process-lambdaclause formalexp types body)])
	     ;;;; TODO: TEMPORARY RESTRICTION:
		  (if (not (null? free*))
		      (error 'uncover-free "No free variables in lambda's allowed right now! ~a were free in:\n ~a" free* expr))
		  (values `(lambda ,@clause) free*))]

          [(if ,[test test-free*] ,[conseq conseq-free*] ,[altern altern-free*])
           (values
             `(if ,test ,conseq ,altern)
             (union test-free* (union conseq-free* altern-free*)))]

	  [(tuple ,[args args-free] ...)
	   (values `(tuple ,args ...)
		   (apply append args-free))]
	  [(tupref ,n ,m ,[x free]) (values `(tupref ,n ,m ,x) free)]

          [(letrec ([,lhs* ,type* ,[rhs* rhs-free*]] ...) ,[body body-free*])
           (values
             `(letrec ([,lhs* ,type* ,rhs*] ...) ,body)
             (difference (union (apply union rhs-free*) body-free*) lhs*))]

	  [(letrec ,foo ,boo)
	   (error 'bad-letrec "~a" foo)]

	  [,prim (guard (regiment-constant? prim)) (values prim '())]
          [(,prim ,[rand* rand-free*] ...)
           (guard (regiment-primitive? prim)) ;; Used to be extended-scheme-primitive?
           (values
             `(,prim ,rand* ...)
             (apply union rand-free*))]
          [,unmatched
            (error 'uncover-free "invalid expression: ~s"
                   unmatched)])))
   
    (lambda (prog)
      (match prog
        [(,input-language (quote (program ,body ,type)))
	 (mvlet ([(body body-free*) (process-expr body)])
		`(uncover-free-language ;uncover-free-language
		  '(program ,body ,type)))]))))

#;
;; [2006.05.02] Rewriting this to use the common free-vars routine.
(define uncover-free
  (let ()
    
    (define process-expr
      (lambda (expr)
        (match expr
          [(quote ,datum)  `(quote ,datum)]
          [,var (guard (symbol? var)) var]
          [(if ,[test] ,[conseq] ,[altern])
	   `(if ,test ,conseq ,altern)]
          [(lambda ,formals ,types ,body)
	   `(lambda ,formals ,types 
		    (free ,(regiment-free-vars `(lambda ,formals ,types ,body))
			  ,(process-expr body)))]
          [(letrec ([,lhs* ,type* ,[rhs*]] ...) ,[body])
	   `(letrec ([,lhs* ,type* ,rhs*] ...) ,body)]
          [(,prim ,[rand*] ...)
           (guard (regiment-primitive? prim))
           `(,prim ,rand* ...)]
	  ;; Shouldn't happen post-elaboration:
;	  [(app ,[rator] ,[rand*] ...) `(app ,rator ,rand* ...)]
          [,unmatched
            (error 'reduce-primitives "invalid expression: ~s"
                   unmatched)])))
    
    (lambda (prog)
      (match prog
        [(,input-language (quote (program ,body ,type)))
	 `(uncover-free-language ;uncover-free-language
	   '(program ,(process-expr body) ,type))]))))
