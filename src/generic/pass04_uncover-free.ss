;;; Pass 04: uncover-free

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


;;; The implementation requires difference, extended-scheme-primitive?,
;;; union, and get-formals from helpers.ss.

(define uncover-free
  (let ()

    (define process-lambdaclause
      (lambda (formalexp body)
        (mvlet ([(body body-free*) (process-expr body)])
          (let ((free* (difference body-free* (get-formals formalexp))))
            (values
	     `(,formalexp (free ,free* ,body))
	     free*)))))
    
    (define process-expr
      (lambda (expr)
        (match expr
          [(quote ,imm) (values `(quote ,imm) '())]
          [,var (guard (symbol? var)) (values var (list var))]

	  [(lambda ,formalexp ,body) 
           (mvlet ([(clause free*) (process-lambdaclause formalexp body)])
	     ;;;; TODO: TEMPORARY RESTRICTION:
		  (if (not (null? free*))
		      (error 'uncover-free "No free variables allowed right now!: ~a" expr))
		  (values `(lambda ,@clause) free*))]

          [(if ,[test test-free*] ,[conseq conseq-free*] ,[altern altern-free*])
           (values
             `(if ,test ,conseq ,altern)
             (union test-free* (union conseq-free* altern-free*)))]

          [(letrec ([,lhs* ,[rhs* rhs-free*]] ...) ,[body body-free*])
           (values
             `(letrec ([,lhs* ,rhs*] ...) ,body)
             (difference (union (apply union rhs-free*) body-free*) lhs*))]
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
        [(,input-language (quote (program ,body)))
	 (mvlet ([(body body-free*) (process-expr body)])
		`(,input-language ;uncover-free-language
		  '(program ,body)))]))))


