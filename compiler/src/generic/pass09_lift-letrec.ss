
;;; Pass 05: lift-letrec

;;  [2004.04.24] RESURECTED: Now this pass serves a different
;;; function.  Convert closure hasn't been run, so it just sucks all
;;; those letrec bindings up to the top of the local lambda *but no
;;; further*.

;;; ALSO:
;;;   The pass outputs *lazy-letrec* instead of letrec, because now
;;;   things have been scrambled and the lazy semantics of my language
;;;   can no longer be ignored.  (Other passes should have this
;;;   too.. I'm just being really, umm, lazy right now, is my test
;;;   programs don't depend on argument evaluation model... )


;; Output Language

;;; <Pgm>  ::= (<language-name> (quote (program <Let>)))
;;; <Let>  ::= (lazy-letrec (<Decl>*) <Exp>)
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= 
;;;            (quote <imm>)
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <Exp>*)
;;; <Formalexp> ::= (<var>*)




;;--------------------------------------------------------------------
;;; OLD COMMENTS

;;; This pass makes all lambda bindings global, i.e., places them into
;;; a single top-level letrec expression that contains the remainder of
;;; the transformed program.  This transformation is possible since the
;;; lambda expressions no longer have free variables.

;;; (letrec ([map$1
;;;           (lambda (cp.10 f.6 ls.5)
;;;             (bind-free (cp.10 map.1)
;;;               (if (null? ls.5)
;;;                   '()
;;;                   (cons (f.6 (car ls.5))
;;;                         ((label map$1) map.1 f.6 (cdr ls.5))))))])
;;;   (closures ([map.1 (label map$1) map.1])
;;;     (letrec ([f$4
;;;               (lambda (cp.9 x.2)
;;;                 (bind-free (cp.9)
;;;                   (letrec ([anon$7
;;;                             (lambda (cp.8 y.3)
;;;                               (bind-free (cp.8 x.2) (* x.2 y.3)))])
;;;                     (closures ([anon.7 (label anon$7) x.2]) anon.7))))])
;;;       (closures ([f.4 (label f$4)])
;;;         ((label map$1)
;;;          map.1
;;;          ((label f$4) f.4 '7)
;;;          (cons '1 (cons '2 (cons '3 '()))))))))
;;;
;;; becomes
;;;
;;; (letrec ([anon$7
;;;           (lambda (cp.8 y.3) (bind-free (cp.8 x.2) (* x.2 y.3)))]
;;;          [f$4
;;;           (lambda (cp.9 x.2)
;;;             (bind-free (cp.9)
;;;               (closures ([anon.7 (label anon$7) x.2]) anon.7)))]
;;;          [map$1
;;;           (lambda (cp.10 f.6 ls.5)
;;;             (bind-free (cp.10 map.1)
;;;               (if (null? ls.5)
;;;                   '()
;;;                   (cons (f.6 (car ls.5))
;;;                         ((label map$1) map.1 f.6 (cdr ls.5))))))])
;;;   (closures ([map.1 (label map$1) map.1])
;;;     (closures ([f.4 (label f$4)])
;;;       ((label map$1)
;;;        map.1
;;;        ((label f$4) f.4 '7)
;;;        (cons '1 (cons '2 (cons '3 '())))))))

;;; The input language is the same as the output language of Pass 12.

;;; The output language differs in that letrec is found only at the
;;; top level of the program.


;;; <Input> ::= (<language-name> <Program>)
;;; <Program> ::= (program (<var>*) (<pkg>*) <class-def>* <Letrec>)
;;; <Letrec> ::= (letrec ((<var> <Lambda>)*) <Exp>)
;;; <Lambda> ::= (lambda <Formalexp> (bind-free (<var> <var>*) <Exp>))
;;; <Exp>  ::= (quote <imm>)
;;;          | (label <var>)
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (begin <Exp> <Exp>*)
;;;          | (let ((<var> <Exp>)*) <Exp>)
;;;          | (closures ([<var> <Exp> <var>*] ...) <Exp>)
;;;          | (<primitive> <Exp>*)
;;;          | (<Exp> <Exp>*)
;;;          | (toplvl-varref <var>)
;;;          | (toplvl-varassign! <var> <Exp>)
;;; <Formalexp> ::= <var>
;;;               | (<var*>)
;;;               | (<var*> . <var>)

;;; The implementation requires extended-scheme-primitive? from helpers.ss.
;;--------------------------------------------------------------------



(define lift-letrec
  (let ()

    (define process-expr
      (lambda (expr)
        (match expr
          [(quote ,imm) (values `(quote ,imm) '())]
          [,var (guard (symbol? var)) (values var '())]

          [(if ,[test test-decl*] ,[conseq conseq-decl*] ,[altern altern-decl*])
           (values
             `(if ,test ,conseq ,altern)
             (append test-decl* conseq-decl* altern-decl*))]

	  [(lambda ,formalexp (free ,free ,[body body-decl]))
	   (if (not (null? free)) (error 'lift-letrec "free was supposed to be null for now!" free))

	   ;; This version lifts to the top of each lambda:
	   ;(values `(lambda ,formalexp (lazy-letrec ,body-decl ,body)) '())
	   ;; This version lifts all the way to the top.
	   (values `(lambda ,formalexp (lazy-letrec () ,body)) body-decl)
	   ]
	   
	  [(letrec ([,lhs* ,[rhs* rhs-decl*]] ...)  ,[body body-decl])
	   (values body
		   (append (apply append rhs-decl*)
			   body-decl
			   (map list lhs* rhs*)))]

          [(,prim ,[rand* rand-decl*] ...)
           (guard (regiment-primitive? prim))
           (values
             `(,prim ,rand* ...)
             (apply append rand-decl*))]

          [,unmatched
            (error 'lift-letrec "invalid expression: ~s"
                   unmatched)])))

    (lambda (prog)
      (match prog
	     [(,input-language (quote (program ,body)))
	      (mvlet ([(body body-fn) (process-expr body)])
		     `(lift-letrec-language
		       '(program (lazy-letrec ,body-fn ,body))))]))
    ))