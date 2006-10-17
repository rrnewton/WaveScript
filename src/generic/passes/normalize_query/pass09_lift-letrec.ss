
;;;; .title Lift letrec

;;;;  [2004.04.24] RESURECTED: Now this pass serves a different
;;;;   function.  Convert closure hasn't been run (lambda's may still
;;;;   have free-vars) so it just sucks all those letrec bindings up
;;;;   to the top of the local lambda *but no further*.  <br><br>

;;;; [2006.02.18] Update. <br>
;;;;   This pass now sucks letrec bindings as far upward as their
;;;;   free-vars will allow.
;;;; <br><br>

;;;; ALSO:                                                       <br>
;;;;   The pass outputs *lazy-letrec* instead of letrec, because now
;;;;   things have been scrambled and the lazy semantics of my language
;;;;   can no longer be ignored.  (Other passes should have this
;;;;   too.. I'm just being really, umm, lazy right now, is my test
;;;;   programs don't depend on argument evaluation model... )   <br>


;;;; Output Language                                             <br><br>

;;; <Pgm>  ::= (<language-name> (quote (program <Let>)))        <br>
;;; <Let>  ::= (lazy-letrec (<Decl>*) <Exp>)                    <br> 
;;; <Decl> ::= (<var> <Exp>)                                    <br> 
;;; <Exp>  ::=                                                  <br> 
;;;            (quote <imm>)                                    <br> 
;;;          | <var>                                            <br> 
;;;          | (if <Exp> <Exp> <Exp>)                           <br> 
;;;          | (lambda <Formalexp> <Let>)                       <br> 
;;;          | (<primitive> <Exp>*)                             <br> 
;;; <Formalexp> ::= (<var>*)                                    <br> 



;;--------------------------------------------------------------------
;;; OLD COMMENTS

;;; This pass makes all lambda bindings global, i.e., places them into
;;; a single top-level letrec expression that contains the remainder of
;;; the transformed program.  This transformation is possible since the
;;; lambda expressions no longer have free variables.

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

	  [(tuple ,[args args-decls] ...)
	   (values `(tuple ,args ...)
		   (apply append args-decls))]
	  [(tupref ,n ,m ,[x decls]) (values `(tupref ,n ,m ,x) decls)]

	  [(lambda ,formalexp ,types (free ,free ,[body body-decl]))
	   (if (not (null? free)) 
	       (error 'lift-letrec "free was supposed to be null for now! ~a" free))

	   ;; This version lifts to the top of each lambda:
	   (values `(lambda ,formalexp ,types (lazy-letrec ,body-decl ,body)) '())
	   ;; This version lifts all the way to the top.
	   ;(values `(lambda ,formalexp ,types (lazy-letrec () ,body)) body-decl)
	   ]
	   
	  [(letrec ([,lhs* ,type* ,[rhs* rhs-decl*]] ...)  ,[body body-decl])
	   (values body
		   (append (apply append rhs-decl*)
			   body-decl
			   (map list lhs* type* rhs*)))]


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
	     [(,input-language (quote (program ,body ,type)))
	      (mvlet ([(body body-fn) (process-expr body)])
		     `(lift-letrec-language
		       '(program (lazy-letrec ,body-fn ,body) ,type)
		       ))]))
    ))
