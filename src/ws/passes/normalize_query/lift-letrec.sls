#!r6rs

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
;;;;   too.. I'm just being really, umm, lazy right now, as my test
;;;;   programs don't depend on argument evaluation model... )   <br>


;;;; Output Language                                             <br><br>
;;; OLD OLD OLD:
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

(library (ws passes normalize_query lift-letrec)
  (export lift-letrec)
  (import (except (rnrs (6)) error) (ws common)
	  )
  
;(define lift-letrec-grammar ...)

;; NOTE: DEPENDS ON LAZINESS.
;; WILL LIFT AN EXPRESSION OUT OF A CONDITIONAL!

(define lift-letrec
  (let ()

    ;; .returns expression & bindings
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
;	   (if (not (null? (difference free formalexp)))
;	       (error 'lift-letrec "free was supposed to be null for now! ~a" free))

	   ;; This version lifts to the top of each lambda:
	   (values `(lambda ,formalexp ,types (lazy-letrec ,body-decl ,body)) '())
	   ;; This version lifts all the way to the top.
	   ;(values `(lambda ,formalexp ,types (lazy-letrec () ,body)) body-decl)
	   ]

	  [(free ,_ ,[e decls]) (values e decls)]
	  [(begin ,[e* decls*] ...) (values `(begin ,e* ...) (apply append decls*))]
	  [(for (,i ,[s sdecl] ,[e edecl]) ,[bod bdecl])
	   (values `(for (,i ,s ,e) ,bod) (append sdecl edecl bdecl))]
	  [(set! ,v ,[e decls]) (values `(set! ,v ,e) decls)]
	   
	  [(letrec ([,lhs* ,type* ,[rhs* rhs-decl*]] ...)  ,[body body-decl])
	   (values body
		   (append (apply append rhs-decl*)
			   (map list lhs* type* rhs*)
			   body-decl))]


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
	     [(,input-language (quote (program ,body ,meta* ... ,type)))
	      (mvlet ([(body body-fn) (process-expr body)])
		     `(lift-letrec-language
		       '(program (lazy-letrec ,body-fn ,body) ,meta* ... ,type)
		       ))]))
    ))
  
) ; End module
