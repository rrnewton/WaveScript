;;; Pass X??: verify-core

;;; This pass verifies that the input is in the simplified core
;;; language.  (Uber simplified!)


;;; <Pgm>  ::= <Let>
;;; <Let>  ::= (let (<Decl>*) <var>)
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= (quote <datum>)
;;;          | <var>
;;;          | (if <var> <var> <var>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <var>*)
;;; <Formalexp> ::= (<var>*)

;; No variable capture is allowed at this point.

;;; The implementation requires constant?, datum?, keyword?,
;;; blanko-primitive?, set?, formalexp?, get-formals, and the list
;;; blanko-primitives from helpers.ss.

(define verify-core 
  (let ()

    (define (process-let expr env)
      (match expr
	 [(let ([,lhs* ,[process-expr -> rhs*]] ...) ,expr)	   
	   (guard (not (memq 'let env))
                  (andmap symbol? lhs*)
                  (set? lhs*))
	   (if (ormap (lambda (s) (memq s env)) lhs*)
	       (error 'verify-core "no variable	capture at this point."))
	   #t]))

    (define process-expr
      (lambda (expr env)
        (match expr
          [,const (guard (constant? const)) #t]
          [(quote ,datum)
           (guard (not (memq 'quote env)) (datum? datum))
	   #t]
          [,var (guard (symbol? var))
		(if (not (memq var env))
		    (error 'verify-core (format "unbound variable: ~a~n" var)))		
		#t]

          [(lambda ,formalexp ,[process-let -> expr])
           (guard (list? formalexp) 
		  (andmap symbol? formalexp)
             	;(formalexp? formalexp)
                  (not (memq 'lambda env)))
	   #t]

          [(if ,test ,conseq ,altern)
           (guard (not (memq 'if env)))	   	  	
	   ;; This is very restrictive.... 
	   (andmap (lambda (s) 
		     (and (symbol? s) 
			  (process-expr s env)))
		   (list test conseq altern))
	   #t]

;          [(,keyword ,form* ...)
;           (guard (not (memq keyword env))
;                  (keyword? keyword))
;           (error 'verify-scheme "invalid syntax ~s" `(,keyword ,form* ...))]
                    
          [(,prim ,rand* ...)
           (guard ;(>= (snet-optimize-level) 2)
                  (not (memq prim env))
                  (blanko-primitive? prim)
		  (andmap symbol? rand*)
		  (andmap (lambda (x) (process-expr x env)) rand*))
	   ;          (check-primitive-numargs prim rand*)
	   #t]

          [,unmatched
	   (error 'verify-core "invalid syntax ~s" unmatched)])))

    (lambda (expr)
      (display "Running on ") (display expr) (newline)
      
      (match expr
	     ;; Doesn't change the input language... 
        [(,input-language (quote (program ,body)))

           (let ([body (process-expr body '())])
             `(,input-language '(program ,body)))]))))  
  