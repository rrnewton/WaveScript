;;; Pass 00: verify-regiment

;;; This pass verifies that the input is in the regiment lanuguage.

;;; <Pgm>  ::= <Exp>
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= 
;;;            (quote <datum>)
;;;          | <constant>
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (lambda <Formalexp> <Exp>)
;;;          | (let (<Decl>*) <Exp>)
;;;          | (<primitive> <Exp>*)
;;; <Formalexp> ::= (<var>*)

;; No variable capture is allowed at this point.

;;; The implementation requires constant?, datum?, keyword?,
;;; regiment-primitive?, set?, formalexp?, get-formals, and the list
;;; regiment-primitives from helpers.ss.



(define verify-regiment
  (let ()

    (define (process-let expr env)
      (match expr
))

    (define process-expr
      (lambda (expr env)
        (match expr
          [,const (guard (constant? const)) const]
          [(quote ,datum)
	   (guard (not (memq 'quote env)) (datum? datum))
	   `(quote ,datum)]
          [,var (guard (symbol? var))
		(if (not (memq var env))
		    (error 'verify-core (format "unbound variable: ~a~n" var))
		    var)]

          [(lambda ,formalexp ,[expr])
           (guard (list? formalexp) 
		  (andmap symbol? formalexp)
		  (set? formalexp)
                  (not (memq 'lambda env)))
	   `(lambda ,formalexp ,expr)]

          [(if ,[test] ,[conseq] ,[altern])
           (guard (not (memq 'if env)))	
	   `(if ,test ,conseq ,altern)]

;          [(,keyword ,form* ...)
;           (guard (not (memq keyword env))
;                  (keyword? keyword))
;           (error 'verify-scheme "invalid syntax ~s" `(,keyword ,form* ...))]

	  [(let ([,lhs* ,[process-expr -> rhs*]] ...) ,expr)	   
	   (guard (not (memq 'let env))
                  (andmap symbol? lhs*)
                  (set? lhs*))
	   (if (ormap (lambda (s) (memq s env)) lhs*)
	       (error 'verify-core "no variable	capture at this point."))
	   #t]
                    
          [(,prim ,rand* ...)
           (guard ;(>= (snet-optimize-level) 2)
                  (not (memq prim env))
                  (regiment-primitive? prim)
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
	 (process-expr body '())])
      )))


(define these-tests
  '( 
    [(verify-core '(some-lang '(program 3))) #t]
    [(verify-core '(some-lang '(program 3))) #t]
    ))

(define (test-this)
  (let ((tests (map car these-tests))
	(intended (map cadr these-tests)))
    (let ((results (map eval tests)))
      (display "Testing pass to verify simplified regiment core language.")
      (newline)
      (newline) (display "Here are intended results:") (newline)
      (write intended) (newline) (newline)
      (newline) (display "Here are actual results:") (newline)
      (write results) (newline) (newline)
      (equal? intended results))))


