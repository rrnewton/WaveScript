;;; Pass 00: verify-regiment

;;; This pass verifies that the input is in the regiment lanuguage.
;;; It also wraps the program in a trivial '(<lang> (program <Exp>)) form.

;;; <Pgm>  ::= <Exp>
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= 
;;;            (quote <datum>)
;;;          | <constant>
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (lambda <Formalexp> <Exp>)
;;;          | (letrec (<Decl>*) <Exp>)
;;;          | (<primitive> <Exp>*)
;;; <Formalexp> ::= (<var>*)

;;; And in the output
;;; <Pgm>  ::= (<language> '(program <Exp>))


;; No variable capture is allowed at this point.

;;; The implementation requires constant?, datum?, keyword?,
;;; regiment-primitive?, set?, formalexp?, get-formals, and the list
;;; regiment-primitives from helpers.ss.


(define verify-regiment
  (let ()
    
    (define process-expr
      (lambda (expr env)
					;        (disp "processing expr" expr env)
        (match expr
          [,const (guard (constant? const)) const]
          [(quote ,datum)
	   (guard (not (memq 'quote env)) (datum? datum))
	   `(quote ,datum)]
          [,var (guard (symbol? var))
            (if (and (not (memq var env))
		     (not (regiment-primitive? var)))
                (error 'verify-regiment (format "unbound variable: ~a~n" var))
                var)]
          
          [(lambda ,formalexp ,expr)
           (guard (list? formalexp) 
		  (andmap symbol? formalexp)
		  (set? formalexp)
                  (not (memq 'lambda env)))         
	   `(lambda ,formalexp ,(process-expr expr (union formalexp env)))]
          
          [(if ,[test] ,[conseq] ,[altern])
           (guard (not (memq 'if env)))
	   `(if ,test ,conseq ,altern)]
          
          ;          [(,keyword ,form* ...)
          ;           (guard (not (memq keyword env))
          ;                  (keyword? keyword))
          ;           (error 'verify-scheme "invalid syntax ~s" `(,keyword ,form* ...))]
          
	  [(letrec ([,lhs* ,rhs*] ...) ,expr)
	   (guard (not (memq 'letrec env))
                  (andmap symbol? lhs*)
                  (set? lhs*))
	   (let ((newenv (union lhs* env)))
	     (let ((rands (map (lambda (r) (process-expr r newenv)) rhs*))
		   (body  (process-expr expr newenv)))
	       `(letrec ([,lhs* ,rands] ...) ,body)))]
          
          [(,prim ,[rand*] ...)
           (guard ;(>= (snet-optimize-level) 2)
            (not (memq prim env))
            (regiment-primitive? prim))
	   ;      (check-primitive-numargs prim rand*)
	   `(,prim ,rand* ...)]
          
          [,unmatched
            (error 'verify-regiment "invalid syntax ~s" unmatched)])))
    
    (lambda (expr)
      (match expr	    
	;; The input is already wrapped with the metadata:
        [(,input-language (quote (program ,body)))
         (let ([body (process-expr body '())]) 
           ;; Doesn't change the input language... 		
           `(,input-language '(program ,body)))]
	;; Nope?  Well wrap that metadata:
        [,body
         (let ([body (process-expr body '())])
           ;; Doesn't change the input language... 		
           `(base-language '(program ,body)))]
	))))

;==============================================================================


(define test-programs 
  '( 3

     (letrec ((a (anchor '(30 40))))
       (letrec ((r (circle 50 a))
	     (f (lambda (tot next)
		  (cons (+ (car tot) (sense next))
			(+ (cdr tot) 1))))
	     (g (lambda (tot) (/ (car tot) (cdr tot)))))
	 (smap g (rfold f (cons 0 0) r))))

     (letrec ((r (circle-at 50 '(30 40)))
	      (f (lambda (tot next)
		   (cons (+ (car tot) (sense next))
			 (+ (cdr tot) 1))))
	      (g (lambda (tot) (/ (car tot) (cdr tot)))))
       (letrec ((avg (smap g (rfold f (cons 0 0) r))))
	 (until (when-any (lambda (x) (> x 15.3)) avg)
		R
		(circle-at 100 '(0 0)))))
     ))

(define these-tests
  (map
   (lambda (prog)
     `[(verify-regiment '(some-lang '(program ,prog)))
       (some-lang '(program ,prog))])
   test-programs))
     

(define test-this (default-unit-tester 
		    "Pass00: Pass to verify initial regiment language."
		    these-tests))

#;(define test-this
  (let ((these-tests these-tests))
    (lambda args 
      (let ((verbose (memq 'verbose args)))
	
	(let ((tests (map car these-tests))
	      (intended (map cadr these-tests)))
	  (let ((results (map eval tests)))
	    (if verbose 
		(begin
		  (display "Testing pass to verify initial regiment language.")
		  (newline)
		  (newline) (display "Here are intended results:") (newline)
		  (write intended) (newline) (newline)
		  (newline) (display "Here are actual results:") (newline)
		  (write results) (newline) (newline)))
	   
	    (equal? intended results)))))))
  

(define test00 test-this)
(define tests00 these-tests)

;==============================================================================

