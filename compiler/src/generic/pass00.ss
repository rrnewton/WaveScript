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

;; Where let behaves like scheme's letrec.

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
            (if (not (memq var env))
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
          
	  [(let ([,lhs* ,rhs*] ...) ,expr)
	   (guard (not (memq 'let env))
                  (andmap symbol? lhs*)
                  (set? lhs*))
	   (let ((newenv (union lhs* env)))
	     (let ((rands (map (lambda (r) (process-expr r newenv)) rhs*))
		   (body  (process-expr expr newenv)))
	       `(let ([,lhs* ,rands] ...) ,body)))]
          
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
        [(,input-language (quote (program ,body)))
         (let ([body (process-expr body '())]) 
           ;; Doesn't change the input language... 		
           `(,input-language '(program ,body)))]))
    ))


;==============================================================================


(define test-programs 
  '( 3
     (let ((a (anchor '(30 40))))
       (let ((r (circle 50 a))
	     (f (lambda (tot next)
		  (cons (+ (car tot) (sense next))
			(+ (cdr tot) 1))))
	     (g (lambda (tot) (/ (car tot) (cdr tot)))))
	 (smap g (rfold f (cons 0 0) r))))

     (let ((r (circle-at 50 '(30 40)))
	   (f (lambda (tot next)
		(cons (+ (car tot) (sense next))
		      (+ (cdr tot) 1))))
	   (g (lambda (tot) (/ (car tot) (cdr tot)))))
       (let ((avg (smap g (rfold f (cons 0 0) r))))
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
     

(define test-this
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

;==============================================================================

