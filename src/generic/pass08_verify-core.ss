;;; Pass X??: verify-core

;;; This pass verifies that the input is in the simplified core
;;; language.  (Uber simplified!)


;;; <Pgm>  ::= <Let>
;;; <Let>  ::= (lazy-letrec (<Decl>*) <var>)
;;; <Decl> ::= (<var> <Exp>)
;;; <Exp>  ::= <Simple>
;;;          | (if <Simple> <Simple> <Simple>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <Simple>*)
;;; <Formalexp> ::= (<var>*)
;;; <Simple> ::= (quote <Lit>) | <Var>

;; Where let really behaves like letrec.

;; No variable capture is allowed at this point.

;;; The implementation requires constant?, datum?, keyword?,
;;; regiment-primitive?, set?, formalexp?, get-formals, and the list
;;; regiment-primitives from helpers.ss.

(define verify-core 
  (let ()

    (define (process-let expr env)
      (match expr
	 [(lazy-letrec ([,lhs* ,rhs*] ...) ,expr)
	   (guard (not (memq 'let env))
                  (andmap symbol? lhs*)
		  (set? lhs*) ;; No duplicate lhs's ..
		  )
	   (if (ormap (lambda (s) (memq s env)) lhs*)
	       (error 'verify-core "no variable	capture at this point."))
	   (let ((newenv (union lhs* env)))
	     (and (andmap (lambda (r) (process-expr r newenv)) rhs*)
		  (process-expr expr newenv)))]))

    (define (simple-rand? expr)
      (match expr
	     [,var (guard (symbol? var)) #t]
	     [(quote ,const) (guard (constant? const)) #t]
	     [,else #f]))

    (define process-expr
      (lambda (expr env)
        (match expr
;          [,const (guard (constant? const)) #t]
          [(quote ,const)
           (guard (not (memq 'quote env)) (constant? const))
	   #t]
	 
          [,var (guard (symbol? var))
		(if (not (memq var env))
		    (error 'verify-core (format "unbound variable: ~a~n" var)))
	       		#t]

          [(lambda ,formalexp ,expr)
           (guard (list? formalexp)
		  (andmap symbol? formalexp)
             	;(formalexp? formalexp)
                  (not (memq 'lambda env)))
	   (process-let expr (union formalexp env))]

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
                  (regiment-primitive? prim)
		  (andmap (lambda (x) (process-expr x env)) rand*)
		  (andmap simple-rand? rand*))
	   ;          (check-primitive-numargs prim rand*)
	   #t]

          [,unmatched
	   (error 'verify-core "invalid syntax ~s" unmatched)])))

    (lambda (expr)
					;      (display "Running on ") (display expr) (newline)      
      (match expr
	     ;; Doesn't change the input language... 
        [(,input-language (quote (program ,body)))
	 (process-let body '())])
      )))

;==============================================================================

(define test-programs   
  '( 
    (let ((a '3)) a)

    (let ((tmp (cons '40 '()))
	  (loc (cons '30 tmp))
	  (a (anchor loc))
	  (r (circle '50 a))
	  (f (lambda (tot next)
	       (let ((sum (car tot))
		     (cnt (cdr tot))
		     (sns (sense next))
		     (newsum (+ sum sns))
		     (newcnt (+ cnt '1))
		     (res (cons newsum newcnt)))
		 res)))
	  (g (lambda (tot) 
	       (let ((sum (car tot))
		     (cnt (cdr tot))
		     (res (/ sum cnt)))
		 res)))	    
	  (start (cons '0 '0))
	  (S (rfold f start r))
	  (avg (smap g S)))
      avg)
    ))


;========================================

(define these-tests
  (map
   (lambda (prog)
     `[(verify-core '(some-lang '(program ,prog))) #t])
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
		  (display "Testing pass to verify the simplifed core language.")
		  (newline)
		  (newline) (display "Here are intended results:") (newline)
		  (write intended) (newline) (newline)
		  (newline) (display "Here are actual results:") (newline)
		  (write results) (newline) (newline)))
	   
	    (equal? intended results)))))))  

(define test07 test-this)
(define tests07 these-tests)

;==============================================================================

