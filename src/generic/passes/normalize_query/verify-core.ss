;;; Pass: verify-core

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

;; No variable capture is allowed at this point!! Ouch, that's a restriction!

;; [2006.02.17] TODO: This should also enforce the stronger
;; (type-based) restrictions that we have considered, since the types
;; are now fully available.

(module verify-core mzscheme
  (require "../../../plt/common.ss")
  (provide verify-core test-verify-core)
  (chezimports)
  
(define verify-core 
  (let ()

    (define (process-let expr env)
      (match expr
	 [(lazy-letrec ([,lhs* ,type* ,rhs*] ...) ,expr)
	   (guard (not (memq 'lazy-letrec env))
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
	     [(quote ,const) (guard (or (constant? const) (symbol? const))) #t]
	     [,else #f]))

    (define process-expr
      (lambda (expr env)
        (match expr
;          [,const (guard (constant? const)) #t]
          [(quote ,const)
	   ;; [2006.02.10] Currently we're still allowing symbols here:
           (guard (not (memq 'quote env)) 
		  (or (constant? const) (symbol? const)))
	   #t]
	 
          [,var (guard (symbol? var) (not (regiment-constant? var)))
		(if (and (not (memq var env))
			 (not (regiment-constant? var)))
		    (error 'verify-core (format "unbound variable: ~a~n" var)))
	       		#t]

          [(lambda ,formalexp ,types ,expr)
           (guard (list? formalexp)
		  (andmap symbol? formalexp)
             	;(formalexp? formalexp)
                  (not (memq 'lambda env)))
	   (process-let expr (union formalexp env))]

	  [(tupref ,n ,m ,[x]) `(tupref ,n ,m ,x)]
	  [(tuple ,[args] ...) `(tuple ,args ...)]

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
                    
	  [,prim (guard (regiment-constant? prim)
			(not (memq prim env)))
		 ;(disp "GOT CONST: " prim)
	   #t]

          [(,prim ,rand* ...)
           (guard 
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
        [(,input-language (quote (program ,body ,type)))
	 (if (process-let body '())
	     expr
	     (error 'verify-core 
		    "input didn't pass (and shouldn't get here): ~s"
		    expr))])
      )))

;==============================================================================

;; These here are examples of core programs:
(define test-programs   
  '( 
    (lazy-letrec ((a _ '3)) a)

;; OLD:
#;    (letrec ((tmp (cons '40 '()))
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

(define-testing these-tests
  (map
   (lambda (prog)
     `[(verify-core '(some-lang '(program ,prog notype)))
       (some-lang '(program ,prog notype))])
   test-programs))

(define test-this (default-unit-tester 
		    "12: Verify-Core: Pass to verify the simplifed core language."
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
		  (display "Testing pass to verify the simplifed core language.")
		  (newline)
		  (newline) (display "Here are intended results:") (newline)
		  (write intended) (newline) (newline)
		  (newline) (display "Here are actual results:") (newline)
		  (write results) (newline) (newline)))
	   
	    (equal? intended results)))))))  

(define test12 test-this)
(define tests12 these-tests)
(define test-verify-core test-this)
(define tests-verify-core these-tests)


;==============================================================================

) ; End module