;;; Pass 2: rename-var

;;; This pass renames each variable to insure that each variable has
;;; a unique name. 

;;; The input language is the same as the input and output languages
;;; of Pass 1.  The output language differs in that all variable
;;; bindings are uniquely named.

;;; OUTPUT LANG:

;;; <Pgm>  ::= (<language-name> (quote (program <Exp>)))
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

;;; The implementation requires constant?, scheme-primitive?, unique-name,
;;; get-formals, and cast-formals from helpers.ss.

;;; In addition to the current expression, this pass carries along
;;; an environment (association list) mapping variable names to
;;; unique variable names.  The environment is extended for lambda,
;;; let, and letrec expressions and consulted for variable
;;; references and assignments.


(define rename-var
  (let ()

    (define process-var
      (lambda (var bound)
	(cond
	 [(regiment-constant? var) var]
	 [(assq var bound) (cdr (assq var bound))]
	 [else (begin
		 (error 'rename-var/process-var
			"variable was not bound, how can this happen?: ~a ~a"
			var bound)
		 var)])))

    ;; This recurs over complex constants:
#;    (define mangle-datum
      (lambda (datum)
        (cond
          [(symbol? datum) datum] ;(mangle-name datum)]
          [(pair? datum)
           (cons (mangle-datum (car datum))
                 (mangle-datum (cdr datum)))]
          [(vector? datum)
           (let ([v (make-vector (vector-length datum))])
             (do ([i 0 (add1 i)])
                 ([= i (vector-length datum)])
                 (vector-set! v i (mangle-datum (vector-ref datum i))))
             v)]
          ;; Other datums (numbers null etc) get left alone:
          [else datum])))

    (define process-expr*
      (lambda (expr* env)
        (map (lambda (expr) (process-expr expr env)) expr*)))

    (define process-expr
      (lambda (expr env)
        (match expr
          [,const (guard (constant? const)) const]
          [(quote ,datum)
           (guard (not (assq 'quote env)))
	   `(quote ,datum)]
					;           `(quote ,(mangle-datum datum))]
                                       
          [,var (guard (symbol? var))
            (process-var var env)]
          
          [(if ,[test] ,[conseq] ,[altern])
           (guard (not (assq 'if env)))
           `(if ,test ,conseq ,altern)]

          [(lambda ,formalexp ,types ,expr)
           (guard (not (assq 'lambda env)))
	   (let* ([formal* formalexp] ;(get-formals formalexp)]
		  [new-formal* (map unique-name formal*)]
		  [env (append (map cons formal* new-formal*) env)])
;	     (inspect env)
	     (let ([expr (process-expr expr env)])
	       `(lambda ,(cast-formals new-formal* formalexp)
		  ,types 
		  ,expr)))]

          [(letrec ([,lhs* ,type* ,rhs*] ...) ,expr ,expr* ...)
           (guard (not (assq 'letrec env)))
           (let ([new-lhs* (map unique-name lhs*)])
             (let ([env (append (map cons lhs* new-lhs*) env)])
               (let ([rhs* (process-expr* rhs* env)]
                     [expr (process-expr expr env)]
                     [expr* (process-expr* expr* env)])
                 `(letrec ([,new-lhs* ,type* 
				      ,rhs*] ...) ,expr ,expr* ...))))]
          
          [(,prim ,[rand*] ...)
           (guard (not (assq prim env)) (regiment-primitive? prim))
           `(,prim ,rand* ...)]          

	  ;; Adding normal applications because the static elaborator will get rid of them.
	  [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]

          [,unmatched (error 'rename-var "invalid syntax ~s" unmatched)])))

    (lambda (expr . optional)
      (let ((run (lambda (expr)
		   (match expr
			  [(,input-language (quote (program ,body ,type)))
			   (let ([body (process-expr body '())])
			     `(,input-language '(program ,body ,type)))]))))
	(match optional
	       [(count ,n) 
		(unique-name-counter n)
		(let ((res (run expr)))
		  (unique-name-counter n)
		  res)]
	       [,else (run expr)])))
    ))
	
;==============================================================================

(define these-tests
  (map
   (lambda (x)
     (let ((prog (car x)) (res (cadr x)))	
       `[(rename-var '(some-lang '(program ,prog notype)))
	 (some-lang '(program ,res ))])) 
   '(
     [3 3]    
     [(letrec ((x notype 1)) x) unspecified]
     )
   )
  )

(define test-this
  (default-unit-tester 
    " 2: Rename-Vars: Pass to rename variabless."
    these-tests))
  

(define test01 test-this)
(define tests01 these-tests)
(define test-rename-vars test-this)
(define tests-rename-vars these-tests)

;==============================================================================


