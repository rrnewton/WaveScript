;;; Pass 2: rename-var

;;; This pass renames each variable to insure that each variable has
;;; a unique name. 

;;; The input language is the same as the input and output languages
;;; of Pass 1.  The output language differs in that all variable
;;; bindings are uniquely named, and that the aforementioned
;;; toplvl-varref and toplvl-varassign! syntaxes are added.

;;; The implementation requires constant?, scheme-primitive?, unique-name,
;;; get-formals, and cast-formals from helpers.ss.

;;; In addition to the current expression, this pass carries along
;;; an environment (association list) mapping variable names to
;;; unique variable names.  The environment is extended for lambda,
;;; let, and letrec expressions and consulted for variable
;;; references and assignments.

;;; RRN[2002.06.19] - now this mangles toplvl var names
;;;                   (as well as quoted symbols)

(define rename-var
  (let ()

    (define process-var
      (lambda (var bound)
        (let ([vpair (assq var bound)])
          (if vpair
              (cdr (assq var bound))
              (begin
                (error 'rename-var/process-var
                       "variable was not bound, how can this happen?: ~a ~a"
                       var bound)
                var)))))

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

    (define process-lambda-clause
      (lambda (formalexp expr expr* env)
        (let* ([formal* formalexp] ;(get-formals formalexp)]
               [new-formal* (map unique-name formal*)]
               [env (append (map cons formal* new-formal*) env)])
          (let ([expr (process-expr expr env)]
                [expr* (process-expr* expr* env)])
            `(,(cast-formals new-formal* formalexp)
               ,expr ,@expr*)))))

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

          [(lambda ,formalexp ,expr ,expr* ...)
           (guard (not (assq 'lambda env)))
           (cons 'lambda
                 (process-lambda-clause formalexp expr expr* env))]

          [(letrec ([,lhs* ,rhs*] ...) ,expr ,expr* ...)
           (guard (not (assq 'letrec env)))
           (let ([new-lhs* (map unique-name lhs*)])
             (let ([env (append (map cons lhs* new-lhs*) env)])
               (let ([rhs* (process-expr* rhs* env)]
                     [expr (process-expr expr env)]
                     [expr* (process-expr* expr* env)])
                 `(letrec ([,new-lhs* ,rhs*] ...) ,expr ,expr* ...))))]

          
          [(,prim ,[rand*] ...)
           (guard (not (assq prim env)) (regiment-primitive? prim))
           `(,prim ,rand* ...)]          

          [,unmatched (error 'rename-var "invalid syntax ~s" unmatched)])))

    (lambda (expr . optional)
      (let ((run (lambda (expr)
		   (match expr
			  [(,input-language (quote (program ,body)))
			   (let ([body (process-expr body '())])
			     `(,input-language '(program ,body)))]))))
	(match optional
	       [(count ,n) 
		(reset-name-count! n)
		(let ((res (run expr)))
		  (reset-name-count! n)
		  res)]
	       [,else (run expr)])))
    ))
	
;==============================================================================

(define these-tests
  (map
   (lambda (x)
     (let ((prog (car x)) (res (cadr x)))	
       `[(rename-var '(some-lang '(program ,prog)))
	 (some-lang '(program ,res))])) 
   '(
     [3 3]    
     [(letrec ((x 1)) x) unspecified]
     )
   )
  ) 

(define test-this
  (default-unit-tester 
    "Testing pass to verify initial regiment language."
    these-tests))
  

(define test01 test-this)
(define tests01 these-tests)

;==============================================================================


