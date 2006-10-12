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

;; [2006.10.07] Rewrote to use generic-traverse.
(define rename-var
  (build-compiler-pass ;; This wraps the main function with extra debugging
   'rename-var
   `(input)
   `(output)
   (let ()
     (define (process-expr expr var-table)
       (define (driver x fallthrough)
	 (match x
	   [,var (guard (symbol? var))
		 (cond
		  [(regiment-constant? var) var]
		  [(assq var var-table) (cdr (assq var var-table))]
		  [else (error 'rename-var "variable was not bound, how can this happen?: ~a ~a"
			       var bound)])]
	   [(lambda (,v* ...) (,t* ...) ,expr)
	    (guard (not (assq 'lambda var-table)))
	    (let* ([new-v* (map unique-name v*)]
		   [new-table (append (map cons v* new-v*) var-table)])					       
	      (let ([expr (process-expr expr new-table)])
		`(lambda ,new-v* ,t* ,expr)))]
          [(letrec ([,lhs* ,type* ,rhs*] ...) ,expr)
           (guard (not (assq 'letrec var-table)))
           (let* ([new-lhs* (map unique-name lhs*)]
		  [var-table (append (map cons lhs* new-lhs*) var-table)])
	     (let ([rhs* (map (lambda (x) (process-expr x var-table)) rhs*)]
		   [expr (process-expr expr var-table)])
	       `(letrec ([,new-lhs* ,type* ,rhs*] ...) ,expr)))]
	  [,other (fallthrough other)]))
       (core-generic-traverse driver (lambda (ls k) (apply k ls)) expr))
     ;; Main pass body:
     (lambda (expr)
       (unique-name-counter 0)
       (match expr
	 [(,input-language (quote (program ,body ,type)))
	  (let ([body (process-expr body '())])
	    `(,input-language '(program ,body ,type)))])))))

#;
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
	  
	  [(for (,i ,[st] ,[en]) ,body)
	   (guard (not (assq 'for env)))
	   
	   ]

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
	  [(app ,[rator] ,[rand*] ...) `(app ,rator ,rand* ...)]

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
  (append (map
	      (lambda (x)
		(let ((prog (car x)) (res (cadr x)))
		  `[(rename-var '(some-lang '(program ,prog notype)))
		    (some-lang '(program ,res notype))]))
	    `([3 3]    
	      [(letrec ((x notype 1)) x) (letrec ([x_1 notype 1]) x_1)]          
	      ))
     `(
       [(rename-var '(some-lang '(program (letrec ((x notype 1)) 
					    (+ (app (lambda (x) (notype) x) 3) x)) notype)))
	,(lambda (p)
	   (match p
	     [(some-lang 
	       '(program
		    (letrec ([,x_1 notype 1])
		      (+ (app (lambda (,x_2) (notype) ,x_2b) 3) ,x_1b))
		  notype))
	      (and (eq? x_1 x_1b) (eq? x_2 x_2b))]
	     [,else #f]))]
       [(rename-var '(some-lang '(program (lambda (f woot) (_ _) (for (i 1 (app f woot)) 0)) notype)))
	(some-lang
	 '(program (lambda (f_2 woot_1) (_ _) (for (i 1 (app f woot_1)) 0))
	    notype))]
       )))

(define test-this
  (default-unit-tester 
    " 2: Rename-Vars: Pass to rename variabless."
    these-tests))
  

(define test01 test-this)
(define tests01 these-tests)
(define test-rename-vars test-this)
(define tests-rename-vars these-tests)

;==============================================================================


