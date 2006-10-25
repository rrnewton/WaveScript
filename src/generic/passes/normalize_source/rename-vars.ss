;;;; Pass 2: rename-vars

;;;; This pass renames each variable to insure that each variable has
;;;; a unique name. 

;;; In addition to the current expression, this pass carries along
;;; an environment (association list) mapping variable names to
;;; unique variable names.  The environment is extended for lambda,
;;; let, and letrec expressions and consulted for variable
;;; references and assignments.


(module rename-vars mzscheme
  (require "../../../plt/common.ss")
  (provide rename-vars test-this these-tests test01 tests01)
  (chezimports)

  ;; [2006.10.07] Rewrote to use generic-traverse.
  (define rename-vars
    (build-compiler-pass ;; This wraps the main function with extra debugging
     'rename-vars
     `(input)
     `(output (grammar ,initial_regiment_grammar PassInput))  ;; No grammar change.

     (let ()
       (define (process-expr expr var-table)
	 (define (driver x fallthrough)
	   (match x
	     [,var (guard (symbol? var))
		   (cond
		    [(assq var var-table) (cdr (assq var var-table))]
		    [(regiment-primitive? var) var]
		    [else (error 'rename-vars "variable was not bound, how can this happen?: ~a ~a"
				 var var-table)])]
	     [(lambda (,v* ...) (,t* ...) ,expr)
	      (guard (not (assq 'lambda var-table)))
	      (let* ([new-v* (map unique-name v*)]
		     [new-table (append (map cons v* new-v*) var-table)])
		(let ([expr (process-expr expr new-table)])
		  `(lambda ,new-v* ,t* ,expr)))]
	     [(for (,i ,[st] ,[en]) ,body)
	      (guard (not (assq 'for var-table)))
	      (let* ([newi (unique-name i)]
		     [var-table `((,i . ,newi) . ,var-table)])
		`(for (,newi ,st ,en) 
		     ,(process-expr body var-table)))]
	     ;; The automatic traversal won't do the variable (it's not an expression):
	     [(set! ,[v] ,[rhs]) `(set! ,v ,rhs)]
	     
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
	      `(rename-var-language '(program ,body ,type)))])))))

	
;==============================================================================

(define these-tests
  (append (map
	      (lambda (x)
		(let ((prog (car x)) (res (cadr x)))
		  `[(,rename-vars '(some-lang '(program ,prog Int)))
		    (rename-var-language '(program ,res Int))]))
	    `([3 3]    
	      [(letrec ((x Int 1)) x) (letrec ([x_1 Int 1]) x_1)]          
	      ))
     `(
       [(,rename-vars '(some-lang '(program (letrec ((x Int 1)) 
					    (+ (app (lambda (x) (Int) x) 3) x)) Int)))
	,(lambda (p)
	   (match p
	     [(rename-var-language
	       '(program
		    (letrec ([,x_1 Int 1])
		      (+ (app (lambda (,x_2) (Int) ,x_2b) 3) ,x_1b))
		  Int))
	      (and (eq? x_1 x_1b) (eq? x_2 x_2b))]
	     [,else #f]))]
       
       ;; Might not be portable, assumes particular numbering:
       ["check on for loops"
	(,rename-vars '(some-lang '(program (lambda (f woot) ((Int -> Int) Int)
						    (for (i 1 (app f woot)) 0)) Int)))
	(rename-var-language '(program (lambda (f_2 woot_1) ((Int -> Int) Int)
					       (for (i_3 1 (app f_2 woot_1)) 0)) Int))]
       ["check on set!" 
	(,rename-vars '(some-lang '(program (letrec ([v Int 3]) 
					    (set! v 39)) Int)))
	(rename-var-language
	 '(program (letrec ([v_1 Int 3]) (set! v_1 39)) Int))]
       

       
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

) ;; End module
