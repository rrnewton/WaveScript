#!r6rs

;;;; Pass 2: rename-vars

;;;; This pass renames each variable to insure that each variable has
;;;; a unique name. 

;;; In addition to the current expression, this pass carries along
;;; an environment (association list) mapping variable names to
;;; unique variable names.  The environment is extended for lambda,
;;; let, and letrec expressions and consulted for variable
;;; references and assignments.


(library (ws passes normalize_source rename-vars)
  (export rename-vars rename-vars-grammar
	  ;test01 tests01
	  test-rename-vars)
  (import (except (rnrs (6)) error) (rnrs lists) (ws common)
	  (ws passes normalize_source ws-label-mutable))

  ;; This is a bit of a hack... really should split rename-var into
  ;; two separate passes for the two places it's used.
  (define rename-vars-grammar
    (cons
     ;; This is really compiler-internal.  Introduced after static-elaborate.
     ;; Including here only because rename-vars is used in multiple places.
     '[Expr ('unionN Expr Expr ...)]
     (cons '[Expr ('foreign-app Const Expr Expr ...)]	   
	   ws-label-mutable-grammar)))

  ;; [2006.10.07] Rewrote to use generic-traverse.
  (define rename-vars
    (build-compiler-pass ;; This wraps the main function with extra debugging
     'rename-vars
     `(input)
     `(output (grammar ,rename-vars-grammar PassInput))  ;; No grammar change.

     ;; TODO: Rewrite to use define-pass with the Bindings clause.
     (let ()
       ;; These are names that we don't mangle.  They're faux-primitive.
       (define special-names 'snuninit) ;; Mutated below.
       (define (make-new-name sym)
	 ;; [2007.10.21] We don't touch special names.  It's currently
	 ;; up to the user to insure that they are only defined once.
	 (if (memq sym special-names)  
	     sym
	     (unique-name sym)))
       
       (define (process-expr expr var-table)
	 (define (driver x fallthrough)
	   (match x

	     [,var (guard (symbol? var))
		   (cond
		    [(assq var var-table) (cdr (assq var var-table))]
		    [(wavescript-primitive? var) var]
		    ;; FIXME: Currently library-primitives are only allowed as operators...
		    [else (error 'rename-vars "variable was not bound, how can this happen?: ~a ~a"
				 var var-table)])]
	     [(lambda (,v* ...) (,t* ...) ,expr)
	      (guard (not (assq 'lambda var-table)))
	      (let* ([new-v* (map make-new-name v*)]
		     [new-table (append (map cons v* new-v*) var-table)])
		(let ([expr (process-expr expr new-table)])
		  `(lambda ,new-v* ,t* ,expr)))]

	     [(let ([,v* ,ty* ,[rhs*]] ...) ,bod)
	      (guard (not (assq 'let var-table)))
	      (let* ([new-v* (map make-new-name v*)]
		     [new-table (append (map cons v* new-v*) var-table)])
		`(let ,(map list new-v* ty* rhs*) 
		   ,(process-expr bod new-table)))]

	     [(for (,i ,[st] ,[en]) ,body)
	      (guard (not (assq 'for var-table)))
	      (let* ([newi (make-new-name i)]
		     [var-table `((,i . ,newi) . ,var-table)])
		`(for (,newi ,st ,en) 
		     ,(process-expr body var-table)))]
	     ;; The automatic traversal won't do the variable (it's not an expression):
	     [(set! ,[v] ,[rhs]) `(set! ,v ,rhs)]
	     
	     [(letrec ([,lhs* ,type* ,rhs*] ...) ,expr)
	      (guard (not (assq 'letrec var-table)))
	      (let* ([new-lhs* (map make-new-name lhs*)]
		     [var-table (append (map cons lhs* new-lhs*) var-table)])
		(let ([rhs* (map (lambda (x) (process-expr x var-table)) rhs*)]
		      [expr (process-expr expr var-table)])
		  `(letrec ([,new-lhs* ,type* ,rhs*] ...) ,expr)))]

	     ;; Safety net: we must have caught all binding forms or we're in trouble.
	     [,bf (guard (binding-form? bf)) 
		  (error 'rename-vars "missed binding form!:\n ~s" bf)]

	     [,other (fallthrough other)]))
	 (core-generic-traverse driver (lambda (ls k) (apply k ls)) expr))

       ;; Main pass body:
       (lambda (expr)	 
	 (unique-name-counter 0)
	 (fluid-let ([special-names    
		      (union '()  ;;special-rewrite-libfuns ;; TEMPTOGGLE
			     (map car (library-primitives)))])
	  (match expr
	   [(,input-language (quote (program ,body ,meta* ... ,type)))
	    (match (or (assq 'union-types meta*) '(union-types))
	      [(union-types [,name* (,tycon** . ,_) ...] ...)
	       (let ([body (process-expr body 
					 (map (lambda (x) (cons x x)) 
					   (apply append tycon**)))])
		 `(rename-var-language '(program ,body ,meta* ... ,type)))
	       ])]))))))

	
;==============================================================================

(define-testing test-rename-vars
  (default-unit-tester 
    " 2: Rename-Vars: Pass to rename variabless."
    (append (map
	      (lambda (x)
		(let ((prog (car x)) (res (cadr x)))
		  `[(',rename-vars '(some-lang '(program ,prog (union-types) Int)))
		    (rename-var-language '(program ,res (union-types) Int))]))
	    `([3 3]    
	      [(letrec ((x Int 1)) x) (letrec ([x_1 Int 1]) x_1)]          
	      ))
     `(
       [(',rename-vars '(some-lang '(program (letrec ((x Int 1)) 
					    (_+_ (app (lambda (x) (Int) x) 3) x)) 
				     (union-types) Int)))
	,(lambda (p)
	   (match p
	     [(rename-var-language
	       '(program
		    (letrec ([,x_1 Int 1])
		      (_+_ (app (lambda (,x_2) (Int) ,x_2b) 3) ,x_1b))
		  (union-types) Int))
	      (and (eq? x_1 x_1b) (eq? x_2 x_2b))]
	     [,else #f]))]
       
       ;; Might not be portable, assumes particular numbering:
       ["check on for loops"
	(',rename-vars '(some-lang '(program (lambda (f woot) ((Int -> Int) Int)
						    (for (i 1 (app f woot)) 0)) 
				     (union-types) Int)))
        ,(lambda (v)
           (match v
             ((rename-var-language '(program (lambda (,F1 ,W1) ((Int -> Int) Int)
					       (for (,I 1 (app ,F2 ,W2)) 0)) 
				      (union-types) Int))
              (and (not (eqv? I 'i))
                   (eqv? F1 F2)
                   (eqv? W1 W2)))
             (,else #f)))]
       ["check on set!" 
	(',rename-vars '(some-lang '(program (letrec ([v Int 3]) 
					    (set! v 39)) 
				     (union-types) Int)))
	(rename-var-language
	 '(program (letrec ([v_1 Int 3]) (set! v_1 39)) 
	    (union-types) Int))]
       

       
       ))))

;==============================================================================

) ;; End module

;(require rename-vars) (test-rename-vars)

