;; TODO: Haven't finished this yet... 

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


;;; [2004.10.14]

;; I added a kinda lame-o type inferencer/checker.  It uses a really
;; simple type system listed in helpers.ss .  (No algebraic/arrow tyeps)

;;   Anchor, Area, Region, Signal, Event, Node, Location, Reading
;;   Function, Number, Integer, Float, Bool, Object  List



(define verify-stage2
  (let ()
    
    ;; Infers cheap and dirty types for some expressions, returns #f otherwise.
    (define (infer-type expr env type-env)
      (match expr
          [,const (guard (constant? const))
		  (cond
		   [(number? const) 'Number]
		   [(list? const) 'List] 
		   [else (error 'verify-regiment:infer-type
			 "Unknown type of constant: " const)])]
          [(quote ,datum)
	   (guard (not (memq 'quote env)) (datum? datum))
	   (cond
	    [(number? datum) 'Number]
	    [(list? datum) 'List]
	    [else (error 'verify-regiment:infer-type
			 "Unknown type of quoteconstant: ~s" `(quote ,datum))])]	  
          [,var (guard (symbol? var)
		       (not (regiment-primitive? var)))
		(let ((entry (assq var type-env)))
		  (if entry
		      (cadr entry)
		      ;; otherwise we have no type information on this variable and must return "Object".
		      'Object))]

	  [,prim (guard (regiment-constant? prim))
		 (caddr (get-primitive-entry prim))]
	  ;; Handle first class refs to other prims: 
	  ;; All those other prims are functions!
	  [,prim (guard (regiment-primitive? prim)) 'Function]

          [(,prim ,[rand*] ...)
           (guard (not (memq prim env)) (regiment-primitive? prim))
	   (let ((ret-type (caddr (get-primitive-entry prim))))
;	     (disp "RETURN TYPE" ret-type)
	     ret-type)]

          [(lambda ,formalexp ,expr) 'Function]
          
          [(if ,[test] ,[conseq] ,[altern])
	   (guard (not (memq 'if env)))
	   (if (not (eq? test 'Bool))
	       (warning 'verify-regiment "if test expr does not have type bool: ~s" test))
	   (if (eq? conseq altern)
	       conseq
	       (error 'verify-regiment:infer-type
		      "if branches don't have the same types: ~s and ~s "
		      altern conseq ))]
          
	  ;; <TODO> <FIXME> THIS IS NOT ACTUALLY RECURSIVE ATM!
	  [(letrec ([,lhs* ,[rhs*]] ...) ,body)
	   (guard (not (memq 'letrec env)))	   
	   (let ([new-type-env (append (map list lhs* rhs*) type-env)])
	     (infer-type body env new-type-env))]

	  [,other (error 'infer-type
			 "unmatched expr: ~s" other)]
      ))

    (define type-check
      (lambda (env type-env)
	(lambda (expr expected-type)
	  (let ([infered-type (infer-type expr env type-env)])
	    (cond 
	     [(eq? infered-type expected-type)
	      (void)] ;; It's all good
	     ;; Locations are just lists for the moment!
	     [(set-equal? (list infered-type expected-type) '(List Location))  (void)]
	     [(set-equal? (list infered-type expected-type) '(Dist Number))  (void)]

	     [(and (eq? infered-type 'Region)
		   (eq? expected-type 'Area))]

	     [(or (eq? 'Object infered-type)
		  (eq? 'Object expected-type))
	      (warning 'type-check-arg
		       "infered type ~s doesn't *quite* match expected type ~s for expression: ~n~s"
		       infered-type expected-type expr)]	     

	     [else (error 'type-check-arg
			    "infered type ~s doesn't match expected type ~s for expression: ~n~s"
			    infered-type expected-type expr)])))))

    (define type-union
      (lambda (t1 t2)
	(cond 
	 [(eq? t1 t2) t1]
	 [(eq? 'Object t1) t2]
	 [(eq? 'Object t2) t1]
	 ;; Subtyping!! (without polymorphism or anything)
	 [(set-equal? (list t1 t2) '(Area Region)) 'Region]
	 [else (error 'type-union
		      "Cannot union types: ~s and ~s" t1 t2)])))

    (define add-type-constraint
      (lambda (var type type-env)
	(disp "add-type-constraint: " var type type-env)
	;; Do nothing if the input is not a varref:
	(if (and (symbol? var) (not (regiment-primitive? var)))
	    (let ([entry (assq var type-env)])
	      (if entry 
		  (cons (type-union (cadr entry) type)
			(list-remove-first entry type-env))
		  (cons (list var type)
			type-env)))
	    type-env)))
      
    (define process-expr
      (lambda (expr env type-env)
					;	(disp "process-expr, env" env "types:" type-env)

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
          
	  ;; In our super simple type inference we don't do arrow
	  ;; types.  So we don't say anything about the types of
	  ;; formal variables unless they can be be infered from references to them..
          [(lambda ,formalexp ,expr)
           (guard (list? formalexp) 
		  (andmap symbol? formalexp)
		  (set? formalexp)
                  (not (memq 'lambda env)))
	   `(lambda ,formalexp 
	      ,(process-expr expr (union formalexp env)
			     type-env))]
          
          [(if ,[test] ,[conseq] ,[altern])
           (guard (not (memq 'if env)))
	   `(if ,test ,conseq ,altern)]
          
	  [(letrec ([,lhs* ,rhs*] ...) ,expr)
	   (guard (not (memq 'letrec env))
                  (andmap symbol? lhs*)
                  (set? lhs*))
	   (let* ([newenv (union lhs* env)]
		  [new-type-env (map list lhs*
				     (map (lambda (x) (infer-type x newenv type-env))
					  rhs*))]
		  [rands (map (lambda (r) 
				(process-expr r newenv new-type-env)) rhs*)]
		  [body  (process-expr expr newenv new-type-env)])
	     `(letrec ([,lhs* ,rands] ...) ,body))]
          
          [(,prim ,[rand*] ...)
           (guard ;(>= (snet-optimize-level) 2)
            (not (memq prim env))
            (regiment-primitive? prim))
	   ;      (check-primitive-numargs prim rand*)

	   (let ([entry (get-primitive-entry prim)])
	     
	     ;; Make sure the infered for each argument matches the expected type:
	     (for-each (type-check env type-env)
		       rand* (cadr entry))
	   
	     ;; Add type constraints to the variables based on their usage in this primitive.
	     (for-each (lambda (rand expected)
			 (set! type-env
			       (add-type-constraint rand expected type-env)))
		        rand* (cadr entry)))

	   `(,prim ,rand* ...)]
          
          [,unmatched
            (error 'verify-regiment "invalid syntax ~s" unmatched)])))
    
    (lambda (expr)
      (match expr	    
	;; The input is already wrapped with the metadata:
        [(,input-language (quote (program ,body)))
         (let ([body (process-expr body '() '())]) 
           ;; Doesn't change the input language... 		
           `(,input-language '(program ,body)))]
	;; Nope?  Well wrap that metadata:
        [,body
         (let ([body (process-expr body '() '())])
           ;; Doesn't change the input language... 		
           `(base-language '(program ,body)))]
	))))

;==============================================================================


(define test-programs 
  '( 
     ))

(define these-tests
  (map
   (lambda (prog)
     `[(verify-regiment '(some-lang '(program ,prog)))
       (some-lang '(program ,prog))])
   test-programs))
     

(define test-this (default-unit-tester 
		    "Pass05: Verify valid stage2 program (unfinished)"
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

