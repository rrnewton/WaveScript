;; TODO: Haven't finished this yet... 

;;; Pass 00: verify-wavescript

;;; This pass verifies that the input is in the wavescript lanuguage.
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
;;; wavescript-primitive?, set?, formalexp?, get-formals, and the list
;;; wavescript-primitives from helpers.ss.


;;; [2004.10.14]

;; I added a kinda lame-o type inferencer/checker.  It uses a really
;; simple type system listed in helpers.ss .  (No algebraic/arrow tyeps)

;;   Anchor, Area, Region, Stream, Event, Node, Location, Reading
;;   Function, Number, Int, Float, Bool, Object  List



(define verify-stage2
  (let ()

    ;; TODO : USE TYPE INFO ALREADY EXISTING!!

    ;; Infers cheap and dirty types for some expressions, returns #f otherwise.
    (define (infer-type expr env type-env)
      (match expr
          [,const (guard (simple-constant? const))
		  (cond
		   [(number? const) 'Number]
		   [(list? const) 'List] 
		   [else (error 'verify-wavescript:infer-type
			 "Unknown type of constant: " const)])]
          [(quote ,datum)
	   (guard (not (memq 'quote env)) (datum? datum))
	   (cond
	    [(number? datum) 'Number]
	    [(list? datum) 'List]
	    [else (error 'verify-wavescript:infer-type
			 "Unknown type of quoteconstant: ~s" `(quote ,datum))])]	  
          [,var (guard (symbol? var)
		       (not (wavescript-primitive? var)))
		(let ((entry (assq var type-env)))
		  (if entry
		      (cadr entry)
		      ;; otherwise we have no type information on this variable and must return "Object".
		      'Object))]

	  [,prim (guard (wavescript-constant? prim))
		 (caddr (get-primitive-entry prim))]
	  ;; Handle first class refs to other prims: 
	  ;; All those other prims are functions!
	  [,prim (guard (wavescript-primitive? prim)) 'Function]

          [(,prim ,[rand*] ...)
           (guard (not (memq prim env)) (wavescript-primitive? prim))
	   (let ((ret-type (caddr (get-primitive-entry prim))))
;	     (disp "RETURN TYPE" ret-type)
	     ret-type)]

          [(lambda ,formalexp ,types ,expr) 'Function]
          
          [(if ,[test] ,[conseq] ,[altern])
	   (guard (not (memq 'if env)))
	   (if (not (eq? test 'Bool))
	       (warning 'verify-wavescript "if test expr does not have type bool: ~s" test))
	   (if (eq? conseq altern)
	       conseq
	       (error 'verify-wavescript:infer-type
		      "if branches don't have the same types: ~s and ~s "
		      altern conseq ))]
          
	  ;; <TODO> <FIXME> THIS IS NOT ACTUALLY RECURSIVE ATM!
	  [(letrec ([,lhs* ,type* ,[rhs*]] ...) ,body)
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
	     [(set-equal? (list->set (list infered-type expected-type)) 
			  (list->set '(List Location)))  (void)]
	     [(set-equal? (list->set (list infered-type expected-type)) 
			  (list->set '(Dist Number)))  (void)]

	     [(and (eq? infered-type 'Region)
		   (eq? expected-type 'Area))]

	     [(or (eq? 'Object infered-type)
		  (eq? 'Object expected-type))
	      (warning 'type-check-arg
		       "infered type ~s doesn't *quite* match expected type ~s for expression: \n~s"
		       infered-type expected-type expr)]	     

	     [else (error 'type-check-arg
			    "infered type ~s doesn't match expected type ~s for expression: \n~s"
			    infered-type expected-type expr)])))))

    (define type-union
      (lambda (t1 t2)
	(cond 
	 [(eq? t1 t2) t1]
	 [(eq? 'Object t1) t2]
	 [(eq? 'Object t2) t1]
	 ;; Subtyping!! (without polymorphism or anything)
	 [(set-equal? (list->set (list t1 t2)) 
		      (list->set '(Area Region))) 'Region]
	 [else (error 'type-union
		      "Cannot union types: ~s and ~s" t1 t2)])))

    (define add-type-constraint
      (lambda (var type type-env)
	(disp "add-type-constraint: " var type type-env)
	;; Do nothing if the input is not a varref:
	(if (and (symbol? var) (not (wavescript-primitive? var)))
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
          [,const (guard (simple-constant? const)) const]
          [(quote ,datum)
	   (guard (not (memq 'quote env)) (datum? datum))
	   `(quote ,datum)]
          [,var (guard (symbol? var))
            (if (and (not (memq var env))
		     (not (wavescript-primitive? var)))
                (error 'verify-wavescript (format "unbound variable: ~a\n" var))
                var)]
          
	  ;; In our super simple type inference we don't do arrow
	  ;; types.  So we don't say anything about the types of
	  ;; formal variables unless they can be be infered from references to them..
          [(lambda ,formalexp ,type ,expr)
           (guard (list? formalexp) 
		  (andmap symbol? formalexp)
		  (list-is-set?o formalexp)
                  (not (memq 'lambda env)))
	   `(lambda ,formalexp 
	      ,(process-expr expr (union formalexp env)
			     type-env))]
          
          [(if ,[test] ,[conseq] ,[altern])
           (guard (not (memq 'if env)))
	   `(if ,test ,conseq ,altern)]
          
	  [(letrec ([,lhs* ,type* ,rhs*] ...) ,expr)
	   (guard (not (memq 'letrec env))
                  (andmap symbol? lhs*)
                  (list-is-set? lhs*))
	   (let* ([newenv (union lhs* env)]
		  [new-type-env (map list lhs*
				     (map (lambda (x) (infer-type x newenv type-env))
					  rhs*))]
		  [rands (map (lambda (r) 
				(process-expr r newenv new-type-env)) rhs*)]
		  [body  (process-expr expr newenv new-type-env)])
	     `(letrec ([,lhs* ,rands] ...) ,body))]
          
          [(,prim ,[rand*] ...)
           (guard 
            (not (memq prim env))
            (wavescript-primitive? prim))
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
            (error 'verify-wavescript "invalid syntax ~s" unmatched)])))
    
    (lambda (expr)
      (match expr	    
	;; The input is already wrapped with the metadata:
        [(,input-language (quote (program ,body ,type)))
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

(define-testing these-tests
  (map
   (lambda (prog)
     `[(verify-wavescript '(some-lang '(program ,prog)))
       (some-lang '(program ,prog))])
   test-programs))
     

(define-testing test-this (default-unit-tester 
		    "Verify-Stage2: Pass to verify valid stage2 program (unfinished)"
		    these-tests))

#;(define-testing test-this
  (let ((these-tests these-tests))
    (lambda args 
      (let ((verbose (memq 'verbose args)))
	
	(let ((tests (map car these-tests))
	      (intended (map cadr these-tests)))
	  (let ((results (map eval tests)))
	    (if verbose 
		(begin
		  (display "Testing pass to verify initial wavescript language.")
		  (newline)
		  (newline) (display "Here are intended results:") (newline)
		  (write intended) (newline) (newline)
		  (newline) (display "Here are actual results:") (newline)
		  (write results) (newline) (newline)))
	   
	    (equal? intended results)))))))
  

(define test07 test-this)
(define tests07 these-tests)
(define test-verify-stage2 test-this)
(define tests-verify-stage2 these-tests)

;==============================================================================

