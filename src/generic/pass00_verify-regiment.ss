;;; Pass 00: verify-regiment

;; TODO: REMOVE CRAPPY OLD MONOMORPHIC TYPE CHECKER.

;;; This pass verifies that the input is in the regiment lanuguage.
;;; It also wraps the program in a trivial '(<lang> (program <Exp>)) form.

;;; <Pgm>  := <Exp>
;;; <Decl> := (<var> <Exp>)
;;; <Exp>  := 
;;;            (quote <datum>)
;;;          | <constant>
;;;          | <var>
;;;          | (if <Exp> <Exp> <Exp>)
;;;          | (lambda <Formalexp> <Exp>)
;;;          | (letrec (<Decl>*) <Exp>)
;;;          | (<primitive> <Exp>*)
;;; <Formalexp> := (<var>*)

;;; And in the output
;;; <Pgm>  := (<language> '(program <Exp>))


;; No variable capture is allowed at this point.

;;; The implementation requires constant?, datum?, keyword?,
;;; regiment-primitive?, set?, formalexp?, get-formals, and the list
;;; regiment-primitives from helpers.ss.


;;; [2004.10.14]

;; I added a kinda lame-o type inferencer/checker.  It uses a really
;; simple type system listed in helpers.ss .  (No algebraic/arrow tyeps)

;;   Anchor, Area, Region, Signal, Event, Node, Location, Reading
;;   Function, Number, Integer, Float, Bool, Object  List

;;  ----------------------------------------

(define these-tests
  '( [(verify-regiment '(some-lang '(program 3)))
      (some-lang (quote (program 3 Integer)))]
      
     [(verify-regiment '(some-lang '(program
       (letrec ((a (anchor-at 30 40)))
       (letrec ((r (circle a 50.))
		(f (lambda (next tot)
		     (cons (+ (car tot) (sense next))
			   (cons (+ (car (cdr tot)) 1)
				 '()))))
		(g (lambda (tot) (/ (car tot) (car (cdr tot))))))
	 (smap g (rfold f '(0 0) r)))))))
      unspecified]
      
     [(verify-regiment '(some-lang '(program
       (letrec ((R (circle-at 30 40 50.))
	      (f (lambda (next tot)
		   (cons (+ (car tot) (sense next))
			 (cons (+ (car (cdr tot)) 1)
			       '()))))
	      (g (lambda (tot) (/ (car tot) (car (cdr tot))))))
       (letrec ((avg (smap g (rfold f (cons 0 (cons 0 '())) R))))
	 (runtil (swhen-any (lambda (x) (> x 15)) avg)
		 R
		 (circle-at 0 0 100.)))))))
      unspecified]
     ))


(define verify-regiment
  (let ()
    
    ;; Infers cheap and dirty types for some expressions, returns #f otherwise.
    (define (infer-type expr env type-env)
      (match expr
          [,const (guard (constant? const))
		  (cond
		   [(boolean? const) 'Bool]
		   [(number? const) (if (inexact? const) 'Float 'Integer)]
		   [(list? const) 'List] 
		   [else (error 'verify-regiment:infer-type
			 "Unknown type of constant: ~a" const)])]
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

	  [,prim (guard (regiment-constant? prim))
		 (caddr (get-primitive-entry prim))]
	  ;; Handle first class refs to other prims: 
	  ;; All those other prims are functions!
	  [,prim (guard (regiment-primitive? prim)) 'Function]	  

	  [(,[rator] ,[rand*] ...) #f]

	  [,other (error 'infer-type
			 "unmatched expr: ~s" other)]
      ))


    ;; This returns nothing, throws error if it hits a problem.
    (define type-check
      (lambda (env type-env)
	(lambda (expr expected-type)
	  (let ([infered-type (infer-type expr env type-env)])
	    (cond 
	     [(eq? infered-type expected-type)
	      (void)] ;; It's all good

	     ;; We're using #f to mean "any type", so it matches anything:
	     [(or (not infered-type) (not expected-type))
	      (void)]

	     ;; Locations are just lists for the moment!
	     [(set-equal? (list infered-type expected-type) '(List Location))  (void)]
	     ;; We're using integral rather than floating point distances for the time being:
	     [(set-equal? (list infered-type expected-type) '(Dist Float))  (void)]
	     

	     ;; We are lenient and allow either a float or an int to match a Number.
	     [(set-equal? (list infered-type expected-type) '(Float Number)) (void)]
	     [(set-equal? (list infered-type expected-type) '(Integer Number)) (void)]

	     [(and (eq? infered-type  'Region)
		   (eq? expected-type 'Area))]
	     [(and (eq? infered-type  'Anchor)
		   (eq? expected-type 'Signal))]

	     ;; Areas and Regions are Signals
	     [(and (or (eq? infered-type 'Area) (eq? infered-type 'Region))
		   (eq? expected-type 'Signal))]


	     [(or (eq? 'Object infered-type)
		  (eq? 'Object expected-type))
	      (if (regiment-verbose)
		  (warning 'type-check-arg
			   "inferred type <~s> doesn't *quite* match expected type <~s> for expression: ~n~s"
			   infered-type expected-type expr))]

	     [else (error 'type-check-arg
			    "inferred type <~s> doesn't match expected type <~s> for expression: ~n~s"
			    infered-type expected-type expr)])))))

    (define type-union
      (lambda (t1 t2)
	(cond 
	 [(eq? t1 t2) t1]
	 [(eq? 'Object t1) t2]
	 [(eq? 'Object t2) t1]
	 ;; Subtyping!! (without polymorphism or anything)
	 ; A region is an area.
	 [(set-equal? (list t1 t2) '(Area Region)) 'Region]
	 ; An anchor is really a signal:
	 [(set-equal? (list t1 t2) '(Signal Anchor)) 'Anchor]

	 [(set-equal? (list t1 t2) '(Float Number)) 'Float]
	 [(set-equal? (list t1 t2) '(Integer Number)) 'Integer]

	 [else (error 'type-union
		      "Cannot union types: ~s and ~s" t1 t2)])))

    (define add-type-constraint
      (lambda (var type type-env)
	(if (regiment-verbose) (disp "add-type-constraint: " var type type-env))
	;; Do nothing if the input is not a varref:
	(if (and (symbol? var) (not (regiment-primitive? var)))
	    (let ([entry (assq var type-env)])
	      (if entry 
		  (cons (list var (type-union (cadr entry) type))
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

	  [(tuple ,[e] ...) `(tuple ,e ...)]
	  [(tupref ,n ,len ,[e])
	   (unless (qinteger? n) (error 'verify-regiment "bad index to tupref: ~a" o))
	   (unless (qinteger? len) (error 'verify-regiment "bad length argument to tupref: ~a" o))
	   `(tupref ,n ,len ,e)]
          
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
           (guard 
            (not (memq prim env))
            (regiment-primitive? prim))
	   ;      (check-primitive-numargs prim rand*)

	   (let ([entry (get-primitive-entry prim)])
	     
	     ;; Make sure the infered for each argument matches the expected type:
;	     (if (not (= (length rand*) (length (cadr entry))))
;		 (error 'verify-regiment "wrong number of arguments to prim: ~a, expected ~a, got ~a" 
;			prim (cadr entry) rand*))
	     (let ((types (fit-formals-to-args (cadr entry) rand*)))

;; [2005.12.08] Disabling, we've got a real type system now.	     
;	       (for-each (type-check env type-env) rand* types)
	   
	       ;; Add type constraints to the variables based on their usage in this primitive.
	       (for-each (lambda (rand expected)
			   (set! type-env (add-type-constraint rand expected type-env)))
		 rand* types)))

	   `(,prim ,rand* ...)]
          
	  ;; Adding normal applications because the static elaborator will get rid of them.
	  [(,[rator] ,[rand*] ...)
	   ;; Don't do any type inference for now.
	   `(,rator ,rand* ...)]

          [,unmatched
            (error 'verify-regiment "invalid syntax ~s" unmatched)])))
    
    (lambda (expr)
      (match expr	    
	;; The input is already wrapped with the metadata:
        [(,input-language (quote (program ,body)))
         (let ([body (process-expr body '() '())]) 
           ;; Changes input language only by annotating types:
	   (mvlet ([(newprog t) (annotate-program body)])
	     `(,input-language '(program ,newprog ,t))))]
	;; Nope?  Well wrap that metadata:
        [,body
         (let ([body (process-expr body '() '())])
	   (mvlet ([(newprog t) (annotate-program body)])
	     `(base-language '(program ,newprog ,t))))]
	))))

;==============================================================================
     

;; these-tests defined up above and set within a scope such that it
;; has access to internal functions.

(define test-this (default-unit-tester 
		    " 0: Verify-Regiment: Pass to verify initial regiment language."
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
(define test-verify-regiment test-this)
(define tests-verify-regiment these-tests)

;==============================================================================

