;;;; Pass 00: verify-regiment

;;;; This pass verifies that the input is in the regiment lanuguage.
;;;; It also wraps the program in the boilerplate '(<lang> '(program <Exp>)) form.

;;;; No variable capture is allowed at this point.

(define verify-regiment
  (build-compiler-pass ;; This wraps the main function with extra debugging
   'verify-regiment
   `(input)
   `(output (grammar ,sugared_regiment_grammar PassInput))
   (let ()

     (define (assert-valid-name! v)
       (IFWAVESCOPE 
	(if (regiment-primitive? v)
	    (error 'verify-regiment 
		   "for the time being you cannot bind/mutate variables that use the same names as primitives: '~s'" v))
	(if (regiment-keyword? v)
	    (error 'verify-regiment 
		   "for the time being you cannot bind/mutate variables that use the same names as keywords: '~s'" v))
	))
     
     ;; .param env is just a list of symbols in scope.
     ;; (Really this is outdated because the type-checker will catch problems
     ;; with unbound variables.)
     (define process-expr
       (lambda (expr env)
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
	   (unless (qinteger? n) (error 'verify-regiment "bad index to tupref: ~a" n))
	   (unless (qinteger? len) (error 'verify-regiment "bad length argument to tupref: ~a" len))
	   `(tupref ,n ,len ,e)]
          
          [(lambda ,formalexp ,expr)
           (guard (list? formalexp) 
		  (andmap symbol? formalexp)
		  (set? formalexp)
                  (not (memq 'lambda env)))
	   (for-each assert-valid-name! formalexp)
	   `(lambda ,formalexp 
	      ,(process-expr expr (union formalexp env)))]	  
          
          [(if ,[test] ,[conseq] ,[altern])
           (guard (not (memq 'if env)))
	   `(if ,test ,conseq ,altern)]
          
	  [(letrec ([,lhs* ,optional ... ,rhs*] ...) ,expr)
	   (guard (not (memq 'letrec env))
                  (andmap symbol? lhs*)
                  (set? lhs*))
	   (for-each assert-valid-name! lhs*)
	   (let* ([newenv (union lhs* env)]
		  [rands (map (lambda (r) (process-expr r newenv)) rhs*)]
		  [body  (process-expr expr newenv)])
	     `(letrec ([,lhs* ,optional ... ,rands] ...) ,body))]
	  
	  ;; This is long-winded, handling all these let-variants:
	  [(let* ([,lhs* ,optional ... ,rhs*] ...) ,expr)
	   (guard (not (memq 'let* env))
                  (andmap symbol? lhs*)
                  (set? lhs*))
	   (for-each assert-valid-name! lhs*)
	   (let loop ([env env] [lhs* lhs*] [rhs* rhs*] [acc '()])
	     (if (null? lhs*)
		 `(let* ,(reverse acc) ,(process-expr body env))
		 (let* ([rhsnew (process-expr (car rhs*) env)]
			[newenv (union (car lhs*) env)])
		   (loop newenv (cdr lhs*) (cdr rhs*)
			 (cons (list (car lhs*) rhsnew) acc)
			 ))))]
	  [(let ([,lhs* ,optional ... ,[rhs*]] ...) ,expr)
	   (guard (not (memq 'let env))
                  (andmap symbol? lhs*)
                  (set? lhs*))
	   (for-each assert-valid-name! lhs*)
	   `(let ([,lhs* ,optional ... ,rands] ...) 
	      ,(process-expr body (union lhs* env)))]

	  ;; [2006.07.25] Adding effectful constructs for WaveScope:
	  [(begin ,[e] ...) `(begin ,e ...)]
	  [(set! ,v ,[e]) (guard (symbol? v)) 
	   (if (and (not (memq v env))
		    (not (regiment-primitive? v)))
	       (error 'verify-regiment (format "set! unbound variable: ~a~n" v)))
	   (assert-valid-name! v)
	   `(set! ,v ,e)]
	  [(for (,v ,[e1] ,[e2]) ,e3) (guard (symbol? v))
	   (assert-valid-name! v)
	   `(for (,v ,e1 ,e2) 
		,(process-expr e3 (cons v env)))]
	  ;; ========================================
	 
          [(,prim ,[rand*] ...)
           (guard 
            (not (memq prim env))
            (regiment-primitive? prim))
	   (let ([entry (get-primitive-entry prim)])
	     ;; Make sure the infered for each argument matches the expected type:
;	     (if (not (= (length rand*) (length (cadr entry))))
;		 (error 'verify-regiment "wrong number of arguments to prim: ~a, expected ~a, got ~a" 
;			prim (cadr entry) rand*))
	     (let ((types (fit-formals-to-args (cadr entry) rand*)))	       
	       (void)
	       ))
	   `(,prim ,rand* ...)]
          
	  ;; Allowing normal applications because the static elaborator will get rid of them.
	  ;;
	  ;; Even though this is just a VERIFY pass, I do a teensy weensy bit of normalization 
	  ;; here -- add "app".  This makes grammar-checking sane.
	  [(app ,[rator] ,[rand*] ...)  `(app ,rator ,rand* ...)]
	  [(,[rator] ,[rand*] ...)      
	   (warning 'verify-regiment "unlabeled app form allowed: ~s" `(,rator ,rand* ...))
	   `(app ,rator ,rand* ...)]

          [,unmatched
            (error 'verify-regiment "invalid syntax ~s" unmatched)])))
    
     (lambda (expr)
      (match expr	    
	;; The input is already wrapped with the metadata:
        [(,input-language (quote (program ,body)))
         (let ([body (process-expr body '())]) 
           ;; Changes input language only by annotating types:
	   (mvlet ([(newprog t) (annotate-program body)])
	     `(,input-language '(program ,newprog ,t))))]
	;; Nope?  Well wrap that metadata:
        [,body
         (let ([body (process-expr body '())])
	   (mvlet ([(newprog t) (annotate-program body)])
	     `(base-language '(program ,newprog ,t))))]
	)))))


;==============================================================================
     

(define these-tests
  '( [(verify-regiment '(some-lang '(program 3)))
      (some-lang (quote (program 3 Int)))]
      
     [(verify-regiment '(some-lang '(program
       (letrec ((a (anchor-at 30 40)))
       (letrec ((r (circle a 50.))
		(f (lambda (next tot)
		     (cons (+ (car tot) (sense "temperature" next))
			   (cons (+ (car (cdr tot)) 1)
				 '()))))
		(g (lambda (tot) (/ (car tot) (car (cdr tot))))))
	 (smap g (rfold f '(0 0) r)))))))
      unspecified]
      
     [(verify-regiment '(some-lang '(program
       (letrec ((R (circle-at 30 40 50.))
	      (f (lambda (next tot)
		   (cons (+ (car tot) (sense "temperature" next))
			 (cons (+ (car (cdr tot)) 1)
			       '()))))
	      (g (lambda (tot) (/ (car tot) (car (cdr tot))))))
       (letrec ((avg (smap g (rfold f (cons 0 (cons 0 '())) R))))
	 (runtil (swhen-any (lambda (x) (> x 15)) avg)
		 R
		 (circle-at 0 0 100.)))))))
      unspecified]
     ))

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

