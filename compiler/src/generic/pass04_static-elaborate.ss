;; Pass: Static Elaborate
;;======================================================================

;;; Input Language:

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


;;; Output language is the same, but with the restriction that there
;;; are no ((lambda ... ) ...)  applications, and no application of
;;; control operators (rfold, rmap) to "unknown" closures.  For the
;;; time being unknown closures means any that are lambda-bound rather
;;; than let-bound.  

;;;  The point of this restriction is that control-flow be totally
;;;  known to the compiler.  (The let-boun restriction is stronger
;;;  than it needs to be, a bit of control-flow analysis would allow
;;;  us to be more permissive.)

(define static-elaborate
  (let ()

    (define (do-prim prim args)
      (case prim
	[(+ - * / car cons cdr
	    = < <= > >= equal? null? pair? number? even? odd? not)
	 `(quote ,(eval `(,prim ,@args)))]
	[else (warning 'do-prim "cannot statically compute primitive! ~a" prim)
	      `(,prim ,@args)]
;	[else `(,prim ,args ...)]
	))

    ;; This does the actual beta-reduction
    (define (inline rator rands)
;      (disp "INLINGING " rator rands)
      (match rator
	[(lambda ,formals ,body)
	 (substitute (map list formals rands) body)]
	[,other (error 'static-elaborate:inline "bad rator: ~a" other)]))

    (define count-refs
      (lambda (v expr)
        (match expr
          [(quote ,datum) 0]
          [,var (guard (symbol? var))
		(if (eq? var v) 1 0)]
          [(lambda ,formals ,expr)
	   (if (memq v formals) 0 (count-refs v expr))]
          [(if ,[test] ,[conseq] ,[altern])
	   (+ test conseq altern)]
	  [(letrec ([,lhs* ,rhs*] ...) ,expr)
	   (if (memq v lhs*) 0
	       (+ (count-refs v expr)
		  (apply + (map (lambda (x) (count-refs v x)) rhs*))))]
;	  [(,prim ,[rands] ...)	   
	  [(,[rator] ,[rands] ...) (+ rator (apply + rands))]
          [,unmatched
            (error 'static-elaborate:count-refs "invalid syntax ~s" unmatched)])))

    ;; TODO FINISH:
    (define count-app-refs
      (lambda (v expr)
        (match expr
          [(quote ,datum) 0]
          [,var (guard (symbol? var))
		(if (eq? var v) 1 0)]
          [(lambda ,formals ,expr)
	   (if (memq v formals) 0 (count-refs v expr))]
          [(if ,[test] ,[conseq] ,[altern])
	   (+ test conseq altern)]
	  [(letrec ([,lhs* ,rhs*] ...) ,expr)
	   (if (memq v lhs*) 0
	       (+ (count-refs v expr)
		  (apply + (map (lambda (x) (count-refs v x)) rhs*))))]
;	  [(,prim ,[rands] ...)	   
	  [(,[rator] ,[rands] ...) (+ rator (apply + rands))]
          [,unmatched
            (error 'static-elaborate:count-refs "invalid syntax ~s" unmatched)])))

    (define substitute
      (lambda (mapping expr)
        (match expr
          [(quote ,datum) `(quote ,datum)]
          [,var (guard (symbol? var)) 
		(let ((entry (assq var mapping)))
		  (if entry (cadr entry) var))]
          [(lambda ,formals ,expr)
	   `(lambda ,formals
	      ,(substitute
		(filter (lambda (x)
			  (not (memq (car x) formals)))
			mapping)
		expr))]
          [(if ,[test] ,[conseq] ,[altern])
	   `(if ,test ,conseq ,altern)]
	  [(letrec ([,lhs* ,rhs*] ...) ,expr)
	   (let ((newmap (filter (lambda (x)
				   (not (memq (car x) lhs*)))
				 mapping)))	     
	   `(letrec ([,lhs* ,(map (lambda (x) (substitute newmap x)) rhs*)] ...)
	      ,(substitute newmap expr)))]
	  [(,[rator] ,[rands] ...) `(,rator ,rands ...)]
          [,unmatched
            (error 'static-elaborate:substitute "invalid syntax ~s" unmatched)])))


    (define not-available (unique-name "NotAvailable"))

    ;; This does a walk down the program and, instead of doing beta
    ;; reductions, it converts direct apps to let's and then inlines
    ;; any lets than only have one reference. 
    ;; (Later when new let's get introduced for them, they will
    ;; hopefully be at a smaller scope.)

    ;;   The "env" argument binds names to *code*.  Or if the code is
    ;; unavailable, to *void*.
    (define process-expr
      (lambda (expr env)
	(letrec ([available?
		  (lambda (x)
		    (if (eq? x not-available) #f
			(match x
			   [(quote ,datum) #t]
			   [(lambda ,vs ,bod) #t]
			   [,var (guard (symbol? var)) 
				 (let ((entry (assq var env)))
				   (and entry (available? (cadr entry))))]
			   [,else #f])))]
		 [getval
		  (lambda (x)
		    (match x
			   [(quote ,datum) datum]
			   [(lambda ,vs ,bod) `(lambda ,vs ,bod)]
			   [,var (guard (symbol? var))
				 (getval (cadr (assq var env)))]
			   [,else (error 'static-elaborate "getval bad input: ~a" x)]))])

        (match expr
          [(quote ,datum) `(quote ,datum)]
	  ;; This does constant inlining:
          [,var (guard (symbol? var))
		;(if (available? var) (getval var) var)
		var]
          [(lambda ,formals ,expr)
	   `(lambda ,formals
	      ,(process-expr expr 
		 (map list formals (make-list (length formals) not-available))))]

	  ;; Here we inline if there's only one reference and it's a non-recursive binding:
#;	  [(letrec ([,lhs ,rhs]) ,expr)
	    (guard (and ;; (= 0 (count-refs lhs rhs)) ;; dissallow recursion
			(= 1 (count-refs lhs expr))))
	    (let ([body (process-expr 
			 (substitute `((,lhs ,rhs)) expr)
			 env)])
	      ;; If we've got a recursive one we need to keep the binding around:
	      (if (< 0 (count-refs lhs rhs))
		  `(letrec ([,lhs ,rhs]) ,body)
		  body))]

	  ;; TODO: This doesn't handle mutually recursive functions yet!!
	  ;; Need to do a sort of intelligent "garbage collection".
	  [(letrec ([,lhs* ,rhs*] ...) ,expr)
	   (if (null? lhs*)
	       (process-expr expr env)
	   (let* ([newenv (append (map list lhs* rhs*) env)]
		  [newrhs* (map (lambda (x) (process-expr x newenv)) rhs*)]
		  [newbod (process-expr expr newenv)]
		  [occurs (map (lambda (v myrhs) 
				 (apply + (count-refs v newbod)
					(map (lambda (x) (count-refs v x)) 
					     (remq myrhs newrhs*))))
			       lhs* newrhs*)]
		  [newbinds (filter id
				    (map 
				     (lambda (lhs rhs refs)
				       (and (> refs 0)
					    `(,lhs ,rhs)))
				     lhs* newrhs* occurs))])
	     `(letrec ,newbinds ,newbod)))]
         
	  ;; Here we do computation if the arguments are available:
          [(if ,[test] ,[conseq] ,[altern])
	   (if (available? test)
	       (if (getval test)
		   conseq  altern)
	       `(if ,test ,conseq ,altern))]
          [(,prim ,[rand*] ...) (guard (regiment-primitive? prim))
	   (if (andmap available? rand*)
	       (do-prim prim rand*)
	       `(,prim ,rand* ...))]

	  ;; Here we convert to a letrec.  Rename-var insures that we
	  ;; don't get any accidental variable capture:
;	  [((lambda ,formals ,expr) ,rands ...)
;	   (substitute (map list formals rands) expr)]
	  [(,[rator] ,[rands] ...)
;	   (disp "APP" rator (available? rator) env)
	   (if (available? rator)
	       (inline (getval rator) rands)
	       `(,rator ,rands ...))]

          [,unmatched
            (error 'static-elaborate:process-expr "invalid syntax ~s" unmatched)]))))
    
    (lambda (expr)
      (match expr	    
        [(,input-language (quote (program ,body)))
         (let loop ([oldbody body]
		    [body (process-expr body '())])
	   (if (equal? oldbody body)	   
	       `(,input-language '(program ,body))
	       (loop body (process-expr body '()))))]
	))))


(define these-tests 
  `( 

    [(static-elaborate '(foo '(program (+ '3 '4))))
     (foo '(program '7))]

    [(static-elaborate
      '(foo '(program
	      (letrec ([f (lambda (x) '#t)])
		(f '3939)))))
     (foo '(program '#t))]

    [(static-elaborate '(foo '(program 
      (letrec ([fact (lambda (n) (if (= '0 n) '1 (* n (fact (- n '1)))))]) (fact '6)))))
     (foo '(program '720))]

    [(static-elaborate '(foo '(program 
			       (letrec ([f (lambda (x) '#t)])
				 (letrec ([loop (lambda (n)
						  (if (= '0 n)
						      world
						      (rfilter f (loop (- n '1)))))])
				   (loop '5))))))
     ;unspecified]
     (foo '(program
	    (letrec ([f (lambda (x) '#t)])
	      (rfilter
	       f
	       (rfilter f (rfilter f (rfilter f (rfilter f world))))))))]

    ))

(define test-this (default-unit-tester
		    "Staic-elaborate: to evaluate the first stage of computation"
		    these-tests))

(define test04 test-this)
(define tests04 these-tests)