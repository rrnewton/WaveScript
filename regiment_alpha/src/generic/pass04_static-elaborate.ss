;; Pass: Static Elaborate
;=======================================================================

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
;;;  known to the compiler.  (The let-bound restriction is stronger
;;;  than it needs to be, a bit of control-flow analysis would allow
;;;  us to be more permissive.)

(define static-elaborate
  (let ()

    (define computable-prims 
      '(+ - * / car cons cdr
	  = < <= > >= 
	  equal? null? pair? number? even? odd? not))

    (define (do-prim prim args)
      (if (ormap symbol? args)
	  (error 'do-prim "args contain unevaluated variable: ~a" args))
      (if (memq prim computable-prims)
	  `(quote ,(eval `(,prim ,@(map (lambda (a) `(quote ,a)) args))))
	  (begin (warning 'do-prim "cannot statically compute primitive! ~a" prim)
		 `(,prim ,@args))))

    ;; This does the actual beta-reduction
    (define (inline rator rands)
;      (disp "INLINGING " rator rands)
      (match rator
	[(lambda ,formals ,type ,body)
	 (substitute (map list formals rands) body)]
	[,other (error 'static-elaborate:inline "bad rator: ~a" other)]))

    (define count-refs
      (lambda (v expr)
        (match expr
          [(quote ,datum) 0]
          [,var (guard (symbol? var))
		(if (eq? var v) 1 0)]
          [(lambda ,formals ,types ,expr)
	   (if (memq v formals) 0 (count-refs v expr))]
          [(if ,[test] ,[conseq] ,[altern])
	   (+ test conseq altern)]
	  [(letrec ([,lhs* ,type* ,rhs*] ...) ,expr)
	   (if (memq v lhs*) 0
	       (+ (count-refs v expr)
		  (apply + (map (lambda (x) (count-refs v x)) rhs*))))]
;	  [(,prim ,[rands] ...)	   
	  [(,[rator] ,[rands] ...) (+ rator (apply + rands))]
          [,unmatched
            (error 'static-elaborate:count-refs "invalid syntax ~s" unmatched)])))

    ;; TODO FINISH:
    #;(define count-app-refs
      (lambda (v expr)
        (match expr
          [(quote ,datum) 0]
          [,var (guard (symbol? var))
		(if (eq? var v) 1 0)]
          [(lambda ,formals ,types ,expr)
	   (if (memq v formals) 0 (count-refs v expr))]
          [(if ,[test] ,[conseq] ,[altern])
	   (+ test conseq altern)]
	  [(letrec ([,lhs* ,type* ,rhs*] ...) ,expr)
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
          [(lambda ,formals ,types ,expr)
	   `(lambda ,formals ,types
	      ,(substitute
		(filter (lambda (x)
			  (not (memq (car x) formals)))
			mapping)
		expr))]
          [(if ,[test] ,[conseq] ,[altern])
	   `(if ,test ,conseq ,altern)]
	  [(letrec ([,lhs* ,type* ,rhs*] ...) ,expr)
	   (let ((newmap (filter (lambda (x)
				   (not (memq (car x) lhs*)))
				 mapping)))	     
	   `(letrec ([,lhs* ,type* ,(map (lambda (x) (substitute newmap x)) rhs*)] ...)
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
	;(printf "ENV: ~a ~a\n" expr env)
	(letrec ([available? ;; Is this value available at compile time.
		  (lambda (x)
		    (if (eq? x not-available) #f
			(match x
			   [(quote ,datum) #t]
			   [(lambda ,vs ,tys ,bod) #t]
			   [,var (guard (symbol? var)) 
				 (let ((entry (assq var env)))
				   (and entry (available? (cadr entry))))]
			   [,else #f])))]
		 [getval ;; If available, follow aliases until you have a real value expression:
		  (lambda (x)
		    (match x
			   [(quote ,datum) datum]
			   [(lambda ,vs ,tys ,bod) `(lambda ,vs ,tys ,bod)]
			   [,var (guard (symbol? var))
				 (getval (cadr (assq var env)))]
			   [,else (error 'static-elaborate "getval bad input: ~a" x)]))])

        (match expr
          [(quote ,datum) `(quote ,datum)]
	  ;; This does constant inlining:
	  [,prim (guard (regiment-primitive? prim)) prim]
          [,var (guard (symbol? var))		
		(match (assq var env)
		  [#f (error 'static-elaborate "variable not in scope: ~a" var)]
		  ;; Anything let-bound with a reference count of 1 gets inlined:
;		  [(,_ ,x 1) 
;`		   (printf "REFCOUNT1: ~a\n" var)
;		   x]
		  ;; Inline constants:
		  [(,_ (quote ,d) ,__) `(quote ,d)]
		  [(,_ ,not-available ,__) var]
		  [(,_ ,v ,__) (guard (symbol? v))
		       (process-expr v env)]
		  ;; Otherwise, nothing we can do with it.
		  [,else var])
		;; This appears to disable the system here:
		;(if (available? var) (getval var) var)
		;var
		]
          [(lambda ,formals ,types ,expr)
	   `(lambda ,formals ,types
	      ,(process-expr expr 
			     (append (map list formals 
					  (make-list (length formals) not-available)
					  ;; The "reference count" for each var 
					  (make-list (length formals) -1)) 
				     env)))]

	  ;; TODO: This doesn't handle mutually recursive functions yet!!
	  ;; Need to do a sort of intelligent "garbage collection".
	  [(letrec ([,lhs* ,type* ,rhs*] ...) ,expr)
	   (if (null? lhs*)
	       (process-expr expr env)
	       ;; TODO: FIXME: NASTY COMPLEXITY:
	       ;; This has complexity number vars * program size.
	       ;; Inefficient ref-counting.
	   (let* ([newenv (append (map list lhs* rhs* 
				       (map (lambda (lhs)
					      ;; TEMP [2006.02.18]: Disabling ref-counting.
					      9999
					      ;(count-refs lhs (cons expr rhs*))
					      )
					 lhs*))
				  env)]
		  [newrhs* (map (lambda (x) (process-expr x newenv)) rhs*)]
;		  [_ 	   (break)]
		  [newbod (process-expr expr newenv)]
;		  [__ 	   (break)]
		  [occurs (map (lambda (v myrhs) 
				 (apply + (count-refs v newbod)
					(map (lambda (x) (count-refs v x)) 
					     (remq myrhs newrhs*))))
			       lhs* newrhs*)]
		  [newbinds (filter id
				    (map 
				     (lambda (lhs type rhs refs)
				       (and (> refs 0)
					    `(,lhs ,type ,rhs)))
				     lhs* type* newrhs* occurs))])
	     `(letrec ,newbinds ,newbod)))]
         
	  ;; Here we do computation if the arguments are available:
          [(if ,[test] ,[conseq] ,[altern])
	   (if (available? test)
	       (if (getval test)
		   conseq  altern)
	       `(if ,test ,conseq ,altern))]
          [(,prim ,[rand*] ...) (guard (regiment-primitive? prim))
	   ;(disp "PRIM: " prim rand* (map available? rand*))
	   (if (and (memq prim computable-prims)
		    (andmap available? rand*))
	       (do-prim prim (map getval rand*))
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
        [(,input-language (quote (program ,body ,type)))
	 ;; Run until we reach a fixed point.
         (let loop ([oldbody body]
		    [body (process-expr body '())])
	   (if (equal? oldbody body)	   
	       `(,input-language '(program ,body ,type))
	       (loop body (process-expr body '()))))]
	))))


(define these-tests 
  `( 

    [(static-elaborate '(foo '(program (+ '3 '4) notype)))
     (foo '(program '7 notype))]

    [(static-elaborate
      '(foo '(program
	      (letrec ([f _ (lambda (x) (_) '#t)])
		(f '3939)) notype)))
     (foo '(program '#t notype))]

    [(static-elaborate '(foo '(program 
      (letrec ([fact _ (lambda (n) (_) 
			       (if (= '0 n) '1 (* n (fact (- n '1)))))]) 
	(fact '6))
      notype)))
     (foo '(program '720 notype))]

    ["Reduce a primop underneath a lambda."
     (static-elaborate '(foolang '(program (lambda (x) (_) (cons (+ '3 '4) x)) notype)))
     (foolang '(program (lambda (x) (_) (cons '7 x)) notype))]

    [(static-elaborate '(foo '(program 
			       (letrec ([f _ (lambda (x) (_) '#t)])
				 (letrec ([loop _ (lambda (n) (_)
							  (if (= '0 n)
							      world
							      (rfilter f (loop (- n '1)))))])
				   (loop '5)))
			       notype)))
     ;unspecified]
     (foo '(program
	    (letrec ([f _ (lambda (x) (_) '#t)])
	      (rfilter
	       f
	       (rfilter f (rfilter f (rfilter f (rfilter f world))))))
	    notype))]

    ["Simple test to make sure we keep the quotes on:" 
     (static-elaborate '(foolang '(program (cons (+ '3 '4) world) notype)))
     (foolang '(program (cons (quote 7) world)
		 notype))]

    ,(let ([prog '(foolang '(program (cons (khood-at '30 '40 '50) world) notype))])
       `["Now run with a regiment-prim that we shouldn't be able to elaborate" 
	 (static-elaborate ',prog)
	 ,prog])

    ))

(define test-this (default-unit-tester
		    " 4: Static-Elaborate: to evaluate the first stage of computation"
		    these-tests))

(define test04 test-this)
(define tests04 these-tests)
(define test-static-elaborate test-this)
(define tests-static-elaborate these-tests)
