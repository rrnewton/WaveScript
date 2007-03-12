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
;;; TODO + for, begin, set!
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


;;;; TODO: MAKE MUTABLE VARS SAFE!

(module static-elaborate mzscheme
  (require "../../../plt/common.ss"
	   "../normalize_source/remove-unquoted-constant.ss"
	   )
  (provide 
   ;(all-defined)
   static-elaborate
   static-elaborate-grammar
   test-this these-tests
   )

  (chezimports )


;; This is the grammar for the output of static-elaborate
;; UNFINISHED: FIXME TODO FIXME
#;
(define static-elaborate-grammar
  ;; TODO, make check-grammar optionally take a procedure which is given a sub-checker.
  (lambda (subcheck)
    `( ,@base_regiment_forms
       [Expr ('lambda . ValidLambda)]
       ;; Lambda's are no longer allowed to have free-vars.
       [ValidLambda ,(lambda (ls)
		       (match ls
			 [((,v* ...) (,t* ...) ,e)
			  (and (subcheck e 'Expr)
			       (= (length v*) (length t*))
			       ;; No free vars allowed!!
			       (subset? ('TODOfree-vars e) v*))]
			 [,else #f]))]
       )))
;; TEMPORARY:
(define static-elaborate-grammar
  (cons 
   ;; After elaboration we have unionN:
   '[Expr ('unionN Expr ...)]  
   (filter (lambda (prod)
	     (match prod
	       ;; And we should not have unionList.
	       [(Prim 'unionList) #f]
	       ;; buildArray is also only for the meta language currently.
	       ;; (It's higher order.)
	       [(Prim 'buildArray) #f]

	       ;; nor should we have user-level applications:
	       [(Expr ('app Expr ...)) #f]
	       [,_ #t]))
     remove-unquoted-constant-grammar)
   ))

;=======================================================================

(define static-elaborate
  (build-compiler-pass 'static-elaborate
   `(input )
   `(output (grammar ,static-elaborate-grammar PassInput))
;   `(output (grammar ,remove-unquoted-constant-grammar PassInput))
;   '(output)
   (let ()

     ;; A table binding computable prims to an expression that evals
     ;; to a function which will carry out the primitive.
     ;; 
     ;; NOTE: this has some duplicated code with the simulators (in particular, wavescript_sim_library)
     ;;
     ;; CONVENTION: if the right hand side is a closure, it takes an env argument also.
    (define computable-prims 
      `((+ +) (- -) (* *) (/ /) (^ expt) 
	(g+ +) (g- -) (g* *) (g/ /) (g^ expt)
	(+_ +) (-_ -) (*_ *) (/_ /) (^_ expt) 
	(+. +) (-. -) (*. *) (/. /) (^. expt) 
	(+: +) (-: -) (*: *) (/: /) (^: expt) 

	;; This doesn't give it the right representation:
	;(gint ,(lambda (x) `(quote ,x)))
	;(gint (lambda (x) x))

	(= =) (< <) (<= <=) (> >) (>= >=)

	(car car) (cdr cdr) ;cons ;; [2006.11.05] removing cons.
	;(cons "cons-done-as-special-case")
	
	(stringToInt ,(lambda (v) 
			`(quote
			  ,(let ([x (string->number v)])
			     (if x 
				 (ASSERT fixnum? x)
				 (error 'stringToInt "couldn't convert string: ~s" v))))))
	(stringToFloat ,(lambda (v)
			 `(quote 
			   ,(let ([x (string->number v)])
			      (if x 
				  (ASSERT flonum? x)
				  (error 'stringToFloat "couldn't convert string: ~s" v))))))
	(stringToComplex ,(lambda (v)
			    (ASSERT string? v)
			    `(quote 
			      ,(let ([x (string->number v)])
				 (cond
				  [(not x) (error 'stringToComplex "couldn't convert string: ~s" v)]
				  [(real? x) (fl-make-rectangular x 0.0)]
				  [else (ASSERT cflonum? x)])))))

	(buildArray ,(lambda (n f)
		       `(vector . ,(map (lambda (i) `(app ,(code-expr f) (quote ,i))) (iota n)))))
	(length vector-length)
	(arr-get vector-ref)	
	;(List:make ,(trace-lambda List:make (n x) `',(make-list n x)))
	(List:make make-list)

	;; Need to put in a real "show" at some point:
	;(show ,(lambda (x) `',(format "~a" x)))
	
	(sqrtI ,(lambda (n) (floor (sqrt n)))) (sqrtF sqrt) (sqrtC sqrt)

	(absI16 fxabs) (absI fxabs) (absF flabs) (absC abs)

	(cos cos) (sin sin) (tan tan)
	(acos acos) (asin asin)	(atan atan)

#;#;
	(letrec ([tmp_142 Float (atan '-1.0)])
	  (if (<= tmp_142 '0.0)
	      (g+ tmp_142 '3.141592653589793)
	      (g- tmp_142 '3.141592653589793)))
	(letrec ([tmp_142 Float (atan '-0.0)])
	  (if (<= tmp_142 '0.0)
	      (g+ tmp_142 '3.141592653589793)
	      (g- tmp_142 '3.141592653589793)))

	(intToFloat fixnum->flonum)
	(intToComplex intToComplex-unimplented)

	(floatToInt flonum->fixnum)
	(floatToComplex ,(lambda (f) `(quote ,(+ f 0.0+0.0i))))
	
	(complexToInt complexToInt-unimplemented)
	(complexToFloat complexToFloat-unimplemented)

	(equal? equal?) (null? null?) (pair? pair?) ;number? 
	(even? even?) (odd? odd?) (not not)

	(GETENV ,(lambda (v)
		   (if (string? v)
		       (let ([x (getenv v)])
			 `(quote ,(if x x "")))
		       (error 'static-elaborate:GETENV "bad input: ~s" v)
		      )))
	(FILE_EXISTS ,(lambda (v)
		       (if (string? v)
			   `(quote ,(file-exists? v))
			   (error 'static-elaborate:FILE_EXISTS "bad input: ~s" v)
			   )))
	))

    (define computable-constants '(IS_SIM))

    (define (do-prim prim args env)
      (when (regiment-verbose) (display-constrained "DOING PRIM: " `[,prim 20] " " `[,args 30] "\n"))
      (if (ormap symbol? args)
	  (error 'do-prim "args contain unevaluated variable: ~a" args))
      (let ([entry (assq prim computable-prims)])
	(if entry
	    (if (procedure? (cadr entry))
		(apply (cadr entry) args)
		`(quote ,(eval `(,(cadr entry) ,@(map (lambda (a) `(quote ,a)) args)))))
	 (begin (error 'do-prim "cannot (currently) statically compute primitive! ~a" prim)
		`(,prim ,@args)))))
    
    (define (do-constant prim)
      (ASSERT (eq? prim 'IS_SIM))
      (if (eq? (compiler-invocation-mode) 'wavescript-simulator)
	  ''#t ''#f))

    ;; This does the actual beta-reduction
    (define (inline rator rands)
      (when (regiment-verbose)(display-constrained "INLINING " `[,rator 40] "\n"))
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
          
	  [(assert-type ,t ,[e]) e]
          [(begin ,(stmt) ...) (apply + stmt)]
          [(for (,i ,(st) ,(end)) ,(bod)) (+ st end bod)]
          [(iterate ,(fun) ,(bod)) (+ fun bod)]
          [(set! ,lhs ,(rhs))
           (if (eq? v lhs) 
	       9988
	       #;
	       (error 'static-elaborate:count-refs
		      "Hmm... shouldn't be counting references to mutable-var: ~s" v)
	       rhs
	       )]

	  [(tupref ,n ,m ,[x]) x]
	  [(tuple ,[args] ...) (apply + args)]
	  [(vector ,[args] ...) (apply + args)]
	  [(unionN ,[args] ...) (apply + args)]
          
	  [(,prim ,[rands] ...)
	   (guard (regiment-primitive? prim))
           (apply fx+ rands)]           
	  [(app ,[rator] ,[rands] ...) (+ rator (apply + rands))]
          [,unmatched
            (error 'static-elaborate:count-refs "unhandled syntax ~s" unmatched)])))

    
    (define get-mutable
      (core-generic-traverse 
       (lambda (x fallthru)
	 (match x
	   [(set! ,v ,[e]) (cons v e)]
	   [(vector ,[x*] ...) (apply append x*)]
	   [,other (fallthru other)]))
       (lambda (ls k) (apply append ls))))

    ;; TEMP: (ironic) HACK:
    (define mutable-vars 'uninit)

    ;; TODO FINISH:
    #;
    (define count-app-refs
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
	  [(assert-type ,t ,[e]) `(assert-type ,t ,e)]
          [(lambda ,formals ,types ,expr)
	   `(lambda ,formals ,types
	      ,(substitute
		(filter (lambda (x)
			  (not (memq (car x) formals)))
			mapping)
		expr))]
	  [(for (,i ,[st] ,[en]) ,bod)
	   `(for (,i ,st ,en)		
		,(substitute
		  (filter (lambda (x) (not (eq? (car x) i))) mapping)
		  bod))]
	  [(begin ,[arg] ...) `(begin ,arg ...)]
	  [(set! ,v ,[rhs])
	   (if (memq v (map car mapping))
	       (error 'static-elaborate:substitute "shouldn't be substituting against a mutated var: ~s" v))
	   `(set! ,v ,rhs)]

	  [(tupref ,n ,m ,[x]) `(tupref ,n ,m ,x)]
	  [(tuple ,[args] ...) `(tuple ,args ...)]
	  [(vector ,[args] ...) `(vector ,args ...)]

          [(if ,[test] ,[conseq] ,[altern])
	   `(if ,test ,conseq ,altern)]
	  [(letrec ([,lhs* ,type* ,rhs*] ...) ,expr)
	   (let ((newmap (filter (lambda (x)
				   (not (memq (car x) lhs*)))
				 mapping)))	     
	   `(letrec ([,lhs* ,type* ,(map (lambda (x) (substitute newmap x)) rhs*)] ...)
	      ,(substitute newmap expr)))]
	  [(,prim ,[rands] ...) (guard (regiment-primitive? prim))
	   `(,prim ,rands ...)]
	  [(app ,[rator] ,[rands] ...) `(app ,rator ,rands ...)]
          [,unmatched
            (error 'static-elaborate:substitute "invalid syntax ~s" unmatched)])))


    (define not-available (unique-name "NotAvailable"))
    ;; This is a simple struct used by *getval* to indicate that the
    ;; value is code, rather than a value-proper.
    (reg:define-struct (code expr))
    ;; Might consider adding "value" too, for safety.

    ;; [2006.03.30] Looks like this is no longer true:
    ;;----------------------------------------
    ;; This does a walk down the program and, instead of doing beta
    ;; reductions, it converts direct apps to let's and then inlines
    ;; any lets than only have one reference. 
    ;; (Later when new let's get introduced for them, they will
    ;; hopefully be at a smaller scope.)
    ;;----------------------------------------

    ;;   The "env" argument binds names to *code*.  Or if the code is
    ;; unavailable, to the special value 'not-available'.
    ;;
    ;; There's a third slot in each env entry that was used for reference counting... unused currently
    (define process-expr           
      (lambda (expr env)
	;(printf "ENV: ~a\n" env)
	(let ([PE-result
	 (letrec ([available? ;; Is this value available at compile time.
		  (lambda (x)
		    (if (eq? x not-available) #f
			(match x
			   [(quote ,datum)         #t]
			   [(lambda ,vs ,tys ,bod) #t]
			   ;[(tuple ,args ...) (guard (andmap available? args)) #t]
			   ;; A tuple is available even if all its components aren't.
			   ;[(tuple ,[args] ...) (andmap id args)]
			   [,var (guard (symbol? var)
					(not (memq var mutable-vars)))
				 (let ((entry (assq var env)))
				   (and entry (available? (cadr entry))))]
			   [(assert-type ,t ,[e]) e]
			   
			   ;; Streams are values, and they're available:
			   ;; Should we have to go deeper here to make
			   ;; sure they're fully available???
			   [,sv (guard (stream-val? sv)) #t]

			   [,else #f])))]
		 
		 ;; Is it, not completely available, but a container that's available?
		 [container-available? 
		  (lambda (x)
		    (if (eq? x not-available) #f
			(match x 
			  [(quote ,datum) #t]
			  [(cons ,x ,y) #t]
			  [(tuple ,args ...) #t]
			  [(vector ,args ...) #t]
			  [,var (guard (symbol? var)
				       (not (memq var mutable-vars)))
				(let ((entry (assq var env)))
				  (and entry (container-available? (cadr entry))))]
			  [(assert-type ,t ,[e]) e]

			  [,sv (guard (stream-val? sv)) #t]

			  [,else #f]
			  )
		    ))]
 
		 [stream-val? 
		  (lambda (exp)
		    (match exp
		      [(,prim ,args ...)
		       (guard (regiment-primitive? prim))
		       (match (caddr (get-primitive-entry prim))
			 [(Stream ,t) #t]
			 [,else #f])]
		      [(assert-type ,t ,[e]) e]
		      [,else #f])
		    )]

		 [getval ;; If available, follow aliases until you have a real value expression:
		  (lambda (x)
		    (match x
		      [(quote ,datum) datum]
		      [(lambda ,vs ,tys ,bod) (make-code `(lambda ,vs ,tys ,bod))]
		      
		      ; For these three cases, if the args are available we can make a value-proper.
		      [(tuple ,args ...)  (make-code x)]
		      [(vector ,args ...) (make-code x)]
		      [(cons ,a ,b)       (make-code x)]
		      
		      [,sv (guard (stream-val? sv)) x]

		      [,var (guard (symbol? var))
			    (getval (cadr (assq var env)))]
		      [(assert-type ,t ,e) (getval e)]
		      [,else (error 'static-elaborate "getval bad input: ~a" x)]))]

		 [getlist ;; Get values until you have the whole list.
		  (lambda (x)
		    (if (container-available? x)			
			(let ([val (getval x)])
			  (if (code? val)
			      (match (code-expr val)
				[(cons ,a ,b) (cons a (getlist b))]
				;;[(assert-type ,t ,[e]) e]
				)
			      (match val
				[(,x* ...) `(',x* ...)]
				;;[(,x* ...) `(',x* ...)]
				;;[,other (error 'getlist "not a list-constructor: ~s" other)])
				)))
			#f))]
		 )
	  
        (match expr
          [(quote ,datum) `(quote ,datum)]
	  ;; This does constant inlining:
	  [,prim (guard (regiment-primitive? prim))
		 (if (memq prim computable-constants)
		     (do-constant prim)
		     prim)]
          [,var (guard (symbol? var) (memq var mutable-vars)) var]
          [,var (guard (symbol? var))	  
		(match (assq var env)
		  [#f (error 'static-elaborate "variable not in scope: ~a" var)]
		  ;; Anything let-bound with a reference count of 1 gets inlined:
;		  [(,_ ,x 1) 
;`		   (printf "REFCOUNT1: ~a\n" var)
;		   x]
		  ;; Inline constants:
		  [(,_ (quote ,d) ,__) 
		   (guard (atom? d)) ; Don't inline constants requiring allocation.
		   `(quote ,d)]

		  ;; Inline lambda expressions as long as they don't capture mutables.
;		  [(,_ (lambda ,vars ,types ,bod) ,__)
;		   (guard (null? (intersection (core-free-vars bod) mutable-vars)))
;		   `(lambda ,vars ,types bod)]

		  ;; FIXME: BUG: SHOULD THIS HAVE A COMMA?:
		  [(,_ ,not-available ,__) var]
		  ;; Resolve aliases:
		  [(,_ ,v ,__) (guard (symbol? v)) (process-expr v env)]
		  ;; Otherwise, nothing we can do with it.
		  [,else   var])
		;; This appears to disable the system here:
		;(if (available? var) (getval var) var)
		]

          [(assert-type ,t ,[e]) `(assert-type ,t ,e)]
          [(lambda ,formals ,types ,expr)
	   `(lambda ,formals ,types
	      ,(process-expr expr 
			     (append (map list formals 
					  (make-list (length formals) not-available)
					  ;; The "reference count" for each var 
					  (make-list (length formals) -1)) 
				     env)))]
          
          ;; Don't go inside iterates for now:
	  ;; I'm not confident that we've updated the inliner to deal with side-effects.
	  ;; [2006.11.05] DANGER, ENABLING NOW BUT AM STILL NOT CONFIDENT:

	  ;; We can't inline teh iterator state however, don't try for now.
	  ;; [2006.11.05] Currently this exposes a bug while executing demo7
;	  [(iterate (letrec ([,lhs* ,ty* ,[rhs*]] ...) ,bod) ,[strm])
;	   (let ([newenv (append (map (lambda (v rhs) (list v rhs 99999)) lhs* rhs*) env)])
;	     `(iterate (letrec ([,lhs* ,ty* ,rhs*] ...) ,(process-expr bod newenv)) ,strm))]
          [(iterate ,[fun] ,[strm])  `(iterate ,fun ,strm)]
	  ;; This is altogether a hack, we need to purify these iterates.

;          [(iterate ,fun ,[strm])  `(iterate ,fun ,strm)]
	  
	  ;; Don't go inside for loops for now:
	  [(for (,i ,[st] ,[en]) ,bod)
	   (let ([newenv (cons `(,i not-available 99999) env)])	     
	     `(for (,i ,st ,en) ,(process-expr bod newenv)))]
	  
          [(begin ,[args] ...) `(begin ,args ...)]
          [(set! ,v ,[rhs]) `(set! ,v ,rhs)]
	  
	  ;; TODO: This doesn't handle mutually recursive functions yet!!
	  ;; Need to do a sort of intelligent "garbage collection".
	  [(letrec ([,lhs* ,type* ,rhs*] ...) ,expr)
	   ;(printf "BINDING: ~s\n" lhs*)
	   (if (null? lhs*)
	       (process-expr expr env)
	       ;; TODO: FIXME: NASTY COMPLEXITY:
	       ;; This has complexity number vars * program size.
	       ;; Inefficient ref-counting.
	   (let* ([newenv (append (map list lhs* rhs* 
				       (map (lambda (lhs)
					      ;; TEMP [2006.02.18]: Disabling ref-counting here:
					      9999
					      ;(count-refs lhs (cons expr rhs*))
					      )
					 lhs*))
				  env)]
		  [newrhs* (map (lambda (x) (process-expr x newenv)) rhs*)]
;		  [_ 	   (break)]
		  [newbod (process-expr expr newenv)]
;		  [__ 	   (break)]
                  ;; How much does each bound variable get referenced:
		  [occurs (map (lambda (v myrhs) 
				 (apply + (count-refs v newbod)
					(map (lambda (x) (count-refs v x)) 
					     (remq myrhs newrhs*))))
			       lhs* newrhs*)]
		  [newbinds (filter id
				    (map 
				     (lambda (lhs type rhs refs)
				       ;; Here we eliminate dead code:
				       (and (> refs 0)
					    `(,lhs ,type ,rhs)))
				     lhs* type* newrhs* occurs))])
	     ;(disp "OCCURS: " lhs* occurs)
	     `(letrec ,newbinds ,newbod)))]
         
	  ;; Here we do computation if the arguments are available:
          [(if ,test ,[conseq] ,[altern])
	   ;(disp "CONDITIONAL: " test)
	   (let ([newtest (process-expr test env)])
	     (if (available? newtest)  
		 (begin 
		   (if (getval newtest) conseq  altern))		 
		 `(if ,newtest ,conseq ,altern))
	     )]

#;
	  [(buildArray ,[n] ,[f])
	   (inspect `(tryingbuildarr!! ,(available? n) ,(available? f) ,env))
	   ]
	  

	  [(tuple ,[args] ...) `(tuple ,args ...)]
	  [(unionN ,[args] ...) `(unionN ,args ...)]
	  [(vector ,[x*] ...)
	   (if (andmap available? x*)
	       ;(lambda (x) (match (getval x) [(quote ,c) c]))
	       (let ([vals (map getval x*)])
		 (if (ormap code? vals)
		     ;; Can't stick code within a constant:
		     `(vector . ,x*)
		     `(quote ,(list->vector vals))
		     ))
	       `(vector . ,x*))]
	  #;
	  [(length ,[vec])
	   ;(inspect (cons (available? vec) vec))
	   (if (available? vec)
	       `(quote ,(match vec [(quote ,v) (vector-length v)]))
	       `(length ,vec)
	       )]
	  
	  ;; TODO: vector-ref.

	  ;; First we handle primitives that work on container types: 
	  ;; NOTE: these could throw away side effects when operating on object code!!!
	  ;; DANGER! FIXME FIXME 
	  ;; ================================================================================
	  [(tupref ,ind ,len ,[tup])
	   (if (container-available? tup)
	       (match (code-expr (getval tup))
		 [(tuple ,args ...)
		  (unless (eq? (length args) len)
		    (error 'static-elaborate "couldn't perform tupref, expected length ~s, got tuple: ~s"
			   len `(tuple ,@args)))
		  (list-ref args ind)]
		 [,else (error 'static-elaborate:process-expr "implementation error, tupref case")])
	       `(tupref ,ind ,len ,tup))]
	  [(car ,[x]) 
	   (if (container-available? x)
	       (let ([val (getval x)])
		 (if (code? val)
		     (match (code-expr val)
		       [(cons ,a ,b) a]
		       [,x (error 'static-elaborate:process-expr "implementation error, car case: ~s" x)])
		     `(quote ,(car val))))
	       `(car ,x))]
	  [(cdr ,[x]) 
	   (if (container-available? x)
	       (let ([val (getval x)])
		 (if (code? val)		     
		     (match val
		       [(cons ,a ,b) b]		       
		       [,x (error 'static-elaborate:process-expr "implementation error, cdr case: ~s" x)])
		     `(quote ,(cdr val))))
	       `(cdr ,x))]
	  [(List:length ,[x])
;	   (inspect `(LEN ,x ,env))
	   (if (container-available? x)
	       (let ([ls (getlist x)])
;		 (inspect `(lenavail ,ls ,env))
		 (if (list? ls)
		     `(quote ,(length ls))
		     `(List:length ,x)
		     ))
	       `(List:length ,x))]
	  
	  ;; TODO: This is too strict, we can get out elements even if the tail of the list is unknown.
	  [(List:ref ,[x] ,[i])
	   (if (and (available? i) (container-available? x))
	       (let ([ls (getlist x)])
		 (if (list? ls)
		     (match i [(quote ,i) (list-ref ls i)])
		     `(List:ref ,x ,i)
		     ))
	       `(List:ref ,x ,i))]

	  ;; Here unionList must be eliminated, replaced by a hardwired unionN.
	  [(unionList ,[x])
	   (if (container-available? x)
	       (let ([ls (getlist x)])
		 (if (list? ls)
		     `(unionN ,@ls)
		     (begin 
		       (warning 'static-elaborate "couldn't elaborate unionList, only got: ~s"
				ls)
		       `(unionList ,x))
		     ))
	       (begin (error 'static-elaborate "couldn't elaborate unionList, value unavailable:~s"
			     `(unionList ,x))
		      `(unionList ,x))
			       )]

	  [(List:map ,[f] ,[ls])
	   (if (container-available? ls)
	       (let ([val (getlist ls)])		 
		 (if (code? val)
		     (match (code-expr val)
		       [(cons ,a ,b) `(cons (app ,f ,a) ,(process-expr `(List:map ,f ,b) env))]
		       [,x (error 'static-elaborate:process-expr "implementation error, map case: ~s" x)]) 
		     (match val
		       [() ''()]
		       [(,h . ,[t]) `(cons (app ,f ,h) ,t)]))
		 )
	       `(List:map ,f ,ls))]

	  ;; Special case, show is identity on strings:
	  [(show ',str) (guard (string? str)) `',str]

	  ;; ================================================================================

	  ;; All other computable prims:
          [(,prim ,[rand*] ...) (guard (regiment-primitive? prim))
	   ;(disp "PRIM: " prim (map available? rand*) rand* )	  
	   (if (and 		
		(andmap available? rand*)
	       ;(assq prim computable-prims)
		;; Exceptions:
		(not (assq prim wavescript-effectful-primitives))
		(not (assq prim wavescript-stream-primitives))
		(not (assq prim regiment-distributed-primitives))
		;; Special exceptions:
		;; We don't want to makeArray in the object code!
		;; (Kind of inconsistent that we *do* currently do List:make.)
		(not (memq prim '(show cons gint makeArray
				       m_invert)))
		)
	       (do-prim prim (map getval rand*) env)
	       `(,prim ,rand* ...))]

	  ;; Here we convert to a letrec.  Rename-var insures that we
	  ;; don't get any accidental variable capture:
;	  [((lambda ,formals ,expr) ,rands ...)
;	   (substitute (map list formals rands) expr)]
	  [(app ,[rator] ,[rands] ...)
;	   (disp "APP" rator (available? rator) env)
	   (if (available? rator)
	       (let ([code (code-expr (getval rator))])
		 (if (not (null? (intersection mutable-vars (core-free-vars rator))))
		     (error 'static-elaborate 
			    "can't currently inline rator with free mutable vars!: ~s"
			    code)
		     (inline code rands)))
	       (begin 
		 (printf "  Can't inline rator: ~s\n" rator)
		 `(app ,rator ,rands ...)))]

          [,unmatched
            (error 'static-elaborate:process-expr "invalid syntax ~s" unmatched)]))])

	  (DEBUGASSERT (compose not code?) PE-result)
	  PE-result
	  )
	))
    
    (lambda (expr)
      (match expr	    
        [(,input-language (quote (program ,body ,type)))
	 (set! mutable-vars (get-mutable body))
	 ;; Run until we reach a fixed point.
	 (let loop ([oldbody body]
		     [body (process-expr body '())])
	    (if (equal? oldbody body)	   
		`(static-elaborate-language '(program ,body ,type))
		(loop body (process-expr body '()))))]
	)))))


(define-testing these-tests 
  `( 

    ["Make sure it folds some simple constants." 
     (static-elaborate '(foo '(program (+_ '3 '4) notype)))
     (static-elaborate-language '(program '7 notype))]

    [(static-elaborate
      '(foo '(program
	      (letrec ([f _ (lambda (x) (_) '#t)])
		(app f '3939)) notype)))
     (static-elaborate-language '(program '#t notype))]
   
    ["Reduce away fact of 6." 
     (static-elaborate '(foo '(program 
      (letrec ([fact _ (lambda (n) (_) 
			       (if (= '0 n) '1 (*_ n (app fact (-_ n '1)))))])
	(app fact '6))
      notype)))
     (static-elaborate-language '(program '720 notype))]
    
    ["Reduce a tuple reference."
     (static-elaborate '(foo '(program (letrec ([x T (tuple '3 '4)]) (+_ '100 (tupref 0 2 x))) notype)))
     (static-elaborate-language '(program '103 notype))]

    ["Reduce a primop underneath a lambda."
     (static-elaborate '(foolang '(program (lambda (x) (_) (cons (+_ '3 '4) x)) notype)))
     (static-elaborate-language '(program (lambda (x) (_) (cons '7 x)) notype))]

    [(static-elaborate '(foo '(program 
			       (letrec ([f _ (lambda (x) (_) '#t)])
				 (letrec ([loop _ (lambda (n) (_)
							  (if (= '0 n)
							      world
							      (rfilter f (app loop (-_ n '1)))))])
				   (app loop '5)))
			       notype)))
     (static-elaborate-language '(program
	    (letrec ([f _ (lambda (x) (_) '#t)])
	      (rfilter
	       f
	       (rfilter f (rfilter f (rfilter f (rfilter f world))))))
	    notype))]

    ["Simple test to make sure we keep the quotes on:" 
     (static-elaborate '(foolang '(program (cons (+_ '3 '4) world) notype)))
     (static-elaborate-language '(program (cons (quote 7) world)
		 notype))]

    ,(let ([prog '(static-elaborate-language '(program (cons (khood-at '30 '40 '50) world) notype))])
       `["Now run with a regiment-prim that we shouldn't be able to elaborate" 
	 (static-elaborate ',prog)
	 ,prog])

    ,(let ([prog '(iterate (lambda (x ___VIRTQUEUE___) (Int (VQueue Int))
				   (letrec ([y Bool '#t])
				     (begin (for (i '1 '10) (set! y '#f))
					    (if y (emit ___VIRTQUEUE___ '77)
						(emit ___VIRTQUEUE___ '100))
					    ___VIRTQUEUE___)
				     )))])
       `["(non-persistent) Mutable variable inside iterate.  This was a bug with conditional-reduction."
	 (static-elaborate '(foolang '(program ,prog (Stream Int))))
	 (static-elaborate-language '(program ,prog (Stream Int)))])
    ))

(define-testing test-this (default-unit-tester
		    " 4: Static-Elaborate: to evaluate the first stage of computation"
		    these-tests))

(define test04 test-this)
(define tests04 these-tests)
(define test-static-elaborate test-this)
(define tests-static-elaborate these-tests)

) ;; End module.


; (require pass04_static-elaborate)
