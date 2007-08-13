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

;;; TODO: This pass is ALREADY way too complex.  But for completeness
;;; it should really lift out letrecs from the right hand side of
;;; letrecs.  But since we can't guarantee that unique variable naming
;;; holds in the interior of this pass....

;; [2007.03.29] Adding ugly hack to only evaluate Array:build
;; *outside* of iterates...

;;;; TODO: MAKE MUTABLE VARS SAFE!

(module static-elaborate mzscheme
  (require (all-except "../../../plt/common.ss" match let-match match-lambda)
;; WEIRD: rn-match is SLOWER for plt on this pass:	   
;	   (all-except "../../../plt/rn-match.ss" )
	   (all-except "../../../plt/iu-match.ss" )
	   "../normalize_source/remove-unquoted-constant.ss"
	   )
  (provide 
   ;(all-defined)
   static-elaborate
   static-elaborate-grammar
   test-this these-tests
   test-static-elaborate
   )

  (chezimports )

;  (IFCHEZ (import rn-match) (void))

;; Dummy implementation for PLT:
 (IFCHEZ (begin)
	 (begin (define par list)
		(define par-list (lambda (th*) (map (lambda (th) (th)) th*)))
		(define par-map map)))

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
  (list* 
   ;; After elaboration we have unionN:
   '[Expr ('unionN Expr ...)]  
   '[Expr ('foreign-app Const Expr Expr ...)]
   '[Expr ('foreign Const Const)]
   '[Expr ('foreign_source Const Const)]
   ;'[Expr ('foreign Const PolyConst PolyConst)]
    ;; This is quirky, will need this for null lists:
   ;'[PolyConst Const]
   ;'[PolyConst ('assert-type Type ('quote Datum))]
   (filter (lambda (prod)
	     (match prod
	       ;; And we should not have unionList.
	       [(Prim 'unionList) #f]

	       [(Prim ',p) (guard (assq p meta-only-primitives)) #f]
	       [(Expr ('foreign . ,_)) #f]

	       ;; nor should we have user-level applications:
	       [(Expr ('app Expr . ,_)) #f]
	       [,_ #t]))
     remove-unquoted-constant-grammar)
   ))


(define (make-nested-letrecs binds body)
  (if (null? binds) body
      `(letrec (,(car binds)) ,(make-nested-letrecs (cdr binds) body))))
(define (make-nested-cons ls)
  (match ls
    [() ''()]
    [(,a . ,[b]) `(cons ,a ,b)]))

;=======================================================================


(define static-elaborate
  (build-compiler-pass 'static-elaborate
   `(input )
   `(output (grammar ,static-elaborate-grammar PassInput))
;   `(output (grammar ,remove-unquoted-constant-grammar PassInput))
;   '(output)
   (let ()

     ;; This expects that you verify available? first.
     (define (outer-getval env)
       (lambda (x)
	 (match x
	   [(quote ,datum) datum]
	   [(lambda ,vs ,tys ,bod) (make-code `(lambda ,vs ,tys ,bod))]
	   ;; For these three cases, if the args are available we can make a value-proper.
	   [(tuple ,args ...)  (make-code x)]
	   [(vector ,args ...) (make-code x)]
	   [(cons ,a ,b)       (make-code x)]
	   ;; This is "available" but it's still code:
	   [,sv (guard (stream-val? sv)) (make-code x)]
	   [,var (guard (symbol? var))
		 ((outer-getval env) (cadr (assq var env)))]
	   [(,annot ,_ ,e) (guard (annotation? annot)) ((outer-getval env) e)]
	   ;[(src-pos ,_ ,e) ((outer-getval env) e)]
	   ;[(assert-type ,_ ,e) ((outer-getval env) e)]

	   [,else (error 'static-elaborate "getval bad input: ~a" x)])))
     [define stream-val? 
      (lambda (exp)
	(match exp
	  [(,prim ,args ...)
	   (guard (regiment-primitive? prim))
	   (match (caddr (get-primitive-entry prim))
	     [(Stream ,t) #t]
	     [,else #f])]
	  [(,annot ,_ ,[e]) (guard (annotation? annot)) e]
	  [,else #f])
	)]

     ;; This quotes the object if its a real value.
     (define (quote-value v)
       (if (code? v) (code-expr v) `',v))

     ;; This is tricky because it has to respect representation constraints:
     (define (generic-div a b)
       (cond
	[(fixnum? a) (fx/ a b)] ;; Int and Int16
	[(and (integer? a) (exact? a)) ;; This is an int64:
	 (ASSERT integer? b) 
	 (floor (/ a b))]
	[(flonum? a) (fl/ a b)] ;; Float and Double
	[else (error 'static-elaborate:generic-divide "unknown number representations: ~s ~s" a b)]
	))

     ;; A table binding computable prims to an expression that evals
     ;; to a function which will carry out the primitive.
     ;; 
     ;; NOTE: this has some duplicated code with the simulators (in particular, wavescript_sim_library)
     ;;
     ;; CONVENTION: if the right hand side is a closure, it takes an env argument also.
    (define computable-prims 
      `((+ +) (- -) (* *) (/ /) (^ expt) 
	(g+ +) (g- -) (g* *) (g^ expt)
	(g/ ,(lambda (env a b) `',(generic-div a b)))
	(+_ +) (-_ -) (*_ *) (/_ /) (^_ expt) 
	(+. +) (-. -) (*. *) (/. /) (^. expt) 
	(+: +) (-: -) (*: *) (/: /) (^: expt) 
	(+I16 +) (-I16 -) (*I16 *) (/I16 /) (^I16 expt) 
	(+I64 +) (-I64 -) (*I64 *) (/I64 /) (^I64 expt) 

	;; This doesn't give it the right representation:
	;; We don't yet know for sure what the type should be.
	;(gint ,(lambda (x) `(quote ,x)))
	;(gint (lambda (x) x))

	(= =) (< <) (<= <=) (> >) (>= >=)

	(car car) (cdr cdr) ;cons ;; [2006.11.05] removing cons.
	;(cons "cons-done-as-special-case")

	(realpart cfl-real-part)
	(imagpart cfl-imag-part)

	(string-append string-append)
	
	(stringToInt ,(lambda (env v) 
			`(quote
			  ,(let ([x (string->number v)])
			     (if x 
				 (ASSERT fixnum? x)
				 (error 'stringToInt "couldn't convert string: ~s" v))))))
	(stringToFloat ,(lambda (env v)
			 `(quote 
			   ,(let ([x (string->number v)])
			      (if x 
				  (ASSERT flonum? x)
				  (error 'stringToFloat "couldn't convert string: ~s" v))))))
	(stringToComplex ,(lambda (env v)
			    (ASSERT string? v)
			    `(quote 
			      ,(let ([x (string->number v)])
				 (cond
				  [(not x) (error 'stringToComplex "couldn't convert string: ~s" v)]
				  [(real? x) (fl-make-rectangular x 0.0)]
				  [else (ASSERT cflonum? x)])))))

	(String:length string-length)
	(String:explode string->list)
	(String:implode list->string)
	
	;; This is VERY slow... worse than I thought.  Building a 256
	;; element filter array statically takes an additional three
	;; seconds of elaboration time!!!
#;
	(Array:build ,(lambda (env n f)
		       `(vector . ,(map (lambda (i) `(app ,(code-expr f) (quote ,i))) (iota n)))))

	;; Hacking it thusly:
	(Array:build 
	 ,(lambda (env n f)
	    ;; Jeez, this is inefficient because we've already done this at least once:
	    (let* ([fv* (list->set (core-free-vars (code-expr f)))]
		   [real-code 

		    `(wavescript-language
		      '(let ,(map (lambda (fv)
				    (let ([val ((outer-getval env) fv)])
				      (ASSERT (not (code? val)))
				      (list fv `',val)))
			       fv*)
			 ,(strip-types (code-expr f))))
#;
		    `(let ,(map (lambda (fv) 
				  (let ([val ((outer-getval env) fv)])
				    (ASSERT (not (code? val)))
				    (list fv `',val)))
			     fv*)
		       (import wavescript_sim_library_push)
		       ,(strip-types (code-expr f)))
		    ])

	      (let ([real-closure (eval real-code)]) 
		;`(vector ,@(list-build n (lambda (i) `',(real-closure i))))
		`',(vector-build n (lambda (i) (real-closure i)))
		))
	    ))

	;(length vector-length)
	(Array:ref vector-ref)	
	(Array:length vector-length)	
	(Array:toList vector->list)


	;; We don't repeat the Array:build hack here... sort of inconsistent.
	(List:build ,(lambda (env n f)
		       (make-nested-cons (map (lambda (i) `(app ,(code-expr f) (quote ,i))) (iota n)))))

	;(List:make ,(trace-lambda List:make (n x) `',(make-list n x)))
	(List:make make-list)
;	(List:append append)
	(List:toArray list->vector)

	;; Need to put in a real "show" at some point:
	;(show ,(lambda (x) `',(format "~a" x)))
	
	(sqrtI ,(lambda (env n) (floor (sqrt n)))) (sqrtF sqrt) (sqrtC sqrt)

	(absI16 fxabs) (absI fxabs) (absF flabs) (absI64 abs) (absC abs)
	(max max) (min min)
	(roundF round)

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

;	(intToInt16 (lambda (x) x))
;	(intToInt64 (lambda (x) x))
	(intToFloat fixnum->flonum)
	(intToComplex intToComplex-unimplented)

	(intToChar integer->char)
	(charToInt char->integer)

;	(int64ToInt16  (lambda (x) (ASSERT int16? x) x))
;	(int64ToInt    (lambda (x) (ASSERT int32? x) x))
;	(int64ToFloat    exact->inexact)
;	(int64ToDouble   exact->inexact)
;	(int64ToComplex  (lambda (n) (+ n 0.0+0.0i)))
	
	(floatToInt flonum->fixnum)
;	(floatToDouble (lambda (x) x))
	(floatToComplex ,(lambda (env f) `(quote ,(+ f 0.0+0.0i))))
	
	(complexToInt complexToInt-unimplemented)
	(complexToFloat complexToFloat-unimplemented)

	(null? null?) (pair? pair?) ;number? 
	(even? even?) (odd? odd?) (not not)

	(GETENV ,(lambda (env v)
		   (if (string? v)
		       (let ([x (getenv v)])
			 `(quote ,(if x x "")))
		       (error 'static-elaborate:GETENV "bad input: ~s" v)
		      )))
	(SHELL ,(lambda (env v)
		   (if (string? v)
		       (let ([x (system-to-str v)])
			 `(quote ,(if x x "")))
		       (error 'static-elaborate:SHELL "bad input: ~s" v)
		      )))

	(FILE_EXISTS ,(lambda (env v)
		       (if (string? v)
			   `(quote ,(file-exists? v))
			   (error 'static-elaborate:FILE_EXISTS "bad input: ~s" v)
			   )))
	))

    (define computable-constants '(IS_SIM))

    (define (do-prim prim args env)
      (IFDEBUG (when (regiment-verbose) (display-constrained "DOING PRIM: " `[,prim 20] " " `[,args 30] "\n")) (begin))
      (if (ormap symbol? args)
	  (error 'do-prim "args contain unevaluated variable: ~a" args))
      (let ([entry (assq prim computable-prims)])
	(if entry
	    (if (procedure? (cadr entry))
		(apply (cadr entry) env args)
		`(quote ,(eval `(,(cadr entry) ,@(map (lambda (a) `(quote ,a)) args)))))
	 (begin (error 'do-prim "cannot (currently) statically compute primitive! ~a" prim)
		`(,prim ,@args)))))
    
    (define (do-constant prim)
      (ASSERT (eq? prim 'IS_SIM))
      
      ;; This is deprecated now.  Use GETENV and the env variable WSARCH
      (ASSERT #f)
      
      (if (eq? (compiler-invocation-mode) 'wavescript-simulator)
	  ''#t ''#f))

    ;; This does the actual beta-reduction
    (define (inline rator rands)
      (IFDEBUG (when (regiment-verbose)(display-constrained "INLINING " `[,rator 40] "\n")) (begin))
      (match rator
#;
	[(lambda ,formals ,type ,body)
	 (substitute (map list formals rands) body)]

	;; [2007.04.05] Experimenting with this strategy.  Just
	;; let-bind at the top of the body to avoid code duplication.
      [(lambda ,formals ,type ,body)
       (make-nested-letrecs
	(map list formals (map (lambda _ `',(unique-name 'newtype)) rands) rands)
	body)]
      
      [,other (error 'static-elaborate:inline "bad rator: ~a" other)]))
   
   ;; [2007.04.05] Seems like this should use generic-traverse...
    ;; [2007.04.16] ITS SPENDING A LOT OF TIME IN THIS FUNCTION!
    ;; FIXME FIXME FIXME! COUNT REFS ONCE!
    (define count-refs
      (lambda (v expr)
        (match expr
          [(quote ,datum) 0]
          [,var (guard (symbol? var))
		(if (eq? var v) 1 0)]
          [(if ,[test] ,[conseq] ,[altern])  (+ test conseq altern)]
	  [(wscase ,[x] (,pat* ,[rhs*]) ...) (apply + x rhs*)]

	  [(letrec ([,lhs* ,type* ,rhs*] ...) ,expr)
	   (if (memq v lhs*) 0
	       (+ (count-refs v expr)
		  (apply + (map (lambda (x) (count-refs v x)) rhs*))))]
          [(lambda ,formals ,types ,expr)
	   (if (memq v formals) 0 (count-refs v expr))]
          
;	  [(,ann ,_ ,[e]) (guard (annotation? ann)) e]
	  [(src-pos ,_ ,[e]) e]

	  [(tupref ,n ,m ,[x]) x]
	  [(tuple ,[args] ...) (apply + args)]
	  [(,prim ,[rands] ...)
	   (guard (regiment-primitive? prim))
           (apply fx+ rands)]
          [(begin ,(stmt) ...) (apply + stmt)]

	  [(unionN ,[args] ...) (apply + args)]
	  [(vector ,[args] ...) (apply + args)]

	  [(assert-type ,_ ,[e]) e]
          [(set! ,lhs ,(rhs))
           (if (eq? v lhs) 
	       9988
	       #;
	       (error 'static-elaborate:count-refs
		      "Hmm... shouldn't be counting references to mutable-var: ~s" v)
	       rhs
	       )]
          [(for (,i ,(st) ,(end)) ,(bod)) (+ st end bod)]
          [(while ,[tst] ,(bod)) (+ tst bod)]
          [(iterate ,(fun) ,(bod)) (+ fun bod)]

	  [(,app ,[rator] ,[rands] ...) 
	   (guard (eq-any? app 'app 'construct-data))
	   (+ rator (apply + rands))]
	  [(foreign-app ',realname ,[arg*] ...)	 (apply + arg*)]

          [,unmatched
            (error 'static-elaborate:count-refs "unhandled syntax ~s" unmatched)])))


    ;; These are vars that are mutated with set!
    ;; [2007.07.26] Should alse include any vars that are bound to values containing arrays!
    (define get-mutable
      (core-generic-traverse 
       (lambda (x fallthru)
	 (match x
	   [(set! ,v ,[e]) (cons v e)]
	   [(vector ,[x*] ...) (apply append x*)]
	   ;; Any kind of array in the type makes the variable potentially mutable.
	   
	   ;; Sigh. this causes problems either way.
	   [,bf (guard (binding-form? bf))
		(let ([mutvars (map (lambda (v t) 
				      (if (type-containing-mutable? t) (list v) ()))
				 (binding-form->vars bf)
				 (binding-form->types bf))])
;		  (unless (andmap null? mutvars) (inspect mutvars))
		  (apply append
  		   (append mutvars
		     (map get-mutable 
		       (append (binding-form->scoped-exprs bf)
			       (binding-form->unscoped-exprs bf)))))
		  )]
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
	 (letrec (
		  ;; This tells us that a function value is ready to
		  ;; be executed -- all the variables in its
		  ;; environment are available.
		  [fully-available? 
		   (lambda (x)
		     (match x
		       [(lambda ,vs ,tys ,bod)
			;; FIXME: INEFFICIENT INEFFICIENT INEFFICIENT INEFFICIENT INEFFICIENT 
			(let ([fv* (difference (core-free-vars bod) vs)])
			  ;(when (regiment-verbose) (unless (null? fv*) (printf "  FV: ~s\n" fv*)))
			  ;(inspect (map available? fv*))
			  (andmap available? fv*))]
		       [,else (available? x)]))]
		  [available? ;; Is this value available at compile time.
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
			   [(,ann ,_ ,[e]) (guard (annotation? ann)) e]
			   
			   ;; Streams are values, and they're available:
			   ;; Should we have to go deeper here to make
			   ;; sure they're fully available???
			   [,sv (guard (stream-val? sv)) #t]
			   
			   [,else #f])))]

		  [foreign-fun? 
		   (lambda  (x)
		     (match x 
		       [,var (guard (symbol? var))
			     (let ((entry (assq var env)))
			       (and entry (foreign-fun? (cadr entry))))]
		       [(,ann ,_ ,[e]) (guard (annotation? ann)) e]
		       [(foreign ',name . ,_)  name]
		       [,else #f]))]
		 
		 ;; Is it, not completely available, but a container that's available?
		 [container-available? 
		  (lambda (x)
		    (if (eq? x not-available) #f
			(match x 
			  [(quote ,datum)    #t]
			  [(cons ,x ,y)      #t]
			  [(tuple ,args ...) #t]
			  [(vector ,args ...) #t]
			  ;; If it's mutable (like an array) then it can't be said to be available for use.
			  [,var (guard (symbol? var)
				       (not (memq var mutable-vars))) 
				(let ((entry (assq var env)))			
				  (and entry (container-available? (cadr entry))))]
			  [(,ann ,_ ,[e]) (guard (annotation? ann)) e]

			  [,sv (guard (stream-val? sv)) #t]

			  [,else #f]
			  )
		    ))]
		 
		 [container-length
		  (lambda (x)
		    (define (incr x) (if x (fx+ 1 x) #f))
		    (if (container-available? x)
			(let ([val (getval x)])
			  (if (code? val)
			      (match (code-expr val) 
				[(cons ,a ,b) (incr (container-length b))]
				[(tuple ,args ...)  (length args)]
				[(vector ,args ...) (legnth args)]
				[,oth #f]
				)
			      (match val
				[(,x* ...) (length x*)]
				[#(,x* ...) (length x*)]
				;;[,other (error 'getlist "not a list-constructor: ~s" other)])
				)))
			#f))]
		 
		 [getval ;; If available, follow aliases until you have a real value expression:
		  (outer-getval env)]

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
				[(,x* ...) (map (lambda (x) `',x) x*)]
				;;[(,x* ...) `(',x* ...)]
				;;[,other (error 'getlist "not a list-constructor: ~s" other)])
				)))
			#f))]

		 [side-effect-free?
		  (lambda (x)
		    (match x
		      [,var (guard (symbol? var)) #t]
		      [,const (guard (simple-constant? const)) #t] ;; for tupref etc
		      [(lambda . ,_) #t]
		      [(quote ,datum) #t]
		      [(,prim ,[args] ...)
		       (if (assq prim wavescript-effectful-primitives)
			   #f
			   (andmap (lambda (x) x) args))]
		      [(set! . ,_) #f]
		      [,oth (error 'side-effect-free? "we forgot to handle this: ~s" oth)]
		      ))]

		 [letrec? (lambda (x) (let ([x (peel-annotations x)])
					(and (pair? x) (eq? (car x) 'letrec))))]
		 )

        (match expr

	  ;; EXPR HANDLING: FIRST THE SPECIAL FORMS
	  ;; ================================================================================

          [(quote ,datum) `(quote ,datum)]
	  ;; This does constant inlining:
	  [,prim (guard (regiment-primitive? prim))
		 (if (memq prim computable-constants)
		     (do-constant prim)
		     prim)]

	  ;; Any variable bound to something that can be mutated cannot be inlined.
          [,var (guard (symbol? var) (memq var mutable-vars)) var]

	  ;; Here's where a lot of the magic happens:
          [,var (guard (symbol? var))	  
		(match (assq var env)
		  [#f (error 'static-elaborate "variable not in scope: ~a\n\n  ENV:\n~s" var env)]
		  ;; Anything let-bound with a reference count of 1 gets inlined:
;		  [(,_ ,x 1) 
;`		   (printf "REFCOUNT1: ~a\n" var)
;		   x]

		  ;; Inline constants:
		  [(,_ (quote ,d) ,__) 
		   (guard (simple-constant? d)) ; Don't inline constants requiring allocation.
		   `(quote ,d)]

		  ;; Inline lambda expressions as long as they don't capture mutables.
;		  [(,_ (lambda ,vars ,types ,bod) ,__)
;		   (guard (null? (intersection (core-free-vars bod) mutable-vars)))
;		   `(lambda ,vars ,types bod)]

		  ;; This symbol is special:
		  [(,_ ,NA ,__) (guard (eq? NA not-available)) var]

		  ;; Resolve aliases:
		  [(,_ ,v ,__) (guard (symbol? v))
;		   (ASSERT (not (memq v mutable-vars)))
		   ;(printf "ALIAS? ~s ~s\n" var v)
		   (process-expr v env)]

		  ;; [2007.04.30] Inline first-class references to closures:
		  [(,_ (lambda . ,rest) ,__)  `(lambda ,@rest)]

		  ;; Otherwise, nothing we can do with it.
		  [,else   var])
		;; This appears to disable the system here:
		;(if (available? var) (getval var) var)
		]

          [(assert-type ,t ,[e]) `(assert-type ,t ,e)]

	  ;; For the time being, after static elaborate we don't track soure positions:
	  [(src-pos ,p ,[e]) e]

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
	  
	  [(for (,i ,[st] ,[en]) ,bod)
	   (let ([newenv (cons `(,i ,not-available 99999) env)])	    
	     `(for (,i ,st ,en) ,(process-expr bod newenv)))]
	  [(while ,[tst]  ,[bod]) `(while ,tst ,bod)]
	  
          [(begin ,[args] ...) `(begin ,@args)]
          [(set! ,v ,[rhs]) `(set! ,v ,rhs)]
	           
	  ;; Here we do computation if the arguments are available:
          [(if ,test ,[conseq] ,[altern])
	   ;(disp "CONDITIONAL: " test)
	   (let ([newtest (process-expr test env)])
	     (if (available? newtest)  
		 (begin 
		   (if (getval newtest) conseq  altern))		 
		 `(if ,newtest ,conseq ,altern))
	     )]
	  
	  ;; TODO: We don't yet statically elaborate case statements....
	  [(wscase ,[x] [,pat* ,[rhs*]] ...) `(wscase ,x ,@(map list pat* rhs*))]
       	  
	  ;; This becomes a quoted constant:
	  [(tuple) ''UNIT]
	  [(tuple ,[args] ...) `(tuple ,@args)]
	  [(unionN ,[args] ...) `(unionN ,@args)]
	  [(vector ,[x*] ...)
	   (if (andmap available? x*)
	       ;(lambda (x) (match (getval x) [(quote ,c) c]))
	       (let ([vals (map getval x*)])
		 ;; [2007.07.27] Fixing this, not generating quoted constant at all:
		 ;`(vector . ,x*)		 
		 (if (ormap code? vals)
		     ;; Can't stick code within a constant:
		     `(vector . ,x*)
		     `(quote ,(list->vector vals))
		     ))
	       `(vector . ,x*))]


	  ;; EXPR HANDLING: NOW FOR A BRIEF INTERMISSION WHEREIN WE LIFT LETRECS
	  ;; ================================================================================
	  ;; These two cases account for A LOT of the static-elaborate slowdown:
	  ;; If I knew freshness concerns would be so bad... I would have probably used debruin indices...
	  ;; ================================================================================

#;
	  ;; It's a little funky that we look for letrec? before evaluating the rands...
          [(,prim ,rand* ...)
	   (guard (regiment-primitive? prim)
		  (list-index letrec? rand*))
	   (let ();([rand* (map (lambda (x) (process-expr x env)) rand*)])
	     ;; For now only do it for ONE rand
	     ;;(ASSERT (fx= 1 (filter letrec? rand*)))
	     (let loop ([rand* rand*] [argacc ()] [bindacc ()])
	       (if (null? rand*)
		   (begin
					;		   (printf "LIFTING LETREC BINDS: ~s\n" (map car (reverse bindacc)))
		     (if (null? bindacc) 
			 `(,prim ,@(reverse argacc))
			 `(letrec ,(reverse bindacc) (,prim ,@(reverse argacc)))))
		   (match (peel-annotations (car rand*))
		     [(letrec ([,lhs* ,ty* ,rhs*] ...) ,bod)
		      (ASSERT (andmap side-effect-free? rhs*))
		      ;; Freshness: rename these bindings:
		      (let* ([new-lhs* (map unique-name lhs*)]
			     [convert (lambda (x) (core-substitute lhs* new-lhs* x))]
			     [new-rhs* (map convert rhs*)]
			     [new-bod (convert bod)])
			(loop (cdr rand*)
			      (cons new-bod argacc)
			      (append (reverse (map list new-lhs* ty* new-rhs*)) bindacc)))]
		     [,oth (loop (cdr rand*) (cons oth argacc) bindacc)])
		   ))
	     )]

	  ;; [2007.08.12]
	  ;; This is a letrec in the RHS of a letrec.
	  ;; Sad to duplicate so much of the rest of the compiler within static-elaborate.
	  ;; FIXME FIXME!!! ONLY IMPLEMENTED FOR ONE-BINDING LETRECS ATM
	  [(letrec ([,lhs ,ty (letrec ([,lhs* ,ty* ,rhs*] ...) ,bod1)]) ,bod2)
;	  [(letrec ([,lhs ,ty (letrec ,binds ,bod1)]) ,bod2)
;	   (ASSERT (not (memq lhs lhs*)))
	   (DEBUGASSERT (not (memq lhs (core-free-vars bod1))))
;	   (printf "FOUND LETREC NEST! ~s ~s\n" lhs lhs*)

	   ;; Make it nice and fresh:
	   ;; It doesn't seem to make it too much slower.
	   (let* ([new-lhs* (map unique-name lhs*)]
		  [convert (lambda (x) (core-substitute lhs* new-lhs* x))]
		  [new-rhs* (map convert rhs*)]
		  [new-bod1 (convert bod1)])
	     ;; This should always be safe wrt side effects.
	     ;; The RHS we lift is already in head position.
	     (process-expr `(letrec ,(map list new-lhs* ty* new-rhs*)
			      (letrec ([,lhs ,ty ,new-bod1])
				,bod2))
			   env))
#;
	   	     ;; This should always be safe wrt side effects.
	     ;; The RHS we lift is already in head position.
	     (process-expr `(letrec ,binds
			      (letrec ([,lhs ,ty ,bod1])
				,bod2))
			   env)]

	  ;; EXPR HANDLING: NOW CONTAINER PRIMITIVES
	  ;; ================================================================================	  
	  ;; NOTE: these could throw away side effects when operating on object code!!!
	  ;; DANGER! FIXME FIXME 
	  ;; ================================================================================	  
	  [(cons ,[a] ,[b])
	   (if (and (available? a) (available? b))
	       (let ([vala (getval a)]
		     [valb (getval b)])
		 ;; Can't stick code within a constant currently:
		 (if (or (code? vala) (code? valb))
		     `(cons ,a ,b)
		     `(quote (,vala . ,valb))))
	       `(cons ,a ,b))]

	  
	  ;; TODO: vector-ref.

	  [(tupref ,ind ,len ,[tup])
	   (if (container-available? tup)
	       (match (code-expr (ASSERT code? (getval tup)))
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
		     (match (code-expr val)
		       [(cons ,a ,b) b]		       
		       [,x (error 'static-elaborate:process-expr "implementation error, cdr case: ~s" x)])
		     `(quote ,(cdr val))))
	       `(cdr ,x))]

	  [(List:length ,[x])
	   (if (container-available? x)
	       (let ([ls (getlist x)])
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

	  [(List:reverse ,[ls])
	   (if (container-available? ls)
	       (let ([x (getlist ls)])
		 (if (list? x)
		     (reverse x)
		     `(List:reverse ,ls)))
	       `(List:reverse ,ls))]

	  [(List:zip ,[a] ,[b])
	   (if (and (container-available? a) (container-available? b))
	       (let ([x (getlist a)] [y (getlist b)])
		 (if (and (list? x) (list? y))
		     (make-nested-cons
		      (map (lambda (one two) `(tuple ,one ,two)) x y))
		     `(List:zip ,a ,b)))
	       `(List:zip ,a ,b))]

	  ;; TODO: Only fires when the whole of A is available.
	  ;; FIXME: We should pull out prefixes if we can...
	  [(List:append ,[a] ,[b])
	   (if (container-available? a)
	       (let ([val (getlist a)])
		 (if (list? val)
		     (match val
		       [() b]
		       [(,hd . ,[tl]) `(cons ,hd ,tl)])
		     (error 'static-elaborate:List:append "partially available first list... finish this case")
		     ;`(List:append ,a ,b)
		     ))
	       `(List:append ,a ,b))]

	  ;; [2007.08.12] Ack, restricting this to make sure the whole list is available...
	  ;; seing if that affects performance.
	  ;; Seemed to make no difference.
#;
	  [(List:map ,[f] ,[ls])
	   (ASSERT side-effect-free? f)
	   (let loop ([ls ls])
	     (if (container-available? ls)
		 (let ([val (getlist ls)])
		   (if (list? val)
		       (make-nested-cons (map (lambda (cd) `(app ,f ,cd)) val))
		       `(List:map ,f ,ls)))
		 `(List:map ,f ,ls)))]

	  [(List:map ,[f] ,[ls])
	   (ASSERT side-effect-free? f)
	   (let loop ([ls ls])
	     (if (container-available? ls)
		 (let ([val (getlist ls)])		 
		   (if (code? val)
		       (match (code-expr val)
			 [(cons ,a ,b) `(cons (app ,f ,a) ,(loop b))]
			 [,x (error 'static-elaborate:process-expr "implementation error, List:map case: ~s" x)]) 
		       (match val
			 [() ''()]
			 [(,h . ,[t]) `(cons (app ,f ,h) ,t)])))
		 `(List:map ,f ,ls))
	     )]
	
	  ;; At meta-program type List:fold is fold RIGHT!!
	  ;; Your program must tolerate left or right folds..
	  [(List:fold ,[fn] ,[zer] ,[ls])
	   (ASSERT side-effect-free? fn)
	   (let loop ([ls ls])
	     (if (container-available? ls)
		 (let ([val (getlist ls)])
		   (if (code? val)
		       (match (code-expr val)
			 [(cons ,a ,b) `(app ,fn ,a ,(loop b))]
			 [,x (error 'static-elaborate:process-expr "implementation error, List:fold case: ~s" x)])
		       (match val
			 [() zer]
			 [(,h . ,[t]) `(app ,fn ,h ,t)])))
		 `(List:fold ,fn ,zer ,ls)))]
	  
	  ;; Here unionList must be eliminated, replaced by a hardwired unionN.
	  [(unionList ,[x])
	   (if (container-available? x)
	       (let ([ls (getlist x)])
		 (if (list? ls)
		     `(unionN ,@ls)
		     (begin 
		       #;
		       (when (regiment-verbose) 
			 (warning 'static-elaborate "couldn't elaborate unionList, only got: ~s" ls))
		       `(unionList ,x))
		     ))
	       ;; This is just a warning, it might be inside dead code.
	       (begin 
		 #;
		 (warning 'static-elaborate "couldn't elaborate unionList, value unavailable: ~s\n Environment entry: ~s"
			     `(unionList ,x)
			     (assq x env))
		      `(unionList ,x))
			       )]

	  ;; [2007.08.12] Yet more unpleasant hackery.
	  ;; We can conclude that containers are UNEQUAL based just on their lengths.
	  ;; This is relevant to null comparisons.
	  ;; DANGER!!! THIS DOESN'T VERIF THAT OPERANDS LACK SIDE EFFECTS BEFORE ELIMINATING THEM!
	  [(wsequal? ,[a] ,[b])
 	   (cond
	    [(and (available? a) (available? b)) `',(equal? (getval a) (getval b))]
	    [(and (container-available? a) (container-available? b))
	     (let ([alen (container-length a)]
		   [blen (container-length b)])
;	       (inspect `(alen blen: ,alen ,blen))
	       (if (and alen blen (not (fx= alen blen)))
		   (begin 
		     (ASSERT side-effect-free? a)
		     (ASSERT side-effect-free? b)
		     ''#f)
		   `(wsequal? ,a ,b)))]
	    ;; Otherwise we can't conclude anything:
	    [else `(wsequal? ,a ,b)])]


	  ;; EXPR HANDLING: OTHER PRIMITIVES
	  ;; ================================================================================	  

	  ;; This is hackish... need to work out all the cases.
	  [(show ',x)
	   (cond 
	    [(string? x) `',x]
	    [(or (fixnum? x) (flonum? x)) `',(number->string x)]
	    [else `(show ',x)])]

	  ;; We inline the arguments.  After this pass this is a special construct.
	  ;; This over-rules our general behavior of not inlining complex constants.
	  [(,frgn ,[name] ,[files]) (guard (memq frgn '(foreign foreign_source)))
	   `(,frgn ,(if (available? name)         `',(getval name) name)
		   ,(if (available? files)        `',(getval files) files))]
	  
	  ;; All other computable prims:
          [(,prim ,[rand*] ...) (guard (regiment-primitive? prim))
	   ;(disp "PRIM: " prim (map available? rand*) rand* )	  
	   
	   ;; Trying to lift out letrecs:
	   (cond 

	    [(and 		
		(andmap available? rand*)

	       ;(assq prim computable-prims)
		;; Exceptions:
		(not (assq prim wavescript-effectful-primitives))
		(not (assq prim wavescript-stream-primitives))
		(not (assq prim regiment-distributed-primitives))
		
		;; TEMP! -- execute higher order prims if their functions are ready:
		(if (assq prim higher-order-primitives)
		    (andmap fully-available? rand*) #t)

		;; Special exceptions:
		;; We prefer not to Array:make in the object code!
		;; (Kind of inconsistent that we *do* currently do List:make.)
		(not (memq prim '(show cons gint 
				       Array:makeUNSAFE Array:make Array:fold Array:map
				       HashTable:make
				       m_invert
				       Mutable:ref deref

				       ;; Alas, these don't have different representations for the constants, 
				       ;; so we shouldn't do it statically:
				       floatToDouble doubleToFloat
				       intToInt16 intToInt64
				       int16ToInt int64ToInt

				       foreign foreign_box foreign_source
				       )))
		)
	     (do-prim prim (map getval rand*) env)]
	    [else `(,prim ,@rand*)]
	    )]

	  ;; EXPR HANDLING: A FEW MORE SPECIAL FORMS AT THE END:
	  ;; ================================================================================

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
		  
		  ;; [2007.04.05] For the time being recursive
		  ;; definitions are not inlined within their own RHS.
		  ;; We don't want to needlessly expand every
		  ;; recursive definition!
		  [newenv2 (append (map (lambda (lhs) (list lhs not-available 9999889)) lhs*) env)]
		  
		  ;; This parallel version works in many places, but gives me invalid mem ref elsewhere:
		  [newall* (par (process-expr expr newenv)
				(par-map (lambda (x) (process-expr x newenv2)) rhs*))]
		  [newbod (car newall*)]
		  [newrhs* (cadr newall*)]
;		  [__ 	   (break)]
                  ;; How much does each bound variable get referenced:
		  ;; FIXME FIXME FIXME: SHOULD MAKE ONE PASS TO GET REF COUNTS:a
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

	  ;; TODO: Need to be able to evaluate this into a "value".
	  [(construct-data ,tc ,[rand*] ...) `(construct-data ,tc ,@rand*)]

	  ;; Foreign function application:
	  [(app ,rator ,[rands] ...)
	   (guard (foreign-fun? rator))
	   (let ([name (foreign-fun? rator)])
	     `(foreign-app ',name ,rator ,@rands))]
	  ;; If you dish it out, you have to take it:
	  [(foreign-app ',realname ,rator ,[rands] ...)
	   (ASSERT string? realname)
	   ;(ASSERT symbol? rator) ;; It could be the (foreign ) construct itself.
	   `(foreign-app ',realname ,rator ,@rands)]

	  ;; Here we convert to a letrec.  Rename-var insures that we
	  ;; don't get any accidental variable capture:
;	  [((lambda ,formals ,expr) ,rands ...)
;	   (substitute (map list formals rands) expr)]
	  [(app ,[rator] ,[rands] ...) 
;	   (disp "APP" rator (available? rator) env)
	   (if (available? rator)
	       (let ([code (code-expr (ASSERT code? (getval rator)))])
		 (inline code rands))
	       (begin 
		 (if (regiment-verbose)
		     (printf "  Can't inline rator this round: ~s\n" rator))
		 `(app ,rator ,@rands)))]

          [,unmatched
            (error 'static-elaborate:process-expr "invalid syntax ~s" unmatched)]))])

	  (DEBUGASSERT (compose not code?) PE-result)
	  PE-result
	  )
	))
    
    (lambda (expr)
      (match expr	    
        [(,input-language (quote (program ,body ,meta* ...  ,type)))
	 (set! mutable-vars (get-mutable body))
	 (match (or (assq 'union-types meta*) '(union-types))
	   [(union-types [,name* [,tycon** . ,_] ...] ...)
	    ;; For now we don't statically evaluate type constructors:
	    (let ([init-env (map (lambda (tycon) (list tycon not-available 9393939))
			      (apply append tycon**))])
	      ;; Run until we reach a fixed point.
	      (let elabloop ([oldbody body]
			     [body (process-expr body init-env)]
			     [iterations 1])
		(if (equal? oldbody body)	   
		    (begin
		      ;(when (regiment-verbose) )
		      (printf "Static elaboration iterated ~s times\n" iterations)
		      `(static-elaborate-language '(program ,body ,@meta* ,type)))
		    ;; [2007.08.12] SADLY, we have to redo the mutable-vars when we iterate:
		    (begin (set! mutable-vars (get-mutable body))
			   (elabloop body (process-expr body init-env) (add1 iterations))
			   ))))
	    ])]
	)))))


(define-testing these-tests 
  `( 

    ["Make sure it folds some simple constants." 
     (static-elaborate '(foo '(program (+_ '3 '4) (union-types) 'notype)))
     (static-elaborate-language '(program '7 (union-types) 'notype))]

    [(static-elaborate
      '(foo '(program
	      (letrec ([f _ (lambda (x) (_) '#t)])
		(app f '3939)) (union-types) 'notype)))
     (static-elaborate-language '(program '#t (union-types) 'notype))]
   
    ["Reduce away fact of 6." 
     (static-elaborate '(foo '(program 
      (letrec ([fact _ (lambda (n) (_) 
			       (if (= '0 n) '1 (*_ n (app fact (-_ n '1)))))])
	(app fact '6))
      (union-types) 'notype)))
     (static-elaborate-language '(program '720 (union-types) 'notype))]
    
    ["Reduce a tuple reference."
     (static-elaborate '(foo '(program (letrec ([x T (tuple '3 '4)]) (+_ '100 (tupref 0 2 x))) 'notype)))
     (static-elaborate-language '(program '103 'notype))]

    ["Reduce a primop underneath a lambda."
     (static-elaborate '(foolang '(program (lambda (x) ('t) (cons (+_ '3 '4) x)) 'notype)))
     (static-elaborate-language '(program (lambda (x) ('t) (cons '7 x)) 'notype))]

    ["Use a loop to apply a chain of rfilters"
     (static-elaborate '(foo '(program 
			       (letrec ([f 't (lambda (x) ('t) '#t)])
				 (letrec ([loop 't (lambda (n) ('t)
							  (if (= '0 n)
							      world
							      (rfilter f (app loop (-_ n '1)))))])
				   (app loop '5)))
			       'notype)))

     ,(let ([f '(lambda (x) ('t) '#t)])
	`(static-elaborate-language
	  '(program
	       (rfilter ,f (rfilter ,f (rfilter ,f (rfilter ,f (rfilter ,f world)))))
	     'notype)))
#;
     (static-elaborate-language '(program
	    (letrec ([f 't (lambda (x) ('t) '#t)])
	      (rfilter
	       f
	       (rfilter f (rfilter f (rfilter f (rfilter f world))))))
	    'notype))
     ]

    ["Simple test to make sure we keep the quotes on:" 
     (static-elaborate '(foolang '(program (cons (+_ '3 '4) world) 'notype)))
     (static-elaborate-language '(program (cons (quote 7) world)
		 'notype))]

;    [(error 'foo "bar") 9]


    ,(let ([prog '(static-elaborate-language '(program (cons (khood-at '30 '40 '50) world) 'notype))])
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
