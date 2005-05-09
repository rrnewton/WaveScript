
;; Sigh, there are some general optimizations that are really lacking now.
;; Alias analysis is needed to fix the "direct calls" that are broken by this pass.

;; I accumulate tests through mutation throughout this file.
;; This method allows me to test internal functions whose definitions
;; are not exposed at the top level.
(define these-tests '())

(define closure-convert
  (let ()

    (define INIT 11)
    (define CALL 99)
;    (define FREEVAR0 'fv0)

    ;; Name of the token which represents the global variable storing the continuation
    ;; token count.
    (define KCOUNTER 'kcounter)
    ;; And the root name of continuation tokens.
    (define KNAME 'K)

    (define (id x) x)

    ;; This is copied from the cps-tokmac pass.  Should share it!
    (define (generic-traverse driver fuse e)
      (let loop ((e e))
	(driver e 
	   (lambda (x)
	     (match x
;		    [,x (guard (begin (printf "~nGenTrav looping: ") (display-constrained (list x 50)) (newline) #f)) 3]
		    [,const (guard (constant? const)) (fuse () const)]
		    [(quote ,const)                (fuse ()   `(quote ,const))]
		    [,var (guard (symbol? var))    (fuse ()    var)]
		    [(tok ,tok)                    (fuse ()   `(tok ,tok))]
		    [(tok ,tok ,n) (guard (integer? n)) (fuse () `(tok ,tok ,n))]
		    [(tok ,tok ,[loop -> expr])    (fuse (list expr) `(tok ,tok ,expr))]
		    [(ext-ref ,tok ,var)           (fuse ()   `(ext-ref ,tok ,var))]
		    [(ext-set! ,tok ,var ,[loop -> expr])  
		                                   (fuse (list expr) `(ext-set! ,tok ,var ,expr))]
		    [(set! ,v ,[loop -> e])        (fuse (list e)    `(set! ,v ,e))]
		    [(leds ,what ,which)           (fuse () `(leds ,what ,which))]
		    [(begin ,[loop -> x] ...)      (fuse x           `(begin ,x ...))]
		    [(if ,[loop -> a] ,[loop -> b] ,[loop -> c])
		                                   (fuse (list a b c) `(if ,a ,b ,c))]
		    [(let ([,lhs ,[loop -> rhs]]) ,[loop -> bod])
		                                   (fuse (list rhs bod) `(let ([,lhs ,rhs]) ,bod))]
		    ;; "activate" and the gradient calls have already been desugared:

		    [(lambda (,v) ,[loop -> e])    (fuse (list e) `(lambda (,v) ,e))]		     
		    [(kcall ,[loop -> k] ,[loop -> e]) (fuse (list k e) `(kcall ,k ,e))]
		    

		    [(,call ,[loop -> rator] ,[loop -> rands] ...)
		     (guard (memq call '(bcast subcall call)))
		     (fuse (cons rator rands) `(,call ,rator ,rands ...))]
		    [(timed-call ,time ,[loop -> rator] ,[loop -> rands] ...)
		     (guard (number? time))		     
		     (fuse (cons rator rands) `(timed-call ,time ,rator ,rands ...))]
		    [(return ,[loop -> x])         (fuse (list x) `(return ,x))]
		    [(,prim ,[loop -> rands] ...)
		     (guard (or (token-machine-primitive? prim)
				(basic-primitive? prim)))
		     (fuse rands `(,prim ,rands ...))]
		    [(app ,[loop -> rator] ,[loop -> rands] ...)
		     (fuse (cons rator rands) `(app ,rator ,rands ...))]
		    [,otherwise
		     (error 'generic-traverse
			    "bad expression: ~s" otherwise)])))))

    (define (subst e1 v e2)
        (generic-traverse
	 ;; driver, fuser, expression
	 (lambda  (x loop)
	   (match x
		  [,var (guard (eq? var v)) e2]
		  [(lambda (,var) ,e) (guard (eq? var v))
		   ;; Stop substitituting:
		   `(lambda (,var) ,e)]		   
		  [,x (loop x)]))
	 (lambda (_ def) def)
	 e1))

    ;; Get the free vars from an expression
    (define (free-vars e)
      (generic-traverse
       ;; driver, fuser, expression
       (lambda  (x loop) 
	 (match x
		[,v (guard (symbol? v)) (list v)]
		[(let ([,lhs ,[rhs]]) ,[bod])
		 (append rhs (remq lhs bod))
		 ]
		[,x (loop x)]))
       (lambda (ls _) (apply append ls))
       e))

    ;; Replace the free vars in an expression with "fv0" "fv1" etc.
    (define (number-freevars e)
      (define (build-fv x fvs) (string->symbol (format "fv~a" (list-find-position x fvs))))
      (let outer ((e e) (fvs (free-vars e)))
	(generic-traverse
	 (lambda  (x loop)
	   (match x
		  [(let ((,lhs ,[rhs])) ,bod)
		   `(let ((,lhs ,rhs))
		      ,(outer bod (remq lhs fvs)))]
		  [,x (guard (memq x fvs))
 		      (build-fv x fvs)]
		  [(set! ,x ,[e]) (guard (memq x fvs))
		   `(set! ,(build-fv x fvs) ,e)]
		  [,other (loop other)]))
	 (lambda (ls def) def)
	 e)))

    (define (build-continuation kname body hole)
      (let* ([FREEVAR0 'fv0] ;(unique-name 'fv0)]
	     
	     [body (subst body hole FREEVAR0)]

	     [fvs (remq FREEVAR0 (free-vars body))]
;		     [__ (disp "GOT fvs: " fvs)]
;		   [args (map (lambda (n)
;				(string->symbol (format "arg~a" n)))
;			      (iota (length fvs)))]
	     [fvns (map (lambda (n)
			  (string->symbol (format "fv~a" n)))
			(iota (length fvs)))]
		   ;; Get a fresh name & subtokindex name for our continuation:
	     )
	      	     
      (values 
       ;; Return an expression which builds the continutation closure:
       (let ([kind (unique-name 'kind)])
	 `(begin "This whole block represents the allocation of a continuation closure:"
		(let ([,kind 
		       (if (token-present? (tok ,kname 0))
			   (let ([new (+ '1 (ext-ref (tok ,kname 0) ,KCOUNTER))])
			     (begin (ext-set! (tok ,kname 0) ,KCOUNTER new)
				    new))
			   (begin "Allocate this zeroeth token object just to hold a counter MEMORY WASTEFUL!:"
				  ;; Here we should just allocate a SEPARATE token for this, holding just the counter.
				  ;; OR we should have one token that holds everybody's counters.  But for now I want 
				  ;; to everything grouped under this one token "class".
				  ;; FIXME: PADDING
				  (call (tok ,kname 0) ',INIT 
					,@(make-list (max 1 (length fvs)) '(void))) ;; The void takes up the fv0 position
				  '1))])
		  (begin 
		    "Do the actual token object (closure) allocation.  Capture freevars:"
		    ;; FIXME!!! Don't have zeroing of omitted arguments yet!!  This includes a dummy arg.
		    ;; FIXME: PADDING.
		    (call (tok ,kname ,kind) ',INIT ,@fvs) ;,@(if (null? fvs) '((void)) fvs))
		    "Return the name of this continuation object:"
		    (tok ,kname ,kind)))))
       
       ;; Return a new token handler to hold the continuations' code.
       `(,kname subtokind (flag ,@(if (null? fvns) (list FREEVAR0) fvns))
		(stored [,KCOUNTER 0] ,@(map (lambda (fv) `[,fv '0]) fvs))
					;(map list fvns args))
					;(make-vector ,(length fvs)))
		(if (= flag ',INIT)
		    (if (= subtokind '0)
			;; No freevars if we're just initializing the counter-object.
			(void)
			(begin
			  ,@(map (lambda (fv fvn)
				   `(set! ,fv ,fvn))
				 fvs fvns)))
		    ;; Otherwise, assume the flag is CALL
		    (begin
		      ,body ;,(number-freevars body)
		      ;; Since these are one-shot continuations, 
		      ;; we deallocate ourselves on the way out:
		      (evict (tok ,kname subtokind))
		      ))))))

    (define (no-first-class? k expr)
      (generic-traverse
       (lambda (x loop)
	 (match x 
	   [,v (guard (eq? v k)) #f]
	   [(app ,v ,[args] ...) (guard (eq? v k))
	    (andmap id args)]
	   [,x (loop x)]))
       (lambda (ls _) (andmap id ls))
       expr))


    (define (stripk k expr)
      (generic-traverse
       (lambda (x loop)
	 (match x 
	   [,v (guard (eq? v k)) 
	       (error 'closure-convert:stripk
		      "should be no first class refs to k: ~a" k)]
	   [(app ,v ,[x]) (guard (eq? v k))
	    x]
	   [,x (loop x)]))
       (lambda (_ def) def)
       expr))
    
    (define (process-expr expr)
      (let ([new-handlers '()]) ;; MUTABLE

      ;; We do a very lame alias-analysis on our way down.
      ;; This lets us get a few obvious direct calls.
      ;; kbinds binds continuation names to token names.
	(values 
	 (let outer-loop ([expr expr] [kbinds '()])
	   (generic-traverse
	    (lambda (x loop)
	      (match x

	   ;; This is a hack that depends on exactly what the previous pass outputs.
	   ;; This transformation should be accomplished by a much more general optimization.
	   [(let ([,k (lambda (,hole) ,kbody)]) (if ,t ,c ,a))
	    (guard (no-first-class? k c)
		   (no-first-class? k a))
	    (loop ;; Done with it, pass it on to loop.
	     (subst kbody hole `(if ,t ,(stripk k c) ,(stripk k a))))]

	   ;; We try to make direct calls here if we can.
	   [(kcall ,k ,[v])
	    (guard (assq k kbinds))
	    (loop
	     `(call (tok ,(cadr (assq k kbinds)) (token->subid ,k)) ',CALL ,v))]

	   [(kcall ,k ,[v])
	    (if (not (symbol? k))
		(error 'closure-convert "kcall with this unexpected rator: ~a" k))
	    (loop `(call ,k ',CALL ,v))]
		   

	   [(let ([,k (lambda (,hole) ,body1)]) ,body2)	      
	    (let ([kname (unique-name 'K)])
	      (mvlet ([(closure khandler) (build-continuation kname body1 hole)])
  	         (set! new-handlers (cons khandler new-handlers))
		 (disp "ADDED NEW HAND" (length new-handlers))
		 `(let ([,k ,closure])
		    ,(outer-loop body2
				 (cons (list k kname) kbinds)))))]
	    
	   [(lambda (,v) ,body)
	    (disp "LAMBDA" v body)
	    (let ([kname (unique-name 'K)])
;		  [newbod (outer-loop body kbinds)])
	      (mvlet ([(closure khandler) (build-continuation kname body v)])
		     (disp "KHANDLER" khandler)
  	         (set! new-handlers (cons khandler new-handlers))
		 (disp "ADDED NEW HAND" (length new-handlers))
		 closure))]
;	    (error 'closure-convert "ran into lambda not in a \"let ([k\" context: ~a" `(lambda ,vars ,bods ...))]
	    
	   [,x (loop x)]))
	    (lambda (ls def) def)
	    expr))
	 ;; Also return the new-handlers:
	 (begin (disp "RETURNING" (length new-handlers))
	 new-handlers))))


    (define (process-tokbind tb)
      (mvlet ([(tok id args stored constbinds body) (destructure-tokbind tb)])
	     (mvlet ([(newexpr newtokbinds) (process-expr body)])
		    (disp "NEWTOKBINDS" newtokbinds)
		    (cons 
		     `[,tok ,id ,args (stored ,@stored) ,newexpr]
		     newtokbinds))))
    
      ;; Add some unit tests that use local bindings:
      (set! these-tests
	    (append these-tests
	      `(
		
		[(,subst '(+ x 9) 'x 3) 
		 (+ 3 9)]
		[(,subst '(+ x (* y x)) 'x 3)
		 (+ 3 (* y 3))]

		[(,subst '(lambda (x) (* y x)) 'x 3)
		 (lambda (x) (* y x))]
		

		["Next testing number-freevars"
		 (,free-vars (,number-freevars '(begin x y (let ((z '3)) z))))
		 ,(lambda (ls) (set-equal? ls '(fv0 fv1)))]
		[(,free-vars (,number-freevars '(begin x y (let ((z 3)) (+ ht z)))))
		 ,(lambda (ls) (set-equal? ls '(fv0 fv1 fv2)))]	       


		[(,no-first-class? 'v 'v) #f]
		[(,no-first-class? 'v '(+ v 1)) #f]
		[(,no-first-class? 'v '(+ r (app v '3 '4))) #t]
		[(,no-first-class? 'v '(+ r (app v '3 v))) #f]


		[(,stripk 'v '(+ r (app v '3))) 
		 (+ r '3)]
		[(,stripk 'v '(+ r (if '1 (app v '3) (app v '4))))
		 (+ r (if '1 '3 '4))]
		
		[(,process-tokbind '[t1 id () (stored) (+ '3 (subcall (tok t2 0) 3))])
		 ,(lambda (x) (disp x))]
		
	      )))      

      (lambda (prog)
	(match prog
	       [(,lang '(program (bindings ,constbinds ...)
				 (nodepgm (tokens ,[process-tokbind -> toks] ...))))
		`(,lang
		  '(program (bindings ,constbinds ...)
			    (nodepgm (tokens ,(apply append toks) ...))))]))
      ))
      

(define test-this (default-unit-tester
		    "27: CPS-Tokmac: use CPS on blocking calls."
		    these-tests))

(define test28 test-this)
(define tests28 these-tests)
(define test-closure-convert test-this)
(define tests-closure-convert these-tests)


