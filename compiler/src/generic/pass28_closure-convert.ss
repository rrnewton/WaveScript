
;; Sigh, there are some general optimizations that are really lacking now.
;; Alias analysis is needed to fix the "direct calls" that are broken by this pass.


;; [2005.05.29] I just went through and wasted some time rewriting
;; generic-traverse to be more general, and rewriting process-expr to
;; pass the new-handlers up explicitely (returning a vector of two
;; results) rather than using mutation to accumulate new-handlers (as
;; in OLDprocess-expr).
;;   As far as I can tell Petite Chez Scheme had a bug a bug which
;; caused OLDprocess-expr not to work.  The "new-handlers" variable
;; would be mutated, but then would still be null when returned.  I
;; could investigate this further, but for now I've rewritten to work
;; around it.
;;   I'm pretty sure it *is* a petite bug though, it works 



;; I accumulate tests through mutation throughout this file.
;; This method allows me to test internal functions whose definitions
;; are not exposed at the top level.
(define these-tests '())

(define closure-convert
  (let ()

    (define INIT 11)
    (define CALL 99)
    (define FREEVAR0 'fv0)

    ;; Name of the token which represents the global variable storing the continuation
    ;; token count.
    (define KCOUNTER 'kcounter)
    ;; And the root name of continuation tokens.
    (define KNAME 'K)

    (define (id x) x)

    (define (remq-all x ls)
      (let loop ((ls ls) (acc ()))
	(cond 
	 [(null? ls) (reverse! acc)]
	 [(eq? x (car ls)) (loop (cdr ls) acc)]
	 [else (loop (cdr ls) (cons (car ls) acc))])))    

    ;; This is copied from the cps-tokmac pass.  Should share it!
    ;; Man, this would be easier to understand if it were strongly typed (sadly, that's tough - see peyton jones "boilerplate"):
    ;; The arguments are as follows:
    ;;   driver : expr -> k -> alpha
    ;;   fuse : [alpha] -> (expr* ... -> expr) -> alpha
    ;;   e : expr

    ;; The driver gives the users code first hack at the data tree.
    ;; The driver may hand off to its "k" argument if it wants to
    ;; continue the "generic" traversal, but it's not obligated to
    ;; return another expression, it may return its own alpha type.
    ;;   The fuser is a function that reassembles the results of
    ;;   generic traversal (closure abstracted over children expressions)
    ;;   with the alpha-type resulting from the drivers execution on all those children.
    ;; The whole traversal returns an alpha value.
        
    (define (generic-traverse driver fuse e)
      (let loop ((e e))
	(driver e 
	   (lambda (expression)
	     (match expression
;		    [,x (guard (begin (printf "~nGenTrav looping: ") (display-constrained (list x 50)) (newline) #f)) 3]
		    [,const (guard (constant? const)) (fuse () (lambda () const))]
		    [(quote ,const)                (fuse ()      (lambda () `(quote ,const)))]
		    [,var (guard (symbol? var))    (fuse ()      (lambda () var))]
		    [(tok ,tok)                    (fuse ()      (lambda () `(tok ,tok)))]
		    [(tok ,tok ,n) (guard (integer? n)) (fuse () (lambda () `(tok ,tok ,n)))]
		    [(tok ,tok ,[loop -> expr])    (fuse (list expr) (lambda (x) `(tok ,tok ,x)))]
		    [(ext-ref ,tok ,var)           (fuse ()      (lambda () `(ext-ref ,tok ,var)))]
		    [(ext-set! ,tok ,var ,[loop -> expr])  
		                                   (fuse (list expr) (lambda (x) `(ext-set! ,tok ,var ,x)))]
		    [(set! ,v ,[loop -> e])        (fuse (list e)    (lambda (x) `(set! ,v ,x)))]
		    [(leds ,what ,which)           (fuse ()      (lambda () `(leds ,what ,which)))]
		    [(begin ,[loop -> xs] ...)     (fuse xs       (lambda ls `(begin ,ls ...)))]
		    [(if ,[loop -> a] ,[loop -> b] ,[loop -> c])
		                                   (fuse (list a b c) (lambda (x y z) `(if ,x ,y ,z)))]
		    [(let ([,lhs ,[loop -> rhs]]) ,[loop -> bod])
		                                   (fuse (list rhs bod) 
							 (lambda (x y) `(let ([,lhs ,x]) ,y)))]
		    ;; "activate" and the gradient calls have already been desugared:

		    [(lambda (,v) ,[loop -> e])    (fuse (list e) (lambda (x) `(lambda (,v) ,x)))]
		    [(kcall ,[loop -> k] ,[loop -> e]) (fuse (list k e) (lambda (x y) `(kcall ,x ,y)))]
		    
		    [(build-kclosure ,kname (,fvs ...)) (fuse () (lambda () `(build-kclosure ,kname (,fvs ...))))]

		    [(,call ,[loop -> rator] ,[loop -> rands] ...)
		     (guard (memq call '(bcast subcall call)))
		     (fuse (cons rator rands) (lambda (x . ls) `(,call ,x ,ls ...)))]
		    [(timed-call ,time ,[loop -> rator] ,[loop -> rands] ...)
		     (guard (number? time))		     
		     (fuse (cons rator rands) (lambda (x . ls) `(timed-call ,time ,x ,ls ...)))]
		    [(return ,[loop -> e])         (fuse (list e) (lambda (x) `(return ,x)))]
		    [(,prim ,[loop -> rands] ...)
		     (guard (or (token-machine-primitive? prim)
				(basic-primitive? prim)))
		     (fuse rands (lambda ls `(,prim ,ls ...)))]
		    [(app ,[loop -> rator] ,[loop -> rands] ...)
		     (fuse (cons rator rands) (lambda (x . ls)`(app ,x ,ls ...)))]
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
		  [(let ((,lhs ,[rhs])) ,e) (guard (eq? lhs v))
		   ;; Stop substitituting:
		   `(let ((,lhs ,rhs)) ,e)]
		  [,x (loop x)]))
	 (lambda (ls f) (apply f ls))
	 e1))

    ;; Get the free vars from an expression
    (define (free-vars e)
      (list->set 
       (generic-traverse
	;; driver, fuser, expression
	(lambda  (x loop) 
	  (match x
;		 [,x (guard (begin (printf "Driver loop: ~a~n" x) #f)) 3]
		 [,v (guard (symbol? v)) (list v)]
		 [(let ([,lhs ,[rhs]]) ,[bod])
		  (append rhs (remq-all lhs bod))]
		 [(lambda (,v) ,[e]) (remq-all v e)]		 
		 [,x (loop x)]))
	(lambda (ls _) (apply append ls))
	e)))

    ;; Replace the free vars in an expression with "fv0" "fv1" etc.
    (define (number-freevars e)
      (define (build-fv x fvs) (string->symbol (format "fv~a" (list-find-position x fvs))))
      (let outer ((e e) (fvs (free-vars e)))
	(generic-traverse
	 (lambda  (x loop)
	   (match x
		  [(let ((,lhs ,[rhs])) ,bod)
		   `(let ((,lhs ,rhs))
		      ,(outer bod (remq-all lhs fvs)))]
		  [,x (guard (memq x fvs))
 		      (build-fv x fvs)]
		  [(set! ,x ,[e]) (guard (memq x fvs))
		   `(set! ,(build-fv x fvs) ,e)]
		  [,other (loop other)]))
	 (lambda (ls f) (apply f ls))
	 e)))

    (define (build-continuation kname body hole)
      (let* ([FREEVAR0 'fv0] ;(unique-name 'fv0)]
	     
	     [body (subst body hole FREEVAR0)]

	     [fvs (remq-all FREEVAR0 (free-vars body))]
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
       (lambda (ls f) (apply f ls))
       expr))

    (define (process-expr expr)
      (define (addhands newhands vec)
	(vector (append newhands (vector-ref vec 0)) (vector-ref vec 1)))
	
      ;; We do a very lame alias-analysis on our way down.
      ;; This lets us get a few obvious direct calls.
      ;; kbinds binds continuation names to token names.
      (let ((result
	     ;; As we go *down* the structure, we accumulate kbinds.
	     ;; As we come *up* we accumulate new handlers.
	 (let outer-loop ([expr expr] [kbinds '()])
	   (generic-traverse
	    (lambda (x loop)
	      (match x
		     ;; This first case is a hack that depends on exactly what the previous pass outputs.
		     ;; This transformation should be accomplished by a much more general optimization.
		     [(let ([,k (lambda (,hole) ,kbody)]) (if ,t ,c ,a))
		      (guard (no-first-class? k c)
			     (no-first-class? k a))
		      (loop ;; Done with it, pass it on to loop.
		       (subst kbody hole `(if ,t ,(stripk k c) ,(stripk k a))))]

		     ;; We try to make direct calls here if we can.
		     [(kcall ,k ,[val])  (guard (assq k kbinds))
		      (let ([newhands (vector-ref val 0)] [v (vector-ref val 1)])
			(addhands newhands
				  (loop `(call (tok ,(cadr (assq k kbinds)) (token->subid ,k)) ',CALL ,v))))]
		     [(kcall ,k ,[val])
		      (let ([newhands (vector-ref val 0)] [v (vector-ref val 1)])
			(if (not (symbol? k))
			    (error 'closure-convert "kcall with this unexpected rator: ~a" k))
			(addhands newhands
				  (loop `(call ,k ',CALL ,v))))]
		     [(let ([,k (lambda (,hole) ,[val])]) ,body2)
		      (let ([newhands1 (vector-ref val 0)] [body1 (vector-ref val 1)])
			(let ([kname (unique-name 'K)])
			  (mvlet ([(closure khandler) (build-continuation kname body1 hole)])
				 (let* ([result (outer-loop body2 (cons (list k kname) kbinds))]
					[newhands2 (vector-ref result 0)]
					[newbody (vector-ref result 1)])
				   (vector (cons khandler (append newhands1 newhands2))
					   `(let ([,k ,closure]) ,newbody))))))]
		     [(lambda (,v) ,[val])
		      (let ([newhands (vector-ref val 0)] [body (vector-ref val 1)])
			(let ([kname (unique-name 'K)])
			  (mvlet ([(closure khandler) (build-continuation kname body v)])
				 (vector (cons khandler newhands)
					 closure))))]
		     [,x (loop x)]))
	    ;; Generic traverse 2nd argument: fuser
	    (lambda (vals f) 
	      (let ([newhands (map (lambda (v) (vector-ref v 0)) vals)]
		    [newexpr* (map (lambda (v) (vector-ref v 1)) vals)])
		(vector (apply append newhands)
			(apply f newexpr*))))
	    ;; Generic traverse 3rd argument: expression
	    expr
	    ))))
	(values (vector-ref result 1) (vector-ref result 0))))
    


    (define (OLDgeneric-traverse driver fuse e)
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
		    
		    [(build-kclosure ,kname (,fvs ...)) (fuse () `(build-kclosure ,kname (,fvs ...)))]


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

    (define (OLDprocess-expr expr)
      (let ([new-handlers '()]) ;; MUTABLE

      ;; We do a very lame alias-analysis on our way down.
      ;; This lets us get a few obvious direct calls.
      ;; kbinds binds continuation names to token names.
	(values 
	 ;; First return the new expression:
	 (let outer-loop ([expr expr] [kbinds '()])
	   (OLDgeneric-traverse
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
		     
		     
		     [(let ([,k (lambda (,hole) ,[body1])]) ,body2)	      
		      (let ([kname (unique-name 'K)])
			(mvlet ([(closure khandler)  
				 (build-continuation kname body1 hole)])
			       (disp "SETTING NEW HANDLERS(1), ADDING " khandler)
			       (set! new-handlers (cons khandler new-handlers))
			       (disp "NEW " (map car new-handlers))
			       `(let ([,k ,closure])
				  ,(outer-loop body2
					       (cons (list k kname) kbinds)))))
		      ]
		     
		     [(lambda (,v) ,[body])
		      (let ([kname (unique-name 'K)])
			(mvlet ([(closure khandler) 
				 (build-continuation kname body v)])
			       (disp "SETTING NEW HANDLERS(2), ADDING " khandler)
			       (set! new-handlers (cons khandler new-handlers))
			       (disp "NEW " (map car new-handlers))
			       closure))]
		     [,x (loop x)]))	    
	    ;; Generic traverse 2nd argument: fuser
	    (lambda (ls def) def)
	    ;; Generic traverse 3rd argument: expression
	    expr))
	 
	 ;; Also return the new-handlers:
	 (begin (disp "RETURNING" (map car new-handlers))
	   new-handlers))))


    (define (process-tokbind tb)
      (mvlet ([(tok id args stored constbinds body) (destructure-tokbind tb)])
	     (mvlet ([(newexpr newtokbinds) (process-expr body)])
;	     (mvlet ([(newexpr newtokbinds) (OLDprocess-expr body)])
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
		[(,free-vars '(lambda (HOLE_2)
			       (call (tok tok1 0)
				     (lambda (HOLE_3)
				       (printf '"result ~a" (+ HOLE_2 HOLE_3)))
				     '3)))
		 ()]
		[(,free-vars '(lambda (HOLE_2)
			       (call (tok tok1 0)
				     (lambda (HOLE_3)
				       (printf '"result ~a" (+ HOLE_2 x5)))
				     '3)))
		 (x5)]

		[(,free-vars '(let ([kind '1]) kind)) ()]
		[(,free-vars '(let ([kind '1]) (tok foo kind))) ()]
		[(,free-vars '(let ([new (let ([kind '1]) (tok foo kind))]) '34))  ()]
		[(,free-vars (let ([new '3]) (begin new new)))   ()]
		[(,free-vars (if '3 (let ([new '3]) (begin new new)) '4)) ()]


		[(,free-vars 
		  '(let ([kind_19 (let ([new '3]) (begin new new))])
		     '5))
		 ()]



#;		[(,free-vars 
		  '(let ([kind_19 (if '3
				      (let ([new '3]) (begin new new))
				      '4)])
		     '5))
		 ()]
		 

#;		[(,free-vars 
		  '(let ([kind_19
			  (if (token-present?
			       (tok K_18 0))
			      (let ([new
				     (+ '1
					(ext-ref
					 (tok K_18 0)
					 kcounter))])
				(begin (ext-set!
					(tok K_18 0)
					kcounter
					new)
				       new))
			      (begin 
				(call (tok K_18 0) '11 (void))
				'1))])
		     (begin (call (tok K_18 kind_19) '11 fv0)
			    (tok K_18 kind_19))))
                   
		 (subtokind fv0 flag)]

#;		[(,free-vars 
		  '(if (= flag '11)
                   (if (= subtokind '0) (void) (begin))
                   (begin (call (tok tok1 0)
                                (begin "This whole block represents the allocation of a continuation closure:"
                                       (let ([kind_19
                                              (if (token-present?
                                                    (tok K_18 0))
                                                  (let ([new
                                                         (+ '1
                                                            (ext-ref
                                                              (tok K_18 0)
                                                              kcounter))])
                                                    (begin (ext-set!
                                                             (tok K_18 0)
                                                             kcounter
                                                             new)
                                                           new))
                                                  (begin "Allocate this zeroeth token object just to hold a counter MEMORY WASTEFUL!:"
                                                         (call (tok K_18 0)
                                                               '11
                                                               (void))
                                                         '1))])
                                         (begin "Do the actual token object (closure) allocation.  Capture freevars:"
                                                (call (tok K_18 kind_19)
                                                      '11
                                                      fv0)
                                                "Return the name of this continuation object:"
                                                (tok K_18 kind_19))))
                                '3)
                          (evict (tok K_20 subtokind)))))
		 (subtokind fv0 flag)]

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
      

;; Now test the whole module:a
(set! these-tests
  (append these-tests
    `(
      ["Closure convert a program with one continuation.  Make sure we get the right token bindings."
       (map car (cdr (deep-assq 'tokens 
         (parameterize ((unique-name-counter 0))
          (closure-convert 
	   '(cleanup-token-machine-lang
	     '(program (bindings)
		       (nodepgm
			(tokens
			 (node-start subtok_ind () (stored) (void))
			 (SOC-start subtok_ind ()
				    (stored)
				    (call (tok tok1 0)
					  (lambda (HOLE_23) (printf '"result ~a" HOLE_23))
					  '3))
			 (tok1 subtok_ind
			       (k_22 x)
			       (stored)
			       (kcall k_22 (+ x '300))))))))))))
       (node-start SOC-start K_1 tok1)]

      ["Trying to make sure we get the right stored vars."
       (map car (apply append (map cdr
       (deep-assq-all 'stored 
	 (closure-convert 
	  '(cleanup-token-machine-lang
	    '(program
	      (bindings)
	      (nodepgm
	       (tokens
		(node-start subtok_ind () (stored) (void))
		(SOC-start subtok_ind ()
		 (stored)
		 (call (tok tok1 0)
		       (lambda (HOLE_59)
			 (call (tok tok1 0)
			       (lambda (HOLE_60)
				 (printf '"result ~a" (+ HOLE_59 HOLE_60)))
			       '3))
		       '4))
		(tok1 subtok_ind (k_58 x)
		      (stored)
		      (kcall k_58 (+ x '1000))))))))))))
       (kcounter kcounter HOLE_59)]
       
      ["This is just for regression, popped up an error before due to token->subid."
       (closure-convert '(rename-stored-lang
        '(program
	  (bindings)
	  (nodepgm
	   (tokens
	    (node-start subtok_ind () (stored) (void))
	    (SOC-start
	     subtok_ind
	     ()
	     (stored)
	     (begin (call (tok tok1 0) '0)
		    (call (tok tok1 0) '1)
		    (call (tok tok1 0) '0)
		    (call (tok tok1 0) '1)))
	    (tok1 subtok_ind
		  (x)
		  (stored
                 (y_80 'let-stored-uninitialized)
                 (storedliftoption_79 '#f))
		  (begin (printf '". ")
			 (let ([k_81 (lambda (HOLE_82) HOLE_82)])
			   (if (= x '0)
			       (let ([k_83
				      (lambda (HOLE_84)
					(begin HOLE_84 (kcall k_81 y_80)))])
				 (if storedliftoption_79
				     (kcall k_83 (void))
				     (begin (set! storedliftoption_79 '#t)
					    (printf '" !!! ")
					    (kcall k_83 (set! y_80 '3)))))
			       (kcall k_81 (void)))))))))))
       ,(lambda (x) #t)]
      

      )))
	       

(define test-this (default-unit-tester
		    "27: CPS-Tokmac: use CPS on blocking calls."
		    these-tests))

(define test28 test-this)
(define tests28 these-tests)
(define test-closure-convert test-this)
(define tests-closure-convert these-tests)


