
;; TODO FIXME:  Should refactor to use MORE seperate tokens.  
;; Shouldn't combine functionality in one token when it's not absolutely necessary.

;; TODO: FIXME:  Are the "subtok_ind" bindings treated properly??

; FIXME: TODO: When lifting node-start to actual-node-start should probably change the references to node-start...

;; FIXME: TODO: Sit down and think carefully about token scheduling order for continuation init, etc.



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


;; NOTE:  Had to add a hack for SOC-start to avoid race condition.
;;;   Now we explicitely call the SOC-start from the node-start handler.
;;;   This is for *sequencing*.  We want the things called by
;;;   node-start to all complete *before* soc-start runs.a

;;;  ACTUALLY... this is currently implemented straight in the simulator.



;; I accumulate tests through mutation throughout this file.
;; This method allows me to test internal functions whose definitions
;; are not exposed at the top level.
(define these-tests '())

(define closure-convert
  (let ()

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

    (define (subst e1 v e2)
        (tml-generic-traverse
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
       (tml-generic-traverse
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
	(tml-generic-traverse
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

#;    (define (build-continuation kname body hole)
      (values
       `(lambda (,hole) ,body)
       `(,kname subtok_ind () (stored) 'FOOO)))

    (define (build-continuation kname env body hole)      
      (let* ([FREEVAR0 'fv0] ;(unique-name 'fv0)]
	     [KCOUNTER (unique-name KCOUNTER)]

	     [body (subst body hole FREEVAR0)]
	     
	     ;; Subtract the set of bound vars to get free-vars
	     ;[fvs (difference (free-vars body) (cons FREEVAR0 env))]
	     [fvs (remq-all FREEVAR0 (free-vars body))]
;		     [__ (disp "GOT fvs: " fvs)]
;		   [args (map (lambda (n)
;				(string->symbol (format "arg~a" n)))
;			      (iota (length fvs)))]
	     [fvns (map (lambda (n)
			  (string->symbol (format "fv~a" n)))
			(iota (length fvs)))]
		   ;; Get a fresh name & subtok index name for our continuation:
	     )

	;(printf "Got fvs for body: ~a \n" fvs) (pretty-print body)
	      	     
      (values 
       ;; Return an expression which builds the continutation closure:
       (let ([kind (unique-name 'kind)])
	 `(begin '"This whole block represents the allocation of a continuation closure."
		 ,(format "Continuation = ~a" kname)
		(let ([,kind 
		       (if (token-present? (tok ,kname 0))
			   (let ([new (+ '1 (ext-ref (tok ,kname 0) ,KCOUNTER))])
			     (begin (ext-set! (tok ,kname 0) ,KCOUNTER new)
				    new))
			   ;; NOTE: [2005.05.31] Now this should never happen because I do it in node-start:
			   (begin '"Allocate this zeroeth token object just to hold a counter MEMORY WASTEFUL!:"
				  ;; Here we should just allocate a SEPARATE token for this, holding just the counter.
				  ;; OR we should have one token that holds everybody's counters.  But for now I want 
				  ;; to everything grouped under this one token "class".
				  ,@(REGIMENT_DEBUG
				     `(dbg '"~a: No counter token for K=~a! Allocating..." (my-id) ',kname))
				  ;; FIXME: PADDING
;; Consider getting rid of:
				  (call (tok ,kname 0) ',KINIT_FLAG 
					,@(make-list (max 1 (length fvs)) 
						     ''counter-holder-tok-dummy-val)) ;; The void takes up the fv0 position
				  '1))])
		  (begin 
		    "Do the actual token object (closure) allocation.  Capture freevars:"
		    ;; NOTE: Relying on the automatic padding/zeroing of omitted args:
		    (call (tok ,kname ,kind) ',KINIT_FLAG ,@fvs) ;,@(if (null? fvs) '((void)) fvs))
		    ,@(REGIMENT_DEBUG
		       `(dbg '"~a: Launched an continuation-allocation call (tok ~a ~a) with fvs ~a = ~a" 
			     (my-id)  ',kname ,kind ',fvs (list ,@fvs))
		       ;'(sim-print-queue (my-id))
		       )
		    "Return the name of this continuation object:"
		    (tok ,kname ,kind)))))
       
       ;; Return a new token handler to hold the continuations' code.
       (let ([newfvs (map unique-name fvs)]
	     [fvs-initialized-yet (unique-name 'fvs-initialized-yet)])
       `(,kname subtok_ind (flag ,@(if (null? fvns) (list FREEVAR0) fvns))
		(stored [,KCOUNTER '0] 
			,@(map (lambda (fv) `[,fv 'stored-captured-var-uninitialized]) 
			       newfvs)
			,@(REGIMENT_DEBUG `[,fvs-initialized-yet #f])
			)		
		(dbg '"~a: Invoked continuation (tok ~a <~a>) with flag:~a args = ~a, fvs ~a = ~a ~n   TOKSTORE: ~a~n~n"
		     (my-id) ',kname subtok_ind flag (list ,@fvns) ',newfvs (list ,@newfvs) 
		     (simobject-token-store this))

		,@(REGIMENT_DEBUG
		   ;`(sim-print-queue (my-id))
		   `(if (not (token-present? (tok ,kname 0)))
			(error ',kname
			       "This continuation token was called before its kcounter was allocated (0th-indexed token).")))
			
		(if (eq? flag ',KINIT_FLAG)
		    (begin
		      ;[2005.09.24];(printf "~a: Init continuation: ~a~n" (my-id) ',kname) (flush-output-port)
			      
		      (if (= subtok_ind '0)
			;; No freevars if we're just initializing the counter-object.
			(void)
			(begin
			  (dbg "~a: Initializing continuation (tok ~a <~a>) freevars! vals: ~a\n" 
			       (my-id) ',kname subtok_ind (list ,@fvns))
			  ,@(REGIMENT_DEBUG `(set! ,fvs-initialized-yet #t))
			  ,@(map (lambda (fv fvn)
				   `(begin 
				      (set! ,fv ,fvn)
				      ,@(REGIMENT_DEBUG
					 `(dbg '"~a:  Set stored/captured freevar: ~a to ~a~n   NEWTOKSTORE: <DISABLED,CHECKCODE>~n" 
					       (my-id)
					       ',fv ,fvn ;(simobject-token-store this)
					       ))
				      ))
				 newfvs fvns))))
		    ;; Otherwise, assume the flag is KCALL_FLAG
		    (begin
#;		      ,@(REGIMENT_DEBUG `(if (not ,fvs-initialized-yet)
					     (error ',kname 
						    "This continuation token was called before its fvs were initialized.")))
		      ,(let loop ((fvs fvs) (newfvs newfvs) (body body))
			 (if (null? fvs) body
			     (loop (cdr fvs) (cdr newfvs)
				   (subst body (car fvs) (car newfvs)))))
					;,(number-freevars body)
		      ;; Since these are one-shot continuations, 
		      ;; we deallocate ourselves on the way out:
		      (evict (tok ,kname subtok_ind))
		      )))))))

    (define (no-first-class? k expr)
      (tml-generic-traverse
       (lambda (x loop)
	 (match x 
	   [,v (guard (eq? v k)) #f]
	   [(app ,v ,[args] ...) (guard (eq? v k))
	    (andmap id args)]
	   [,x (loop x)]))
       (lambda (ls _) (andmap id ls))
       expr))


    (define (stripk k expr)
      (tml-generic-traverse
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

    (define (process-expr expr env)
      ;; env: Just a list of bound variables (the arguments plus the subtok_id)
      (define (addhands newhands vec)
	(vector (append newhands (vector-ref vec 0)) (vector-ref vec 1)))
	
      ;; We do a very lame alias-analysis on our way down.
      ;; This lets us get a few obvious direct calls.
      ;; kbinds binds continuation names to token names.
      (let ((result
	     ;; As we go *down* the structure, we accumulate kbinds.
	     ;; As we come *up* we accumulate new handlers.
	 (let outer-loop ([expr expr] [kbinds '()])
	   (tml-generic-traverse
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
			(let-match ([#(,newhands ,v) val])
			  ;; It should be ok to duplicate this code:
			  (DEBUGMODE (if (not (symbol? (cadr (assq k kbinds))))
					 (error 'convert-closure 
						"tried to duplicate code that was supposed to be a symbol, but it's not: ~s"
						(cadr (assq k kbinds)))))
			  (addhands newhands
				    (loop `(if (eq? ,(cadr (assq k kbinds)) ,NULLK)
					       (void) ;; Fizzle if it's the null continuation object.
					       (call (tok ,(cadr (assq k kbinds)) (token->subid ,k)) ',KCALL_FLAG ,v)))))]
		     [(kcall ,k ,[val])		      
		      (let ([newhands (vector-ref val 0)]
			    [temp (unique-name 'temp)]
			    [v (vector-ref val 1)])
			(if (not (symbol? k))
			    (error 'closure-convert "kcall with this unexpected rator: ~a" k))
			(addhands newhands
				  (loop 
				   `(let ((,temp ,v))
				      (begin
					,@(REGIMENT_DEBUG
					   `(dbg "~a: Scheduling invocation of continution token: ~a  arg: ~a"
						 (my-id) ,k ,temp))
					(if (eq? ,k ,NULLK)
					    (void) ;; Fizzle on null continuation.
					    (call ,k ',KCALL_FLAG ,temp)))))))]
		     [(let ([,k (lambda (,hole) ,[val])]) ,body2)
		      (let ([newhands1 (vector-ref val 0)] 
			    [body1 (vector-ref val 1)])
			(let ([kname (unique-name 'K)])
			  (mvlet ([(closure khandler) (build-continuation kname env body1 hole)])
				 (let* ([result (outer-loop body2 (cons (list k kname) kbinds))]
					[newhands2 (vector-ref result 0)]
					[newbody (vector-ref result 1)])
				   (vector (cons khandler (append newhands1 newhands2))
					   `(let ([,k ,closure]) ,newbody))))))]
		     [(lambda (,v) ,[val])
		      (let ([newhands (vector-ref val 0)] [body (vector-ref val 1)])
			(let ([kname (unique-name 'K)])
			  (mvlet ([(closure khandler) (build-continuation kname env body v)])
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
    


    (define (process-tokbind tb)
      (mvlet ([(tok subtokid args stored constbinds body) (destructure-tokbind tb)])
	     (mvlet ([(newexpr newtokbinds) (process-expr body (cons subtokid args))])
;	     (mvlet ([(newexpr newtokbinds) (OLDprocess-expr body)])
		    (cons 
		     `[,tok ,subtokid ,args (stored ,@stored) ,newexpr]
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
                (let* ([ktoks (apply append (map cdr toks))]
		       [all-toks (apply append toks)]
		       [node-start-tok (assq 'node-start all-toks)]
		       [new-starts
			(match node-start-tok
			       [(node-start ,subtok_ind () (stored) ,body)
				(list `[node-start ,subtok_ind () (stored)
						   (begin 
;						     (dbg '"~a: node-start initializing continuation counters: ~a" 
;							  (my-id) ',(map car ktoks))
;; Disabling for now:

						     ,@(map (lambda (k)
							      `(call (tok ,(car k) 0) ',KINIT_FLAG))
							    ktoks)
;						     ,@(make-list (max 1 (length fvs)) 
;								  ''counter-holder-tok-dummy-val))
						     (call (tok actual-node-start ,subtok_ind)))]
				      `[actual-node-start ,subtok_ind () (stored) 
							  (begin 
;							    (dbg '"~a: running actual-node-start..." (my-id))
							    ,body)])]
			       [,other (error 'closure-convert "bad node-start tokhandler: ~a" other)])])
		  `(,lang
		    '(program (bindings ,constbinds ...)
			      (nodepgm (tokens ,(append new-starts
							(remq node-start-tok all-toks)) ...)))))]))
      ))
      

;; Now test the whole module:
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
       (node-start actual-node-start SOC-start K_2 tok1)]

      ["Trying to make sure we get the right stored vars."       
       (parameterize ((unique-name-counter 0))
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
		      (kcall k_58 (+ x '1000)))))))))))))
       ,(lambda (x)
	  (match (filter (lambda (x) (memq x '(kcounter HOLE))) (map deunique-name x))
		 [(kcounter kcounter HOLE) #t]
		 [,else #f]))]
		 

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
		    "28: Closure-convert: explicitely construct continuation closures."
		    these-tests))

(define test28 test-this)
(define tests28 these-tests)
(define test-closure-convert test-this)
(define tests-closure-convert these-tests)





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
                   
		 (subtok_ind fv0 flag)]

#;		[(,free-vars 
		  '(if (eq? flag '11)
                   (if (= subtok_ind '0) (void) (begin))
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
                          (evict (tok K_20 subtok_ind)))))
		 (subtok_ind fv0 flag)]
