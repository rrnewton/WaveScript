
;; simulator_alpha.ss
;;  -Ryan Newton [2005.02.25]
;===============================================================================

;; This will be a second attempt simulator.
;; However, it will support only core tml (no gradients).

;; It will have a single thread of control and a queue of simulator
;; events sorted by virtual clock times.

;; Later, it may serve as a place to test scheduling algorithms so
;; that we may actually implement the atomic action model.


(define this-unit-description 
  "simulator_alpha.ss: event-queue simulator for nodal language")

;; This structure contains all the global data needed a simulation.
(define-structure (simulation graph object-graph all-objs))

;; Positions are just 2-element lists.
(define-structure (node id pos))

;; This structure represents a simulated node:
;; Incoming is a list of messages (or return-objs).
;; Redraw is a boolean indicating whether the object needs be redrawn.
;; [2004.06.11] Added homepage just for my internal hackery.
;; [2004.06.13] Be careful to change "cleanse-world" if you change
;; this, we don't want multiple simulation to be thrashing eachother.
;; [2004.07.08] I don't know why I didn't do this, but I'm storing the
;; token-cache in the structure too
(define-structure (simobject node incoming timed-tokens redraw gobj homepage 
			     token-cache local-sent-messages local-recv-messages
			     ))

;; This structure represents a message transmitted across a channel.
(define-structure (msg-object token 
			      timestamp ;; when it was sent 
			      parent ;; :: simobject - who I got it from
			      args))

;; ======================================================================

;; This is our logger for events in the simulator:
(define (logger ob . args)
  (if (simulation-logger)
      (if (null? args)
	  (critical-section
	   (begin (display ob (simulation-logger))
		  (newline (simulation-logger))))
	  (critical-section
	   (display (apply format ob args) (simulation-logger))))))
;; This is just another variant:
;; This has no critical section for now!!! [2005.02.25]
(define-syntax with-logger
  (syntax-rules ()
      [(_ exp ...)
      (if (simulation-logger)
	  (parameterize ([current-output-port (simulation-logger)])
			exp ...))]))

;; This makes it use a lame sort of text display instead of the graphics display:
(define-regiment-parameter simulator-output-text #f (lambda (x) x))
;; This is a SEPERATE LOGGER for debug info as opposed to simulation events.
(define-regiment-parameter sim-debug-logger 
  (lambda args
    (critical-section
     (apply printf args)))
  (lambda (x)
    (unless (procedure? x)
	    (error 'simulator-debug-logger "~s is not a procedure" x))
    x))
(define-syntax silently
  (syntax-rules ()
    [(_ expr ...) (parameterize ([sim-debug-logger (lambda args (void))])
				expr ...)]))

;; ======================================================================

(define (free-vars expr)
  (let loop ((env ()) (expr expr))
    (match expr	 
	   [,var (guard (symbol? var)) (if (memq var env) '() (list var))]   
	   [(quote ,x) '()]
	   [(,prim ,rand* ...) (regiment-primitive? prim)
	    (let ((frees (map (lambda (x) (loop env x)) rand*)))
	      (apply append frees))]
	   [(lambda (,formals) ,expr)
	    (loop (append formals env) expr)]
	   [,else (error 'free-vars "not simple expression: ~s" expr)])))


;; Safer version:
#;(define (construct-msg-object token timestamp origin parent count args)
  (if (eq? token SPECIAL_RETURN_TOKEN)
      (error 'construct-msg-object "cannot manually make a return message")
      (begin
	(unless (token? token) (error 'construct-msg-object "bad token: ~s" token))
	(unless (or (number? timestamp) (not timestamp))
	  (error 'construct-msg-object "bad timestamp: ~s" timestamp))
	(unless (or (simobject? origin) (not origin))
		(error 'construct-msg-object "bad origin: ~s" origin))
	(unless (or (simobject? parent) (not parent))
		(error 'construct-msg-object "bad parent: ~s" origin))
	(unless (number? count)
		(error 'construct-msg-object "bad count: ~s" count))
	(unless (list? args)
		(error 'construct-msg-object "bad args: ~s" args))))
  (make-msg-object token timestamp origin parent count args))



;; [2004.06.28] This is a helper to construct the locally used
;; messages that don't have a parent, timestamp, etc.
#;(define (bare-msg-object rator rands)
  (if (eq? rator SPECIAL_RETURN_TOKEN)
      (error 'bare-msg-object "Can't build a return msg!!"))
  (construct-msg-object rator ;; token
		   #f    ;; timestamp
		   #f    ;; origin
		   #f    ;; parent
		   0     ;; count
		   rands))

;; Here's a helper to check the invariants on a msg-object
#|(define (valid-msg-object? mo)
  (and (msg-object? mo)
       (let ([token  (msg-object-token  mo)]
	     [timestamp (msg-object-timestamp mo)]
	     [origin (msg-object-origin mo)]
	     [parent (msg-object-parent mo)]
	     [count  (msg-object-count  mo)]
	     [args   (msg-object-args   mo)])	
	 (or 
	  #;(and (return-msg? mo) ;; Things are different for return-messages.
		  (list? count) (andmap integer? count)
		  (list? timestamp) (andmap integer? timestamp)
		  (or (not parent) (simobject? parent))
		  (or (not origin) (simobject? origin))
		  (list? args)		  
		  )
	     (and (token? token)
		  (or (not timestamp) (integer? timestamp))
		  (or (not parent) (simobject? parent))
		  (or (not origin) (simobject? origin))
		  (integer? count)
		  (list? args)))))
|#

;; This globally defined functions decides the sensor values.
;; Here's a version that makes the sensor reading the distance from the origin:
(define (sense-dist-from-origin loc)
  (let ([x (car loc)] [y (cadr loc)])
    (sqrt (+ (expt x 2) (expt y 2)))))

;(define-parameter 
(define (current-sense-function) sense-dist-from-origin)

;;========================================

(define (base-station? x)
  (cond 
   [(simobject? x) (= BASE_ID (node-id (simobject-node x)))]
   [(node? x)      (= BASE_ID (node-id x))]
   [else (error base-station? "bad input: ~a" x)]))

(define (id x) x)

(define (random-node) 
  (make-node 
   (let loop ((id (random 1000)))
     (if (eq? id BASE_ID) (loop (random 1000))
	 id))
   (list (random world-xbound)
	 (random world-ybound))
   ))
  
(define (dotted-append ls ob)
  (let loop ((ls ls))
    (if (null? ls) ob
	(cons (car ls) (loop (cdr ls))))))

;; Increment a top-level binding.  Totally dynamic.
(define (incr-top-level! v)
  (set-top-level-value! v (add1 (top-level-value v))))

;; Helper to determine the distance between two 2d positions.
(define (posdist a b)
  (sqrt (+ (expt (- (car a) (car b)) 2)
	   (expt (- (cadr a) (cadr b)) 2))))

(define structure-copy  vector-copy)

;; TODO, returns all the nodes in the graph that are connected to the
;; given simobject.  Gonna use this for unit testing oracles.
(define (all-connected simob sim)
  (graph-get-connected simob (simulation-object-graph sim)))

;; This generates the default, random topology: 
;; (There are more topologies in "network_topologies.ss"
(define (make-object-graph g) 
  (graph-map (lambda (nd) (make-simobject nd '() '() #f #f '() 
					  (make-default-hash-table) 0 0))
	     g))

(define (fresh-simulation)
   (let* ([graph 
          (let ((seed (map (lambda (_) (random-node)) (iota numprocs))))
            ;; TEMP: Here I give the nodes distinct, consecutive ids.
            (if (regiment-consec-ids)
                (for-each set-node-id! 
                          seed (iota (length seed))))
            ;; Now we just SET the first node to have the BASE_ID and serve as the SOC.
            (set-node-id! (car seed) BASE_ID)
            ;; Connect the graph:
            (set! seed
                  (map (lambda (node)
                         (cons node 
                               (filter (lambda (n) 
                                         (and (not (eq? node n))
                                              (< (posdist (node-pos node) (node-pos n)) radius)))
                                       seed)))
                       seed))
            seed)]
          [obgraph (make-object-graph graph)]
          [allobs  (map car obgraph)])
     (make-simulation graph obgraph allobs)))


                         
   

;; ======================================================================


(define (process-statement tokbinds allstored)
  (letrec ([get-arg-index
	    (lambda (tok argname)
	      (let ([entry (assq tok tokbinds)])
		(if (not entry)
		    (error 'simulator_nought:get-arg-index
			   "No entry for token! ~a" tok))
		(list-find-position argname (cadr entry))))]

	   [process-expr 
	 (lambda (expr)
	   ;; This is a little wider than the allowable grammar to allow
	   ;; me to do test cases:
	   (match expr
		  [,x (guard (and (symbol? x) (memq x allstored))) `(car ,x)]
		  [,x (guard (or (symbol? x) (constant? x))) x]
		  [(quote ,x) `(quote ,x)]

		  ;; NOTE! These rands ARE NOT simple.
		  [(call ,rator ,[rand*] ...)
		   `(set-simobject-outgoing! this
		    (cons (bare-msg-object ,rator (list ,@rand*))
			  (simobject-outgoing this)))]

;		  [(activate ,rator ,rand* ...)
;		   (build-activate-call `(quote ,rator) rand*)]

		  [(timed-call ,delay ,rator ,rand* ...)
		   ;; Delay is in milleseconds.
		  `(set-simobject-timed-tokens!
		    this (cons (cons (+ ,delay (cpu-time))
				     (bare-msg-object ,rator (list ,@rand*)))
			       (simobject-timed-tokens this)))]

		  [(let-stored ([,lhs* ,[rhs*]] ...) ,[bodies] ...)
		   `(begin "Doing let stored."
			   ,@(map (lambda (lhs rhs)
				    `(if (eq? lhs '()) (set-car! lhs rhs)))
				  lhs* rhs*)
			   ,bodies ...)]
		  
		  ;; is_scheduled
		  ;; 
		  ;; is
		  ;; evict

		  [(if ,[test] ,[conseq] ,[altern])
		   `(if ,test ,conseq ,altern)]

		  [(my-id) '(node-id (simobject-node this))]
		  [(dist) '(sim-dist)]
  		  ;; <TODO> WHY NOT QUOTED:
		  [(dist ,tok) `(sim-dist ',tok)]
		  [(loc) '(sim-loc)]
		  [(locdiff ,[l1] ,[l2]) `(sim-locdiff ,l1 ,l2)]

		  [(light-up ,r ,g ,b) `(sim-light-up ,r ,g ,b)]
		  [(leds ,which ,what) `(sim-leds ',which ',what)]
		  [(dbg ,str ,[args] ...)
		   ;; TODO FIX ME: would be nice to print properly
		   `(begin (display (format ,str ,@args)) (newline))]

		  [(,prim ,[rand*] ...)
		   (guard (token-machine-primitive? prim))
		   `(,prim ,rand* ...)]

		  ;; We're being REAL lenient in what we allow in token machines being simulated:
		  [(let ((,lhs ,[rhs]) ...) ,[bods] ...)
		   `(let ((,lhs ,rhs)  ...) ,bods ...)]

		  ;; We're letting them get away with other primitives because
		  ;; we're being lenient, as mentioned above.
		  [(,rator ,[rand*] ...)
		   ;; Don't want to be too lenient tho:
		   (guard (not (token-machine-primitive? rator))
			  (not (memq rator '(emit call timed-call activate relay return))))
;;		   (disp "processing app:" rator rand*)
		   `(,(process-expr rator) ,rand* ...)]
		  
		  [,otherwise (error 'simulator_nought.process-expr 
				"don't know what to do with this: ~s" otherwise)])
	   )])
    (lambda (stmt) (process-expr stmt))))


(define (process-binds binds)
;  (disp "processbinds" binds expr)
  (let* ([graph (map (lambda (bind)
		       (cons (car bind)
			     (free-vars (cadr bind))))
		     binds)]
	 [flat (reverse (topological-sort graph eq?))]
	 [newbinds 
	  (map (lambda (sym) 
		 (let ([bind (assq sym binds)])
		   (if bind bind
		       (error 'simulator_nought:process-binds
			      "no entry for sym '~s' in constant binds ~s" sym binds))))
	       flat)])
    (if (cyclic? graph)
	(error 'process-binds "for now can't take a cyclic graph: ~s" newbinds))
    newbinds))
	 
;; This assumes that all the stored vars (and all vars in general)
;; have unique names at this point.
(define (find-stored expr)
       (match expr
 	     [(quote ,_) '()]
 	     [,var (guard (symbol? var)) '()]
 	     [(begin ,[exprs] ...) (apply append exprs)]
 	     [(if ,[exprs] ...) (apply append exprs)]
 	     [(let* ( (,_ ,[rhs]) ...) ,[body])	(apply append body rhs)]
	     [(let-stored ( (,lhs* ,[rhs*]) ...) ,[body])	
	      (apply append lhs* rhs*)]
; 	     [(emit ,tok ,[args*] ...)	(cons tok (apply append args*))]
; 	     [(relay ,_ ...) '()]
; 	     [(return ,[expr] (to ,t) (via ,v) (seed ,[seed_val]) (aggr ,a))
; 	      (append expr seed_val)]	     	   
 	     [(dist ,_ ...) '()]
 	     [(leds ,what ,which) '()]
	     [(call ,_ ,[args*] ...) (apply append args*)]
; 	     [(activate ,_ ,[args*] ...) (apply append args*)]
 	     [(timed-call ,_ ,__ ,[args*] ...) (apply append args*)]
 	     [(,[rator] ,[rands] ...) (apply append rator rands)]
 	     [,otherwise
 	      (error 'desugar-gradient:process-expr 
 		     "bad expression: ~s" otherwise)]
	     ))


;; Every token handler, once converted for the simulator, has a signature: 

;; tokstore, vtime, tokargs -> newmsgs, newtokobjs


;; Takes token bindings, returns compiled bindings
;; along with an association list of stored-vars.
(define (process-tokbinds tbinds)
  (let* ([allstored
          (map (lambda (bind) 
                 (match bind 
		   [(,tok (,args ...) ,expr)
                    (list tok (find-stored expr))]))
               tbinds)]
         [binds 
          (map
           (lambda (tbind)
	    (match tbind 
		   [(,tok (,args ...) ,expr* ...)
		    (let ([stored (assq tok allstored)])
		      `[,tok 
                         (lambda ,args 
                           #;(DEBUGPRINT2 
                            (disp "Token " ',tok 
                                  "running at" (node-id (simobject-node this)) 
                                  " with message: " ',args))
                           (set-simobject-outgoing! this '())
                           ,@(map (process-statement tbinds stored) expr*)
                           (let ([result
                                  (values (simobject-outgoing this)                                          
                                          )])
                             (set-simobject-outgoing! this '())
                             result)
                           )
                         ])]))
	  tbinds)])
    (printf "~n;  Converted program for Simulator:~n")
    (printf "<-------------------------------------------------------------------->~n")
    (pretty-print binds)

    (values binds allstored)))

;; This produces the compiled "main" program. 
(define (build-body tbinds expr)
  (let-values ([(binds allstored) (process-tokbinds tbinds)])
    `(let ([,(apply append (map cadr allstored)) '()] ...)
       (letrec ([this-message #f]
                ,@binds)
         ,expr))))

;; ======================================================================

(define (compile-simulate-alpha prog)
  ;; Accept either with or without the language wrapper:
  (let ((prog (match prog 
		     [(program ,_ ...) prog]
		     [(,input-lang '(program ,stuff ...)) `(program ,stuff ...)])))
    (match prog
      [(program (bindings ,nodebinds ...)
		(socpgm (bindings ,socbinds ...) ,socstmts ...)
		(nodepgm (tokens ,nodetoks ...) (startup ,starttoks ...)))
       (let* (
       
#;       [socprog
	 `(lambda (soc-return soc-finished SOC-processor this object-graph all-objs)
	    (let ([I-am-SOC #t]) 
	      ;; We have to duplicate the tokbinds here...
              `(let* (,@(process-binds nodebinds)
                      ,@(process-binds socbinds))
                 ,(build-body
                   nodetoks generic-defs			
                   `(begin ,@(map (process-statement nodetoks)
                                  socstmts)
                           'soc_finished)))))]
              
       [nodeprog
        ;; Takes: the two soc procedures, 
        ;;        simobj for the soc, 
        ;;        simobj for this,
        ;;        simulation
	`(lambda (soc-return soc-finished SOC-processor this sim)
	    (let ([I-am-SOC (eq? this SOC-processor)]
		  [local-sense (lambda ()
				 ((current-sense-function)
				  (node-pos (simobject-node this))))])

	      (let* ,(process-binds nodebinds)
		,(build-body
                  nodetoks
                  `(begin 
		    ;; <TODO>: FIX THIS UP, MAKE SURE NO ARGS IS OK!?:
		    ;; Call all the starting tokens with no arguments:
                     ,@(map list starttoks)
                     
                     (let main-node-loop ([incoming (simobject-incoming this)])
                       (let ([current-time (cpu-time)]
                             [triggers (simobject-timed-tokens this)])
                         (let ([fired-triggers 
                                (filter (lambda (trigger) (>= current-time (car trigger)))
                                        triggers)])
                           ;; Timed calls get priority over other incoming:
                           (if (not (null? fired-triggers))
                               (begin 
                                 (set-simobject-incoming! this
                                                          (append (map cdr fired-triggers)
					 (simobject-incoming this)))
                                 (set-simobject-timed-tokens! this
					 (difference triggers fired-triggers))
                                 (main-node-loop (simobject-incoming this)))
                               (cond
		      [stop-nodes 
		       (display "*") ;(disp "Node stopped by external force!")
		       'node-stopped]		     

		      [(null? incoming)
		       ;; No good way to wait or stop the engine execution?
		       (yield-thread)
		       (main-node-loop (simobject-incoming this) returns-next-handled returns)]

		      [else
		       ;; This might introduce message loss (because of no
		       ;; semaphores) but I don't care:					
		       (let ((msg 'NOT-SET-YET))
			       
			 (DEBUGPRINT
			  ;; Print out the process number when we handle a message:
			  (printf "~s." (node-id (simobject-node this))))
			 ;; Pop that message off:
			 (critical-section
			  (set! msg (last incoming))
			  (if (null? (cdr incoming))
			      (set-simobject-incoming! this '())
			      (list-remove-last! incoming)))

			 (if (return-obj? msg)
			     (main-node-loop (simobject-incoming this) 
					     returns-next-handled
					     (cons msg returns))
			     (begin 
			       (DEBUGMODE
				(if (not (valid-msg-object? msg))
				    (begin
				      (define-top-level-value 'x msg)
				      (error 'node-handler 
				     "invalid message to node, should be a valid msg-object: ~s "
				     (pop-msg-object msg )))))
			       (handler msg)
			       (main-node-loop (simobject-incoming this)
					       returns-next-handled 
					       returns))
			 ))]))
		     ))))))))])

       (DEBUGPRINT
	(printf "~nGOT TOKEN HANDLERS:~n" )
	(pretty-print nodetoks)(newline))

;       (disp "Socprog")
;       (pretty-print socprog)
;       (newline)
;       (disp "Nodeprog")
;       (pretty-print nodeprog)
;;       (set! f socprog)
;;       (set! sp socprog)
;;       (set! np nodeprog)
       ;; DANGEROUS:
;       (list socprog nodeprog)
nodeprog
       )])))


(define these-tests
  `(
    [(find-stored '(if '3 '4 (let-stored ([a '3]) '4)))
     (a)]

    [(compile-simulate-alpha
      '(program
	(bindings)
	(socpgm  (bindings) (call tok1))
	(nodepgm 
	 (tokens
	  [tok1 () (bcast tok2 '3)]
	  [tok2 (x) (bcast tok2 (+ x x))])
	 (startup))))
     '??]
    ))


(define (wrap-def-simulate test)
  `(begin (define simulate run-simulation) ,test))

;; Use the default unit tester from helpers.ss:
;; But this also makes sure the world is initialized before doing unit tests:
(define test-this
  (let ((tester (default-unit-tester 
		  this-unit-description 
		  ;; Make sure that the world is clean before each test.
		  (map (lambda (test)
			 (match test
			   [(,prog ,res) `((begin (cleanse-world) ,prog) ,res)]
			   [(,name ,prog ,res) 
			    `(,name (begin (cleanse-world) ,prog) ,res)]))
			 these-tests)
		  tester-eq?
		  wrap-def-simulate)))
    (lambda args
      ;; First init world:
      ;(init-world)
      (apply tester args))))

(define (t)
(compile-simulate-alpha
      '(program
	(bindings)
	(socpgm  (bindings) (call tok1))
	(nodepgm 
	 (tokens
	  [tok1 () (bcast tok2 '3)]
	  [tok2 (x) (bcast tok2 (+ x x))])
	 (startup)))))
