
;; simulator_nought.ss
;;  -Ryan Newton [2004.05]
;===============================================================================

;; This is a simulator for the output of pass10_deglobalize.  

;; This file uses and abuses top-level bindings (like mad), because it
;; uses eval, so it's not very well encapsulated right now.

;; NOTE: Unlike some of the files in the chez directory, this expects
;; to be loaded from its own parent directory.

;; <TODO>: Make the simulator not use global state for the graph!!

;;============================================================
;; REQUIRES: This file requires that the slib 'tsort module be loaded
;; providing the topological-sort function.

;; REQUIRES: Also on hash tables from SLIB.

;; REQUIRES: This file requires the "flat_threads.ss" interface, which
;; is a simple interface over engines or threads.

;===============================================================================

;; TYPE SIGNATURES:

;; Simulator = Simulation, ?timeout -> ReturnVals
;; Simulation = soc-return, soc-finish -> (vector socthunk [nodethunks])


;; compile-simulate-nought :: tokmachineprog -> [socprog, nodeprog]

;; build-simulation :: [socprog, nodeprog] -> Simulation

;; generate-simulator :: threadrunner, simcore ->
;;                       thesim, ?timeout -> 
;;                       returnvals
;;    threadrunner :: Thunks, ?TimeOut -> (All_Threads_Returned | Threads_Timed_Out)
;;    simcore :: ThunksVec ->? Timeout -> ThunksList




;===============================================================================
;; Some CHANGES (not keeping a complete log):

;; [2004.07.27] Adding return timer.

;; Ok, I was going to add a time-window for the return values, but
;; right now I'm deciding between that and an explicit generation
;; counter.  But how would I know whether I've gotten everything in
;; the generation?  Could use a safely large time-window and seperate
;; out the different generations within it?
;;   Well, either way, I'm adding a timer now.

;; [2004.07.11] Fixing soc-return and soc-finish.
;; Doing a refactoring to depend less on my mutation of the global
;; environment.  I've been really sloppy in this because I thought it
;; would be such a "quick and easy" little simulator.  Now the
;; simulation object returned by build-simulation takes and passes on
;; soc-return and soc-finish functions.  The node-prog and soc-progs
;; have been modified accordingly.

;; [2004.06.28]
;; Added a special under-the-table "return" message.  It is handled
;; seperately from other messages.  This message must have six arguments:
;; msg-object:  timestamp, origin, parent, count, ARGS
;; ARGS: return_val  -- the value being returned
;;       to_tok      -- the token to which the value shall be passed when it gets home
;;       via_tok     -- the token supplying the back-trail for the return to follow
;;       seed_val    -- the base-value for the binary-op 
;;       aggr_tok    -- the token (binary function) for aggregating this return
;;       senders     -- this is temporary, it keeps a list of all nodes contributing 
;;                      to the current aggregated result

;; NOT FINISHED:

;; [2004.06.09] (soc-return v) Will make v *one of* the return values (a
;; whole stream of them can be provided). (soc-finished) will terminate
;; the computation if there has been a returned value, 
;; ???? WHAT IF ITS NOT SOC CALLING? ????

;;otherwise it
;; will just terminate the thread for the processor

;;[2004.06.09] RRN: Adding implicit 'I-am-SOC' boolean variable for
;; use by node programs.. This is only used by generated code, or my
;; handwritten test cases.  Along with it is a SOC-processor binding
;; which is bound to the simobject for the SOC.


;===============================================================================
;; GLOBAL VARIABLES AND TYPES:

;; The main global variables are graph, object-graph, and all-objs

;; NOTE!  The simulator dynamically defines top level variables:
;;  total-messages, soc-return, soc-finished, stop-nodes

;; This is the simplest simulator ever.  Takes the output of pass "deglobalize".
(define this-unit-description 
  "simulator_nought.ss: simplest simulator for nodal language")

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

;; Positions are just 2-element lists.
(define-structure (node id pos))
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

;; This record holds the info that the token cache needs to maintain
;; per each token name.
;; NOTE: The lack of a *parent* indicates that the message is a local call:
(define-structure (msg-object token 
			      timestamp ;; when it was sent 
			      origin ;; :: simobject - original source of message
			      parent ;; :: simobject - who I got it from
			      count
			      args))

;; [2004.07.27]
;; Doing a refactoring to add this, seperating msg-object's from return-objs
(define-structure 
  (return-obj
   ;; has some analogous fields to msg-object:
   timestamps parent counts
   ;; And other fields for the purpose of propogating return values.
   return_vals ;; -- the value being returned
   to_tok   ;;   -- the token to which the value shall be passed when it gets home
   via_tok  ;;   -- the token supplying the back-trail for the return to follow
   seed_val ;;   -- the base-value for the binary-op 
   aggr_tok ;;   -- the token (binary function) for aggregating this return
   senders  ;;   -- this is temporary, it keeps a list of all nodes contributing 
   ))

;; Temporary:
(define (pop-msg-object mo)
  (list 
   (msg-object-token  mo)
   (msg-object-timestamp mo)
   (msg-object-origin mo)
   (msg-object-parent mo)
   (msg-object-count  mo)
   (msg-object-args   mo)))

;; Safer version:
(define (construct-msg-object token timestamp origin parent count args)
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
(define (bare-msg-object rator rands)
  (if (eq? rator SPECIAL_RETURN_TOKEN)
      (error 'bare-msg-object "Can't build a return msg!!"))
  (construct-msg-object rator ;; token
		   #f    ;; timestamp
		   #f    ;; origin
		   #f    ;; parent
		   0     ;; count
		   rands)); args

;; Here's a helper to check the invariants on a msg-object
(define (valid-msg-object? mo)
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
		  (list? args))))))

;; These global vars start off uninitialized and are initialized with
;; "init-world", below.
(define graph #f) ;; Graph of 'node'
(define object-graph #f) ;; Graph of 'simobject'
(define all-objs #f) ;; List of 'simobject' (this is just computed
		     ;; from object-graph; its existence is merely an
		     ;; optimization

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
(define (all-connected simob)
  (graph-get-connected simob object-graph))

;; This generates the default, random topology: 
;; (There are more topologies in "network_topologies.ss"
(define (make-object-graph g) 
  (graph-map (lambda (nd) (make-simobject nd '() '() #f #f '() 
					  (make-default-hash-table) 0 0))
	     g))
(define (init-world)
  (set! graph   
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
	  ;; If there's a prexisting (and graphical) world, clean the screen.
	  (and all-objs 
	       (not (null? all-objs))
	       (simobject-gobj (car all-objs))
	       (clear-buffer))
	  seed))
  (set! object-graph (make-object-graph graph))
  (set! all-objs (map car object-graph))
  )

;; Call it now as we start up:
(init-world)

(define (destroy-world)
  (set! graph #f)
  (set! object-graph #f)
  (set! all-objs #f))

;; init-world allocates the world, but we also want a procedure to cleanse it.
(define (cleanse-world)
  (printf "Cleansing world.~n")
  (if object-graph
      (for-each (lambda (entry)
		  (let ((simob (car entry)))
		    (set-simobject-redraw! simob #f)
		    (set-simobject-homepage! simob '())
		    (set-simobject-incoming! simob '())
		    (set-simobject-timed-tokens! simob '())
		    (set-simobject-token-cache! simob (make-default-hash-table))
		    (set-simobject-local-sent-messages! simob 0)
		    (set-simobject-local-recv-messages! simob 0)
		    ))
		object-graph)
      (error 'cleanse-world "world must not be allocated object-graph is false.")))

;; This is used to do an evaluation within some particular graph:
(define (with-graph g th)
  (fluid-let ((graph g)
	      (object-graph (make-object-graph g)))
    (fluid-let ((all-objs (map car object-graph)))
      (th))))

;;========================================  

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

(define build-call
  (lambda (rator rand*)
;    (disp "build call" rator rand*)
    (let* ([sym (string->symbol
		 (string-append "call-result-" 
				(number->string (random 1000))))])
      `(handler (bare-msg-object ,rator (list ,@rand*)))
;    `(let ((,sym
;;	    ;(sendmsg (bare-msg-object ,rator (list ,@rand*)) this)
;	    (handler (bare-msg-object ,rator (list ,@rand*)))
;	    ))
;       (if (eq? ,sym 'multiple-bindings-for-token)
;	   (error 'call "cannot perform a local call when there are multiple token handlers for: ~s"
;		  ,rator)
;	   ,sym))    
    )))

;; Doesn't have a return value...  Used to start up an independent loop.
;; This version doesn't mess with an existing pulsating loop.
(define build-activate-call
  (lambda (rator rand*)   
    ;; Is the token cached in the timed buffer?
    `(if (null? (filter (lambda (entry) (eq? ',rator (cdr entry)))
			     (simobject-timed-tokens this)))
	 ;; If not, it's safe to call it, we schedule it for asap:
	 (set-simobject-timed-tokens! this
	  (cons (cons (cpu-time) (bare-msg-object ,rator (list ,@rand*)))
		(simobject-timed-tokens this))))))

;; FIXME Need some quotes here no!!

;; Doesn't have a return value... used for delayed scheduling.
(define build-timed-call
  (lambda ( delay rator rand*)
    ;; Set the time the appropriate amount in the future and drop in the token!
    `(set-simobject-timed-tokens!
      this (cons (cons (+ ,delay (cpu-time))
		       (bare-msg-object ,rator (list ,@rand*)))
		 (simobject-timed-tokens this)))))

(define (process-statement tokbinds)
  (letrec ([get-arg-index
	    (lambda (tok argname)
	      (let ([entry (assq tok tokbinds)])
		(if (not entry)
		    (error 'simulator_nought:get-arg-index
			   "No entry for token! ~a" tok))
		(list-find-position argname (cadr entry))))]

	   [process-expr 
	 (lambda (expr)
;	   (disp "Process expr" expr)
	   (match expr
		  ;; This is a little wider than the allowable grammar to allow
		  ;; me to do test cases:
		  [,x (guard (or (symbol? x) (constant? x))) x]
		  [(quote ,x) `(quote ,x)]
		  ;; NOTE! These rands ARE NOT simple.
		  [(call ,rator ,[rand*] ...)
		   (DEBUGMODE
		    (if (not (token? rator))
			(error 'simulator_nought:process-statement
			       "call form expects rator to be a token name: ~s"
			       rator)))
		   (build-call `(quote ,rator)
			       rand*)]
		  [(activate ,rator ,rand* ...)
		   (build-activate-call `(quote ,rator) rand*)]
		  [(timed-call ,delay ,rator ,rand* ...)
		   ;; Delay is in milleseconds.
		   (build-timed-call delay `(quote ,rator) rand*)]		  

		  [(reject)
		   '(set! reject-incoming-token #t)]

		  [(cache ,tok)
		   `(hashtab-get (simobject-token-cache this) ',tok)]

		  [(cache ,tok ,field)
		   `(let ([entry (hashtab-get (simobject-token-cache this) ',tok)])
		      (if entry			  
			  (list-ref (msg-object-args entry)
				    ,(get-arg-index tok field))))]


		  ;; It's like call but doesn't add quotes.
		  [(internal-call ,rator ,rand* ...)
;;		   (disp "processing internal-call" rator rand*)
		   ;; Could add the message to incoming instead!
		   ;`(handler (construct-msg-object ',rator #f #f 0 ',rand*))
		   ;; [2004.06.16] For now I'm raising an error... 
		   ;; don't know what the correct behaviour should be:
		   ;; NOTE: there is the possibility of variable capture with 'call-result'
		   `(let ((call-result-9758
			   (sendmsg (bare-msg-object ,rator (list ,@rand*)) this)))
		      (if (eq? call-result-9758 'multiple-bindings-for-token)
			  (error 'call "cannot perform a local call when there are multiple token handlers for: ~s"
				 ,rator)
			  call-result-9758))
		   ]

;		  [(if ,a ,b ,c)
;		   (disp "IF" a b c)]
		  
		  [(if ,[test] ,[conseq] ,[altern])
		   `(if ,test ,conseq ,altern)]

		  [(flood ,tok) `(sim-flood (quote ,tok))]
		  [(emit ,opera ...)
		   ;; This is an original emission, and should get a count of 0.
		   `(sim-emit (quote ,(car opera)) (list ,@(cdr opera)) 0)]

		  [(dist) '(sim-dist)]		
  		  ;; <TODO> WHY NOT QUOTED:
		  [(dist ,tok) `(sim-dist ',tok)]
		  [(loc) '(sim-loc)]		
		  [(locdiff ,[l1] ,[l2]) `(sim-locdiff ,l1 ,l2)]
	 	  
		  [(relay) `(sim-relay)]		  
		  [(relay ,rator ,rand* ...) '(VOID-FOR-NOW) ]

		  [(elect-leader ,tok) `(sim-elect-leader ',tok)]

;		   [(elect-leader ,tok ,tokproc) ]

		  ;; Right now I'm recurring on the argument, this
		  ;; shouldn't be required by code generated from my
		  ;; compiler currently. [2004.06.09]
		  [(return ,[x] ,optional ...)
		   (let ([totok (if (assq 'to optional)
				    `(quote ,(cadr (assq 'to optional)))
				    '(string->symbol
				      (string-append 
				       (symbol->string (msg-object-token this-message))
				       "_return")))]
			 [via (if (assq 'via optional)
				  `(quote ,(cadr (assq 'via optional)))
				  '(msg-object-token this-message))]
			 [seed (if (assq 'seed optional)
				   (cadr (assq 'seed optional))
				   #f)]
			 [aggr (if (assq 'aggr optional)
				   (cadr (assq 'aggr optional))
				   #f)])
;		     (DEBUGMODE
;		      (if (not (and (token? via)
;				    (token? totok)))
;			  (error 'process-statement:return
;				 "One of these is not a token: via: ~s, totok: ~s" via totok)))			       
		     ;; FIXME: does seed need a quote!!!???
		     `(sim-return ,x ,totok ,via ,seed ',aggr))]

		  [(light-up ,r ,g ,b) `(sim-light-up ,r ,g ,b)]		  
		  [(leds ,which ,what) `(sim-leds ',which ',what)]
		  [(dbg ,str ,args ...) 
		   ;; TODO FIX ME: would be nice to print properly
		   `(begin (display ,(cons str args)) (newline))]

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
    (lambda (stmt)
      (process-expr stmt))))

(define (process-binds binds expr)
;  (disp "processbinds" binds expr)
  (let* ([graph (map (lambda (bind)
		       (cons (car bind)
			     (free-vars (cadr bind))))
		     binds)]
	 [flat (reverse (topological-sort graph eq?))]
	 [binds 
	  (map (lambda (sym) 
		 (let ([bind (assq sym binds)])
		   (if bind bind
		       (error 'simulator_nought:process-binds
			      "no entry for sym '~s' in constant binds ~s" sym binds))))
	       flat)])
    (if (cyclic? graph)
	(error 'process-binds "for now can't take a cyclic graph: ~s" binds))
    `(let* ,binds ,expr)))

;; This removes duplicates among the token bindings.
;; <TOOPTIMIZE> Would use a hash-table here for speed.
(define (remove-duplicate-tokbinds tbinds)
;  (disp "Removing token dups" tbinds)
  (let loop ((ls tbinds))
    (if (null? ls) '()
	(let* ([tok (caar ls)]
	       [dups (filter (lambda (entry) (eq? tok (car entry))) (cdr ls))])
	  (if (null? dups)
	      (cons (car ls) (loop (cdr ls)))	      	      
	      (let* ([all-handlers (cons (car ls) dups)]
		     [formals* (map cadr all-handlers)]
		     [body* (map caddr all-handlers)]
		     [thunks (map (lambda (bod) `(lambda () ,bod)) body*)])
;		(disp "GOT DUPS " tok formals*)
		(DEBUGMODE
		 (if (not (apply myequal? formals*))
		     (error 'simulator_nought:remove-duplicate-tokbinds
			    "handlers for token ~s don't all take the same arguments: ~s" 
			    tok formals*))
		 )
		(let ([mergedhandler
		       `[,tok ,(car formals*)
			      (begin (for-each 
				      (lambda (th) (th))
				      (randomize-list ,(cons 'list thunks)))
				     'multiple-bindings-for-token)
;		             (begin ,@body* 'multiple-bindings-for-token)
			     ]])
;		  (disp "MERGED:" mergedhandler)
		(cons mergedhandler
		      (loop (filter (lambda (entry) (not (eq? tok (car entry)))) (cdr ls)))))
		  ))))))
	 
;; Takes token bindings and a body expression:
(define (process-tokbinds tbinds extradefs expr)
  (let ([binds 
	 (map
	  (lambda (tbind)
	    (match tbind 
		   [(,tok (,args ...) ,expr* ...)
		    `[,tok (lambda ,args 
			     (DEBUGPRINT2 
			      (disp "Token " ',tok 
				    "running at" (node-id (simobject-node this)) 
				    " with message: " ',args))
			     ,@(map (process-statement tbinds) expr*))]]))
	  (remove-duplicate-tokbinds tbinds))]
	[handler (build-handler tbinds)]
	)

    (printf "~n;  Converted program for Simulator:~n")
    (printf "<-------------------------------------------------------------------->~n")
    (pretty-print binds)
    

    `(let () ,@(map (lambda (x) (cons 'define x)) binds)
	  [define handler ,handler] 
	  [define this-message #f];)
	  ,@extradefs ;; <TODO> THIS IS WEIRD AND LAME <TOFIX>
    ,expr)))

;; This builds the function that handles incoming tokens.
(define (build-handler tbinds)
	;; These inputs to handler must be the *child* message-object
	;; (that is, already updated to have an incremented count, the
	;; correct parent, etc).  So we can shove it right in our 
	`(lambda (themessage) ;(origin parent count tok args)
;		    (disp "Handling! msg" (msg-object-token themessage) 
;			  " parent? " (if (msg-object-parent themessage) #t #f)
;			  " Soc? " I-am-SOC 
;			  "Args:" (msg-object-args   themessage))
		    (let ([origin (msg-object-origin themessage)]
			  ;[parent (msg-object-parent themessage)]
			  [count  (msg-object-count  themessage)]
			  [tok    (msg-object-token  themessage)]
			  [args   (msg-object-args   themessage)])
		      
;		    (disp "HANDLER at" (node-id (simobject-node this)) ": " tok args)
		    ;; Redraw every time we handle a message our state might have changed.
		    (set-simobject-redraw! this #t)
		    ;; This refers to the token cache for this processor:
		    (let ([entry (hashtab-get (simobject-token-cache this) tok)]
			  [handle-it (lambda (newentry)
				       (fluid-let ((this-message newentry))
					 (case tok
					   ,@(map (lambda (tok)
						    `[(,tok) 
						      (apply ,tok args)])
						  (map car tbinds))
					   [else (error 'node_program "Unknown message: ~s" tok)]
					   )))])
			  
			  (if (not origin) 
			      ;; This is a local call, don't touch the cache:
			      (handle-it (bare-msg-object tok args))

			      ;; TODO: Could optimize a *wee* bit by mutating instead of recreating here.
			      ;; BUT! No premature optimization.		      
			      (if (or (not entry) ;; There's no entry for that token name.
				      (= 0 count)
				      (< count (msg-object-count entry))) ;; This could be <=, think about it. TODO

				  (begin
				    (if (= 0 local-recv-messages)
					(silently (sim-light-up 200 0 0)))
				    (set! local-recv-messages (add1 local-recv-messages))
				    
				    ;; This is where I'm gonna cut in support for "reject".. [2004.10.23]
				    (fluid-let ([reject-incoming-token #f])
				      (handle-it themessage)
				      ;; Only add it to the cache if it hasn't been rejected:
				      (if (not reject-incoming-token)
					  (hashtab-set! (simobject-token-cache this) tok themessage)))
				    )

				  (begin 
				    (incr-top-level! 'total-fizzles)
				;(disp "Message fizzle(no backflow) " tok " to " (node-id (simobject-node this)))
				    (void))
				  )))
		    )))


'(define-syntax remove-last!
  (syntax-rules ()
    [(_ id)
     (cond 
      [(null? id) (error 'remove-last "can't remove last from null")]
      [(null? (cdr id)) (set! id '())]
      [else
       (let loop ([prior id]
		  [next (cdr id)])
	 (if (null? (cdr next))
	     (set-cdr! prior '())
	     (loop next (cdr next))))])]))

(define f 0)
(define np 'non-initialized)
(define sp 0)

;; These are used by compile-simulate-nought.  They are the helper
;; functions used by the generated code.  Right now I'm actually just
;; defining these functions globally so that I may test them.  They
;; make liberal use of our three primary global variables (object-graph, all-objs).
(define generic-defs	
  `(
    ;; Is set to a list of all the leds that are toggled on.    
    [define led-toggle-state '()]
    [define local-sent-messages 0] ;; We should stick this in the simobject...
    [define local-recv-messages 0]
;; Getting rid of this: moving it into the simobject data structure..
;    [define token-cache (make-default-hash-table)]
    
    ;; MAKE SURE NOT TO INCLUDE OURSELVES:
    [define neighbors (lambda (obj)
			(let ((entry (assq obj object-graph)))
;			  (disp "ENTRY : " entry)
			  (if (null? entry)
			      (error 'neighbors "generated code.. .cannot find obj in graph: ~s ~n ~s"
				     obj object-graph)
			      (begin 
				(if (memq obj (cdr entry))
				    (error 'neighbors "we're in our own neighbors list"))
				(cdr entry)))))]

    [define sendmsg (lambda (data ob)
;		      (disp "IN sendmsg")(disp "  this = " this)
;	   (disp (list 'sendmsg data (node-id (simobject-node ob))))
		   (set-simobject-incoming! ob
		    (cons data (simobject-incoming ob)))
		   ;(set-simobject-redraw! ob #t)
		   )]

    [define (sim-emit t m count)
           
      (DEBUGMODE 
       (if (eq? t SPECIAL_RETURN_TOKEN)
	   (error 'sim-emit "should never be emitting a return token!: ~n~s ~n~s ~n~s~n" t m count)))
      
      ;; Count messages at this node
      (set! local-sent-messages (add1 local-sent-messages))
      ;; Count total messages sent.
      (incr-top-level! 'total-messages)
					;		    (newline)(disp "  " (list 'sim-emit t m count))
      
      (let ([ourentry   (construct-msg-object t (cpu-time) this #f count m)]
	    [childentry (construct-msg-object t (cpu-time) this this (add1 count) m)])
	;; Is it fair for emitting to overwrite our cache entry?
	;; I need to do it so that the return-handler can figure things out.
	(hashtab-set! (simobject-token-cache this) t ourentry) ;; OVERWRITE CURRENT ENTRY
	(for-each (lambda (nd) (sendmsg childentry nd))
		  (neighbors this)))

      (with-logger
       (critical-section 
	(printf "----------------------------------------~n")
	(printf "INCOMING: ~n")
	(print-incoming)
	(printf "----------------------------------------~n")))
      ]

    [define (sim-relay . tok)
      (DEBUGMODE 
       (if (eq? (msg-object-token this-message) SPECIAL_RETURN_TOKEN)
	   (error 'sim-relay "should never be relaying a return token!")))
      
      (if (not (null? tok))
	  (error 'relay "Can't handle optional argument yet"))
      ,(DEBUGMODE '(if (not this-message) 
		       (error 'inside-node-prog "this-message was #f")))
      (if (not (msg-object-parent this-message))
	  (error 'simulator_nought.relay 
		 "inside simulator. can't relay a message that was~a"
			" sent by a local 'call' rather than an 'emit'"))
      
      (set! local-sent-messages (add1 local-sent-messages))
      (incr-top-level! 'total-messages)
      (let* ([ourentry   this-message]
	     [relayentry (construct-msg-object 
			  (msg-object-token ourentry)
			  (cpu-time)
			  (msg-object-origin ourentry)
			  this 
			  (+ 1 (msg-object-count ourentry))
			  (msg-object-args ourentry))])
	(for-each (lambda (nd) (sendmsg relayentry nd))
		  (neighbors this)))
      ]

	   ;; These should be macros but now I'm cheesily hacking
	   ;; these to work automagically and instantly:
    [define (sim-flood t . m)
      (DEBUGMODE 
       (if (eq? t SPECIAL_RETURN_TOKEN)
	   (error 'sim-flood "should never be flooding a return token!:")))

	     ;; FOR NOW THIS TELEPORTS THE MESSAGE EVERYWHERE IN THE NETWORK.
					;		    (disp (list "FLOODING" t m))
	     (let ((msg (if (null? m) '() (car m))))
	       (for-each (lambda (nd) (sendmsg (construct-msg-object t (cpu-time) this this 0 m) nd))
			 all-objs))]

	   ;; This is just a cludge for now.
    [define (sim-elect-leader t)
      (DEBUGMODE 
       (if (eq? t SPECIAL_RETURN_TOKEN)
	   (error 'sim-elect-leader "should never be electing a return token!:")))
      
      (DEBUGPRINT (disp "sim-elect-leader!!" t))
      
      (set-simobject-homepage! this (cons (list 'inside t) (simobject-homepage this)))
      (let ((possible
	     (filter (lambda (simob)
		       (member (list 'inside t) (simobject-homepage simob)))
		     all-objs)))
	(DEBUGPRINT
	 (disp "election: electing from" (node-id (simobject-node this))
	       " number candidaties " (length possible)))
	;; This is horribly yucky.
	;; Proceed if there are any candidates...a
					;	       (if (not (null? possible))
	(let ([leader (list-get-random possible)])
	  (DEBUGPRINT
	   (disp "election: got leader" (node-id (simobject-node leader))))
	  ;; Tell the message it's leader.  This might happen
	  ;; at multiple nodes.  Need to make sure there's a
	  ;; dependency relationship here and that a killing a
	  ;; false leader kills all its subsequent doings.
	  (sendmsg (construct-msg-object t (cpu-time) #f #f 0 '()) leader)))]
    

    [define (sim-light-up r g b)
      ((sim-debug-logger) "~n~a: light-up ~a ~a ~a"
       (node-id (simobject-node this)) r g b)
      (if (simobject-gobj this)
	  (change-color! (simobject-gobj this) (rgb r g b))
	  ;; We're allowing light-up of undrawn objects atm:
	   ;(error 'sim-light-up "can't change color on undrawn object!: ~s" this)
	  )]

    ;; INCOMPLETE (we don't yet draw the leds directly.)
    [define (sim-leds what which)
      (let* ([colors 
	      (case which
		[(red)   '(255 0 0)]
		[(green) '(0 255 0)]
		[(blue)  '(0 0 255)]
		[else (error 'sim-leds "bad color: ~a" which)])]
	     ;; INCOMPLETE:
;	     [oldcolors '(0 0 0)]
	     )
	(let ((string (format "~a: (time ~s) (Leds: ~a ~a ~a)~n" 	
	 (node-id (simobject-node this)) (cpu-time) which what
	 (case what
	   [(on) 
	    (set! led-toggle-state (list->set (cons which led-toggle-state)))
	    (apply sim-light-up colors)
	    "" ]
	  [(off)
	   (set! led-toggle-state (remq which led-toggle-state))
	   (sim-light-up '(0 0 0))
	   "" ]
	  [(toggle)
	   (if (memq which led-toggle-state)
	       (begin 
		 (set! led-toggle-state (remq which led-toggle-state))
		 (sim-light-up 0 0 0)
		 "off")
	       (begin 
		 (set! led-toggle-state (list->set (cons which led-toggle-state)))
		 (apply sim-light-up colors)
		 "on")
	       )]
	  [else (error 'sim-leds "bad action: ~a" what)]))))
	  ;((sim-debug-logger) string)
	  (logger string)
	 
	)]
	  
    [define (sim-dist . tok)
	     (if (null? tok)
		 (begin 
		   (if (msg-object-count this-message)
		       (msg-object-count this-message)
		       (error 'simulator_nought.process-statement:dist
			      "inside simulator (dist) is broken!")))
		 (let ((entry (hashtab-get (simobject-token-cache this) (car tok))))
		   (if (and entry (msg-object-count entry))
		       (msg-object-count this-message)
		       (error 'simulator_nought.process-statement:dist
			      "inside simulator (dist ~s) but ~s has not been received!"
			      (car tok) (car tok))
		       )))]
	   
    [define (sim-loc) ;; Return this nodes location.
	     (node-pos (simobject-node this))]
    [define (sim-locdiff a b)
	     (sqrt (+ (expt (- (car a) (car b)) 2)
		      (expt (- (cadr a) (cadr b)) 2)))]

    ;; <TODO> FINISH!
    ;; How much should the return-system use the token system
    ;; vs. bypass it?
    ;; It needs to set a timer wait that long for child
    ;; values, then push on up.

    ;; [2004.07.08] OK. Handle-return does all the real work.  This
    ;; merely needs to launch a message, and the system will handle
    ;; it.
    [define (sim-return retval totok via seed aggr)
	     ,(DEBUGMODE '(if (not this-message)
			      (error 'simulator_nought.sim-return
				     "broken")))
	     ,(DEBUGMODE '(if (not (and (token? totok) (token? via)))
			      (error 'simulator_nought.sim-return
				     "bad token or via: ~s ~s" totok via)))
	     (let ([themessage
		    (make-return-obj
		     (list (cpu-time)) ;; timestamps
		     this     ;; parent -- This is sketchy!
		     (list 0) ;; counts
		     (list retval) ;; ret_vals
		     totok ;; to_tok
		     via ;; via_tok
		     seed ;; seed_val
		     aggr ;; aggr_tok
		     (list (node-id (simobject-node this))) ;; senders
		     )])
	       ;; We drop that into the local hopper, and the postal system will handle it!
	       (set-simobject-incoming! 
		this
		(cons themessage (simobject-incoming this))))]
					
    ;; [2004.06.28] Return messages are special, they're handled
    ;; somewhat "under the table" from the normal token system.
    ;; Rather than a single timestamp, they store a list of
    ;; time-stamps, marking all handlers.  This takes a batch of
    ;; return messages, processes them, and sends them onwards.
    ;; DEPENDS: Uses sendmsg to change the global object-graph.
    ;; DEPENDS: Also uses "handler", and "this".
    [define handle-returns 
	    (lambda (returns)
	      ;(display #\H)(flush-output-port)
	      (with-logger
	       (crit-printf "~s: (time ~s) (HandlingReturns ~s)" 
			    (node-id (simobject-node this)) 
			    (cpu-time)
			    (returns)))

	      (DEBUGMODE (if (not (pair? returns))
			     (error 'handle-returns "must take a nonempty list: ~s" returns))
			 (if (not (andmap return-obj? returns))
			     (error 'handle-returns "must take return-objs only: ~s" returns))
			 )

;	      (disp "Handle Ret at " (node-id (simobject-node this))
;		    " : totok" 
;		    (map cadr (map msg-object-args returns))
;		    "via"
;		    (map caddr (map msg-object-args returns))
;		    "senders" (map last (map msg-object-args returns)))

	      ;; Divide the returns into related sets based on which token they belong to.
	      (let ([channels (partition-equal returns
			        (lambda (ob1 ob2)
				  ;; This compares the "to" portions of the arglists:
				  ;; It's going to be hard to remember to update this!
				  ;; So this piece of code could be a source of errors:
				  (eq? (return-obj-to_tok ob1)
				       (return-obj-to_tok ob2)) ))])
		;; That divided them up into related clumps, now process each:
		(for-each 
		 (lambda (returns) ;; Each chunk of returns going to the same target.		   		   

		   ;; TEMP		   
;		   (if (> (length returns) 1)
;		       (disp "   GOT WIDER RETURN-CHANNEL BATCH:" (length returns)))

		   (let (
			 ;; Some timestamps may be false:
			 ;; This had better be a list of lists of ints.
			 [timestamps (apply append (map (lambda (x) (if x x '()))
					  (map return-obj-timestamps returns)))]
			 [counts (apply append (map return-obj-counts returns))]
			 )
		    
		     (DEBUGMODE 
		      (if (not (andmap integer? timestamps))
			  (error 'handle-returns "these are invalid timestamps: ~s" timestamps))
			  )

		     (let ([vals* (map return-obj-return_vals returns)]
			   [tos (map return-obj-to_tok returns)]
			   [vias (map return-obj-via_tok returns)]
			   [seeds (map return-obj-seed_val returns)]
			   [aggrs (map return-obj-aggr_tok returns)]
			   [senders* (map return-obj-senders returns)])
		       (DEBUGMODE
			;; These had better have homogenous seeds and aggregators!!!
			;; (Although theoretically, this restriction could be lifted.)
			(if (not (and (apply myequal? aggrs)
				      (apply myequal? seeds)
				      (apply myequal? vias)
				      (apply myequal? tos)
				      ;; All those vals are lists..
				      (andmap list? vals*)))
			    (error 'simulator_nought:handle-returns
				   "not all these return's args ok: ~n~s"
				   returns-args)))
		       ;; Once those are all verified to be the same, we select one of each:
		       (let ([the_totok  (car tos)]
			     [via  (car vias)]
			     [seed (car seeds)]
			     [aggr (car aggrs)]
			     [senders (apply append senders*)])
			 ;; Now we've verified homogeneity among the batch 
			 ;; and know the parameters for this return.
			 
    		      ;; Only call this if aggr is true!
			 (letrec ([collapse (lambda (vals timestamps)
;					      (disp "COLLAPSING: " vals)
					      (DEBUGMODE
					       (if (not aggr) (error 'handle-return:collapse "no aggregator.")))
					      (let loop ([acc seed] [vals vals] [timestamps timestamps])
						(if (null? vals) acc
						    (loop (handler (bare-msg-object aggr (list (car vals) acc)))
							  (cdr vals)
							  (cdr timestamps)))))])
			   (DEBUGMODE (if (and (not seed) aggr)
					  (error 'handle-return
						 "There is an aggregator ~s but no seed!"
						 aggr)))

			   (let* ([vals (begin (DEBUGASSERT 
						(or (andmap list? vals*)
						    (error 'assert "hmm vals not lists: ~s" vals)))
					     (apply append vals*))]
				  [aggregated-values			  
				   (if aggr 
				       (list (collapse vals timestamps))
				       ;; If there is no aggregator, then we just 
				       ;; accumulate *all* those values together!
				       vals)])
			     
;			   (disp "AGGREGATED:"  (node-id (simobject-node this)) aggregated-values)

			     ;; Now that the message is aggregated, we check to 
			     ;; see if this node is the destination..			    
			     ;; Try to look up the via token in the local cache.
			     (let ([via_parent
				    (let ([entry (hashtab-get (simobject-token-cache this) via)])
				      (if entry 
					  ;; Target is the parent of the via token
					  (msg-object-parent entry)
					  (error 'simulator_nought:handle-returns
						 "Should not happen! Could not get entry for via token! (at node ~s): ~s in ~n~s"
						 (node-id (simobject-node this)) via
						 (simobject-token-cache this)
						 )
					  ))])
			       ;; If via_parent is #f, that means that *THIS* is the parent.
			       (if (not via_parent)
				   ;; So we must fire the *to* token.  We fire it once for each return val actually.
				   (begin
'				     (disp "RETURN accomplished at " (node-id (simobject-node this)) 
					   "#vals: "  (length aggregated-values)
					   "to " the_totok "via " via)
				   ;; FIXME: TEMP: TODO:
				     ;; This is just for debugging, adding senders to homepage
				     (DEBUGMODE
				      (let ((temp (assq 'return-senders-list  (simobject-homepage this))))
					(if temp
					    (set-car! (cdr temp) (cons senders (cadr temp)))
					    (set-simobject-homepage! 
					     this (cons (list 'return-senders-list
							      (list senders))
							(simobject-homepage this))))))
				     
				     (for-each
				      (lambda (retval)
					,(build-call 'the_totok '(retval)))
				      ;; Hmm, this should make sure it's one:
				      aggregated-values)
				     ) ;; End base-case (reached destination) block
				   
				 ;; Otherwise we must pass this return onto the next:
				   (begin 
				     (display #\^)(flush-output-port)
				     (sendmsg  
				      (make-return-obj
				       (cons (cpu-time) timestamps) ;; Timestamps
				       this ;; Parent
				       (map add1 counts) ;; Count - all the leaf counts
				       ;; And here we build the arguments for this leg of the return journey:
				        aggregated-values
					the_totok via seed aggr 
					(cons (node-id (simobject-node this)) senders))
				      via_parent)) ;; End sendmsg 
				 ))))))))
		 channels) ;; End for-each
		))] ;; End handle-returns   
    )) ;; END Generic-defs


;; Takes: a program in the language of pass10_deglobalize
;; Returns: a list (socprog nodeprog) with both programs as SEXPS.
(define (compile-simulate-nought prog)
  ;; Accept either with or without the language wrapper:
  (let ((prog (match prog 
		     [(program ,_ ...) prog]
		     [(,input-lang '(program ,stuff ...)) `(program ,stuff ...)])))
    (match prog
      [(program (bindings ,nodebinds ...)
		(socpgm (bindings ,socbinds ...) ,socstmts ...)
		(nodepgm (tokens ,nodetoks ...) (startup ,starttoks ...)))
       (let* (
       
       [socprog
	 `(lambda (soc-return soc-finished SOC-processor this object-graph all-objs)
;	    (printf "CALLING SocProg: ~s~n" this)
	    (let ([I-am-SOC #t]) 
	      ;; We have to duplicate the tokbinds here...
	      ,(process-binds 
		nodebinds
		(process-binds 
		 socbinds
		(process-tokbinds
		 nodetoks generic-defs			
		 `(begin ,@(map (process-statement nodetoks)
				socstmts)
			 'soc_finished))))))]
              
       [nodeprog
	`(lambda (soc-return soc-finished SOC-processor this object-graph all-objs)
;	    (printf (format "CALLING Nodeprog: ~s~n" (if (simobject-gobj this) #t #f)))

	   ;; This is a little weird, but even the node program
	   ;; running on the SOC has to know that it's physically on
	   ;; the SOC:
	    (let ([I-am-SOC (eq? this SOC-processor)]
		  [local-sense (lambda ()
				 ((current-sense-function)
				  (node-pos (simobject-node this))))])

	      ,(process-binds 
		nodebinds 
		(process-tokbinds 
		 nodetoks generic-defs
		 `(begin 
		    ;; <TODO>: FIX THIS UP, MAKE SURE NO ARGS IS OK!?:
		    ;; Call all the starting tokens with no arguments:
		   ,@(map list starttoks)
		   ;; [2004.06.28] The simulator is asynchronous, yet
		   ;; communication should be vaguely synchronized by
		   ;; the fact that this loop handles a batch of
		   ;; incoming messages at a time before yielding.
		   ;; Hopefully "return" messages from all the
		   ;; children will make their way into a single one
		   ;; of these batches.  We'll see.
		   (let main-node-loop ([incoming (simobject-incoming this)]
					[returns-next-handled (+ (cpu-time) return-window-size)]
					[returns '()])
		     (if (not (andmap return-obj? returns))
			 (error 'main-node-loop "Hmm, got a non return-obj: ~s" returns))
			 

;		     (if (> (length returns) 0)
;			 (disp "loop" (cpu-time) returns-next-handled (length returns)))

		     (let ([current-time (cpu-time)]
			   [triggers (simobject-timed-tokens this)])
		       (let ([fired-triggers 
			      (filter (lambda (trigger) (>= current-time (car trigger)))
				      triggers)])
			 ;; This is a little weird because it gives
			 ;; timed calls priority over other incoming
			 ;; tokens:
			 (if (not (null? fired-triggers))
			     (begin 
			       (critical-section
				(set-simobject-incoming! this
			         (append (map cdr fired-triggers)
					 (simobject-incoming this))))
			       (set-simobject-timed-tokens! this
					 (difference triggers fired-triggers))
			       (main-node-loop (simobject-incoming this)
					       returns-next-handled
					       returns))
		     (cond
		      [stop-nodes 
		       (display "*") ;(disp "Node stopped by external force!")
		       'node-stopped]
		      
		      ;; If the time has come, handle those returns!
		      [(>= current-time returns-next-handled)
		       ;; This progress indicator goes to stdout regardless of where the logger goes.
		       (if (> (length returns) 1)
			   (display #\!) 
			   (display #\.))
		       (flush-output-port)
		       (if (not (null? returns))
			   (with-logger
			    (display (list "Handling rets from" (node-id (simobject-node this)) "got " (length returns)))
			    (newline)))

		       ;; Handle all the returns to date (either
		       ;; locally generated or from neighbors). 
		       (if (not (null? returns))
			   (handle-returns returns))
		       ;(yield-thread)
		       ;; Should I give us a time bonus for the time
		       ;; we spent *handling* the returns?  Or try to
		       ;; hold us to a fixed frequency?
		       (main-node-loop incoming
				       (+ returns-next-handled return-window-size)
				       '())]

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
       (set! f socprog)
       (set! sp socprog)
       (set! np nodeprog)
       ;; DANGEROUS:
       (list socprog nodeprog))]))) ;; End compile-simulate-nought

;; [2004.07.02] Modifying this to write the progs to files and load
;; from there.  This way PLT scheme should have better debugging
;; information.


;; build-simulation ::
;; [socprog, nodeprog] ->  
;;  (soc-return -> soc-finish -> (vector thunk (thunks)))
;; Takes programs in sexp form and returns a simulation object.
(define (build-simulation progs)
  (let ([socprog (car progs)]
	[nodeprog (cadr progs)]
	[socnode (car all-objs)])
    ;; Gotta turn off the limits on pretty printing first:
    (parameterize ([print-level #f]
		   [print-length #f]
		   )
      (with-output-to-file "_SIM_socprog.ss" 
	(lambda ()
	  (pretty-print `(define SIM-socfun ,socprog))) ;; Hope the depth doesn't overflow
	'replace)
      (with-output-to-file "_SIM_nodeprog.ss" 
	(lambda ()
	  (pretty-print `(define SIM-nodefun ,nodeprog))) ;; Hope the depth doesn't overflow
	'replace)
      ;; NOTE.  We will have the problem that when we load this stuff,
      ;; dynamically defined stuff like soc-return isn't bound yet.
      (define-top-level-value 'soc-return 'love-yeah)

      (load "_SIM_socprog.ss") ;; defines socfun
      (load "_SIM_nodeprog.ss") ;; defines nodefun
      )
    ;; Now had better capture those global vars quick!
    ;; (Rentry into build-simulation could kill them.)
    ;;   ALSO, we capture the global object-graph and all-obj variables
    ;; here, *at build time*.
    ;;   THUS, the resulting pack-o-thunks that build-simulation returns
    ;; shouldn't have any awkward dependencies on global vars (hopefully).
    (let ([socfun (top-level-value 'SIM-socfun)] ;; Must indirect to the toplevel value because of PLT modules.
	  [nodefun (top-level-value 'SIM-nodefun)]
	  [our-object-graph object-graph]
	  [our-all-objs all-objs])
      (lambda (soc-return soc-finish)
	(vector 
	 (lambda () 
	   ;; GOTTA HAVE soc-return before doing this!
	   (socfun soc-return soc-finish socnode socnode our-object-graph our-all-objs))
	 (map (lambda (nd)
		(lambda ()
		  ;; GOTTA HAVE soc-return before doing this!
		  (nodefun soc-return soc-finish socnode nd our-object-graph our-all-objs)))
	      our-all-objs)))
      )))

;; This is the "language definition" which will use the compiler nd
;; simulator to evaluate the expression.  It'll be hard to write test
;; cases with meaningful results, though.
;; <TODO> FINISh
;(define (simulator-nought-language expr)
;  (void)

;;===============================================================================

;; [2004.06.16] This is the shared structure of both the text
;; simulator and the graphical one.  It serves the role of a parent
;; object from which run-simulation and graphical-simulation inherit.
;; ITS A MESS.  The two arguments to this function don't make any
;; sense, very weak abstraction.

;; [2004.06.17] - Modifying this so that it returns an actual stream...
;;  [- My first attempt didn't work because nested engines are dissallowed. -]

;; Takes: 
;;  threadrunner :: Thunks, ?TimeOut -> (All_Threads_Returned | Threads_Timed_Out)
;;  simcore :: ThunksVec ->? Timeout -> ThunksList
;;    simcore runs the core of the specialized simulator.
;; Returns: a simulator function of type:
;;    Simulation, ?timeout -> (Stream of ReturnVals ending in 'All_Threads_Returned | 'Threads_Timed_Out)
;; Where 
;;   Simulation = soc-return -> soc-finish -> (vector thunk (thunks))

;; That is, generate-simulator returns all values that were returned to the SOC via soc-return.a
(define (generate-simulator threadrunner simcore)
  (lambda (the-sim . timeout)
    (let ([return-vals '()])
      (let ([soc-return 
	     (lambda (x)
;	       (disp "SOCRETURN ACCUMULATING RETURN VALS:" x )
	       ;; Should get a LOCK here...
	       (critical-section
		(set! return-vals (cons x return-vals))))]
	    [soc-finish 
	     (lambda () (set-top-level-value! 'stop-nodes #t))])
	(let ([thunks (the-sim soc-return soc-finish)])

  (DEBUGMODE (if (not (and (vector? thunks) (= 2 (vector-length thunks))))
		 (error 'generate-simulator 
			"not a valid return value from simulation object, should be 2-elem vector: ~s"
			thunks)))
	    
    ;; First, set up a global counter for communication cost:
  ;; This is defined dynamically so as to avoid PLTs module system. 
  (define-top-level-value 'total-messages 0)
  ;; This one count all the messages that bounce because of
  ;; backpropogation prevention mechanisms:
  (define-top-level-value 'total-fizzles 0)
  ;; This is a global flag which can be toggled to shut down all the
  ;; running processors.
  (define-top-level-value 'stop-nodes #f)

  ;; This is a flag which is treated with various fluid-let's to
  ;; reveal whether a given token handler wishes to reject the
  ;; incoming token.  
  (define-top-level-value 'reject-incoming-token #f)
  
  ;;-------------------------------
  ;; We've done our job by initializing.  Apply the core function;
  ;; this sets up the simulation and gives us back new thunks:
  (let ([newthunks (apply simcore (cons thunks timeout))])
    ;; Next the threadrunner processes all these thunks and returns either an engine
    (let ([result (if (null? timeout)
		      (threadrunner newthunks)
		      (threadrunner newthunks (car timeout)))])
      
      ;; Now if the threads are done running, we just return the
      ;; existing values.  Otherwise we return a stream.
      (let promiseloop ([result result])
	(cond 
	 [(memq result '(All_Threads_Returned Threads_Timed_Out)) 
	  (reverse (cons result return-vals))]
	 
	 [(promise? result) ;; It's an engine.
	  ;; Do next chunk of computation:
	  (let ([next (force result)]
		[so-far #f])
	    ;; This needs to be threadsafe.
	    (critical-section 	  
	     (set! so-far (reverse return-vals)) ;; Gather the results of computation thusfar.
	     (set! return-vals '()))             ;; CLEAR buffer.
	    ;; Now return the appropriate stream.
	    (stream-append so-far (delay (promiseloop next))))]
	 
	 [else (error 'generate-simulator "invalid return value from threadrunner: ~a" result)]))

      )))))))



;; I'm essentially doing a lot of ad-hoc inheritance here.  There are
;; different simulators that all share some parts -- excuse the mess.
;; This basically adds on another engine that handles the text display.
;;    simcore :: ThunksVec ->? Timeout -> ThunksList
(define generic-text-simulator-core
   (lambda (thunks . timeout)
     (let ([soceng (vector-ref thunks 0)]
	   [nodeengs (vector-ref thunks 1)]
	   [display_engine
	    (lambda ()
	      (let loop ()
		(for-each (lambda (simob)
			    (let ((len (length (simobject-incoming simob))))
			      (cond 
			       [(< len 10) (display len)]
			       [else (display "#")])))
			  all-objs)
		(yield-thread)
		(loop)))])       
       ;; If we're in text mode use that simple display engine:
       (if (simulator-output-text)
	   (cons display_engine (cons soceng nodeengs))
	   (cons soceng nodeengs))
       )))

;; A simulation function of type:
;;    Simulation, ?timeout -> (Stream of ReturnVals ending in 'All_Threads_Returned | 'Threads_Timed_Out)
(define run-simulation 
  (generate-simulator 
   ;; Thread runner:
   run-flat-threads
   ;; Build engines and possibly display driver:
   generic-text-simulator-core))

;;===============================================================================
;; Some display and helper functions.

(define print-caches
  (lambda ()
    ;; NOTE - this breaks the abstraction for the hashtables:
      (for-each 
       (lambda (ob)
	 ;; 
	 (let* ([cache (simobject-token-cache ob)]
		[entries (apply append (vector->list cache))])
	   (printf "~a:  ~a~n"  
		   (node-id (simobject-node ob))
		   (map car entries))))
       (sort (lambda (x y)
	       (< (node-id (simobject-node x))
		  (node-id (simobject-node y))))
	     all-objs))))

(define print-incoming
  (lambda ()
    ;; NOTE - this breaks the abstraction for the hashtables:
      (for-each 
       (lambda (ob)
	 ;; 
	   (printf "~a: (time ~s) (Incoming ~a)~n"
		   (cpu-time)
		   (node-id (simobject-node ob))		   
		   (map (lambda (o)
			  (if (msg-object? o)
			      (msg-object-token o)
			      (list 'ret (return-obj-to_tok o))))
			(simobject-incoming ob))
		   ))
       (sort (lambda (x y)
	       (< (node-id (simobject-node x))
		  (node-id (simobject-node y))))
	     all-objs))))

;;===============================================================================

;; Include some example programs used by the tests.
(include "simulator_nought.examples.ss")

;; [2004.07.01] Here I include stub definitions for some of the
;; variables used by our simulated programs.  This way I can do
;; fluid-lets on them in the test-cases.
(define handler 'simulator-variable-not-bound-yet)


;; A single node called "thenode" (and 'this').
(define (one-node-scenario testcase)
  `(begin
	;; Have to give a pure
	(eval '(define handler 'initialized-by-test-case))
	(eval '(define this 'initialized-by-test-case))
	(for-each eval generic-defs)
	;; Set up a two-node graph:
	(let ([thenode (random-node)])
	  (let ([graph (list (list thenode))])
	    ;; This will set the object-graph and all-obj vars:
	    (with-graph graph
	      (lambda ()
		;; Bind the corresponding simobjects for nodes 'a' and 'b'
		(let ([thesimob (car all-objs)] )
		  (fluid-let ([this thenode])
		    ,@testcase))))))))

;; One named 'a', one 'b'
(define (two-node-scenario testcase)
  `(begin
     ;; defining stubs:
	(eval '(define handler 'initialized-by-test-case))
	(eval '(define this 'initialized-by-test-case))
	(eval '(define this-message 'initialized-by-test-case))
	(for-each eval generic-defs)
	;; Set up a two-node graph:
	(let ([a (random-node)]
	      [b (random-node)])
	  (let ([graph (list (list a b) (list b a))])
	    ;; This will set the object-graph and all-obj vars:
	    (with-graph graph
	      (lambda ()
		;; Bind the corresponding simobjects for nodes 'a' and 'b'
		(let ([a_simob (car (filter (lambda (so) (eq? a (simobject-node so))) all-objs))]
		      [b_simob (car (filter (lambda (so) (eq? b (simobject-node so))) all-objs))]
		      )
		  ,@testcase)))))))


(define these-tests
  `(
    ;; First we test some of the smaller procedures before running the simulator:
    ;; I make the assumption here that the function process-statement
    ;; can get by with an inconsistent tokbinds list for these simple
    ;; evaluations:
    [ ((process-statement '()) '(emit foo 2 3)) (sim-emit 'foo (list 2 3) 0)]

    [ ((process-statement '()) '(flood foo)) (sim-flood 'foo)]    

    ;; Making sure that it recurs on arguments to return, even though
    ;; this is not necessary for code generated by my compiler.
    [ " Test process-statement on a return."
     ((process-statement '()) '(return (dist)))
      (sim-return
       (sim-dist)
       (string->symbol
	(string-append
	 (symbol->string (msg-object-token this-message))
	 "_return"))
       (msg-object-token this-message)
       #f
       '#f)]    

    ["Now a return with all the condiments"
     ((process-statement '()) '(return (dist) (to aaa) (via bbb) (seed 0) (aggr fff)))
      (sim-return (sim-dist) 'aaa 'bbb 0 'fff)]
    ;; Just to make sure erros work with my unit-tester:

    [ ((process-statement '()) '(return))  error]

    [ (let ((x (make-simobject (make-node 34 '(1 2)) '() '() #f #f '() 
			       (make-default-hash-table) 0 0)))
	(let ((y (structure-copy x)))
	  (and (eq? (simobject-node x) 
                    (simobject-node y))
               (eq? (simobject-incoming x) 
                    (simobject-incoming y))
               (eq? (simobject-gobj x) 
                    (simobject-gobj y))
	       (not (eq? x y))))) #t]

    [ (free-vars '(cons (quote 30) x)) (x) ]

    ["process-statement: test nested calls"
     ((process-statement '()) '(call a (call b 933939)))
     ;; [2004.07.31] Updating this because I changed how calls work.
     (handler 
      (bare-msg-object 'a
      (list (handler (bare-msg-object 'b (list 933939))))))
;     ,(lambda (exp)	
;	(= 2 (length 
;	      (deep-all-matches 
;	       (lambda (x) (match x 
;				  [(let (,bind ...) ,bods ...) #t] 
;				  [,else #f])) 
;	       exp))))
     ]
    
    ;; This is an evil test which mutates the global environment!  (By
    ;; evaluating generic-defs.)  Yech!
    ;; This test goes to a lot of work to set up a stub world to get
    ;; handle-returns to run...
    [ "test handle-returns"
      ,(two-node-scenario
	`(
		  ;; The b_node has received a message from via.
		  (hashtab-set! (simobject-token-cache b_simob) 'via 
				(construct-msg-object 'via    ;; token
						 #f      ;; timestamp
						 a_simob ;; origin
						 a_simob ;; parent
						 1       ;; count
						 '()))
		  ;; The a_node sent the original message through 'via:
		  (hashtab-set! (simobject-token-cache a_simob) 'via 
				(construct-msg-object 'via    ;; token
						 #f      ;; timestamp
						 a_simob ;; origin
						 #f      ;; parent
						 0       ;; count
						 '()))
		  ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		  ;; We're trying to handle a return message.
		  (let ([this-message (make-return-obj
				       (list (cpu-time))
				       #f ;; parent
				       (list 0) ;; counts
				       '(3) ;; vals, a list..
				       'to  ;; token
				       'via ;; token
				       0 ;; seed
				       'plus ;; aggr token
				       '()   ;; senders list
				       )]
			[plus +] ;; Function for use by handler
;			[token-cache b_cache]
			)
		  ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		    (fluid-let ([this b_simob]) ;; We're seeing from the b-nodes perspective
		      ;; The handler for the 'b'-node thinks it got a token from 'a'
		      (fluid-let ([handler
				   ,(build-handler ;'()
				     '([plus (x y) 
					     (disp "ADDING:" x y)
					     (+ x y)]
;				       [via () (error 'tester-via-token 
;						      "this shouldnt be called")]
						   )
				       )])		       
			(handle-returns (list this-message))
;			(pretty-print all-objs)
;			(printf "==================================================~n")
;			(for-each pretty-print (map simobject-incoming all-objs))
			;; This should put one message in a's inbox:
			(simobject-incoming a_simob)
			)))))
	,(lambda (x) 
	   (and (list? x)
		(eq? (length x) 1);; the a_simob should have received a return message
		(return-obj? (car x))
		(equal? '(1) (return-obj-counts  (car x)))
		;; TODO: Could add some more criterion here.
		))]

    ;; <TODO> TOFINISH NOT DONE!
    ;; This should do a single aggregation.  I've removed comments
    ;; that are redundant with the above.
    [ "test handle-returns with aggregation"
      ,(two-node-scenario
	`(
		  (hashtab-set! (simobject-token-cache b_simob) 'via 
				;; token timestamp origin parent count args
				(construct-msg-object 'via #f a_simob a_simob 1 '()))
		  (hashtab-set! (simobject-token-cache a_simob) 'via 
				(construct-msg-object 'via #f a_simob #f 1 '()))

		  (let ([message1 (make-return-obj (list (cpu-time)) #f (list 0) 
				       '(3) 'to 'via 0 'plus '())]
			[message2 (make-return-obj (list (cpu-time)) #f (list 0) 
				       '(4) 'to 'via 0 'plus '())]
			[plus +])
		    (fluid-let ([this b_simob]) 
		      (fluid-let ([handler
				   ,(build-handler ;'()
				     '([plus (x y) 
					     (disp "ADDING:" x y)
					     (+ x y)]) )])
			(handle-returns (list message1 message2))
			(simobject-incoming a_simob)
			)))))
	,(lambda (x) 
;	   (disp "Checking aggregation, here's incoming, length " (length x))
;	   (disp "Here's return value:" (car (msg-object-args (car x))))
;	   (parameterize ([print-level 3])
	;		 (pretty-print x))
			 
	   (and (list? x)
		(eq? (length x) 1)
		(return-obj? (car x))
		(equal? '(7) (return-obj-return_vals (car x)))
		(equal? '(1 1) (return-obj-counts  (car x)))
		))]

    ;; Generic tests for both this and the graphical sim:

    ,@(include "simulator_nought.tests")

  [ "Compile Flood lights program..."
     (build-simulation (compile-simulate-nought ',example-nodal-prog1))
     unspecified ]

  [ "Run Flood lights program..."
    (run-simulation
     (build-simulation (compile-simulate-nought ',example-nodal-prog1))
     2.0)
    unspecified ]

;    [ (let ((s (open-output-string)))
;	(parameterize ([current-output-port s])
;	   (list 
;	    (run-simulation (vector (make-engine (lambda () 3))
;				    (list (make-engine (lambda () 4))
;					  (make-engine (lambda () 5))))
;			    .5)
;	    (get-output-string s))))	   
;	Simulation_Done]


    ;; [2004.07.29]
  ["Test timed-call"
   (run-simulation
    (build-simulation (compile-simulate-nought ',example-nodal-prog6))
    2.0)
   unspecified]

;; [2004.10.01] This isn't passing right now on my laptop but is at home!!!! <FIXME>
;; TODO TODO FIXME
#;  [ "Return all distances from root, no aggregation"
    (run-simulation
     (build-simulation 
      (compile-simulate-nought 
       ',example-nodal-prog4))
     1.7)
    ,(lambda (ls)
       ;; THis is a list of distances.
       (or (andmap integer? ls) (error 'test "not all ints!"))

       (or #t ;; TEMPORARILY DISABLING... DON'T UNDERSTAND... FIXME
	   (= (length ls) (sub1 (length all-objs)))
	   (error 'test "Not right length!!"))
       ;; Make sure the first half sums to larger than the second
       ;; half... this is a rough way of checking that it's
       ;; increasing.
       (or
	(let ([half (quotient (length ls) 2)])
	  (let ([front (list-head ls half)]
		[back  (list-tail ls half)])
	    (< (apply + front)
	       (apply + back))))
	(error 'test "not increasing return dists!"))
       )]

  [ "Return all distances from root, WITH aggregation"
    (run-simulation
     (build-simulation 
      (compile-simulate-nought 
       ',example-nodal-prog4b))
     1.7)
    ,(lambda (ls)
       (disp "Returned All DISTS: " ls)
       ;; The last element will be the All_Threads_Returned symbol..
       (andmap integer? (rdc ls))
      ; (and (= (length ls) (sub1 (length all-objs))))
       )]

  ["Spread a global tree to all nodes, and make sure that it reaches all nodes that are connected to the SOC."    
   (run-simulation
    (build-simulation 
     (compile-simulate-nought 
      '(program
	(bindings )
	(socpgm (bindings ) (emit tok1))
	(nodepgm (tokens
		  [tok1 () (relay)])
		 (startup ))) ))
    1.7)
    ,(lambda (ignored)
       ;; Look at the global state, make sure everybody connected has got exactly one token:
       ;; The first member of all-objs better be the soc-node!
       (let* ([socnode (car all-objs)]
	      [connected (all-connected socnode)])
	 (andmap (lambda (simob)		  
		   (hashtab-get (simobject-token-cache simob) 'tok1))
		 connected)))]
 ["Test the logger."
  ;; Make a log message from every node.  
  ;; Make sure we get the right number.
  (let ([count 0])
    (parameterize ([sim-debug-logger (lambda args 
				       (critical-section
					(printf "~nLogger called")
					(set! count (+ 1 count))))])
      (run-simulation
       (build-simulation 
	(compile-simulate-nought 
	 '(program
	   (bindings )
	   (socpgm (bindings ))
	   (nodepgm (tokens [tok1 () ((sim-debug-logger) "foo")])
		    (startup tok1)))))
       1.7))
    count)
  ,(lambda (result)
    (if (= result (length all-objs))
	#t
	(begin (printf "Got ~a messages, but there are ~a nodes!"
		       result (length all-objs))
	       #f)))]

;; COMMENTING FOR NOW
;;  ["Test program that anchors, circles, maps and folds"
;   (run-simulation
;    (build-simulation (compile-simulate-nought ',THEPROG))
;    10.0)
;   unspecified]

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
      (init-world)
      (apply tester args))))

(define testsim test-this)
(define testssim these-tests)

;; TEMPORARY!! <TODO> REMOVE THIS 
;; This is just for testing...
;;(for-each eval generic-defs)
;; Wait, I actually like putting them in a file more, that way we get
;; more debugging info:
;; WARNING: Be careful that this doesn't max out the pretty-print depth and give us "..."
(define load-generic-defs
  ;; For PLT this load has to dynamically occur after the require has completed.
  (let ([dump_file
	 (delay
	   (begin (let ((f (open-output-file "_SIM_generic-defs.ss" 'replace)))
		    (fprintf f ";; Autogenerated file.  Contains defs for simulator code.~n~n")
		    (for-each (lambda (def) (pretty-print def f) (newline f))
			      generic-defs)
		    (close-output-port f))))])
	(lambda ()
	  (force dump_file)
	  (load "simulator-generic-defs.ss"))))

;; FIXME: TODO: TEMPORARY -- this dumps the test-case I'm working on to a file:
(with-output-to-file "rrn-current-test.ss"  
  (lambda ()  (pretty-print (cadadr these-tests)))
  'replace)
      
;;===============================================================================
;; JUNK 

(define csn  compile-simulate-nought)

(define temprog
  '(program
    (socpgm (bindings) (emit tok1))
    (nodepgm
;       result_2
       (bindings)
       (tokens
	[tok1 () (flood tok2)]
	[tok2 () (light-up 255 0 100)])
       ()
       )))


(define stuff
'(begin  
  (cleanse-world)
  (run-simulation
   (lambda (sr sf)
     (vector (lambda () 3) (list (lambda () 4) (lambda () 5))))
     2)))

;(define x (cadr (list-ref these-tests 25)))

