
;; TODO: is_scheduled etc...

;; TODO: REHIDE the scheduler state if we don't need it!!

;; simulator_alpha.ss
;;  -Ryan Newton [2005.02.25]
;; Related files include:
;;   alpha_lib.ss -- "run time" library for the running simulator.
;;   alpha_lib_scheduler_simple.ss -- Basic action scheduler
;;   alpha_lib_scheduler.ss -- NOT USED right now [2005.09.27]
;;   simulator_nought.examples.ss -- Some sample programs.
;===============================================================================

;; This will be a second attempt simulator.  (First was simulator_nought.)

;; It will support only core tml (no gradients).

;; It will have a single thread of control and a queue of simulator
;; events sorted by virtual clock times.

;; Later, it may serve as a place to test scheduling algorithms so
;; that we may actually implement the atomic action model.


;; NOTE: Right now all calls go through the dyndispatch table.

(define this-unit-description 
  "simulator_alpha.ss: event-queue simulator for nodal language")

;===============================================================================
;; Changes:

;===============================================================================

;; This structure contains all the global data needed a simulation.
(define-structure (simworld graph object-graph all-objs obj-hash scheduler-queue))
;; obj-hash maps node-ids onto simobjects

;; [2005.03.13]  Adding this to represent events-to-happen in the simulator.
(define-structure (simevt vtime msgobj))

;; [2005.05.06]
;; A first class representation of tokens:
(define-structure (simtok name subid))
;; TODO: Change the system to use these ^^


;; This structure contains everything an executing token handler needs
;; to know about the local node.  "this" is a simobject.  tokstore is
;; a struct containing all the stored values.

;  [2005.03.05] Putting everything in simobject, "this" provides everything.
;(define-structure (localinfo this I-am-SOC tokstore))

;; Positions are just 2-element lists.
(define-structure (node id pos))

;; This structure represents a simulated node:
;; Incoming is a list of token messages.
;; Redraw is a boolean indicating whether the object needs be redrawn.
;; [2004.06.11] Added homepage just for my internal hackery.
;; [2004.06.13] Be careful to change "cleanse-world" if you change
;;   this, we don't want multiple simulation to be thrashing eachother.
;; [2004.07.08] I don't know why I didn't do this, but I'm storing the
;;   token-cache in the structure too
(define-structure (simobject node I-am-SOC
			     token-store ;; Changing this to hash table mapping names to 

			     ;; All these buffers get changed when a token handler runs:
			     incoming-msg-buf ;; Stores simulation events
			     local-msg-buf    ;; Stores simulation events
			     outgoing-msg-buf ;; Stores simulation events
			     timed-token-buf  ;; Stores simulation events

			     local-sent-messages local-recv-messages
			     redraw gobj homepage 

			     ;; This is a function that processes incoming messages
			     scheduler ;; and returns simulation actions.
			     ;; Not used in the simple scheduler as of [2005.09.27]

			     ;; This function takes msg-obj and vtime and executes a token handler:
			     meta-handler
			     
			     worldptr ;; A pointer to the relevent simworld object.
			     ))
;; The token store is a hash table mapping simtok objects to token objects.
;; The token objects themselves are just vectors of stored variables.
;; By convention, the first slot of the token object is a counter for how many times the 
;; handler has been invoked.


;; This structure represents a message transmitted across a channel.
;; None of these should be mutated:
(define-structure (msg-object token ;; This is a simtok object.  Used to just be a symbol (name).
			      sent-time ;; when it was sent --This is currently mutated within the scheduler [2005.09.27]
			      parent ;; :: simobject - who I got it from
			      to   ;; :: nodeid - who its going to, #f for broadcast
			      args))

;; ======================================================================

;; This is our logger for events in the simulator:
(define logger
  (lambda input
  (mvlet ([(level ob args)
	   (match input
	     [(,lvl ,str ,args ...)
	      (guard (number? lvl) (string? str))
	      (values lvl str args)]
	     [(,str ,args ...)	      
	      (values 1 str args)])])

  (define (column-width w ob)
    (let ((s (format "~a" ob)))
      (if (< (string-length s) w)
	  (string-append s (make-string (- w (string-length s)) #\space))
	  s)))

  (define (print-header)
    (fprintf (simulation-logger) "~a{~a} " 
	     (column-width 4
	     (if (simulation-logger-count)
		 (begin (simulation-logger-count (+ 1 (simulation-logger-count)))
			(- (simulation-logger-count) 1))
		 "foo"))
	     level))

  (if (and (simulation-logger)
	   (<= level (simulation-logger-level)))
      (if (null? args)
	  ;(critical-section
	   (begin (print-header)
		  (display ob (simulation-logger))
		  (newline (simulation-logger)))
           (begin ;critical-section
	   (print-header)
	   (display (apply format ob args) (simulation-logger))))))))

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
    (begin ;critical-section
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
(define (safe-construct-msg-object token timestamp parent args)
  ;(unless (token-name? token) (error 'safe-construct-msg-object "bad token name: ~s" token))
  (unless (simtok? token) (error 'safe-construct-msg-object "bad token: ~s" token)) ;
  (unless (or (number? timestamp) (not timestamp))
	  (error 'safe-construct-msg-object "bad timestamp: ~s" timestamp))
  (unless (list? args)
	  (error 'safe-construct-msg-object "bad args: ~s" args))
  (make-msg-object token timestamp #f parent args))

;; [2004.06.28] This is a helper to construct the locally used
;; messages that don't have a parent, timestamp, etc.
(define (bare-msg-object rator rands . time)  
  (safe-construct-msg-object rator ;; token
		   (if (null? time) #f (car time))    ;; timestamp
		   #f    ;; parent
		   rands))

;; Here's a helper to check the invariants on a msg-object
#|(define (valid-msg-object? mo)
  (and (msg-object? mo)
       (let ([token  (msg-object-token  mo)]
	     [timestamp (msg-object-timestamp mo)]
	     [origin (msg-object-origin mo)]
	     [parent (msg-object-from mo)]
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
	     (and (token-name? token)
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

;; This is done safely so that it cannot conflict with the BASE_ID or NULL_ID.
(define (random-node) 
  (make-node 
   (let loop ((id (reg:random-int 1000)))
     (if (or (eq? id BASE_ID) 
	     (eq? id NULL_ID))
	 (loop (reg:random-int 1000))
	 id))
   (list (reg:random-int world-xbound)
	 (reg:random-int world-ybound))
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
  (graph-get-connected-component simob (simworld-object-graph sim)))

;; Returns a simworld object.
(define (fresh-simulation)  
  ;; This subroutine generates the default, random topology: 
  ;; (There are more topologies in "network_topologies.ss"
  (define (make-object-graph g world) 
    (graph-map (lambda (nd) 
		 (let ([so (apply make-simobject (make-list 15 'simobject-field-uninitialized))])
		   (set-simobject-node! so nd)
		   (set-simobject-token-store! so (make-default-hash-table))

		   (set-simobject-incoming-msg-buf! so '())
		   (set-simobject-outgoing-msg-buf! so '())
		   (set-simobject-local-msg-buf! so '())
		   (set-simobject-timed-token-buf! so '())

		   (set-simobject-local-sent-messages! so 0)
		   (set-simobject-local-recv-messages! so 0)

		   (set-simobject-redraw! so #f)
		   (set-simobject-gobj! so #f)
		   (set-simobject-homepage! so '())
		   (set-simobject-I-am-SOC! so #f)

		   (set-simobject-scheduler! so #f)
		   (set-simobject-meta-handler! so #f)

		   (set-simobject-worldptr! so world)
		   so)) g))

   (let* ([theworld (make-simworld #f #f #f #f #f)]
	  [graph 
          (let ((seed (map (lambda (_) (random-node)) (iota (simalpha-num-nodes)))))
            ;; TEMP: Here I give the nodes distinct, consecutive ids.
	    ;; They should have randomized ids.
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
;	  [soc (caar graph)]
          [obgraph (make-object-graph graph theworld)]
          [allobs  (map car obgraph)]
	  [hash
	   (let ([h (make-default-hash-table)])
	     (for-each (lambda (ob)
			 (hashtab-set! h (node-id (simobject-node ob)) ob))
		       allobs)
	     h)]
	  [scheduler-queue '()])

     ;; Set I-am-SOC
     (for-each (lambda (ob)
		 (set-simobject-I-am-SOC! ob		  
		  (= (node-id (simobject-node ob)) BASE_ID)))
	       allobs)
     (set-simworld-graph! theworld graph)
     (set-simworld-object-graph! theworld obgraph)
     (set-simworld-all-objs! theworld allobs)
     (set-simworld-obj-hash! theworld hash)
     (set-simworld-scheduler-queue! theworld scheduler-queue)
     theworld))

                         
;; ======================================================================

;; Subroutine of compile-simulate-alpha below
(define (process-statement current-handler-name tokbinds stored)
  
  (let ([allstored (apply append (map cadr stored))])
    (letrec ([find-which-stored
	      (lambda (v)
		(let loop ([ls stored])
		  (let ((pos (list-find-position v (cadr (car ls)))))
		    (if pos (values (caar ls) pos)
			(loop (cdr ls))))))]
    
	     [process-expr 
	      (lambda (expr)
		;; This is a little wider than the allowable grammar to allow
		;; me to do test cases:
		(match expr		       
		  [(quote ,const) `(quote ,const)]
		  [,num (guard (number? num)) num]
		  ;; Token references return pairs which index the token store hash table.
		  [(tok ,t ,[e]) `(make-simtok ',t ,e)]

		  ;; This is sketchy: we just fail silently and return #f if there is no token there.
		  ;; FIXME TODO: We should seriously have some sort of better error handling convention.
		  [(ext-ref (tok ,tokname ,subtok) ,x)
		   (guard (and (symbol? x) (memq x allstored)))
		   ;; The stored names should be unique at this point!  So this should be ok:
		   (mvlet ([(which-tok pos) (find-which-stored x)])
			  (DEBUGMODE
			  (if (not (eq? which-tok tokname))
			      (error 'simulator_alpha:process-statement 
				     "bad ext-ref: (ext-ref (~a . ~a) ~a)" tokname subtok x)))
			  `(let ([exttokobj (hashtab-get the-store 
							 (make-simtok ',tokname ,subtok))])
			     "FOOBAR"
			     ,(format "Ext-ref of (tok ~a ~a) variable ~a" tokname subtok x)
			     (if exttokobj
				 (vector-ref exttokobj ,(+ 1 pos))
				 #f)))]
		  [(ext-set! (tok ,tokname ,subtok) ,x ,[e])
		   (guard (and (symbol? x) (memq x allstored)))		   
		   (mvlet ([(which-tok pos) (find-which-stored x)])
			  (DEBUGMODE
			   (if (not (eq? which-tok tokname))
			       (error 'simulator_alpha:process-statement 
				      "bad ext-set to: (ext-ref (~a . ~a) ~a)" tokname subtok x)))
			  `(let ([exttokobj (hashtab-get the-store (make-simtok ',tokname ,subtok))])
			     ,(format "Ext-set! of (tok ~a ~a) variable ~a" tokname subtok x)
			     (if exttokobj
				 (vector-set! exttokobj ,(+ 1 pos) ,e)
				 (warning 'ext-set! "var ~a: token not present: ~a" ',x `(,tokname . subtok))
				 )))]

		  ;; Local tokstore-reference:
		  [,x (guard (and (symbol? x) (memq x allstored)))
                    (mvlet ([(which-tok pos) (find-which-stored x)])
                           (if (not (eq? which-tok current-handler-name))
                               (error 'simulator_alpha:process-statement 
				      "bad local stored-ref: ~a actually belongs to ~a not ~a" 
				      x which-tok current-handler-name))
                           ;; 'tokobj' is already bound to our token object
			   `(begin 
			     ,(format "Local Stored Ref of variable: ~a" x)
			     "We add one to the position because zeroth is the invocation counter."
			     (vector-ref tokobj ,(+ 1 pos))))]

		  [,x (guard (or (symbol? x) (constant? x))) x]
		  [(quote ,x) `(quote ,x)]

		  [(begin) '(void)]
		  [(begin ,[x] ...) `(begin ,x ...)]
		  
		  ;; TODO: Implement subcall directly:
		  [(subcall ,[rator] ,[rand*] ...)		  
		   ;; Call the handler directly
		   `(begin "Simulator subcall code" 
			   ((simobject-meta-handler this)
			    (bare-msg-object ,rator
					     (list ,@rand*) current-vtime)
			    current-vtime))]

		  ;; NOTE! These rands ARE NOT simple.
		  [(call ,[rator] ,[rand*] ...)
                     `(set-simobject-local-msg-buf! this
                           (cons (make-simevt #f ;; No scheduled time, ASAP
                                              (bare-msg-object ,rator
							       (list ,@rand*) current-vtime))
                                 (simobject-local-msg-buf this)))]
		  
		  [(bcast ,[rator] ,[rand*] ...)
		   `(set-simobject-outgoing-msg-buf! this
  		      (cons (make-simevt #f ;; No scheduled time, ASAP
				       (bare-msg-object ,rator (list ,@rand*) current-vtime))
			    (simobject-outgoing-msg-buf this)))]

;; These are desugared before now.
;		  [(activate ,rator ,rand* ...)
;		   (build-activate-call `(quote ,rator) rand*)]

		  [(timed-call ,delay ,[rator] ,[rand*] ...)
		   ;; Delay is in milleseconds.
		  `(set-simobject-timed-token-buf!
		    this (cons (make-simevt (+ ,delay current-vtime)
					    (bare-msg-object ,rator (list ,@rand*) current-vtime))
			       (simobject-timed-token-buf this)))]

		  ;; TODO
		  [(token-scheduled? ,[tok]) ;; INEFFICIENT
		   ;; queue is list containing (simevt . simob) pairs.
		   ;; We go through the current queue looking for a scheduled event that matches this token.
		   ;; NOTE: This is the queue of *all* events, we also must make sure it happens on this *node*.
		   `(begin		      
		      '(disp "SCANNING QUEUE: " (length (simworld-scheduler-queue (simobject-worldptr this)))
			    "and locals: "     (length (simobject-local-msg-buf this))
			    (map msg-object-token 
				 (map simevt-msgobj
				      (simobject-local-msg-buf this))))

		      (or ;;; First, check the schedulers queue:
		       ;; (TODO FIXME, CHECK: THIS MIGHT NOT EVEN BE NECESSARY:)
		       (let loop ((queue (simworld-scheduler-queue (simobject-worldptr this))))
			 (if (null? queue) #f 
			     (let ((simtok (msg-object-token (simevt-msgobj (caar queue)))))
			       (or (and (eq? this (cdar queue)) ;; Is it an event on this node.
					(equal? ,tok simtok)
					(begin 	       
					  (if (regiment-verbose)
					  (DEBUGMODE (printf "Wow! we actually found the answer to ~a\n"
							     "token-scheduled? in the scheduler-queue!")))
					  #t)
					) ;; If so is it the token in question?
				   (loop (cdr queue))))))
		       ;; Second, check the local msg buf:
		       (let loop ((locals (simobject-local-msg-buf this)))
			 (if (null? locals) #f
			     (let ((simtok (msg-object-token (simevt-msgobj (car locals)))))
			       (or (equal? ,tok simtok)
				   (loop (cdr locals)))))))
		       )]
		   
		  ;; is_scheduled TODO TODO
		  ;; deschedule   TODO TODO
		  ;(token-scheduled? (Token) Bool)
		  ;(token-present? (Token) Bool)
		  ;(evict (Token) Void)

		  ;; If it's in the hash table, it's present:
		  ;; This is static wrt to token name for the moment:
		  [(token-present? (tok ,t ,n)) (guard (number? n)) 
		   `(if (hashtab-get the-store (make-simtok ',t ,n)) #t #f)]
		  [(token-present? (tok ,t ,[e])) 
		   `(if (hashtab-get the-store (make-simtok ',t ,e)) #t #f)]

		  [(evict (tok ,t ,[e])) `(hashtab-remove! the-store (make-simtok ',t ,e))]

		  ;; Local Stored variable mutation:
		  [(set! ,v ,[rhs]) (guard (memq v allstored))
		   (mvlet ([(which-tok pos) (find-which-stored v)])
			  (DEBUGMODE
			   (if (not (eq? which-tok current-handler-name))
			       (error 'simulator_alpha:process-statement 
				      "(set!) bad local stored-ref: ~a actually belongs to ~a not ~a" 
				      v which-tok current-handler-name)))
			  `(begin 
			     ,(format "Local Stored Set! of variable: ~a" v)
			     (vector-set! tokobj ,(+ 1 pos) ,rhs)))]

		  [(set! ,v ,[rhs])  `(set! ,v ,rhs)]

		  [(if ,[test] ,[conseq] ,[altern])
		   `(if ,test ,conseq ,altern)]
		  [(my-id) '(node-id (simobject-node this))]
		  [(my-clock) 
		   'current-vtime]

		  ;; [2005.05.07] Shouldn't run into this now:
#;		  [(soc-return ,x)
		   (process-expr `(return ,x 
					  (to (tok SOC-return-handler 0) )
					  (via (tok global-tree 0))
					  (seed '#f)
					  (aggr #f)))]
;		   (process-expr `(call (tok SOC-return-handler 0) ,x))]
		 
		  ;[(soc-return-finished ,x) 		  

		[(token->subid ,[e]) `(simtok-subid ,e)]

		  [(loc) '(sim-loc)]
		  [(locdiff ,[l1] ,[l2]) `(sim-locdiff ,l1 ,l2)]

		  [(light-up ,r ,g ,b) `(sim-light-up ,r ,g ,b)]
		  [(leds ,which ,what) `(sim-leds ',which ',what)]
		  [(dbg (quote ,str) ,[args] ...)
		   ;; TODO FIX ME: would be nice to print properly
		   (let ([massage-str 
			  (lambda (s)
			    (let ((newstr (string-copy s)))
			      (let loop ((i (- (string-length s) 2)))
				(cond
				 [(< i 0) (void)]
				 [(and (eq? (string-ref s i) #\%)
				       (eq? (string-ref s (add1 i)) #\d))
				  (string-set! newstr i #\~)
				  (string-set! newstr (add1 i) #\a)
				  (loop (sub1 i))]
;				 [(and (eq? (string-ref s i) #\\)
;				       (eq? (string-ref s (add1 i)) #\n))
;				  (string-set! newstr i #\~)
;				  (loop (sub1 i))]
				 [else (loop (sub1 i))]))
			      newstr))])
;		     (disp "MANGLED" (massage-str str))
		     `(if (simalpha-dbg-on)
			  (begin (display (format ,(massage-str str) ,@args) (current-error-port))
				 (newline (current-error-port)))))]
		  [(,prim ,[rand*] ...)
		   (guard (or (token-machine-primitive? prim)
			      (basic-primitive? prim)))
		   `(,prim ,rand* ...)]
		  ;; We're being REAL lenient in what we allow in token machines being simulated:
		  [(let ((,lhs ,[rhs]) ...) ,[bods] ...)
		   `(let ((,lhs ,rhs)  ...) ,bods ...)]
		  ;; We're letting them get away with other primitives because
		  ;; we're being lenient, as mentioned above.
		  [(app ,[rator] ,[rand*] ...)
		   `(,rator ,rand* ...)]

		  ;; Supporting the output of cps-tokmac also:
		  [(kcall ,[rator]  ,[rand*] ...)
		   `(,rator ,rand* ...)]
		  [(lambda (,v) ,[bod]) `(lambda (,v) ,bod)]

		  [(,rator ,[rand*] ...)
		   ;; This is an arbitrary scheme application. Where would these come from?
		   ;; Don't want to be too lenient tho:
		   (guard (not (token-machine-primitive? rator))
			  (not (memq rator '(emit bcast call timed-call activate relay return))))
		   (warning 'simulator_alpha.process-expr
			    "arbitrary rator applied: ~a" rator)
		   `(,(process-expr rator) ,rand* ...)]
		  
		  [,otherwise (error 'simulator_alpha.process-expr 
				"don't know what to do with this: ~s" otherwise)])
	   )])
    (lambda (stmt) (process-expr stmt)))))


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
		       (error 'simulator_alpha:process-binds
			      "no entry for sym '~s' in constant binds ~s" sym binds))))
	       flat)])
    (if (cyclic? graph)
	(error 'process-binds "for now can't take a cyclic graph: ~s" newbinds))
    newbinds))
	 




;; Every token handler, once converted for the simulator, has a signature: 

;;   simobject, vtime, subtoken-index -> args -> ()      
;;   (mutates tokstore, outgoing-msg-buf, local-msg-buf, timed-tokens)

;; The result of running a handler is to mutate certain fields of the
;; simobject.  The handler *does not* directly interface with the
;; scheduler.  It merely changes the token store and accumulates local,
;; remote, and timed messages.


;; [2005.03.28]  Moved destructure-tokbind to helpers.ss


;; Takes token bindings, returns compiled bindings
;; along with an association list of stored-vars.
(define (process-tokbinds tbinds)
  (let* ([allstored
          (map (lambda (bind)
		 (mvlet ([(tok id args stored bindings body) (destructure-tokbind bind)])
			(list tok (map car stored))))
               tbinds)]
			 
         [binds 
          (map
           (lambda (tbind)
	     (mvlet ([(tok id args stored bindings body) (destructure-tokbind tbind)])
		      `[,tok 
			(lambda (current-vtime ,id . vals) ;world)
			  (let ,(map list args (make-list (length args) ''sim-alpha-uninitialized))

			    (let ([numvals (length vals)])

 			      (if (< numvals ,(length args))
				  (if (simalpha-padding-warning)
				      (warning 'simulator-alpha "executing ~a padding args ~a with zero." 
					       ',tok (list-tail ',args numvals))))
 			      (if (> numvals ,(length args))
 				  (error 'simulator-alpha "executing ~a, got excess vals ~a for args ~a"					 
 					   ',tok vals ',args))
; 			      (begin 
 				,@(map (lambda (arg)
 					 `(if (null? vals)
 					      (set! ,arg 0)
 					      (begin (set! ,arg (car vals))
 						     (set! vals (cdr vals)))))
 				       args)
				"Done initializing arguments."



;			  (lambda args			 
			    (let* ([the-store (simobject-token-store this)]
				   [simtok-obj (make-simtok ',tok ,id)]
				   [old-outgoing (simobject-outgoing-msg-buf this)]
				   [old-local    (simobject-local-msg-buf this)])
			      (DEBUGMODE 
			       ;; Check invariants on the store:
			       (check-store the-store))

			      "Is there already an allocated token object?:"
			      ;; Requires equal? based hash table:
			      (let ([tokobj (hashtab-get the-store simtok-obj)])
				(if (not tokobj)				   
				    (begin "If not, then we allocate that token object..."
					   " setting the invoke counter to zero."
					   (set! tokobj (vector 0 ,@(map cadr stored)))
					   (hashtab-set! the-store simtok-obj tokobj)))
				(set-simobject-outgoing-msg-buf! this '())
				(set-simobject-local-msg-buf! this '())
				;; Timed-token-buf need not be reversed, because it is ordered by vtime.
				,((process-statement tok tbinds allstored) body)
				;; We must reverse the outgoing order because of how they were added:
				(set-simobject-outgoing-msg-buf! this 
	   		  	  (append (reverse (simobject-outgoing-msg-buf this)) old-outgoing))
				(set-simobject-local-msg-buf! this 
                                  (append (simobject-local-msg-buf this) ;(reverse (simobject-local-msg-buf this)) 
					  old-local))
				(void))))))
                         ]))
	  tbinds)])
    (printf "~n;  Converted program for Simulator:~n")
    (printf "Allstored was:~n") (pretty-print allstored)
    (printf "<-------------------------------------------------------------------->~n")
    ;(pretty-print binds)

    (values binds allstored)))

;; ======================================================================

(define (compile-simulate-alpha prog)
  ;; Accept either with or without the language wrapper:
  (let ((prog (match prog 
		     [(tokens ,t ...) `(program (bindings) (nodepgm (tokens ,t ...)))]
		     [(program ,_ ...) prog]
		     [(,input-lang '(program ,stuff ...)) `(program ,stuff ...)])))
    (match prog
      [(program (bindings ,nodebinds ...)
		(nodepgm (tokens ,nodetoks ...) ;(startup ,starttoks ...)
			 ))
       ;; Here we hack in an extra handler for doing SOC-return's...
       (set! nodetoks
	     (cons `[SOC-return-handler subtokid (socrethandlerval) (stored)
		       (if (= ',BASE_ID (my-id))
			   (simulator-soc-return socrethandlerval)				  
			   (error 'SOC-return-handler
				  "ran on non-base node! id: ~a"
				  (my-id)))]
		   nodetoks))

       ;; Here we mutate the node-start to also call SOC-start.
       (printf "TOKS: ~s\n" (map car nodetoks))
       (set! nodetoks
	     (cons 
	      (match (assq 'node-start nodetoks)
		     [(node-start ,id () (stored ,s ...) ,body)
		      ;; CALL SOC START EXPLICITELY FROM NODE_START: (AFTER NODE-START!)
		      `(node-start ,id () (stored ,@s) 
			 (begin ,body
			    (if (= (my-id) ',BASE_ID) (call (tok SOC-start 0))  (void))))]
		     [,other (error 'compile-simulate-alpha "bad node-start! ~s" other)])
	      (alist-remove 'node-start nodetoks)))
       
       (mvlet ([(tbinds allstored) (process-tokbinds nodetoks)])

	      ;; Here we hack an extra handler into tbinds:
; 	      (set! tbinds
; 		    (cons `[SOC-return-handler
; 			    (lambda (current-vtime subtok_ind x)
; 			      (if (eq? ,BASE_ID (node-id (simobject-node this)))
; 				  (simulator-soc-return x)				  
; 				  (error 'SOC-return-handler
; 					 "ran on non-base node! id: ~a"
; 					 (node-id (simobject-node this)))))]
; 			  tbinds))

	`(define (node-code this)
	   ;; Now we have subtoks:
;	   (define-structure (tokstore ,@(apply append (map cadr allstored))))
	   ;; Need to update the sensing machinery...
	   (let ([local-sense (lambda ()
				((current-sense-function)
				 (node-pos (simobject-node this))))])
	      (let* ,(process-binds nodebinds)
		(letrec ,tbinds		  
		 ;; Within this body, toks are bound, we return a list of start actions
		  ;"Initialize our simobject message buffers"

		  ;; This is the table through which dynamic token references can be invoked:
		  (let ([dyndispatch_table (make-default-hash-table)])
		    (begin (void) ,@(map (lambda (tokname)
					   `(hashtab-set! dyndispatch_table ',tokname ,tokname))
					 (map car tbinds)))
		    
		    ;; Return the real meta-handler
		     (lambda (msgob current-vtime)
		       (mvlet ([(name subtok)
				(let ((tok (msg-object-token msgob)))
				  (values (simtok-name tok)
					  (simtok-subid tok)))])
			      (let ([handler (hashtab-get dyndispatch_table name)])
				
;				(fprintf (current-error-port) "Dyndispatch: ~a in table: " name)
;				(hashtab-for-each (lambda (name _) (fprintf (current-error-port) "~a " name)) dyndispatch_table)
;				(newline (current-error-port))

				(if (not handler)
				    (error 'node-code
					   "dyndispatch: no handler for token name: ~a in table: ~n~a" name dyndispatch_table))
				;; Invoke:
				(apply handler current-vtime subtok 
				       (msg-object-args msgob))
				;; That returns nothing but fills up the simobjects buffers.
				)))
		 ))))))]
      [,otherwise (error 'compile-simulate-alpha
			 "unmatched input program: ~a" prog)])))


;; ======================================================================

;; This requires pass21_cleanup-token-machine.ss as well as helpers.ss
;; This handles writing the generated code to a file and loading it back.
;; FLAGS:
;; 'numnodes int -- Set the number of nodes in the world to int.
;; 'outport prt  -- Set the printed output of the simulation to port prt.
;; 'srand int    -- Seed the random number generator with int.
(define run-simulator-alpha
  (letrec ([run-alpha-loop
	    (lambda args
	      (match args
		     ;; This is a weak requirement... 
		     ;; We consider the first arg to represent a token machine if it is a *list*.
		     [(,tm . ,rest) (guard (list? tm))
		      (match tm
			;; Already compiled
			[(define (node-code this) ,e)
			 (let ((out (open-output-file "_genned_node_code.ss" 'replace)))			      
			   (parameterize ([print-level #f]
					  [pretty-maximum-lines #f]
					  [print-graph #t])
   		              (write tm out);(pretty-print tm out)
			      (newline out)
			      (close-output-port out)))
			 (read-params rest)]
			;; Otherwise compile it
			[,tm			     
			 (let ((cleaned tm )) ;;;(cleanup-token-machine tm)))
			   (let ([comped (compile-simulate-alpha cleaned)])
			     (let ((out (open-output-file "_genned_node_code.ss" 'replace)))
;			    (printf "Ouputting token machine to file: _genned_node_code.ss~n")
			    (parameterize ([print-level #f]
					   [pretty-maximum-lines #f]
					   [print-graph #t])				    					  
			    (pretty-print comped out)
			    (newline out)
			    (newline out)
			    (display ";; Above is compiled program for this token machine: " out)
			    (newline out)
			    (display "'" out)
			    (pretty-print tm out)
			    (newline out))
			    (close-output-port out))
			  (read-params rest)
			  ))])]
		     [(,rest ...) (read-params rest)]
		     ))]
	    [read-params
	     (lambda (params)	       
	       (match params
;		      [,x (guard (disp "read params" params) #f) 3]
		      [() 
		       (load "_genned_node_code.ss")
                       ;; We have to do this because of the module system:
                       (let ((node-code (eval 'node-code)))
                         ;(disp "NODE CODE:" node-code) ;" eq: " (eq? node-code (eval 'node-code)))
                         ;(printf "Node code loaded from file.~n")
                         ;(if (not node-code)  (error 'run-simulator-alpha "node-code not defined!"))
                         (start-alpha-sim node-code 10.0 'simple))]
		      [(numnodes ,n . ,rest)
		       (if (not (integer? n))
			   (error 'run-simulator-alpha
				  "'numnodes switch should be followed by an integer, not: ~a" n))
		       (parameterize ([simalpha-num-nodes n])
				     (read-params rest))]
		      [(outport ,p . ,rest)
		       (if (not (output-port? p))
			   (error 'run-simulator-alpha
				  "'outport switch should be followed by a port object, not: ~a" n))
		       (parameterize ([simalpha-output-port p])
				     (read-params rest))]
		      [(srand ,n . ,rest)
;		       (if (not (integer? n))
;			   (error 'run-simulator-alpha
;				  "'srand switch should be followed by an integer, not: ~a" n))
		       (let ([stored-state #f])
			 (dynamic-wind
			     (lambda () (set! stored-state (reg:get-random-state)))
			     (lambda () (read-params rest))
			     (lambda () (reg:set-random-state! stored-state))))
		       ]))])
	   run-alpha-loop))

;; ======================================================================


(define these-tests
  `(
    [3 3] ;; UNIT TESTER BROKEN ATM...
    
#|
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


    [(compile-simulate-alpha
      '(program
	(bindings)
	(nodepgm 
	 (tokens
	  [SOC-start () (stored) (void)]
	  [node-start () (stored) (call tok1)]
	  [tok1 () (stored) (bcast tok2 '3)]
	  [tok2 (x) (stored [x '99])
		(begin (set! x (+ x '1))
		       (bcast tok2 x))])
	 ;(startup)
	 )))
     '??]


    [(compile-simulate-alpha
      '(program
	(bindings)
	(nodepgm 
	 (tokens
	  [SOC-start () (stored) (display "S")]
	  [node-start () (stored) ];(begin (printf "N~a" (simobject-I-am-SOC this)) (call tok1))]
	  [tok1 () (stored) (begin (display ".") (bcast tok2 " "))]
	  [tok2 (x) (stored) (display x)]
	 ))))
     '??]
|#
   

))

'    (compile-simulate-alpha 
     '(cleanup-token-machine-lang (quote (program (bindings) (nodepgm (tokens (node-start subtok_ind () (stored) (void)) (SOC-start subtok_ind () (stored) (call (tok tok1 0) (begin #0="This whole block represents the allocation of a continuation closure:" (let ((kind_4 (if (token-present? (tok K_3 0)) (let ((new (+ (quote 1) (ext-ref (tok K_3 . #1=(0)) kcounter)))) (begin (ext-set! (tok K_3 . #2=(0)) kcounter new) new)) (begin #3="Allocate this zeroeth token object just to hold a counter MEMORY WASTEFUL!:" (call (tok K_3 0) (quote 11) (void)) (quote 1))))) (begin #4="Do the actual token object (closure) allocation.  Capture freevars:" (call (tok K_3 kind_4) (quote 11)) #5="Return the name of this continuation object:" (tok K_3 kind_4)))) (quote 4))) (K_3 subtok_ind (flag fv0) (stored (kcounter . #6=(0))) (if (= flag (quote 11)) (if #7=(= subtok_ind (quote 0)) #8=(void) (begin)) (begin (call (tok tok1 0) (begin #0# (let ((kind_2 (if (token-present? (tok K_1 0)) (let ((new (+ (quote 1) (ext-ref (tok K_1 . #1#) kcounter)))) (begin (ext-set! (tok K_1 . #2#) kcounter new) new)) (begin #3# (call (tok K_1 0) (quote 11) (void)) (quote 1))))) (begin #4# (call (tok K_1 kind_2) (quote 11) fv0) #5# (tok K_1 kind_2)))) (quote 3)) (evict (tok K_3 . #9=(subtok_ind)))))) (K_1 subtok_ind (flag fv0) (stored (kcounter . #6#) (HOLE_59 (quote 0))) (if (= flag (quote 11)) (if #7# #8# (begin (set! HOLE_59 fv0))) (begin (printf (quote "result ~a") (+ HOLE_59 fv0)) (evict (tok K_1 . #9#))))) (tok1 subtok_ind (k_58 x) (stored) (call k_58 (quote 99) (+ x (quote 1000))))))))))


(define testsalpha (map (lambda (test)
			  (match test
				 [(,prog ,res) `(,prog ,res)] ;`((begin (cleanse-world) ,prog) ,res)]
				 [(,name ,prog ,res) 
				  `(,name ,prog ;(begin (cleanse-world) ,prog)
					  ,res)]))
			these-tests))

;; Use the default unit tester from helpers.ss:
;; But this also makes sure the world is initialized before doing unit tests:
(define test-this
  (let ((tester (default-unit-tester 
		  this-unit-description 
		  ;; Make sure that the world is clean before each test.
		  testsalpha
		  tester-eq?
		  id ;wrap-def-simulate
		  )))
    (lambda args
      (apply tester args))))

(define testalpha test-this)

(define csa compile-simulate-alpha) ;; shorthand
