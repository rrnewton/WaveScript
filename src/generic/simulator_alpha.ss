
;; TO: is_scheduled etc...


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

;===============================================================================

;; This structure contains all the global data needed a simulation.
(define-structure (simworld graph object-graph all-objs obj-hash))
;; obj-hash maps node-ids onto simobjects

;; [2005.03.13]  Adding this to represent events-to-happen in the simulator.
(define-structure (simevt vtime duration msgobj))

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
;; this, we don't want multiple simulation to be thrashing eachother.
;; [2004.07.08] I don't know why I didn't do this, but I'm storing the
;; token-cache in the structure too
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
			     ;; This function takes msg-obj and vtime and executes a tokhand:
			     meta-handler
			     ))
;; The token store is a hash table mapping token names, like (Red . 34), to token objects.
;; The token objects themselves are just vectors of stored variables.
;; By convention, the first slot of the token object is a counter for how many times the 
;; handler has been invoked.


;; This structure represents a message transmitted across a channel.
;; None of these should be mutated:
(define-structure (msg-object token 
			      sent-time ;; when it was sent 
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
  (unless (token-name? token) (error 'safe-construct-msg-object "bad token name: ~s" token))
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
   (let loop ((id (random 1000)))
     (if (or (eq? id BASE_ID) 
	     (eq? id NULL_ID))
	 (loop (random 1000))
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
  (graph-get-connected-component simob (simworld-object-graph sim)))

;; This generates the default, random topology: 
;; (There are more topologies in "network_topologies.ss"
(define (make-object-graph g) 
  (graph-map (lambda (nd) 
	       (let ([so (apply make-simobject (make-list 14 'simobject-field-uninitialized))])
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

		 so)) g))

(define (fresh-simulation)
   (let* ([graph 
          (let ((seed (map (lambda (_) (random-node)) (iota numsimnodes))))
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
;	  [soc (caar graph)]
          [obgraph (make-object-graph graph)]
          [allobs  (map car obgraph)]
	  [hash
	   (let ([h (make-default-hash-table)])
	     (for-each (lambda (ob)
			 (hashtab-set! h (node-id (simobject-node ob)) ob))
		       allobs)
	     h)])

     ;; Set I-am-SOC
     (for-each (lambda (ob)
		 (set-simobject-I-am-SOC! ob		  
		  (= (node-id (simobject-node ob)) BASE_ID)))
	       allobs)
     
     (make-simworld graph obgraph allobs hash)))


                         
;; ======================================================================

;; This makes up a vtime for the duration of a given handler body.
;; Right now I use a VERY unrealistic approximation.  I make up random costs for different constructs.
(define (compute-handler-duration expr)
  (match expr
	 [(ext-ref '(,tokname . ,subtok) ,x) 1]
	 ;; ext-set!
	 [,x (guard (symbol? x)) 0]
	 [(quote ,x) 0]
	 ;; Lenient...
	 [,v (guard (or (number? v) (string? v))) 0]

	 [(call ,rator ,[rand*] ...) (+ 2 (apply + rand*))]
	 [(bcast ,rator ,[rand*] ...) (+ 2 (apply + rand*))]
;		  [(activate ,rator ,rand* ...)
	 [(timed-call ,delay ,rator ,[rand*] ...) (+ 2 (apply + rand*))]
	 ;; is_scheduled
	 ;; 
	 ;; is_
	 ;; evict
	 [(set! ,v ,[rhs]) (+ 1 rhs)]
	 [(begin ,[exps] ...) (apply + exps)]
	 [(if ,[test] ,[conseq] ,[altern]) (+ test (max conseq altern))]
	 [(my-id) 1]
	 [(loc) 1]
	 [(locdiff ,[l1] ,[l2]) (+ 1 l1 l2)]
	 [(light-up ,r ,g ,b) 1]
	 [(leds ,which ,what) 1]
	 [(dbg ,str ,[args] ...) 0]
	 [(,prim ,[rand*] ...)
	  (guard (or (token-machine-primitive? prim)
		     (basic-primitive? prim)))
	  (+ 1 (apply + rand*))]
	 [(let ((,lhs ,[rhs]) ...) ,[bods] ...)
	  (+ (apply + rhs) (apply + bods))]
	 ;; Lenient: this lets other prims through:
	 [(,[rator] ,[rand*] ...) (apply + 1 rator rand*)]
	 [,otherwise (error 'simulator_alpha:compute-handler-duration
			    "don't know what to do with this: ~s" otherwise)]))


(define (process-statement current-handler-name tokbinds stored cost-table)
  (disp "PROCSTATMENT: COSTTABLE" (map car cost-table))
  
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
		  [(tok ,t ,[e]) `(cons ',t ,e)]

		  ;; This is sketchy: we just fail silently and return #f if there is no token there.
		  ;; TODO: We should seriously have some sort of better error handling convention.
		  [(ext-ref (tok ,tokname ,subtok) ,x)
		   (guard (and (symbol? x) (memq x allstored)))
		   (mvlet ([(which-tok pos) (find-which-stored x)])
			  (if (not (eq? which-tok tokname))
			      (error 'simulator_alpha:process-statement 
				     "bad ext-ref: (ext-ref (~a . ~a) ~a)" tokname subtok x))
			  `(let ([exttokobj (hashtab-get the-store 
							 ,(if (number? subtok)
							      `'(,tokname . ,subtok)
							      `(cons ',tokname ,subtok)))])
			     (if exttokobj
				 (vector-ref exttokobj ,(+ 1 pos))
				 #f)))]
		  [(ext-set! (tok ,tokname ,subtok) ,x ,[e])
		   (guard (and (symbol? x) (memq x allstored)))		   
		   (mvlet ([(which-tok pos) (find-which-stored x)])
			  (if (not (eq? which-tok tokname))
			      (error 'simulator_alpha:process-statement 
				     "bad ext-ref: (ext-ref (~a . ~a) ~a)" tokname subtok x))
			  `(let ([exttokobj (hashtab-get the-store '(,tokname . ,subtok))])
			     (if exttokobj
				 (vector-set! exttokobj ,(+ 1 pos) ,e)
				 (error 'ext-set! "token not present: ~a" `(,tokname . subtok)))))]

		  ;; Local tokstore-reference:
		  [,x (guard (and (symbol? x) (memq x allstored)))
		      (disp "Local TokStore reference:" x)
                    (mvlet ([(which-tok pos) (find-which-stored x)])
                           (if (not (eq? which-tok current-handler-name))
                               (error 'simulator_alpha:process-statement 
				      "bad local stored-ref: ~a actually belongs to ~a not ~a" 
				      x which-tok current-handler-name))
                           ;; 'tokobj' is already bound to our token object
                           `(vector-ref tokobj ,(+ 1 pos)))]

		  [,x (guard (or (symbol? x) (constant? x))) x]
		  [(quote ,x) `(quote ,x)]

		  ;; NOTE! These rands ARE NOT simple.
		  [(call ,rator ,[rand*] ...)
                   (let ((tok (assq (token->name rator) cost-table)))
                     (if (not tok)
                         (error 'compile-simulate-alpha "No cost-table entry for token: ~a name: ~a" 
                                rator (token->name rator)))
                     `(set-simobject-local-msg-buf! this
                           (cons (make-simevt #f ;; No scheduled time, ASAP
                                              ,(cadr tok) ;; Time cost
                                              (bare-msg-object ',(token->name rator) 
							       (list ,@rand*) current-vtime))
                                 (simobject-local-msg-buf this))))]

		  [(bcast ,rator ,[rand*] ...)
		   `(set-simobject-outgoing-msg-buf! this
  		      (cons (make-simevt #f ;; No scheduled time, ASAP
				       ,(cadr (assq (token->name rator) cost-table)) ;; Time cost
				       (bare-msg-object ',(token->name rator) (list ,@rand*) current-vtime))
			    (simobject-local-msg-buf this)))]

;;TODO:
;		  [(activate ,rator ,rand* ...)
;		   (build-activate-call `(quote ,rator) rand*)]

		  [(timed-call ,delay ,rator ,[rand*] ...)
		   ;; Delay is in milleseconds.
		  `(set-simobject-timed-token-buf!
		    this (cons (make-simevt (+ ,delay current-vtime)
					    ,(cadr (assq (token->name rator) cost-table)) ;; Time cost
					    (bare-msg-object ',rator (list ,@rand*) current-vtime))
			       (simobject-timed-token-buf this)))]


		  ;; is_scheduled TODO TODO
		  ;; deschedule   TODO TODO

		  ;; If it's in the hash table, it's present:
		  ;; This is static wrt to token name for the moment:
		  [(token-present? (tok ,t ,n)) (guard (number? n)) `(hashtab-get the-store (cons ',t ,n))]
		  [(token-present? (tok ,t ,[e])) `(hashtab-get the-store (cons ',t ,e))]

		  [(evict (tok ,t ,[e])) `(hashtab-remove! the-store (cons ',t ,e))]

		  [(set! ,v ,[rhs]) (guard (memq v allstored))
		   (mvlet ([(which-tok pos) (find-which-stored v)])
			  (if (not (eq? which-tok current-handler-name))
			      (error 'simulator_alpha:process-statement 
				     "(set!) bad local stored-ref: ~a actually belongs to ~a not ~a" 
				     v which-tok current-handler-name))
                          `(vector-set! tokobj ,(+ 1 pos) ,rhs))]

		  [(set! ,v ,[rhs])  `(set! ,v ,rhs)]

		  [(if ,[test] ,[conseq] ,[altern])
		   `(if ,test ,conseq ,altern)]

		  [(my-id) '(node-id (simobject-node this))]

		  [(soc-return ,x)		   
		   (process-expr `(return ,x 
					  (to (tok SOC-return-handler 0) )
					  (via (tok 

,x))]

		   (process-expr `(call (tok SOC-return-handler 0) ,x))]
		 
		  ;[(soc-return-finished ,x) 		  

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
		     `(begin (display (format ,(massage-str str) ,@args)) (newline)))]

		  [(,prim ,[rand*] ...)
		   (guard (or (token-machine-primitive? prim)
			      (basic-primitive? prim)))
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
		   `(,(process-expr rator) ,rand* ...)]
		  
		  [,otherwise (error 'simulator_nought.process-expr 
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
		       (error 'simulator_nought:process-binds
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
(define (process-tokbinds tbinds cost-table)
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
			(lambda (current-vtime subtok-index ,@args) ;world)
;			  (lambda args			 
			    (let* ([the-store (simobject-token-store this)]
				   [tokname-pair (cons ',tok subtok-index)]
				   [old-outgoing (simobject-outgoing-msg-buf this)]
				   [old-local    (simobject-local-msg-buf this)])
			      (DEBUGMODE 
			       ;; Check invariants on the store:
			       (check-store the-store))

			      "Is there already an allocated token object?:"
			      ;; Requires equal? based hash table:
			      (let ([tokobj (hashtab-get the-store tokname-pair)])
				(if (not tokobj)				   
				    (begin "If not, then we allocate that token object..."
					   " setting the invoke counter to zero."
					   (set! tokobj (vector 0 ,@(map cadr stored)))
					   (hashtab-set! the-store tokname-pair tokobj)))
				(set-simobject-outgoing-msg-buf! this '())
				(set-simobject-local-msg-buf! this '())
				;; Timed-token-buf need not be reversed, because it is ordered by vtime.
				,((process-statement tok tbinds allstored cost-table) body)
				;; We must reverse the outgoing order because of how they were added:
				(set-simobject-outgoing-msg-buf! this 
	   		  	  (append (reverse (simobject-outgoing-msg-buf this)) old-outgoing))
				(set-simobject-local-msg-buf! this 
                                  (append (reverse (simobject-local-msg-buf this)) old-local))
				(void))))
                         ]))
	  tbinds)])
    (printf "~n;  Converted program for Simulator:~n")
    (printf "Allstored was:~n") (pretty-print allstored)
    (printf "~n   Cost table: ~a~n" cost-table)
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
	     (cons `[SOC-return-handler (x)
		       (if (= ',BASE_ID (my-id))
			   (simulator-soc-return x)				  
			   (error 'SOC-return-handler
				  "ran on non-base node! id: ~a"
				  (my-id)))]
		   nodetoks))

       (let  ([cost-table 
	 ;; More cluginess, just stick in these hand-added handlers:
;	 (cons '(SOC-return-handler 0)
	 (map (lambda (tbind)
                (mvlet ([(tok id args stored bindings body) (destructure-tokbind tbind)])
                       (list tok 
                             (compute-handler-duration body))))
	      nodetoks)])
         
         (disp "COSTTABLE" (map car cost-table))

       (mvlet ([(tbinds allstored) (process-tokbinds nodetoks cost-table)])

	      ;; Here we hack an extra handler into tbinds:
; 	      (set! tbinds
; 		    (cons `[SOC-return-handler
; 			    (lambda (current-vtime subtok-index x)
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
		  
		  (let ([dyndispatch_table (make-default-hash-table)])
		    (begin ,@(map (lambda (tok)
				    `(hashtab-set! dyndispatch_table ',tok ,tok))
				  (map car tbinds)))

		    ;; Return the real meta-handler
		    (values 
		     ;; First the meta handler:
		     (lambda (msgob current-vtime)
		       (mvlet ([(name subtok)
				(let ((tok (msg-object-token msgob)))
				  (values (token->name tok)
					  (token->subtok tok)))])
			      (let ([handler (hashtab-get dyndispatch_table name)])
				(if (not handler)
				    (error 'node-code
					   "no handler for token name: ~a in table: ~n~a" name dyndispatch_table))
				;; Invoke:
				(apply handler current-vtime subtok 
				       (msg-object-args msgob))
				;; That returns nothing but fills up the simobjects buffers.
				)))
		     ;; Then the cost-table.
		     ',cost-table
		     )
		 )))))))]
      [,otherwise (error 'compile-simulate-alpha
			 "unmatched input program: ~a" prog)])))


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



