
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

;; [2005.10.03] Implemented token-deschedule

;; [2005.10.18] Factored out datatype defs and global params (and even
;; some helper functions) to a separate files.

;===============================================================================

;; ======================================================================


(define (simalpha-free-vars expr)
  (let loop ((env ()) (expr expr))
    (match expr	 
	   [,var (guard (symbol? var)) (if (memq var env) '() (list var))]   
	   [(quote ,x) '()]
 	   [(,prim ,rand* ...) (regiment-primitive? prim)
	    (let ((frees (map (lambda (x) (loop env x)) rand*)))
	      (apply append frees))]
	   [(lambda (,formals) ,expr)
	    (loop (append formals env) expr)]
	   [,else (error 'simalpha-free-vars "not simple expression: ~s" expr)])))


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


;;========================================

(define (base-station? x)
  (cond 
   [(simobject? x) (= BASE_ID (node-id (simobject-node x)))]
   [(node? x)      (= BASE_ID (node-id x))]
   [else (error base-station? "bad input: ~s" x)]))

(define (id x) x)

;; This is done safely so that it cannot conflict with the BASE_ID or NULL_ID.
(define (random-node) 
  (make-node 
   (let loop ((id (reg:random-int 1000)))
     (if (or (eq? id BASE_ID) 
	     (eq? id NULL_ID))
	 (loop (reg:random-int 1000))
	 id))
   (list (reg:random-int (simalpha-world-xbound))
	 (reg:random-int (simalpha-world-ybound)))
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

;; TODO, returns all the nodes in the graph that are connected to the
;; given simobject.  Gonna use this for unit testing oracles.
(define (all-connected simob sim)
  (graph-get-connected-component simob (simworld-object-graph sim)))

;; ================================================================================

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
  
  ;; This is our helper function that checks for collisions.
  (define (collide?  n1 n2)
    (let ((connectivity ((simalpha-connectivity-function)
			 (node-pos n1) (node-pos n2))))
      (if (not (memq connectivity '(0 100))) (printf "connectivity: ~s\n" connectivity))
      (not (eqv? 0 connectivity))))

  (define (make-random-topology)    
    ;; Old method, doesn't produce connected graph:
    (let ((seed (map (lambda (_) (random-node)) (iota (simalpha-num-nodes)))))
      (if (simalpha-consec-ids)
	  (for-each set-node-id! 
		    seed (iota (length seed))))
      ;; Now we just SET the first node to have the BASE_ID and serve as the SOC.
      (set-node-id! (car seed) BASE_ID)
      ;; Connect the graph:
      ;; TODO: Make the graph representation better.  Should cache the connectivity functions on the edges.
      (set! seed
	    (map (lambda (node)
		   (cons node 
			 (filter (lambda (n) 
				   (and (not (eq? node n))
					(let ((connection ((simalpha-connectivity-function) (node-pos node) (node-pos n))))
					  ;; We establish a connection unless we get zero-reception
					  (not (eqv? connection 0))
					  )))
				 seed)))
		 seed))
      seed))

  (define (make-connected-topology)
    ;; This might have some serious biases in the layout and distribution of degree.
    (let ((start-node (let ((x (random-node)))				   
			(set-node-id! x BASE_ID)
			x)))
      (let loop ((graph (list (list start-node))) (count (sub1 (simalpha-num-nodes))))
					;		 (printf "\nLooping: graph:\n")
					;		 (pretty-print graph)
	(if (<= count 0)
	    graph
	    (let ((newnode (random-node)))
	      ;; Here we number them as we go.  
	      ;; This will result in a non-random spatial distribution of node-ids.
	      (if (simalpha-consec-ids)
		  (set-node-id! newnode count))
	      (let ((nbr-rows (filter (lambda (row) (collide? newnode (car row))) graph)))
		(if (null? nbr-rows)
		    (loop graph count)
		    (begin 
		      (for-each (lambda (row)
				  ;; Splice in this as a neighbor:
				  (set-cdr! row (cons newnode (cdr row))))
				nbr-rows)
		      (loop (cons (cons newnode (map car nbr-rows))
				  graph)
			    (sub1 count))))))))))

  (define (make-gridlike-topology perfect?)
    (let* ([num-nodes (simalpha-num-nodes)]
	   [seed (make-vector num-nodes)]
	   ;; Calculate dimensions
	   [height (inexact->exact (floor (sqrt num-nodes)))]
	   [width (ceiling (/ num-nodes height))]
	   [step (fxmin (fx/ (simalpha-world-xbound) width)
			(fx/ (simalpha-world-ybound) height))]
	   [offsetx (fx/ (fx- (simalpha-world-xbound) (fx* (fx- width 1) step)) 2)]
	   [offsety (fx/ (fx- (simalpha-world-ybound) (fx* (fx- height 1) step)) 2)])

      ;; For now we use a simple linear perturbation.
      (define (perturb coord)
	(if perfect? coord
	    (+ coord (reg:random-int (quotient step 2)))))

      (for i = 0 to (sub1 num-nodes) ;; Allocate the nodes.
	   (vector-set! seed i (random-node)))
      (if (simalpha-consec-ids)
	  (begin (set-node-id! (vector-ref seed 0) BASE_ID)
		 (for i = 1 to (sub1 num-nodes)
		      (set-node-id! (vector-ref seed i) i))
		 ;; Have to mix it up after that
		 (randomize-vector seed))
	  ;; Otherwise a random node becomes base:
	  (set-node-id! (vector-ref seed (reg:random-int num-nodes)) BASE_ID))
      ;; Now we tile them in a gridlike fashion, and perturb them.
      (for i = 0 to (sub1 num-nodes) 
	   (set-node-pos! (vector-ref seed i)
			  (let ((pos (list (fx+ offsetx (fx* step (quotient i height)))
					   (fx+ offsety (fx* step (remainder i height))))))
			    ;; Then we perturb them randomly off that position:
			    (map perturb pos))))
      ;; Finally, connect the graph:
      (let ((seed (vector->list seed)))
	(map (lambda (node)
	       (cons node 
		     (filter (lambda (n) 
			       (and (not (eq? node n))
				    (let ((connection ((simalpha-connectivity-function) (node-pos node) (node-pos n))))
				      ;; We establish a connection unless we get zero-reception
				      (not (eqv? connection 0))
				      )))
				 seed)))
	     seed))))

  (define (make-topology) ;; Returns the graph	  
    (case (simalpha-placement-type)
      [(random) (make-random-topology)]
      [(connected) (make-connected-topology)]
      [(gridlike) (make-gridlike-topology #f)]
      [else (error 'simulator_alpha:fresh-simulation 
		   "unknown node placement strategy: ~s" (simalpha-placement-type))]))
  
  ;; Set global parameter:
  ;; This is a function which takes two locations and returns either
  ;;  1) a number, representing the loss percentage
  ;;  2) a function of time, representing the loss percentage over time
  (simalpha-connectivity-function
    (case (simalpha-channel-model)
      [(lossless)
       (lambda (p1 p2) ;x1 y1 x2 y2)
	 (let ((dist (posdist p1 p2))) ;(sqrt (^ (- x2 x1) 2) (^ (- y2 y1) 2))))
	   (if (< dist (simalpha-outer-radius))
	       100
	       0)))]
      [(linear-disc)
       (printf "Got linear disc...\n")
       (lambda (p1 p2)	
	 (let ((dist (posdist p1 p2))
	       (outer (simalpha-outer-radius))
	       (inner (simalpha-inner-radius)))
;	   (printf "Considering ~s (~s, ~s) between ~s and ~s\n" dist p1 p2 inner outer)
	   (cond
	    [(< dist inner) 100]
	    [(> dist outer) 0]
	    [else
	     (inexact->exact (floor (* 100 (/ (- dist inner) (- outer inner)))))])))]))
   
  ;; TODO, FIXME, need to use a real graph library for this!
  ;; (One that treats edges with due respect, and that uses hash tables.)
   (let* ([theworld (make-simworld #f #f #f #f #f #f #f)]
	  [graph (make-topology)]
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

     (DEBUGMODE
      (andmap (lambda (row) (andmap node? row))
	      graph))
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
     (set-simworld-vtime! theworld 0)
     
     (set-simworld-led-toggle-states! theworld (make-default-hash-table))
     theworld))  ;; End fresh-simulation

;; ================================================================================


;; This takes a simworld object and draws it on the screen:
(define (simalpha-draw-world world)
  (IF_GRAPHICS
   (let ()
;     (define edge-table (make-default-hash-table))
;     (define proc-table (make-default-hash-table))
     
     ;; This is a promise so as to be called only once.
;     (define wipe-screen (delay clear-buffer))
     ;; Contains a graphics object, and the last drawn state.
     (reg:define-struct (edgestate gobj oldstate))

     (if (not the-win) (init-graphics))
     (clear-buffer)

     ;; Draw edges:
     (for-each (lambda (graph-entry)
		 (let ([here (node-pos (car graph-entry))])
		   (for-each (lambda (nbr)
			       (draw-edge here (node-pos nbr)))
			     (cdr graph-entry))))
	       (simworld-graph world))
     ;; This is not a good abstraction boundary.
     ;; We just drew the edges, and now we call "draw network" just to draw nodes:
     ;; Associate with each simobject the resultant graphics object "gobj".
     (let* ((all-objs (simworld-all-objs world))
	    (nodes (map simobject-node all-objs)))
       (let ((gobjs (draw-network (map node-pos nodes)
				  (map node-id nodes))))
	 (for-each set-simobject-gobj! all-objs  gobjs)
	 )))
  (error 'simalpha-draw-world "graphics not loaded.")))


(define (print-connectivity world)
  (for-each (lambda (row)
;	      (printf "Bang : ~s \n" (map node-id row))
	      (printf "~s: ~s\n" (car row)
		      (map (lambda (nbr) 			     
;			     (printf "Woot ~s ~s ~s ~s \n" 
;				     (node? (car row)) (node? nbr)
;				     (car row) nbr ;(node-pos (car row)) (node-pos nbr)
;				     )
			     ((simalpha-connectivity-function)
			      (node-pos (car row))
			      (node-pos nbr)))
			   (cdr row))))
	    (simworld-graph world)))


                         
;; ======================================================================

;; Subroutine of compile-simulate-alpha below
(define (process-statement current-handler-name tokbinds stored)
  
  ;; [2005.10.20] This is a substitution list, used for simple renaming of primitives:
  ;; Right now I'm tightening up my numeric ops, getting it more ready for static typing.
  ;; TODO: Make this PLT compatible.
  (define prim-substs
    '([+ fx+] [- fx-] [* fx*] [/ fx/]
      [+. fl+] [-. fl-] [*. fl*] [/. fl/]
      [int->float fixnum->flonum]
      [float->int flonum->fixnum]
      ))
  
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

		  [(BLACKBOX ,[stuff]) 
		   ;; This finally gets popped open here at the simulator.
		   stuff]

		  ;; Token references return pairs which index the token store hash table.
		  [(tok ,t ,[e]) `(make-simtok ',t ,e)]

		  ;; This is sketchy: we just fail silently and return #f if there is no token there.
		  ;; FIXME TODO: We should seriously have some sort of better error handling convention.
		  [(ext-ref (tok ,tokname ,[subtok]) ,x)
		   (guard (and (symbol? x) (memq x allstored)))
		   ;; The stored names should be unique at this point!  So this should be ok:
		   (mvlet ([(which-tok pos) (find-which-stored x)])
		     (DEBUGMODE
		      (if (not (eq? which-tok tokname))
			  (error 'simulator_alpha:process-statement 
				 "bad ext-ref: (ext-ref (~s . ~s) ~s)" tokname subtok x)))
		     `(let ([exttokobj (retrieve-token the-store (make-simtok ',tokname ,subtok))])
			"FOOBAR"
			     ,(format "Ext-ref of (tok ~s ~s) variable ~s" tokname subtok x)
			     (if exttokobj
				 (,(string->symbol (format "~a-~a" tokname x)) exttokobj)
				 #f)))]
		  [(ext-set! (tok ,tokname ,[subtok]) ,x ,[e])
		   (guard (and (symbol? x) (memq x allstored)))		   
		   (mvlet ([(which-tok pos) (find-which-stored x)])
			  (DEBUGMODE
			   (if (not (eq? which-tok tokname))
			       (error 'compile-simulate-alpha:process-statement 
				      "bad ext-set to: (ext-ref (~s . ~s) ~s)" tokname subtok x)))
			  `(let ([exttokobj (retrieve-token the-store (make-simtok ',tokname ,subtok))])
			     ,(format "Ext-set! of (tok ~s ~s) variable ~s" tokname subtok x)
			     (if exttokobj
				 (,(string->symbol (format "set-~a-~a!" tokname x)) exttokobj ,e)
				 (warning 'ext-set! "var ~s: token not present: ~s" ',x `(,tokname . subtok))
				 )))]

		  ;; Local tokstore-reference:
		  [,x (guard (and (symbol? x) (memq x allstored)))
                    (mvlet ([(which-tok pos) (find-which-stored x)])
                           (if (not (eq? which-tok current-handler-name))
                               (error 'compile-simulate-alpha:process-statement 
				      "bad local stored-ref: ~s actually belongs to ~s not ~s" 
				      x which-tok current-handler-name))
                           ;; 'tokobj' is already bound to our token object
			   `(begin 
			     ,(format "Local Stored Ref of variable: ~s" x)
			     (,(string->symbol (format "~a-~a" which-tok x)) tokobj)))]

		  [,x (guard (or (symbol? x) (constant? x))) x]
		  [(quote ,x) `(quote ,x)]

		  [(begin) '(void)]
		  [(begin ,[x] ...) `(begin ,x ...)]
		  
		  ;; TODO: Implement subcall directly:
		  [(,subcall ,[rator] ,[rand*] ...)		  
		   (guard (memq subcall '(subcall direct-subcall)))
		   ;; Call the handler directly
		   `(begin "Simulator subcall code" 
			   ((simobject-meta-handler this)
			    (bare-msg-object ,rator
					     (list ,@rand*) current-vtime)
			    current-vtime))]
		  [(return ,[x]) x]

		  ;; NOTE! These rands ARE NOT simple.
		  [(call ,[rator] ,[rand*] ...)
		   (let ((rands (unique-name 'rands)))
                     `(let ((,rands (list ,@rand*)))
			;; Make sure those ^^ are all done evaluating before we mess with the msg buf:
			(set-simobject-local-msg-buf! this
                           (cons (make-simevt #f ;; No scheduled time, ASAP
                                              (bare-msg-object ,rator ,rands current-vtime))
                                 (simobject-local-msg-buf this)))))]

		  
		  ;; [2005.10.31] This is my first hack at high-priority scheduling for subcalls:
		  [(call-fast ,[rator] ,[rand*] ...)
		   (let ((rands (unique-name 'rands)))
                     `(let ((,rands (list ,@rand*)))
			;; Make sure those ^^ are all done evaluating before we mess with the msg buf:
			(set-simobject-timed-token-buf! this
                           (cons (make-simevt (- current-vtime 1) ;; Scheduled time is yesterday!
                                              (bare-msg-object ,rator ,rands current-vtime))
                                 (simobject-timed-token-buf this)))))]
		     
		  [(bcast ,[rator] ,[rand*] ...)		   
		   `(begin 
		      (simalpha-total-messages (add1 (simalpha-total-messages)))
		      (set-simobject-outgoing-msg-buf! this
  		        (cons (make-simevt #f ;; No scheduled time, ASAP
					   (bare-msg-object ,rator (list ,@rand*) current-vtime))
			      (simobject-outgoing-msg-buf this))))]

;; These are desugared before now.
;		  [(activate ,rator ,rand* ...)
;		   (build-activate-call `(quote ,rator) rand*)]

		  [(timed-call ,delay ,[rator] ,[rand*] ...)
		   ;; Delay is in milleseconds.
		  `(set-simobject-timed-token-buf!
		    this (cons (make-simevt (+ ,delay current-vtime)
					    (bare-msg-object ,rator (list ,@rand*) current-vtime))
			       (simobject-timed-token-buf this)))]

		  ;; If it's in the hash table, it's present:
		  ;; This is static wrt to token name for the moment:
;		  [(token-present? (tok ,t ,n)) (guard (number? n)) 
;		   `(if (hashtab-get the-store (make-simtok ',t ,n)) #t #f)]  ;; Needs not be separate case. [2005.10.03]
		  [(token-present? (tok ,t ,[e]))
		   `(if (retrieve-token the-store (make-simtok ',t ,e)) #t #f)]
		  [(evict (tok ,t ,[e])) `(evict-token the-store (make-simtok ',t ,e))]
		  [(token-scheduled? ,[tok]) ;; INEFFICIENT
		   ;; queue is list containing (simevt . simob) pairs.
		   ;; We go through the current queue looking for a scheduled event that matches this token.
		   ;; NOTE: This is the queue of *all* events, we also must make sure it happens on this *node*.
		   `(begin		      
		      #;
		      (disp "SCANNING QUEUE: " (length (simworld-scheduler-queue (simobject-worldptr this)))
			    "and locals: "     (length (simobject-local-msg-buf this))
			    (map msg-object-token 
				 (map simevt-msgobj
				      (simobject-local-msg-buf this))))
		      
		      ;; FIXME: 
		      ;; PROFILING INDICATES THAT THIS IS VERY SLOW:

		      (or ;;; First, check the schedulers queue:
		       ;; (TODO FIXME, CHECK: THIS MIGHT NOT EVEN BE NECESSARY:)
		       (let loop ((queue (simworld-scheduler-queue (simobject-worldptr this))))
			 (if (null? queue) #f 
			     (let ((simtok (msg-object-token (simevt-msgobj (caar queue)))))
			       (or (and (eq? this (cdar queue)) ;; Is it an event on this node.
					(simtok-equal? ,tok simtok)
					(begin 	       
					  (if (regiment-verbose)
					  (DEBUGMODE (printf "Wow! we actually found the answer to ~s\n"
							     "token-scheduled? in the scheduler-queue!")))
					  #t)
					) ;; If so is it the token in question?
				   (loop (cdr queue))))))
		       ;; Second, check the msg bufs:
		       (let loop ((locals (append 
					   (simobject-local-msg-buf this)
					   (simobject-timed-token-buf this)
					   )))
			 (if (null? locals) #f
			     (let ((simtok (msg-object-token (simevt-msgobj (car locals)))))
			       (or (simtok-equal? ,tok simtok)
				   (loop (cdr locals)))))))
		       )]
		  [(token-deschedule ,[tok]) ;; INEFFICIENT
		   ;; queue is list containing (simevt . simob) pairs.
		   ;; We go through the current queue looking for a scheduled event that matches this token.
		   ;; NOTE: This is the queue of *all* events, we also must make sure it happens on this *node*.
		   `(let* ([thistok ,tok]
			   [notthistok (lambda (x) 
					 (let ((tok (cond 
						     [(and (pair? x) (simevt? (car x)))
						      (msg-object-token (simevt-msgobj (car x)))]
						     [(simevt? x)
						      (msg-object-token (simevt-msgobj x))]
						     [else (error 'token-deschedule-generated-code "code error.")])))
					;(not (and (eq? (simtok-name thistok) (simtok-name tok))
					;	  (eq? (simtok-subid thistok) (simtok-subid tok))))
					   (not (simtok-equal? tok thistok))))]
			   [world (simobject-worldptr this)])
		      ;; Clear the actual simulation queue.
		      (set-simworld-scheduler-queue! world (filter notthistok (simworld-scheduler-queue world)))
		      (set-simobject-local-msg-buf! this (filter notthistok (simobject-local-msg-buf this)))
		      (set-simobject-timed-token-buf! this (filter notthistok (simobject-timed-token-buf this)))

		       )]



		  ;; Local Stored variable mutation:
		  [(set! ,v ,[rhs]) (guard (memq v allstored))
		   (mvlet ([(which-tok pos) (find-which-stored v)])
			  (DEBUGMODE
			   (if (not (eq? which-tok current-handler-name))
			       (error 'compile-simulate-alpha:process-statement 
				      "(set!) bad local stored-ref: ~s actually belongs to ~s not ~s" 
				      v which-tok current-handler-name)))
			  `(begin 
			     ,(format "Local Stored Set! of variable: ~s" v)
			     (,(string->symbol (format "set-~a-~a!" which-tok v)) tokobj ,rhs)))]

		  [(set! ,v ,[rhs])  `(set! ,v ,rhs)]

		  [(if ,[test] ,[conseq] ,[altern])
		   `(if ,test ,conseq ,altern)]

		  ;; ========================================
		  ;; Now we generate specific code for many of the primitives

		  [(,senseprim)
		   (guard (memq senseprim '(async-sense sync-sense)))
		   ;; Feed id, x, y, t to sensor function:
		   `((simalpha-sense-function) 
		     ,(process-expr '(my-id))
		     (car ,(process-expr '(loc)))
		     (cadr ,(process-expr '(loc)))
		     ,(process-expr '(my-clock)))]

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

		;; [2005.10.02] Inserting magical soc-returns so that
		;; we can (kind-of) simulate code where they haven't
		;; been desugared.
		[(soc-return ,[x])
		 `(simulator-soc-return ,x)]

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
		     (let ((massaged (massage-str str)))
		       `(begin (if (simalpha-dbg-on)
				   (begin (display (format ,massaged ,@args) (console-output-port))
					  (newline (console-output-port))))
			       ;; Send a copy to the logger as well:
			       (logger (string-append "<dbg> " (format ,massaged ,@args)))))
		     )]

		;; Any prim apps that didn't get caught above are equivalent to the normal Scheme versions:
		[(,prim ,[rand*] ...)
		 (guard (or (token-machine-primitive? prim)
			    (basic-primitive? prim)))
;		 (printf "Prim : ~s\n" prim)
		 (let ((entry (assq prim prim-substs)))
		   (if entry
		       `(,(cadr entry) ,rand* ...)
		       `(,prim ,rand* ...)))]

		;; We're being REAL lenient in what we allow in token machines being simulated:
		[(let ((,lhs ,[rhs]) ...) ,[bods] ...)
		 `(let ((,lhs ,rhs)  ...) ,bods ...)]
		;; We're letting them get away with other primitives because
		;; we're being lenient, as mentioned above.
		[(app ,[rator] ,[rand*] ...)
		 `(,rator ,rand* ...)]
		
		;; Supporting the output of cps-tokmac also:
		[(kcall ,[rator]  ,[rand*] ...)
		 (let ((tmp (unique-name 'tmprator))
		       (tmp2 (unique-name 'tmprands)))
		   `(let ((,tmp ,rator)
			  (,tmp2 (list ,@rand*)))
		      ;(printf "Kcall ~s ~s ..\n" ,tmp ,tmp2)
		      (if (eq? ,tmp ,NULLK)
			  "kcall fizzles."
			  (apply ,tmp ,tmp2))))]
		[(lambda (,v ...) ,[bod]) `(lambda (,v ...) ;(printf "cont invoked...\n")
						   ,bod)]
		
		[(,rator ,[rand*] ...)
		 ;; This is an arbitrary scheme application. Where would these come from?
		 ;; Don't want to be too lenient tho:
		   (guard (not (token-machine-primitive? rator))
			  (not (memq rator '(emit bcast call timed-call activate relay return call-fast))))
		   (warning 'simulator_alpha.process-expr
			    "arbitrary rator applied: ~s" rator)
		   `(,(process-expr rator) ,rand* ...)]
		
		[,otherwise (error 'simulator_alpha.process-expr 
				   "don't know what to do with this: ~s" otherwise)])
	   )])
    (lambda (stmt) (process-expr stmt)))))


(define (process-binds binds)
;  (disp "processbinds" binds expr)
  (let* ([graph (map (lambda (bind)
		       (cons (car bind)
			     (simalpha-free-vars (cadr bind))))
		     binds)]
	 [flat (reverse (topological-sort graph ))];eq?))]
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



;; This takes token bindings, returns compiled bindings along with an
;; association list of stored-vars.
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
			  ;; Set the global "this" for the below dynamic extent.
			  (parameterize ((current-simobject this))
			  (let ,(map list args (make-list (length args) ''sim-alpha-uninitialized))

			    (let ([numvals (length vals)])
			      ;; Issue a single warning if we're padding arguments:
 			      (if (< numvals ,(length args))
				  (case (simalpha-zeropad-args) 
				    [(#t) (void)]
				    [(warning)
				      (warning 'simulator-alpha "executing handler <~s.~s> padding args ~s with zero." 
					       ',tok ,id (list-tail ',args numvals))]
				    [(#f) (error 'simulator-alpha "executing handler <~s.~s> padding args ~s with zero.\nSupplied args were:\n~s\n"
						 ',tok ,id (list-tail ',args numvals) vals)]))
 			      (if (> numvals ,(length args))
 				  (error 'simulator-alpha "executing ~s, got too many args: ~s for inputs ~s" 
					 ',tok vals ',args))
			      
			      ,@(map (lambda (arg)
				       `(if (null? vals)
					    (set! ,arg 0)
					    (begin (set! ,arg (car vals))
						   (set! vals (cdr vals)))))
				  args)
			      "Done initializing arguments."

			    (let* ([the-store (simobject-token-store this)]
				   [simtok-obj (make-simtok ',tok ,id)]
				   [old-outgoing (simobject-outgoing-msg-buf this)]
				   [old-local    (simobject-local-msg-buf this)])
			      (DEBUGMODE 
			       ;; Check invariants on the store:
			       (check-store the-store))

			      "Is there already an allocated token object?:"
			      ;; Requires equal? based hash table:
			      (let ([tokobj (retrieve-token the-store simtok-obj)])
				(if (not tokobj)				   
				    (begin "If not, then we allocate that token object..."
					   " setting the invoke counter to zero."
					   (set! tokobj (,(string->symbol (format "make-~a" tok))
							 0 ,@(map cadr stored)))
					   (add-token the-store simtok-obj tokobj)
					   ))
				(set-simobject-outgoing-msg-buf! this '())
				(set-simobject-local-msg-buf! this '())
				;; Timed-token-buf need not be reversed, because it is ordered by vtime.
				(let ((this-handler-retval ,((process-statement tok tbinds allstored) body)))
				  ;; We must reverse the outgoing order because of how they were added:
				  (set-simobject-outgoing-msg-buf! this 
	   		  	    (append (reverse (simobject-outgoing-msg-buf this)) old-outgoing))
				  (set-simobject-local-msg-buf! this 
                                    (append (simobject-local-msg-buf this) ;(reverse (simobject-local-msg-buf this)) 
					    old-local))
				  this-handler-retval)
				))))))
                         ]))
	  tbinds)])
    (printf "~n;  Converted program for Simulator:~n")
    (printf "Allstored was:~n") (pretty-print allstored)
    (printf "<-------------------------------------------------------------------->~n")
    ;(pretty-print binds)

    (values binds allstored)))

;; ======================================================================

(define (compile-simulate-alpha prog . extraparams)
  ;; Accept either with or without the language wrapper:
  (let ((prog (match prog 
		     [(tokens ,t ...) `(program (bindings) (nodepgm (tokens ,t ...)))]
		     [(program ,_ ...) prog]
		     [(,input-lang '(program ,stuff ...)) `(program ,stuff ...)])))
    (match prog
      [(program (bindings ,nodebinds ...)
		(nodepgm (tokens ,nodetoks ...) ;(startup ,starttoks ...)
			 ))
       
       ;; Here we hack in some special handlers that are assumed by TML:
       (set! nodetoks
	     ;; An extra handler for doing SOC-return's...
	     `([SOC-return-handler subtokid (socrethandlerval) (stored)
		       (if (= ',BASE_ID (my-id))
			   (simulator-soc-return socrethandlerval)				  
			   (error 'SOC-return-handler
				  "ran on non-base node! id: ~s"
				  (my-id)))]
	       ;; The actual sense function:
	       ;; This is flexible and does not assume that cps-conversion
	       ;; has taken place.  It optionally takes a continuation argument:
	       [SenseTok subtokid (k_maybe) (stored)
			 (if (eq? '0 k_maybe)
			     (sync-sense)
			     ;; If we have run cps-tokmac without convert closure
			     ;; then it will be an actual procedure:
			     (if (procedure? k_maybe)
				 (kcall k_maybe (sync-sense))
				 (call k_maybe ',KCALL_FLAG (sync-sense))
				 ))]
	       ,@nodetoks))

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
	      (assq-remove-all 'node-start nodetoks)))
       
       (mvlet ([(tbinds allstored) (process-tokbinds nodetoks)])

	      ;; Here we hack an extra handler into tbinds:
; 	      (set! tbinds
; 		    (cons `[SOC-return-handler
; 			    (lambda (current-vtime subtok_ind x)
; 			      (if (eq? ,BASE_ID (node-id (simobject-node this)))
; 				  (simulator-soc-return x)				  
; 				  (error 'SOC-return-handler
; 					 "ran on non-base node! id: ~s"
; 					 (node-id (simobject-node this)))))]
; 			  tbinds))
	 (let ((node-code
	 `(define (node-code this)	    

	   ;; First define datatype definitions for the tokens:
	   ,@(let ((alltoks (list->set (map car allstored))))
	       (map (lambda (t)		
		      `(reg:define-struct (,t invoke-counter 
					      ,@(cadr (assq t allstored)))))
		 alltoks))

	   ,@(DEBUGMODE '(if (not (simobject? this)) (error 'node-code "'this' was not a simobject.")))

	   ;; Set the global parameter that library code needs to access "this".
	   (parameterize ((current-simobject this)
			  ,@extraparams)
	   ;; Now we have subtoks:
;	   (reg:define-struct (tokstore ,@(apply append (map cadr allstored))))
	   ;; Need to update the sensing machinery...
	   (let (
		 #;
		 [sync-sense (lambda (t)
				((current-sense-function)
				 (node-id (simobject-node this))
				 (car (node-pos (simobject-node this)))
				 (cadr (node-pos (simobject-node this)))
				 current-vtime
				 ))]
		 )
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
				
;				(fprintf (current-error-port) "Dyndispatch: ~s in table: " name)
;				(hashtab-for-each (lambda (name _) (fprintf (current-error-port) "~s " name)) dyndispatch_table)
;				(newline (current-error-port))

				(if (not handler)
				    (error 'node-code
					   "dyndispatch: no handler for token name: ~s in table: ~n~s" name dyndispatch_table))
				;; Invoke:
				(apply handler current-vtime subtok 
				       (msg-object-args msgob))
				;; That returns nothing but fills up the simobjects buffers.
				)))
		 ))))))))
	   ;; Final return value of compile-simulate-alpha:
	   (build-genned-code-module node-code)
	   )
	 )]
      [,otherwise (error 'compile-simulate-alpha
			 "unmatched input program: ~s" prog)])))


;; ======================================================================

;; This requires pass21_cleanup-token-machine.ss as well as helpers.ss
;; This handles writing the generated code to a file and loading it back.
;; FLAGS:
;; 'numnodes int -- Set the number of nodes in the world to int.
;; 'outport prt  -- Set the printed output of the simulation to port prt.
;; 'srand int    -- Seed the random number generator with int.
(define run-simulator-alpha
  (lambda args
    (define run-alpha-loop
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
					  [pretty-line-length 150]
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
					   [pretty-line-length 150]
					   [print-graph #f])
			    (pretty-print comped out)
			    (newline out)
			    (newline out)
			    (display ";; Above is compiled program for this token machine: " out)
			    (newline out)
			    (display "#;\n" out)
			    (pretty-print tm out)
			    (newline out))
			    (close-output-port out))
			  (read-params rest)
			  ))])]
		     [(,rest ...) (read-params rest)]
		     )))
    (define read-params
	     (lambda (params)	       
	       (match params
;		      [,x (guard (disp "read params" params) #f) 3]
		      ;; When we get to the end of the params we start it up:
		      [() 
		       (load "_genned_node_code.ss")
                       ;; We have to do this because of the module system:
                       (let ((node-code (eval 'node-code)))
                         ;(disp "NODE CODE:" node-code) ;" eq: " (eq? node-code (eval 'node-code)))
                         ;(printf "Node code loaded from file.~n")
                         ;(if (not node-code)  (error 'run-simulator-alpha "node-code not defined!"))
                         (start-alpha-sim node-code 'simple))]
		      [(timeout ,n . ,rest)
		       (parameterize ((simalpha-timeout n))
			 (read-params rest))]
		      [(numnodes ,n . ,rest)
		       (if (not (integer? n))
			   (error 'run-simulator-alpha
				  "'numnodes switch should be followed by an integer, not: ~s" n))
		       (parameterize ([simalpha-num-nodes n])
			 (read-params rest))]
		      [(outport ,p . ,rest)
		       (if (not (output-port? p))
			   (error 'run-simulator-alpha
				  "'outport switch should be followed by a port object, not: ~s" p))
		       (parameterize ([simalpha-output-port p])
			 (read-params rest))]
		      [(srand ,n . ,rest)
;		       (if (not (integer? n))
;			   (error 'run-simulator-alpha
;				  "'srand switch should be followed by an integer, not: ~s" n))
		       (printf "Setting up random number generator for simulator.  Srand: ~s\n" n)
		       (let ([stored-state #f])
			 (dynamic-wind
			     (lambda () (set! stored-state (reg:get-random-state)))
			     (lambda () (read-params rest))
			     (lambda () (reg:set-random-state! stored-state))))
		       ]
		      [,other (error 'run-simulator-alpha "unrecognized parameters: ~s" other)]
		      )))
    ;; Reset global message counter:
    (simalpha-total-messages 0)
    (simalpha-total-tokens 0)
    (apply run-alpha-loop args)))




;; [2005.09.29] Moved from alpha_lib.ss
;; This just sets up the sim and the logger and invokes one of the scheduler/execution engines.
;; I've written two different engines at different levels of time-modeling complexity.
(define (start-alpha-sim node-code-fun . args)
  (disp "Start-alpha-sim" node-code-fun args)
  ;; In some scheme's these internal defines don't evaluate in order!!
  (let* ([logfile "__temp.log"]
	 [simple-scheduler #f]
	 [flags-processed (filter (lambda (arg)
				    (disp "ARG" arg (eq? arg 'simple))
				    (if (eq? arg 'simple)
					(begin (set! simple-scheduler #t) #f)
					#t))
				  args)]
	 ;; With flags out of the way
	 [stopping-time? 
	  (let ([stop-time (simalpha-timeout)])
	    (disp "STOP TIME" stop-time)
	    (if (not stop-time)
		(lambda (t) #f)
		(if (inexact? stop-time)
		    ;; It's in seconds:
		    (let ([end-time (+ (* 1000 stop-time) (cpu-time))])
		      (printf "Stopping after ~s seconds.~n" stop-time)
		      (lambda (_) (>= (cpu-time) end-time)))
		    ;; Otherwise, vtime:
		    (begin (printf "Stopping after vtime ~s.~n" stop-time)
			   (lambda (t) (>= t stop-time))))))]
	 [sim (fresh-simulation)])

  (disp "RUNNING ALPH" args simple-scheduler)

  (if (file-exists? logfile) (delete-file logfile))


  ;; Here, we also leave our simworld behind after we finish for anyone who wants to look at it.
  (simalpha-current-simworld sim)
  
  ;; If there are no graphics available, we turn it off:
  (parameterize ([simalpha-graphics-on
		  (IF_GRAPHICS (simalpha-graphics-on) #f)])
			       
  ;; Draw the world, if necessary.
  (if (simalpha-graphics-on)  (simalpha-draw-world sim))
 
  (parameterize ([soc-return-buffer '()]
		 ;; This is not used by simalpha itself, but anyone else who wants to peek:
		 [simalpha-current-simworld sim])
  (let/cc exitk	
	  ;; FOR NOW: only log if we're in debugmode [2005.10.17]
  (parameterize ([simulation-logger (IFDEBUG (open-output-file logfile 'replace) #f)]
		 [simulation-logger-count (IFDEBUG 0 #f)]
		 [escape-alpha-sim exitk])
		(printf "Running simulator alpha (~s version) (logfile ~s)" 
			(if simple-scheduler 'simple 'full)
			logfile)
		(DEBUGMODE (display " with Debug-Mode enabled"))
		(printf ".~n")
		(printf "<-------------------------------------------------------------------->~n")

	;; DEBUG DEBUG DEBUG
	(DEBUGMODE
	 (global-graph (simworld-graph sim)))

	;(printf "Starting!  Local: ~s~n" (map simobject-local-msg-buf (simworld-all-objs sim)))
	;; Redirect output to the designated place:
	(let ((old-output-port (current-output-port)))
	  (parameterize ([current-output-port
			  (if (simalpha-output-port)
			      (begin (printf "~n!!!  Redirecting output to port: ~s  !!!~n" (simalpha-output-port))
				     (simalpha-output-port))
			      (current-output-port))]
			 ;; Just to be safe I'm resetting this here.  Don't want to freeze on any print statements! -[2005.10.26] 
			 [print-graph #t]
			 )
  	    (if simple-scheduler
		(run-alpha-simple-scheduler sim node-code-fun stopping-time? old-output-port)
		; [2005.09.30] Disabling for now, not loading the full scheduler:
		; (run-alpha-full-scheduler sim node-code-fun stopping-time?)
		(error 'start-alpha-sim "full scheduler not loaded")
		)
	  ))
		   
    ;; Out of main loop:
    (if (simulation-logger) (close-output-port (simulation-logger)))))
  ;; Out of let/cc:
  (let ((result (reverse (soc-return-buffer))))
    (printf "~nTotal globally returned values:~n ~s~n" result)
    result))  
  )))

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
	  [node-start () (stored) ];(begin (printf "N~s" (simobject-I-am-SOC this)) (call tok1))]
	  [tok1 () (stored) (begin (display ".") (bcast tok2 " "))]
	  [tok2 (x) (stored) (display x)]
	 ))))
     '??]
|#
   

))

#;    (compile-simulate-alpha 
     '(cleanup-token-machine-lang (quote (program (bindings) (nodepgm (tokens (node-start subtok_ind () (stored) (void)) (SOC-start subtok_ind () (stored) (call (tok tok1 0) (begin #0="This whole block represents the allocation of a continuation closure:" (let ((kind_4 (if (token-present? (tok K_3 0)) (let ((new (+ (quote 1) (ext-ref (tok K_3 . #1=(0)) kcounter)))) (begin (ext-set! (tok K_3 . #2=(0)) kcounter new) new)) (begin #3="Allocate this zeroeth token object just to hold a counter MEMORY WASTEFUL!:" (call (tok K_3 0) (quote 11) (void)) (quote 1))))) (begin #4="Do the actual token object (closure) allocation.  Capture freevars:" (call (tok K_3 kind_4) (quote 11)) #5="Return the name of this continuation object:" (tok K_3 kind_4)))) (quote 4))) (K_3 subtok_ind (flag fv0) (stored (kcounter . #6=(0))) (if (= flag (quote 11)) (if #7=(= subtok_ind (quote 0)) #8=(void) (begin)) (begin (call (tok tok1 0) (begin #0# (let ((kind_2 (if (token-present? (tok K_1 0)) (let ((new (+ (quote 1) (ext-ref (tok K_1 . #1#) kcounter)))) (begin (ext-set! (tok K_1 . #2#) kcounter new) new)) (begin #3# (call (tok K_1 0) (quote 11) (void)) (quote 1))))) (begin #4# (call (tok K_1 kind_2) (quote 11) fv0) #5# (tok K_1 kind_2)))) (quote 3)) (evict (tok K_3 . #9=(subtok_ind)))))) (K_1 subtok_ind (flag fv0) (stored (kcounter . #6#) (HOLE_59 (quote 0))) (if (= flag (quote 11)) (if #7# #8# (begin (set! HOLE_59 fv0))) (begin (printf (quote "result ~s") (+ HOLE_59 fv0)) (evict (tok K_1 . #9#))))) (tok1 subtok_ind (k_58 x) (stored) (call k_58 (quote 99) (+ x (quote 1000))))))))))


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
