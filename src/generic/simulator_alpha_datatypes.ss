;; simulator_alpha_datatypes.ss
;; [2005.10.18] This file encapsulates the datatype definitions and
;; global parameters used by the simulator.

;; NOTE: If I had this to do over again I would use some kind of oop system.
;; There is basically a class-hierarchy three deep:
;;  NODE              -- Basic information about a node.
;;  |-> SIMOBJECT     -- State associated with the simulator
;;      |-> GOBJECT   -- State associated with the visualization of the simulator.

; =======================================================================

;; This structure contains all the global data needed by a simulation.
;; (Well, that's a bit of a lie, because the simulator also requires a
;; number of global parameters to be set appropropriately.)
(reg:define-struct 
 (simworld graph object-graph all-objs 
	   ;; obj-hash maps node-ids onto simobjects:
	   obj-hash 
	   ;; This is a pointer to the queue used by the scheduler.
	   ;; It's a sorted list of simevts?
	   scheduler-queue 
	   ;; This is updated by the scheduler, the current global vtime.
	   vtime
	   ;; [2005.11.07] A hash table mapping node-ids to a list of all the leds that are toggled on.
	   ;; Could have added this to the simobject structure, but I'm reluctant, as it is only a presentation detail:
	   led-toggle-states

	   ;; This is a function which models the channels, it takes two locations and returns either:
	   ;;  1) a number, representing a fixed loss percentage
	   ;;  2) a function of time, representing the loss percentage over time
	   connectivity-function
	   ))

;; [2005.03.13]  Adding this to represent events-to-happen in the simulator.
(reg:define-struct (simevt vtime msgobj))

;; [2005.05.06]
;; A first class representation of tokens:
(reg:define-struct (simtok name subid))
;; TODO: Change the system to use these ^^
(define (simtok-equal? x y)
  (DEBUGMODE 
   (unless (and (simtok? x) (simtok? y))
     (error 'simtok-equal? "These are not both simtoks: ~s ~s" x y)))
  (and (eq? (simtok-name x) (simtok-name y))
       (eqv? (simtok-subid x) (simtok-subid y))))

;; This structure contains everything an executing token handler needs
;; to know about the local node.  "this" is a simobject.  tokstore is
;; a struct containing all the stored values.

;  [2005.03.05] Putting everything in simobject, "this" provides everything.
;(reg:define-struct (localinfo this I-am-SOC tokstore))

;; Positions are just 2-element lists.
(reg:define-struct (node id pos))


;; [2005.11.16] NOT USED YET:
;; Graphical Node-Object
;; This is the graphical representation of a node, it consists of several graphical subparts.
(reg:define-struct (gobject circ           ;; The circle
			    rled gled bled ;; The LEDs
			    title label    ;; The title above the node, and debug-text/label below the node.
			    edgelist ;; An association list binding neighbor ID to a graphical line object.
			    ))
;; Optionally put in a guarded constructor:
#;(DEBUGMODE
 (define make-gobject
   (let ((orig make-gobject))
     (lambda (c r g b t l e)
       (if (and (list? e)
		;; Don't know how to check instance relationships for SWL objects...
		)
	   (orig c r g b t l e)
	   (error 'make-gobject "Invalid edge table: ~s" e))))))


;; This is a very important and central structure that represents a simulated node.
;;<br>
;;<br> [2004.06.11] Added homepage just for my internal hackery.
;;<br> [2004.06.13] Be careful to change "cleanse-world" if you change
;;<br>   this, we don't want multiple simulation to be thrashing eachother.
;;<br> [2004.07.08] I don't know why I didn't do this, but I'm storing the
;;<br>   token-cache in the structure too
(reg:define-struct (simobject node I-am-SOC

			     ;; The token store is a hash table mapping simtok objects to token
			     ;; objects.  The token objects themselves are just records of stored
			     ;; variables.  However, by convention, the first slot of the token
			     ;; object is a counter for how many times the handler has been
			     ;; invoked.
			     token-store ;; Changing this to hash table indexed by token names.

			     ;; All these buffers get changed when a token handler runs:
			     incoming-msg-buf ;; Stores simulation events
			     local-msg-buf    ;; Stores simulation events
			     outgoing-msg-buf ;; Stores simulation events
			     timed-token-buf  ;; Stores simulation events

			     local-sent-messages local-recv-messages
			     ;; This stores #(invoked sent received) counters for every token name:
			     token-table
			     
			     redraw   ;; A boolean indicating whether the object needs be redrawn.
			     gobj     ;; Pointer to the graphical representation of this object.
			     homepage ;; Not currently used, a "blackboard".

			     ;; This is a function that processes incoming messages
			     scheduler ;; and returns simulation actions.
			     ;; Not used in the simple scheduler as of [2005.09.27]

			     ;; This function takes msg-obj and vtime and executes a token handler:
			     meta-handler
			     
			     worldptr ;; A pointer to the relevent simworld object.
			     ))
;; The following builds a simobject from a node and initializes all the values to their default state.
;; This is essentially the constructor for the type 'simobject.
;; Optionally takes 
;; .returns A fresh, initialized simobject.
(define node->simobject 
  (case-lambda 
    [(nd) (node->simobject nd #f)]
    [(nd world)
     (DEBUGASSERT (or (not world) (simworld? world)))
     (let ([so (apply make-simobject (make-list 16 'simobject-field-uninitialized))])
       (set-simobject-node! so nd)
       (set-simobject-token-store! so (make-default-hash-table 100))
       
       (set-simobject-incoming-msg-buf! so '())
       (set-simobject-outgoing-msg-buf! so '())
       (set-simobject-local-msg-buf! so '())
       (set-simobject-timed-token-buf! so '())
       
       (set-simobject-local-sent-messages! so 0)
       (set-simobject-local-recv-messages! so 0)
       (set-simobject-token-table! so (make-default-hash-table 100))
       
       (set-simobject-redraw! so #f)
       (set-simobject-gobj! so #f)
       (set-simobject-homepage! so '())
       (set-simobject-I-am-SOC! so #f)
       
       (set-simobject-scheduler! so #f)
       (set-simobject-meta-handler! so #f)
       
       (set-simobject-worldptr! so world)
       so)]))


;; This structure represents a message transmitted across a channel.
;; None of these should be mutated:
(reg:define-struct (msg-object token ;; This is a simtok object.  Used to just be a symbol (name).
			      sent-time ;; when it was sent --This is currently mutated within the scheduler [2005.09.27]
			      parent ;; :: simobject - who I got it from
			      to   ;; :: nodeid - who its going to, #f for broadcast
			      args))

;; [2005.11.03] These totals were simply global vars.  But PLT's module
;; system had a problem with that.  I could maybe think of something
;; more efficient to do here since these are called hundreds of thousands of times.
;; This one is just used to count up the messages during a simulation: <br>
;;
;; [2005.11.26] This information is now in the individual simobjects,
;; replacing these with functions that sum up the simobject values.
;(define simalpha-total-messages (make-parameter 0 (lambda (x) x)))
;; This one counts total token handlers fired.
;(define simalpha-total-tokens (make-parameter 0 (lambda (x) x)))

;; Safer version:
(define (safe-construct-msg-object token timestamp parent args)
  ;(unless (token-name? token) (error 'safe-construct-msg-object "bad token name: ~s" token))
  (DEBUGMODE
   (unless (simtok? token) (error 'safe-construct-msg-object "bad token: ~s" token)) ;
   (unless (or (number? timestamp) (not timestamp))
	   (error 'safe-construct-msg-object "bad timestamp: ~s" timestamp))
   (unless (list? args)
	   (error 'safe-construct-msg-object "bad args: ~s" args)))
  (make-msg-object token timestamp parent #f args))

;; [2004.06.28] This is a helper to construct the locally used
;; messages that don't have a parent, timestamp, etc.
(define (bare-msg-object rator rands . time)  
  (safe-construct-msg-object rator ;; token
		   (if (null? time) #f (car time))    ;; timestamp
		   #f    ;; parent
		   rands))


;; This is our logger for events in the simulator:                  <br> 
;; .form (logger str args ...)                                      <br>        
;; .form (logger print-level str args ...)                          <br><br>
;;
;; This uses the parameter "simulation-logger", expecting it to be bound to an output port. <br><br>
;;
;; TODO: make this a syntax so that the calls disappear entirely in non-debug mode...  <br><br>
;;
;; The current model for storing SExp logs allows them to be of the following type:    <br> 
;;   LogLine ::= (vtime nodeid Symbol (field val)* )                                   <br>
;;   Log ::= <LogLine>*                        -- A flat file of log-lines             <br>
;;        |  #( <LogLine>* )*                  -- Log-lines chunked/batched by vectors <br><br>
;;
;; And further, log files can either be .log or .log.gz.
(define logger
  (let ()
    ;; This is a bit of added complexity, but I'm going to write out
    ;; chunks of log-file as vectors.  Thus I'm going to buffer the logging.
    ;; UNFINISHED UNFINISHED::
    (define buffered-writer
      (if #f ;(top-level-bound? 'fork-thread) ;; TODO: FINISH
	  (let ()
	    (define obj-buffer (make-vector 500 #f))
	    (define num-objs 0)
;	    (define buffer-mutex (make-mutex))
;	    (fork-thread ;; Reader thread:
	    
	    (case-lambda 
	      [() ;; This is a flush.
	       (begin ;with-mutex buffer-mutex
		 (fasl-write obj-buffer (simulation-logger))
		 (set! num-objs 0)
		 (vector-fill! obj-buffer #f))]
	      [(obj)
	       (begin ;with-mutex buffer-mutex
		 (vector-set! obj-buffer num-objs obj)
		 (set! num-objs (fx+ num-objs)))
	       (if (= num-objs (vector-length obj-buffer))
		   (buffered-writer))]))
	  #f))


    (define (do-logging input)
      (mvlet ([(level ob args)
	       (match input
		 [(,lvl ,str ,args ...)
		  (guard (number? lvl) (string? str))
		  (values lvl str args)]
		 [(,str ,args ...) (guard (string? str))
		  (values 1 str args)]
		 [(,lvl ,time ,nodeid ,sym ,pairs ...)
		  (guard (number? lvl) (number? time) ;(integer? nodeid) 
			 (symbol? sym) (andmap list? pairs))
		  (values lvl (cons time (cons nodeid (cons sym pairs))) '())]
		 [,else (error 'logger "invalid arguments: ~a" input)]
		 )]
	      [(port) (simulation-logger)])
	(if (simulation-logger-human-readable)
	    (display (log-line->human-readable level ob args) port)
	    (begin
	      (if (null? args)
		  (write ob port)
		  (write (apply format ob args) port))
	      (newline port)
	      ;; TEMP TEMP TEMP FIXME:
	      ;(flush-output-port port)
	      ))))
    ;; Body of logger:
    (lambda input
      (when (simulation-logger)
	(do-logging input)))))

;; This is just another variant:
;; This has no critical section for now!!! [2005.02.25]
(define-syntax with-logger
  (syntax-rules ()
      [(_ exp ...)
      (if (simulation-logger)
	  (parameterize ([current-output-port (simulation-logger)])
			exp ...))]))

;; This makes it use a lame sort of text display instead of the graphics display:
;(define-regiment-parameter simulator-output-text #f (lambda (x) x))
;; This is a SEPERATE LOGGER for debug info as opposed to simulation events.
;; [2005.10.25]  This doesn't appear to be used
; (define-regiment-parameter sim-debug-logger 
;   (lambda args
;     (begin ;critical-section
;      (apply printf args)))
;   (lambda (x)
;     (unless (procedure? x)
; 	    (error 'simulator-debug-logger "~s is not a procedure" x))
;     x))
; (define-syntax silently
;   (syntax-rules ()
;     [(_ expr ...) (parameterize ([sim-debug-logger (lambda args (void))])
; 		    expr ...)]))



;; #f trumps any time, EXCEPT 0, 0 trumps all.
(define (evntlessthan a b)
  (vtimelessthan (simevt-vtime a) (simevt-vtime b)))

(define (vtimelessthan at bt)
  (cond
   [(eq? at 0) #t]
   [(eq? bt 0) #f]
   [(not at) #t]
   [(not bt) #f]
   [else (<= at bt)]))


(define global-graph (make-parameter #f (lambda (x) x)))

;; Global parameter to hold globally returned values:
(define soc-return-buffer
  (make-parameter '()
		  (lambda (ls) ls)))

;; Global parameter contains continuation for exiting the alpha-sim.  Invoked by soc-finished.
(define escape-alpha-sim
  (make-parameter (lambda (x) (error 'escape-alpha-sim "parameter holds no continuation"))
		  (lambda (k) (if (procedure? k) k
				  (error 'escape-alpha-sim "bad continuation: ~a" k)))))

;; These shouldn't need to be reset after/before a run of the simulator.
(define reverse-table (make-default-hash-table))
;; This is not going to *change* over the course of our evaluation:
(define max-positive (most-positive-fixnum))
(define (token->key t)
  (DEBUGMODE
   (if (not (simtok? t))
       (error 'token->key "This is not a simtok object: ~s" t)))
  (let ((n (hash (list (simtok-name t) (simtok-subid t)) max-positive)))
    (hashtab-set! reverse-table n t)
    ;(disp " Token->key " t n)
    n))

(define (key->token k)
  (hashtab-get reverse-table k))

;; Helper to determine the distance between two 2d positions.
(define (posdist a b)
  (sqrt (+ (expt (- (car a) (car b)) 2)
	   (expt (- (cadr a) (cadr b)) 2))))
