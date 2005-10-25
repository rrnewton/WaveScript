;; simulator_alpha_datatypes.ss
;; [2005.10.18] This file encapsulates the datatype definitions and
;; global parameters used by the simulator.

;; ======================================================================

;; This structure contains all the global data needed a simulation.
(define-structure (simworld graph object-graph all-objs obj-hash scheduler-queue vtime))
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

;; This is just used to count up the messages during a simulation.
(define simalpha-total-messages 0) ;(make-parameter 0 (lambda (x) x)))
;; This one counts total token handlers fired.
(define simalpha-total-tokens 0)

;; Safer version:
(define (safe-construct-msg-object token timestamp parent args)
  ;(unless (token-name? token) (error 'safe-construct-msg-object "bad token name: ~s" token))
  (DEBUGMODE
   (unless (simtok? token) (error 'safe-construct-msg-object "bad token: ~s" token)) ;
   (unless (or (number? timestamp) (not timestamp))
	   (error 'safe-construct-msg-object "bad timestamp: ~s" timestamp))
   (unless (list? args)
	   (error 'safe-construct-msg-object "bad args: ~s" args)))
  (make-msg-object token timestamp #f parent args))

;; [2004.06.28] This is a helper to construct the locally used
;; messages that don't have a parent, timestamp, etc.
(define (bare-msg-object rator rands . time)  
  (safe-construct-msg-object rator ;; token
		   (if (null? time) #f (car time))    ;; timestamp
		   #f    ;; parent
		   rands))



;; This is our logger for events in the simulator:
;; TODO: make this a syntax so that the calls disappear entirely in non-debug mode.
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
;	     (column-width 4 (number->string current-vtime))
	     (column-width 3 level)
	     ))

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



(define global-graph #f)

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
  (let ((n (hash t max-positive)))
    (hashtab-set! reverse-table n t)
    ;(disp " Token->key " t n)
    n))

(define (key->token k)
  (hashtab-get reverse-table k))
