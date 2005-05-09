
;; Doing what Matt said and simplifying.
;; This took little time and appears to work.  Fantastic.


;; This is written in totally imperative style.
;; One buffer and one vtime, no reason to thread them otherwise.
(define (run-alpha-simple-scheduler sim node-code-fun stopping-time? meta-port)
  
;  (define SOC (car (filter (lambda (n) (eq? BASE_ID (node-id (simobject-node n))))
;			   (simworld-all-objs sim))))

  ;; Constant: amount of virtual time consumed by an action.  Nonzero to force forward progress.
  (define ACTION_LENGTH 1)  ;; Thus we ignore the "duration" field of simevts.

  (define SCHEDULE_DELAY 1)

  (define buffer '()) ;; Contains pairs (simevt . simob) where simob is the object handling the event.
  (define vtime 0)    ;; Clock for the whole simulator.

  (define (pop)
    (if (null? buffer)
	(error 'alpha-lib:build-node-sim "Can't pop from null scheduling queue"))
    (logger 3 "Popped off action: ~a at vtime ~a ~n" 
	    (msg-object-token (simevt-msgobj (caar buffer)))
	    (simevt-vtime (caar buffer)))
    (set! buffer (cdr buffer)))

  (define (lessthan a b) (evntlessthan (car a) (car b)))

  ;; Is called with the local time that the scheduling hapens.
  ;; New events may have times in the future, but should not have times in the past.
  (define (schedule ob . newevnts)        
    (DEBUGMODE
     (for-each (lambda (ne)
		 (if (and (simevt-vtime ne) (< (simevt-vtime ne) vtime))
		     (logger 0 "ERROR: Scheduled event has time in past (now ~a): ~a~n"
			     vtime
			     (list (simevt-vtime ne) (msg-object-token (simevt-msgobj ne))))))
	       newevnts))
    ;; We tag specific times on those "ASAP" events without them.
    (let ([timedevnts
	   (map (lambda (e) 
		  (cons
		   (make-simevt (let ((t (simevt-vtime e)))
				  (if t t (+ SCHEDULE_DELAY vtime)))
				;(simevt-duration e)
				(simevt-msgobj e))
		   ob))
		newevnts)])
      (unless (null? newevnts)
	      (set! buffer (merge lessthan timedevnts buffer))

	      (logger 3 "Scheduling ~a new events ~a, new schedule len: ~a~n"
		      (length newevnts)
		      (map (lambda (e) 
			     (list (msg-object-token (simevt-msgobj e))
				   (simevt-vtime e)))
			   newevnts)
		      (+ (length buffer) (length newevnts))
		      ;(map (lambda (e) (list (simevt-vtime (car e)) (msg-object-token (simevt-msgobj (car e)))))  buffer)
		      )
	      )))

  ;; Initializes some of the simobject's state.
  (define (init-simobject ob)
    (let ([mhandler (node-code-fun ob)])

    ;; Install null scheduler and the handler:
    (set-simobject-scheduler! ob #f)
    (set-simobject-meta-handler! ob mhandler)

    ;; Clear out the buffers from any prior simulations:
    (set-simobject-local-msg-buf! ob '())
    (set-simobject-timed-token-buf! ob '())
    (set-simobject-outgoing-msg-buf! ob '())
    ;; The incoming buffer starts out with just the start actions SOC-start and node-start.
    (set-simobject-local-msg-buf! ob				
	(list (make-simevt 0
;			   #f ; ignored
			   (bare-msg-object (make-simtok 'node-start 0) '() 0))))
    (if (= BASE_ID (node-id (simobject-node ob)))
	(set-simobject-local-msg-buf! ob
	   (cons (make-simevt 0
;			      #f ; ignored
			      (bare-msg-object (make-simtok 'SOC-start 0) '() 0))
		 (simobject-local-msg-buf ob))))
    ))


  ;; This processes the incoming messages on a given simobject, and
  ;; schedules them in the global scheduler.
  (define (process-incoming ob)
    (if (not (null? (append (simobject-local-msg-buf ob)
			    (simobject-timed-token-buf ob)
			    (simobject-incoming-msg-buf ob))))
	(logger 1.5 "~a: Receiving (t:~a): ~a local, ~a timed, ~a remote. Buffer len ~a~n"		  
		(node-id (simobject-node ob))
		vtime
		(map (lambda (x) (msg-object-token (simevt-msgobj x))) (simobject-local-msg-buf ob))
		(map (lambda (x) (msg-object-token (simevt-msgobj x))) (simobject-timed-token-buf ob))
		(map (lambda (x) (msg-object-token (simevt-msgobj x))) (simobject-incoming-msg-buf ob))
		(length buffer)
		))

    (let ([timed (simobject-timed-token-buf ob)]
	  [local (simobject-local-msg-buf ob)]
	  [incoming (simobject-incoming-msg-buf ob)])
      (set-simobject-local-msg-buf! ob '())
      (set-simobject-timed-token-buf! ob '())
      (set-simobject-incoming-msg-buf! ob '())

      (DEBUGMODE
	(if (not (andmap simevt? (append timed local incoming)))
	    (printf "NOT ALL SIMEVT ~a ~a ~a~n" 
		    timed local incoming)))
      
      ;; Process incoming and local msgs:
      ;; Schedule timed local tokens:
      (apply schedule ob (append timed local incoming))))

  ;; This scrapes the outgoing messages off of a simobject and puts them in the global scheduler.
  (define (launch-outgoing ob)
      ;; This does the "radio transmission", and puts msgs in their respective incoming buffers.
      ;; TODO: Here's where we insert the better radio model!!!
      (let ([outgoing (simobject-outgoing-msg-buf ob)])
	
	(unless (null? outgoing)
	;; They're all broadcasts for now
	(for-each (lambda (evt)
		    ;; Timestame the message:
		    (set-msg-object-sent-time! (simevt-msgobj evt) vtime)
		    ;(let ([newmsg (structure-copy (simevt-msgobj evt))])
		    )
		  outgoing)

	(let ((neighbors (graph-neighbors (simworld-object-graph sim) ob)))
	  (logger "~a: bcast ~a at time ~a to -> ~a~n" 
		  (node-id (simobject-node ob)) 
		  (map (lambda (m) (msg-object-token (simevt-msgobj m))) outgoing)
		  vtime
		  (map (lambda (x) (node-id (simobject-node x))) neighbors))
	  
	  (for-each 
	   (lambda (nbr)
	     (set-simobject-incoming-msg-buf! 
	      nbr (append  outgoing
			   (simobject-incoming-msg-buf nbr))))
	   neighbors)
	;; They're all delivered, so we clear our own outgoing buffer.
	  (set-simobject-outgoing-msg-buf! ob '())))))

  ;; ======================================================================
  ;; First Initialize.
  (for-each init-simobject (simworld-all-objs sim))
  ;; Then, run loop.
  (let main-sim-loop ()
    ;; First process all incoming-buffers, scheduling events.
    (for-each process-incoming (simworld-all-objs sim))
    (cond
     [(stopping-time? vtime) 
      ;; This is a meta-message, not part of the output of the simulation:
      (fprintf meta-port "Out of time.~n")]
     [(null? buffer)
      (fprintf meta-port "~n<-------------------------------------------------------------------->~n")
      (fprintf meta-port "Simulator ran fresh out of actions!~n")]
     [else 
      (let ([ob (cdar buffer)]
	    [evt (caar buffer)])	
	;; Set the clock to the time of this next action:
	(set! vtime (simevt-vtime (caar buffer)))  
	(logger 2 "  Main sim loop: vtime ~a (vtime of next action) buffer len ~a ~n" vtime (length buffer))
	;(printf "<~a>" vtime)

                 ;(printf "Busting thunk, running action: ~a~n" next)
                 ;; For now, the time actually executed is what's scheduled
                 (logger "~a: Executing: ~a at time ~a~n"
			 (node-id (simobject-node ob))
			 (msg-object-token (simevt-msgobj (caar buffer)))
			 vtime)

		 '(DEBUGMODE ;; check invariant:
		   (if (not (null? (simobject-outgoing-msg-buf ob)))
		     (error 'build-node-sim 
			    "Trying to execute action at time ~a, but there's already an outgoing(s) msg: ~a~n"
			    global-mintime (simobject-outgoing-msg-buf ob))))
	
	;; Now the lucky simobject gets its message.
	((simobject-meta-handler ob) (simevt-msgobj evt) vtime)


	;; Then we discard that event.
	(pop)
	;; Finally, we push outgoing-buffers to incoming-buffers:
	(for-each launch-outgoing (simworld-all-objs sim))	
	(main-sim-loop))]))
  )
