
;; This is written in totally imperative style.
;; One buffer and one vtime, no reason to thread them otherwise.
(define (run-alpha-simple-scheduler sim stopping-time?)
  
  (define soc (car (filter (lambda (n) (eq? BASE_ID (node-id (simobject-node n))))
			   (simworld-all-objs sim))))

  ;; Constant: amount of virtual time consumed by an action.  Nonzero to force forward progress.
  (define ACTION_LENGTH 1)  ;; Thus we ignore the "duration" field of simevts.

  (define SCHEDULE_DELAY 1)

  (define buffer '()) ;; Contains simevts
  (define vtime 0)    ;; Clock for the whole simulator.

  (define (pop)
    (if (null? buffer)
	(error 'alpha-lib:build-node-sim "Can't pop from null scheduling queue"))
    (logger 3 "~a: Popped off action: ~a at vtime ~a ~n" 
	    (node-id (simobject-node ob))
	    (msg-object-token (simevt-msgobj (car buffer)))
	    (simevt-vtime (car buffer)))
    (set! buffer (cdr buffer)))

  ;; Is called with the local time that the scheduling hapens.
  ;; New events may have times in the future, but should not have times in the past.
  (define (schedule . newevnts)        
    (DEBUGMODE
     (for-each (lambda (ne)
		 (if (vtimelessthan (simevt-vtime ne) current-vtime)
		     (logger 0 "~a: ERROR: Scheduled event has time in past (now ~a): ~a"
			     (node-id (simobject-node ob))
			     current-vtime
			     (list (simevt-vtime ne) (msg-object-token (simevt-msgobj ne))))))
	       newevnts))
    ;; We tag specific times on those "ASAP" events without them.
    (let ([timedevnts
	   (map (lambda (e) (make-simevt (let ((t (simevt-vtime e)))
					   (if t t (+ SCHEDULE_DELAY vtime)))
					 (simevt-duration e)
					 (simevt-msgobj e)))
		newevnts)])
      (unless (null? newevnts)
	      (set! buffer (merge evntlessthan copies buffer))
	      ;; Setting the new time will reorder the buffer rationally
	      ;;(private-scheduler 'catchup-time current-vtime)
	      (logger 3 "~a: Scheduling ~a new events ~a, new schedule: ~a~n"
		      (node-id (simobject-node ob))
		      (length newevnts)
		      (map (lambda (e) (msg-object-token (simevt-msgobj e))) newevnts)
		      (map (lambda (e) (list (simevt-vtime e) (msg-object-token (simevt-msgobj e))))
			   buffer))
	      )))

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
      (apply schedule 'schedule localclock timed)
      (apply schedule 'schedule localclock local)
      (apply schedule 'schedule localclock incoming)))

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

	(let ((neighbors (graph-neighbors (simworld-object-graph world) ob)))
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


  (define (run-msg m)
    (void)
    )

  ;; ======================================================================
  (let main-sim-loop ()
      (logger 2 "  Main sim loop: vtime ~a (vtime of last action)~n" vtime)      
      (printf "<~a>" vtime)
      (cond
       [(stopping-time? vtime) (printf "Out of time.~n")]
       [(null? buffer) (printf "Simulator ran fresh out of actions!~n")]
       [else 
	;; First process all incoming-buffers:
	(for-each process-incoming (simworld-all-objs sim))
	;; Run this atomic action (token handler), it gets to run at its intended virtual time.
	;; We have lamely stored it in the msgobj field:
	(run-msg (simevt-msgobj (car buffer)))
	(pop)
	;; Advance the clock:
	(if (not (null? buffer)) (set! vtime (simevt-vtime (car buffer))))
	;; Finally, push outgoing-buffers to incoming-buffers:
	(for-each launch-outgoing (simworld-all-objs sim))
	
	(main-sim-loop)])))
    





    ;;========================================    
    ;; MAIN BODY:
    (mvlet ([(mhandler cost-table) (node-code ob)])
    ;; Install the scheduler and handler incase anybody else wants to use them:
    (set-simobject-scheduler! ob private-scheduler)
    (set-simobject-meta-handler! ob mhandler)

    ;; Clear out the buffers from any prior simulations:
    (set-simobject-local-msg-buf! ob '())
    (set-simobject-timed-token-buf! ob '())
    (set-simobject-outgoing-msg-buf! ob '())
    ;; The incoming buffer starts out with just the start actions SOC-start and node-start.
    (set-simobject-local-msg-buf! ob				
	(list (make-simevt 0
			   (cadr (assq 'node-start cost-table))
			   (bare-msg-object 'node-start '() 0))))
    (if (simobject-I-am-SOC ob)
	(set-simobject-local-msg-buf! ob
	   (cons (make-simevt 0
			      (cadr (assq 'node-start cost-table))
			      (bare-msg-object 'SOC-start '() 0))
		 (simobject-local-msg-buf ob))))

    ;; This is the simulation object, each time its executed it
    ;; processes incoming messages for the nodes, decides what action
    ;; is next and returns that action (as a simevt).  If there's no
    ;; next action, returns #f.  

    ;; If the simulation driver wishes to execute that action, it
    ;; does, which will run the handler, update the scheduler, and
    ;; modify the simobject.
    (lambda (global-mintime)

      (define scheduler (simobject-scheduler ob))
      (define ourtime_starting (scheduler 'get-time))
      
      ;; If our local clock has fallen behind the real one, advance it.
      ;; Ourtime represents the actual start time at which this potential action will run.
      (define ourtime
	(if (>= ourtime_starting global-mintime)
	    ourtime_starting 
	    (begin (logger "~a: Fell behind global timer (our ~a trailing global ~a).  Advancing.~n"
			   (node-id (simobject-node ob))
			   ourtime_starting global-mintime)
		   (scheduler 'catchup-time global-mintime)
		   global-mintime)))

      ;; First schedule any incoming messages we've received
      (process-incoming ourtime)

      (let ([next (scheduler 'head)])
	(if (not next) 
	    #f    ;; We just fizzle if our schedule is empty.  
            (begin 
	      (logger 3 "~a: Updated schedule, got head: vt~a d~a ~a, buffer ~a~n" 
		      (node-id (simobject-node ob))
		      (simevt-vtime next) (simevt-duration next)
		      (msg-object-token (simevt-msgobj next)) 
		      (map (lambda (evt) (list (simevt-vtime evt)
					       (msg-object-token (simevt-msgobj evt))))
			   (scheduler 'get-buffer)))
	      (make-simevt
               (simevt-vtime next)
               (simevt-duration next)
               ;; Action thunk that executes message:
               (lambda ()
		 (DEBUGMODE
		  (if (not (= ourtime (scheduler 'get-time)))
		     (logger 0 ; 'build-node-sim
			    "~a: ERROR: Local time changed between getting the new head and executing it!  orig time ~a, current-time ~a~n"
			    (node-id (simobject-node ob))
			    ourtime
			    (scheduler 'get-time))))

                 ;(printf "Busting thunk, running action: ~a~n" next)
                 ;; For now, the time actually executed is what's scheduled
                 (logger "~a: Executing: ~a at scheduled time ~a, global/mintime ~a,  localclock ~a~n" 
			 (node-id (simobject-node ob))
			 (msg-object-token (simevt-msgobj next))
			 (simevt-vtime next)
			 global-mintime
			 (scheduler 'get-time))

		 '(DEBUGMODE ;; check invariant:
		   (if (not (null? (simobject-outgoing-msg-buf ob)))
		     (error 'build-node-sim 
			    "Trying to execute action at time ~a, but there's already an outgoing(s) msg: ~a~n"
			    global-mintime (simobject-outgoing-msg-buf ob))))

		 ;; Do the actual computation:
                 ((simobject-meta-handler ob) (simevt-msgobj next) (simevt-vtime next))
                 ;; Now that the atomic action is finished, do the radio transimission:		 
		 ;; The actual time that outgoing messages are launched is at the 
		 ;; end of the atomic actions completion:
                 (launch-outgoing (+ ourtime (simevt-duration next)))
                 
                 ;; Finally we must tell our scheduler that we have executed this action:
                 ;; First, advance the clock appropriately.
                 (scheduler 'advance-time (simevt-duration next))
                 (scheduler 'pop)  ;; Next, pop off that action since we're done with it.
                 )))))))))

