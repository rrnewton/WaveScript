

;; This builds a simulation object for a specific node in the network.
;; This object drives the simulation for that node.  
;; When the simulation object is invoked once, it processes all
;; incoming and queued messages and returns the next action to be executed.
;; The execution of that atomic action (token handler) produces new messages
;; both in *this* simobject, and in the incoming fields of other simobjects.

;; Matt's right, this is more complexity than was necessary.
;; The local-time/global-time distinction is confusing.

;; It returns this simulation object as a thunk.
;; The thunk produces simevt's, but with action-thunks in the msgobj field 
(define build-node-sim
  (lambda (ob world node-code)
    ;; the scheduling queue contains event entries of the format:
    ;;   [<vtime-to-exec>, <duration>, <msg-object>]
    ;; which are also returned from sim-seed in this format.
    
      ;; Schedule either adds an event to the schedule, 
      ;; or returns the current schedule in list form.
    (define private-scheduler
      (let ([buffer '()] ;; Contains simevts
            [private-vtime 0]) ;; Local clock
	(lambda args
	  (match args 
	    [(get-buffer) buffer]
	    [(get-time) private-vtime]

	    ;; The head action on the buffer (next to execute) must be
	    ;; scheduled for a time g/equal the current local clock
	    ;; time.
            [(head) (if (null? buffer) #f
                        (let ([next (car buffer)])
			  (DEBUGMODE 
			   (if (and (simevt-vtime next)
				    (< (simevt-vtime next) private-vtime))
			       (logger 0 
				       "~a: WARNING: token event ~a has time in the past (~a) relative to current local clock (~a)~n"
				       (node-id (simobject-node ob))
				       (msg-object-token (simevt-msgobj next))
				       (simevt-vtime next)
				       private-vtime)))

                          (make-simevt
                           (if (simevt-vtime next)
			       ;; If for some reason it was scheduled earlier than the current clock time,
			       ;; bring it up to that time!
                               (max (simevt-vtime next) private-vtime)
                               private-vtime)
                           (simevt-duration next)
                           (simevt-msgobj next))))]

	    [(pop)
	     (if (null? buffer)
		 (error 'alpha-lib:build-node-sim "Can't pop from null scheduling queue"))
	     (logger 3 "~a: Popped off action: ~a at vtime ~a ~n" 
		     (node-id (simobject-node ob))
		     (msg-object-token (simevt-msgobj (car buffer)))
		     (simevt-vtime (car buffer)))
	     (set! buffer (cdr buffer))]

	    ;; This is a questionable method.  When we have a gap in
	    ;; our local schedule, the only way we time-advance is
	    ;; when the "global clock" runs along past ours, and then
	    ;; we set ours to catch up with it.
            [(catchup-time ,globvtime)
	     (logger 3 "~a: Catchup time: ~a ~n"
		     (node-id (simobject-node ob))
		     globvtime)
             (private-scheduler 'advance-time (- globvtime private-vtime))]

	    [(advance-time ,increment)
	     (if (< increment 0)
		 (logger 2 "~a: WARNING, attempted negative time advance by ~a~n"
			 (node-id (simobject-node ob))
			 increment))
	     ;; Here we update the schedule to deal with the new local clock.
             (when (> increment 0) ;; Fizzle if the increment is not positive
	       (logger 3 "~a: Incrementing time by ~a, from ~a ~n"
		       (node-id (simobject-node ob))
		       increment private-vtime)
               (set! private-vtime (+ private-vtime increment))
	       ;; But with that new time, we have to see if we still have time 
	       (private-scheduler 'reconsider-schedule)
	       )]

	    ;; Slides the #f timed events around in the hard-timed events.
	    [(reconsider-schedule)
               ;; This is where we make sure we have time to run the
               ;; things we want to run before the next hard timing constraint:
               (mvlet ([(flexible rest) (split-before simevt-vtime buffer)])
                      (if (not (null? rest))
                          (let* ([nexttime (simevt-vtime (car rest))]


                                 [time-remaining (- nexttime private-vtime)])
                            (mvlet ([(cando letslide)
                                     (let loop ([acc '()] [budget time-remaining] [evts flexible])
                                       (cond 
                                         [(null? evts) (values flexible '())]
                                         [(< budget (simevt-duration (car evts)))
                                          ;; We can't afford it, stop here:
                                          (values (reverse! acc) evts)]
                                         [else (loop (cons (car evts) acc) 
                                                     (- budget (simevt-duration (car evts)))
                                                     (cdr evts))]))])
                                   (if (not (null? letslide))
                                       (set! buffer
                                             (append cando
                                                     (list (car rest))
                                                     letslide ;; Let them slide till after the timed event
                                                     (cdr rest))))))))]

	    ;; Is called with the local time that the scheduling hapens.
	    ;; New events may have times in the future, but should not have times in the past.
            [(schedule ,current-vtime . ,newevnts)
	     (DEBUGMODE
	      (for-each (lambda (ne)
			  (if (vtimelessthan (simevt-vtime ne) current-vtime)
			      (logger 0 "~a: ERROR: Scheduled event has time in past (now ~a): ~a"
				      (node-id (simobject-node ob))
				      current-vtime
				      (list (simevt-vtime ne) (msg-object-token (simevt-msgobj ne))))))
			newevnts))
	     
	     (unless (null? newevnts)
		(set! buffer (merge evntlessthan newevnts buffer))
		;; Setting the new time will reorder the buffer rationally
		;;(private-scheduler 'catchup-time current-vtime)
		(logger 3 "~a: Scheduling ~a new events ~a, new schedule: ~a~n"
			(node-id (simobject-node ob))
			(length newevnts)
			(map (lambda (e) (msg-object-token (simevt-msgobj e))) newevnts)
			(map (lambda (e) (list (simevt-vtime e) (msg-object-token (simevt-msgobj e))))
			     buffer))
		)]
	    
	    [,err (error 'build-node-sim "Did not understand message: ~a" err)]
	    ))))

    
    (define (process-incoming localclock)      
      (define schedule (simobject-scheduler ob))
      
      (if (not (null? (append (simobject-local-msg-buf ob)
			      (simobject-timed-token-buf ob)
			      (simobject-incoming-msg-buf ob))))			      
	  (logger 1.5 "~a: Receiving (t:~a): ~a local, ~a timed, ~a remote. Buffer: ~a~n"		  
		  (node-id (simobject-node ob))
		  localclock
		  (map (lambda (x) (msg-object-token (simevt-msgobj x))) (simobject-local-msg-buf ob))
		  (map (lambda (x) (msg-object-token (simevt-msgobj x))) (simobject-timed-token-buf ob))
		  (map (lambda (x) (msg-object-token (simevt-msgobj x))) (simobject-incoming-msg-buf ob))
		  (map msg-object-token (map simevt-msgobj (schedule 'get-buffer)))
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
      (apply schedule 'schedule localclock incoming))
      )
  
    (define (launch-outgoing current-vtime)
      ;; This does the radio transmission, and puts msgs in their respective incoming buffers.
      ;; TODO: Here's where we insert the better radio model!!!
      (let ([outgoing (simobject-outgoing-msg-buf ob)])
	
	(unless (null? outgoing)
	;; They're all broadcasts for now
	(for-each (lambda (evt)
		    ;; Timestame the message:
		    (set-msg-object-sent-time! (simevt-msgobj evt) current-vtime)
		    ;(let ([newmsg (structure-copy (simevt-msgobj evt))])
		    )
		  outgoing)

	(let ((neighbors (graph-neighbors (simworld-object-graph world) ob)))
	  (logger "~a: bcast ~a at time ~a to -> ~a~n" 
		  (node-id (simobject-node ob)) 
		  (map (lambda (m) (msg-object-token (simevt-msgobj m))) outgoing)
		  current-vtime
		  (map (lambda (x) (node-id (simobject-node x))) neighbors))
	  
	  (for-each 
	   (lambda (nbr)
	     (set-simobject-incoming-msg-buf! 
	      nbr (append  outgoing
			   (simobject-incoming-msg-buf nbr))))
	   neighbors)
	;; They're all delivered, so we clear our own outgoing buffer.
	  (set-simobject-outgoing-msg-buf! ob '())))))

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





(define (run-alpha-full-scheduler sim stopping-time?)
  (let* ([soc (car (filter (lambda (n) (eq? BASE_ID (node-id (simobject-node n))))
			       (simworld-all-objs sim)))]
	 [node-sims
	  (map (lambda (ob)
		 (build-node-sim ob
				 sim ;; the world
				 node-code ;; TODO: read from file
				 ))
	       (simworld-all-objs sim))])
	;; As this loop runs, the global (main-sim-loop) time is just the start time of the last executed action.
    (let/ec exit-loop
    (let main-sim-loop ([vtime 0])
      (logger 2 "  Main sim loop: vtime ~a (vtime of last action)~n" vtime)
      (logger 1.5 "  Local vtimes ~a~n" 
	      (map (lambda (ob) ((simobject-scheduler ob) 'get-time))
		   (simworld-all-objs sim)))
      
      (printf "<~a>" vtime)
      (if (stopping-time? vtime)
	  (printf "Out of time.~n")
;      (let ([nextevt
; 	     (let actionsloop ([sims node-sims] [best #f])
; 	       (if (null? sims) best
; 		   (let ([action ((car sims) vtime)])
; 		     (actionsloop (cdr sims)
; 				  (cond
; 				   [(not action) best]
; 				   [(not best) action]
; 				   [(evntlessthan action best) action]
; 				   [else best])))))])
; 	(if (not nextevt)
; 	    (printf "~n~a: Simulator ran fresh out of actions!~n"
; 		    'alpha-lib:run-alpha-sim)

	     ;; Less efficient but more functional way:
	      (let ([nextevt
		     (let ([actions (filter id (map (lambda (f) (f vtime)) node-sims))])
		       (if (null? actions)
			   (exit-loop 
			    (printf "~n~a: Simulator ran fresh out of actions!~n"
				    'alpha-lib:run-alpha-sim))
			   (let* ([nexttime (apply min (map simevt-vtime actions))])
			     (car (filter (lambda (se) (eq? nexttime (simevt-vtime se))) actions)))))])
	
	    (begin
	      ;; Run this atomic action (token handler), it gets to run at its intended virtual time.
              ;; We have lamely stored it in the msgobj field:
	      ((simevt-msgobj nextevt)) ;; This will execute the action and advance the clock internally.
              
	      ;; That had the effect of launching new messages to process.
	      (main-sim-loop (simevt-vtime nextevt)))))))
))
