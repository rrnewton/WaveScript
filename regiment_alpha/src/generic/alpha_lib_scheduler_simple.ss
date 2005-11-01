
;; alpha_lib_scheduler_simple.ss
;; This file contains a single function (the scheduler) used by alpha_lib.ss

;; Doing what Matt said and simplifying.  No reason for complex scheduling.
;; This took little time and appears to work.  Fantastic.

;===============================================================================
;; Changes:

;; [2005.09.27] Changing the scheduler to use a queue in the global
;; sim instead of a piece of local state.  This allows other code to
;; see the queue.

;===============================================================================

;; This is written in totally imperative style.
;; One schedule-queue and one vtime, no reason to thread them otherwise.
(define (run-alpha-simple-scheduler sim node-code-fun stopping-time? meta-port)
  ;; Inputs:
  ;; Output: 


;  (define SOC (car (filter (lambda (n) (eq? BASE_ID (node-id (simobject-node n))))
;			   (simworld-all-objs sim))))

  ;; Buffer is the scheduling queue:
  ;(define buffer '()) ;; Contains pairs (simevt . simob) where simob is the object handling the event.
  ;; First refactoring to abstract gets/sets to queue:
  ;(define (set-queue! x) (set! buffer x))
  ;(define (get-queue) buffer)
  ;; That passed all regression tests.  NOW REFACTORING TO USE (simworld-scheduler-queue sim) !!!
  (define (set-queue! x) (set-simworld-scheduler-queue! sim x))
  (define (get-queue) (simworld-scheduler-queue sim))

  (define vtime 0)    ;; Clock for the whole simulator.

  (define (pop)
    (let ((queue (get-queue)))
      (if (null? queue)
	  (error 'alpha-lib:build-node-sim "Can't pop from null scheduling queue"))
      (logger 3 "~s Popped off action: ~s at vtime ~s ~n"
	      (pad-width 5 (simevt-vtime (caar queue)))
	      (msg-object-token (simevt-msgobj (caar queue)))
	      (simevt-vtime (caar queue)))
      (set-queue! (cdr queue))))

  (define (lessthan a b) (evntlessthan (car a) (car b)))

  ;; ================================================================================
    ;; This is called with the local time that the scheduling hapens.
    ;; New events may have times in the future, but should not have times in the past.
    (define (schedule ob . newevnts)
      ;; ASSUME: all the new events are already tagged with concrete times.
      (DEBUGASSERT (simobject? ob))
      (DEBUGMODE     
       (for-each (lambda (ne)
		   (if (and (simevt-vtime ne) (< (simevt-vtime ne) vtime))
		       (logger 0 "ERROR: Scheduled event has time in past (now ~s): ~s~n"
			       vtime
			       (list (simevt-vtime ne) (msg-object-token (simevt-msgobj ne))))))
		 newevnts))
      (let ([pairedevnts (map (lambda (x) (cons x ob)) newevnts)])
	(unless (null? newevnts)
		;; [2005.05.31] I'm having a scheduling bug, so just to be careful I'm sorting these:
		;; Before I merely merged them:
		(IFDEBUG ;; [2005.10.17] Profiling indicates this wastes a lot of time.  Only doing this in debug mode.
		 (set-queue! (sort lessthan (append pairedevnts (get-queue))))
		 (set-queue! (merge! lessthan (sort lessthan pairedevnts) (get-queue))))

		;; TEMP:
		#;
		(DEBUGMODE
		 (let ((current-times (map simevt-vtime (map car (get-queue)))))
		   ;(fprintf (console-output-port) "~s\n\n" current-times )
		   (if (not (equal? current-times (sort < current-times)))
		       (error 'run-alpha-simple-scheduler
			      "Queue was in an invalid state at time ~s:  events not in order:\n ~s"
			      current-vtime current-times))))
		#;
		(DEBUGMODE
		 (if (not (equal? 
			   (map simevt-vtime (map car 
			   (sort lessthan (append pairedevnts (get-queue)))))
			   (map simevt-vtime (map car 
						  (merge lessthan (sort lessthan pairedevnts) (get-queue))))))
		     (error 'run-alpha-simple-scheduler 
			    "Something is wrong with merge or evntlessthan: merged in ~s and \n ~s \n not same as : \n ~s\n"
			    (map simevt-vtime (map car pairedevnts))
			    (map simevt-vtime (map car 
				 (sort lessthan (append pairedevnts (get-queue)))))
			    (map simevt-vtime (map car
				 (merge lessthan pairedevnts (get-queue)))))))


		;(printf "Woot: ~s\n" (map simevt-vtime (map car (get-queue))))

		(logger 3 "~s  Scheduling ~s new events ~s, new schedule len: ~s~n"
			(pad-width 5 vtime) ;(apply min (map simevt-vtime newevnts)))
			(length newevnts)
			(map (lambda (e) 
			       (list (msg-object-token (simevt-msgobj e))
				     (simevt-vtime e)))
			     newevnts)
			(+ (length (get-queue)) (length newevnts))
			;(map (lambda (e) (list (simevt-vtime (car e)) (msg-object-token (simevt-msgobj (car e)))))  buffer)
			)
		)))
    
    ;; ================================================================================
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
    ;; Node-start executes before SOC-start.
    (set-simobject-local-msg-buf! ob				
	(list (make-simevt 0
;			   #f ; ignored
			   (bare-msg-object (make-simtok 'node-start 0) '() 0))))
;     (if (= BASE_ID (node-id (simobject-node ob)))
; 	(set-simobject-local-msg-buf! ob
; 				      (append 
; 				       (simobject-local-msg-buf ob)
; 				       (list (make-simevt 0
; 					;			      #f ; ignored
; 							  (bare-msg-object (make-simtok 'SOC-start 0) '() 0)))
; 				       )))
    ))


  ;; ================================================================================
  ;; This processes the incoming messages on a given simobject, and
  ;; schedules them in the global scheduler.
  ;; [2005.10.31]  Currently we allow actions scheduled in the past!  (They're considered "overdue".)
  ;; Overdue actions can't actually occur in the past.  They're just given scheduling priority.
  (define (process-incoming ob)
    (if (not (null? (append (simobject-local-msg-buf ob)
			    (simobject-timed-token-buf ob)
			    (simobject-incoming-msg-buf ob))))
	(logger 1.5 "~s ~s: Receiving: ~s local, ~s timed, ~s remote. Queue len ~s~n"
		(pad-width 5 vtime)
		(node-id (simobject-node ob))
		(map (lambda (x) (msg-object-token (simevt-msgobj x))) (simobject-local-msg-buf ob))
		(map (lambda (x) (msg-object-token (simevt-msgobj x))) (simobject-timed-token-buf ob))
		(map (lambda (x) (msg-object-token (simevt-msgobj x))) (simobject-incoming-msg-buf ob))
		(length (get-queue))
		))

    (let ([timed (simobject-timed-token-buf ob)]
	  [local (simobject-local-msg-buf ob)]
	  [incoming (simobject-incoming-msg-buf ob)])
      (set-simobject-local-msg-buf! ob '())
      (set-simobject-timed-token-buf! ob '())
      (set-simobject-incoming-msg-buf! ob '())

      (DEBUGMODE
	(if (not (andmap simevt? (append timed local incoming)))
	    (printf "NOT ALL SIMEVT ~s ~s ~s~n" 
		    timed local incoming)))
      
;      (if (not (null? incoming)) (disp "INCOMING TIMES:" (map simevt-vtime incoming)))
            
      ;; We tag specific times on those "ASAP" events without them.
      (let ([timedevnts
	     (append timed		     
		     (map (lambda (e) 
			    (make-simevt (let ((t (simevt-vtime e)))
					   (if t t 
					       (+ SCHEDULE_DELAY vtime)))
					 (simevt-msgobj e)))	  ;(simevt-duration e)
		       local)
		     (map (lambda (e) 
			    (make-simevt 
			     (let ((t (simevt-vtime e)))
			       (if t
				   (max t (+ RADIO_DELAY SCHEDULE_DELAY vtime))
				   (+ RADIO_DELAY SCHEDULE_DELAY vtime)))
			     (simevt-msgobj e)))
			  incoming))])

;; FIXME: Should we do a little sorting of the events here?

	;; Process incoming and local msgs:
	;; Schedule timed local tokens:
	(apply schedule ob timedevnts))))


  ;; ================================================================================
  ;; This scrapes the outgoing messages off of a simobject and puts them in the global scheduler.
  (define (launch-outgoing ob)
      ;; This does the "radio transmission", and puts msgs in their respective incoming buffers.
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
	  (logger "~s ~s: bcast ~s to -> ~s~n" 
		  (pad-width 5 vtime )
		  (node-id (simobject-node ob)) 
		  (map (lambda (m) (msg-object-token (simevt-msgobj m))) outgoing)
		  (map (lambda (x) (node-id (simobject-node x))) neighbors))
	  
	  (for-each 
	   (lambda (nbr)
	     ;; Here's where we simulate the channel and determine if the message goes through.
	     (let ((connectivity ((simalpha-connectivity-function) 
				  (node-pos (simobject-node ob))
				  (node-pos (simobject-node nbr)))))
#;
	       (printf "Checking connectivity: ~s ~s ~s\n"
		       connectivity
		       (simobject-node ob)
		       (simobject-node nbr))
	       (if (cond
		    [(fixnum? connectivity)
		     (fx< (reg:random-int 100) connectivity)]
		    [(procedure? connectivity)
		     (fx< (reg:random-int 100) (connectivity vtime))]
		    [else 
		     (error 'launch-outgoing "bad connectivity function result: ~s" connectivity)])
		   (set-simobject-incoming-msg-buf! 
		    nbr (append  outgoing
				 (simobject-incoming-msg-buf nbr)))
		   ;; Otherwise fizzle:
		   (void))))
	   neighbors)
	;; They're all delivered, so we clear our own outgoing buffer.
	  (set-simobject-outgoing-msg-buf! ob '())))))

  ;; ======================================================================
  ;; First Initialize.
  (for-each init-simobject (simworld-all-objs sim))
  (set-queue! '()) ;; Start with no scheduled events.

  ;; ======================================================================
  ;; Then, run loop.
  (let main-sim-loop ()
    ;; First process all incoming-buffers, scheduling events.
    (for-each process-incoming (simworld-all-objs sim))
    (cond
     [(null? (get-queue))
      (fprintf meta-port "~n<-------------------------------------------------------------------->~n")
      (fprintf meta-port "Simulator ran fresh out of actions!~n")]
     [(stopping-time? (simevt-vtime (caar (get-queue))))
      ;; This is a meta-message, not part of the output of the simulation:
      (fprintf meta-port "Out of time.~n")]
     [else 
      (let ([first (car (get-queue))])
	;; Now discard that event from the queue:
	(pop)

	(let ([ob (cdr first)]
	      [evt (car first)])

	  ;; Set the clock to the time of this next action:
	  ;; HOWEVER, the clock can't move backwards:
	  (set! vtime (max vtime (simevt-vtime evt)))

	  ;; Also copy this value to the simworld object so alpha_lib closures can get to it:
	  (set-simworld-vtime! sim vtime)

	(logger 2 "~s  Main sim loop: (vtime of next action) queue len ~s ~n" 
		(pad-width 5 vtime) (add1 (length (get-queue))))
	;(printf "<~s>" vtime)

                 ;(printf "Busting thunk, running action: ~s~n" next)
                 ;; For now, the time actually executed is what's scheduled
	
	;; This might print big structures, keep it tight:
	(parameterize ([print-level 5]
		       [print-length 15]
		       [print-graph #t])
	(logger "~s ~s: Executing: ~s args: ~s~n"
		(pad-width 5 vtime)
		(node-id (simobject-node ob))
		(msg-object-token (simevt-msgobj evt))
		(msg-object-args (simevt-msgobj evt))
		))
	
	'(DEBUGMODE ;; check invariant:
	  (if (not (null? (simobject-outgoing-msg-buf ob)))
	      (error 'build-node-sim 
		     "Trying to execute action at time ~s, but there's already an outgoing(s) msg: ~s~n"
		     global-mintime (simobject-outgoing-msg-buf ob))))
	
	;; Now the lucky simobject gets its message.
	(set! simalpha-total-tokens (add1 simalpha-total-tokens))
	((simobject-meta-handler ob) (simevt-msgobj evt) vtime)

	;; Finally, we push outgoing-buffers to incoming-buffers:
	(for-each launch-outgoing (simworld-all-objs sim))
	(main-sim-loop)))]))
  )
