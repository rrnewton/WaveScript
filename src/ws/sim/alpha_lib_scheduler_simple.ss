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

;#cs ;; Case Sensitivity
(module alpha_lib_scheduler_simple mzscheme
  (require 
;   "iu-match.ss"
   (lib "include.ss")
   "alpha_lib.ss"
   "simulator_alpha_datatypes.ss"
   "../constants.ss"  
   "../compiler_components/logfiles.ss"
   "../../plt/chez_compat.ss"
   "../../plt/hashtab.ss"
   (all-except "../util/helpers.ss" test-this these-tests)
   (all-except "../compiler_components/wavescript_helpers.ss" test-this these-tests filter)
   )
  (provide 
   ;(all-defined)
   run-alpha-simple-scheduler
   )
  (chezimports)
  
 
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
  (define realtime #f) ;; This records the realtime corresponding to the vtime.

  (define (pop)
    (let ((queue (get-queue)))
      (if (null? queue)
	  (error 'alpha-lib:build-node-sim "Can't pop from null scheduling queue"))
      (logger 3 (simevt-vtime (caar queue)) '_
	      'Popped-off-action
	      `[token ,(msg-object-token (simevt-msgobj (caar queue)))])
      (set-queue! (cdr queue))))

  ; =================================================================================
  ; Throw away helpers:

  (define (lessthan a b) (evntlessthan (car a) (car b)))

  (define (token-table-entry ob evt)
    ; The vector is of the form #(invoked sent received)
    (let ((vec (hashtab-get (simobject-token-table ob) 
			    (simtok-name (msg-object-token (simevt-msgobj evt))))))
      (if (not vec)
	  (let ((vec (vector 0 0 0)))
	    (hashtab-set! (simobject-token-table ob) 
			  (simtok-name (msg-object-token (simevt-msgobj evt)))
			  vec)
	    vec)
	  vec)))

  ; =================================================================================
    ;; This is called with the local time that the scheduling hapens.
    ;; New events may have times in the future, but should not have times in the past.
  
    ;; [2005.11.03] Note just had a bug concerning INSTABILITY of
    ;; equally timed tokens in the queue.  Examining now.
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

		 ;; [2005.11.03]  The below line just caused an eggregious scheduling bug 
		 ;; thanks to its instability.  I'm not clear how it did that.  Sort should be stable!
		 ;(set-queue! (sort lessthan (append pairedevnts (get-queue))))

		 (set-queue! (merge! lessthan (sort lessthan pairedevnts) (get-queue)))
		 (set-queue! (merge! lessthan (sort lessthan pairedevnts) (get-queue)))
		 )

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

		(logger 3 vtime (node-id (simobject-node ob))
			'Scheduling
			;`[num ,(length newevnts)]
			`[new-events ,(map (lambda (e) 
					     (list (msg-object-token (simevt-msgobj e))
						   (simevt-vtime e)))
					newevnts)]
			`[new-schedule-len ,(+ (length (get-queue)) (length newevnts))])
		))) ;; end (schedule ob . newevnts)
    
    ; =================================================================================
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


  ; =================================================================================
  ;; This processes the incoming messages on a given simobject, and
  ;; schedules them in the global scheduler.
  ;; [2005.10.31]  Currently we allow actions scheduled in the past!  (They're considered "overdue".)
  ;; Overdue actions can't actually occur in the past.  They're just given scheduling priority.
  (define (process-incoming ob)
    (unless (and (null? (simobject-local-msg-buf ob))
		 (null? (simobject-timed-token-buf ob))
		 (null? (simobject-incoming-msg-buf ob)))
      (logger 2 vtime (node-id (simobject-node ob))
	      'ProcessingIncoming
	      `[local ,(map (lambda (x) (msg-object-token (simevt-msgobj x))) (simobject-local-msg-buf ob))]
	      `[timed ,(map (lambda (x) (msg-object-token (simevt-msgobj x))) (simobject-timed-token-buf ob))] 
	      `[remote ,(map (lambda (x) (msg-object-token (simevt-msgobj x))) (simobject-incoming-msg-buf ob))] 
	      `[QueueLen ,(length (get-queue))]))

    (let ([timed (simobject-timed-token-buf ob)]
	  [local (simobject-local-msg-buf ob)]
	  [incoming (simobject-incoming-msg-buf ob)])

      ; Filter incoming for messages directed to this node (or broadcast 
;      (set! incoming (filter (lambda (msg)
;			       (let ((dest (msg-object-to (simevt-msgobj msg))))
;				 (or (not dest)
;				     (= dest (node-id (simobject-node ob))))))
;		       incoming))

      ; Increment message counter:
      (set-simobject-local-recv-messages! ob (fx+ (length incoming) (simobject-local-recv-messages ob)))
      (for-each (lambda (evt)
		  ;; Record that we received this particular token in our token table.
		  ;; The vector is of the form #(invoked sent received)
		  (let ((vec (token-table-entry ob evt)))
		    (vector-set! vec 2 (fx+ 1 (vector-ref vec 2)))))
	incoming)

      ;; If GUI message counters are turned on, print our count on the screen:
;      (IF_GRAPHICS
;       (if (simalpha-label-msgcounts)
;	   (sim-setlabel (format "~a->~a"
;				 (simobject-local-recv-messages ob)
;				 (simobject-local-sent-messages ob)) ob)))

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
	(apply schedule ob timedevnts)
	
(let ((sim-print-queue 
       (lambda id
  (let ([Q (get-queue)]
	[format-evt
	 (lambda (evt)
		 (list (simevt-vtime evt)
		       (list (simtok-name (msg-object-token (simevt-msgobj evt)))
			     (simtok-subid (msg-object-token (simevt-msgobj evt))))
		       (msg-object-args (simevt-msgobj evt))))])
  (printf "Current queue: \n")
  (pretty-print
	  (map (lambda (x) (format-evt (car x)))
	       (if (null? id)
		   Q
		   (filter 
		    (lambda (pr) (= (car id)
				    (node-id (simobject-node (cdr pr)))))
		    Q))))
  (printf "  With local messages: \n")
  (pretty-print (map format-evt (simobject-local-msg-buf ob)))
  (printf "  And incoming messages: \n")
  (pretty-print (map format-evt (simobject-incoming-msg-buf ob)))
  (printf "  And timed messages: \n")
  (pretty-print (map format-evt (simobject-timed-token-buf ob)))
  (newline)
  ))))

  (void)
#;
  (unless (null? timedevnts)
    (disp "SCHEDULED NEW")
    (sim-print-queue BASE_ID)
    )
  )
)))


  ; =================================================================================
  ; This scrapes the outgoing messages off of a simobject and puts them in the global scheduler.
  (define (launch-outgoing ob)
      ; This does the "radio transmission", and puts msgs in their respective incoming buffers.
      (let ([outgoing (simobject-outgoing-msg-buf ob)])
	; Increment message counter:
        (set-simobject-local-sent-messages! ob (fx+ (length outgoing) (simobject-local-sent-messages ob)))

        ; If GUI message counters are turned on, print our count on the screen:
;       (IF_GRAPHICS
;          (if (simalpha-label-msgcounts)
;	    (sim-setlabel (format "~a->~a" 
;				 (simobject-local-recv-messages ob)
;				 (simobject-local-sent-messages ob)) ob)))

	(unless (null? outgoing)

	(for-each (lambda (evt)
		    ;; Record that we sent this particular token in our token table.
		    (let ((vec (token-table-entry ob evt)))
		      ; The vector is of the form #(invoked sent received)
		      (vector-set! vec 1 (fx+ 1 (vector-ref vec 1))))

		    ;; Timestame the message:
		    (set-msg-object-sent-time! (simevt-msgobj evt) vtime)
		    ;(let ([newmsg (structure-copy (simevt-msgobj evt))])
		    )
		  outgoing)

	(let ([neighbors (graph-neighbors (simworld-object-graph sim) ob)])
	  (for-each (lambda (out-msg)
		      (let* ([is_ucast (msg-object-to (simevt-msgobj out-msg))]
			     [type (if is_ucast 'Ucast 'Bcast)])
			(logger 1 vtime (node-id (simobject-node ob))
				type
				`[token ,(msg-object-token (simevt-msgobj out-msg))])
			(for-each (lambda (nbr)
				    ;; Here's where we simulate the channel and determine if the message goes through.
				    ;; UCASTs have already been pre-screened, we don't need to put them through the rigamarole.
				    (if (or is_ucast   (attempt-message-transmission ob nbr))
					;; The message beat the channel; add it appropriately:
					(when (or (not is_ucast) (= (node-id (simobject-node nbr)) is_ucast))
					  (logger 1 vtime (node-id (simobject-node nbr))
						  'ReceivedMsg
						  `[type ,type]
						  `[sender ,(node-id (simobject-node ob))])
					  (set-simobject-incoming-msg-buf! 
					   nbr (cons out-msg (simobject-incoming-msg-buf nbr)))
					  )
					;; Otherwise fizzle:
					(void)))
			  neighbors)))
	    outgoing)
	  ;; They're all delivered, so we clear our own outgoing buffer.
	  (set-simobject-outgoing-msg-buf! ob '())))))

  ; =======================================================================
  ;; Now for the main body of run-alpha-simple-scheduler.

  ;; First Initialize.
  (for-each init-simobject (simworld-all-objs sim))
  (set-queue! '()) ;; Start with no scheduled events.
  ;; Snapshot the real time:
  (set! realtime (real-time))

  ; =======================================================================
;; Then, run loop.  This is the MAIN LOOP that drives the simulation.
(let main-sim-loop ()
  ; Pause if there is a request to do so:
  (if (simalpha-pause-hook) 
      (begin (printf "Simulator Paused.\n")
	     ((simalpha-pause-hook))))

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
	(let ((last-vtime vtime))
	  (set! vtime (max vtime (simevt-vtime evt)))

	  ;; Now we update the simulated world that provides us with sensor values:
	  ((simalpha-sense-function) vtime)

	  ;; Now if the realtime flag is set we wait for realtime to catch up to this virtual time.
	  (if (simalpha-realtime-mode)
	      (let ((last-rtime realtime))
		(set! realtime (real-time))
		(let ((v_elapsed (- vtime last-vtime))
		      (r_elapsed (- realtime last-rtime)))
		  ;; If more virtual time has elapsed in this last clock-jump than real-time, then wait.
		  (let ((gap (- v_elapsed r_elapsed)))
					;(disp "GAP BETWEEN REAL AND VIRT " v_elapsed r_elapsed gap)
		    (if (> gap 50)
			(sleep gap)) ;; Might want to subtract some for overhead
		    )))))

	;; Also copy this value to the simworld object so alpha_lib closures can get to it:
	(set-simworld-vtime! sim vtime)

	(logger 2 vtime '_ 'Main-sim-loop `[queue-len ,(add1 (length (get-queue)))])

	;; For now, the time actually executed is what's scheduled
	
	;; This might print big structures, keep it tight:
	(parameterize ([print-level 5]
		       [print-length 15]
		       [print-graph #t])
	  (logger 3 vtime (node-id (simobject-node ob))
		  'Executing 
		  `[token ,(msg-object-token (simevt-msgobj evt))]
		  `[args ,(msg-object-args (simevt-msgobj evt))])
		  )
	
	'(DEBUGMODE ;; check invariant:
	  (if (not (null? (simobject-outgoing-msg-buf ob)))
	      (error 'build-node-sim 
		     "Trying to execute action at time ~s, but there's already an outgoing(s) msg: ~s~n"
		     global-mintime (simobject-outgoing-msg-buf ob))))
	
	;; Now the lucky simobject gets its message.
					;(set! simalpha-total-tokens (add1 simalpha-total-tokens))
	;(simalpha-total-tokens (add1 (simalpha-total-tokens)))     

	; But first increment the invocation counter for our statistics-keeping:
	(let ((vec (token-table-entry ob evt)))
	  ; The vector is of the form #(invoked sent received)
	  (vector-set! vec 0 (fx+ 1 (vector-ref vec 0))))
	
	((simobject-meta-handler ob) (simevt-msgobj evt) vtime)

	;; Finally, we push outgoing-buffers to incoming-buffers:
	(for-each launch-outgoing (simworld-all-objs sim))

	;; If we're in debug mode we check the datatype invariants on the world.
	(UBERDEBUGMODE (invcheck-simworld sim))
	;; Then it's time for another time-step.
	(main-sim-loop)))]))
) ; End run-alpha-simple-scheduler 

) ; End module
