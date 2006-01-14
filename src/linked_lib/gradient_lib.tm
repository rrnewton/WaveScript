
;; [2006.01] 

;; Thinking about how to write a better gradient library so that not
;; so much complexity is pushed into the desugar-gradients compiler
;; pass, and more can sit seperately in a library.

;; This goes hand in hand with my current effort to improve gradients to use an LQI type metric to build

(token handler ()

  )


`(token GRAD:return-handler (flag val acctok aggrtok desttok totok viatok) ;; Bet you wish we had a struct here!
   (if (eq? flag ',RHLOCAL)
       ;; When we get the local value, we lump it together:
      (begin
	(ext-set! acctok ACC
		  (if aggrtok
		      (subcall aggrtok val (ext-ref acctok ACC))
		      (cons val (ext-ref acctok ACC))))
	
	;; Now kill the scheduled timer token if there is one, and set a new timer.
	;; (Don't bother with the if, because default semantics for deschedule 
	;; is to fizzle if its not there.)
					;(if (token-scheduled? (tok ,return-timeout-handler retid))

	;; Do aggregation right now:
	(call (tok ,return-aggr-and-send retid) toind viaind)

	,@(COMMENT "Reset the default time-out timer")
	,@(if aggr
	      `((begin 
		  (token-deschedule (tok ,return-timeout-handler retid));)
		  ,@(DEBUG_GRADIENTS
		     `(dbg "%d.%d: Setting time-out!!" (my-clock) (my-id)))
		  (timed-call ,DEFAULT_RHSEND (tok ,return-timeout-handler retid) toind viaind)
		  ,@(DEBUG_GRADIENTS
		     `(dbg "%d.%d: Time-out set: %d" 
			   (my-clock) (my-id) (token-scheduled? (tok ,return-timeout-handler retid))))))
	      ())
	)
      
      ;; Otherwise, flag = RHREMOTE
      ;; If called remotely, we only proceed if we are the intended destination.
      (if (not (or (= destid ',NULL_ID) (= destid (my-id))))
					;(DEBUG_GRADIENTS (dbg '"  CANCELED, not destination."))
	  (void) ;; TODO: FIXME OPTIMIZATION: Might want to evict self here -- wasted space on useless tokens.
	  ;; Now we simply accumulate and wait for the local call.
	  (begin 
	    (set! ,acc ,(if aggr `(subcall ,aggr val ,acc) ;; [2005.11.03] Making direct for now
			    `(cons val ,acc)))

	    ,(if aggr 
		 `(begin 
		    ,@(COMMENT "Now we don't reset the timer, but we ENSURE that it's set:")
		    (if (not (token-scheduled? (tok ,return-timeout-handler retid)))
			(timed-call ,DEFAULT_RHSEND (tok ,return-timeout-handler retid) toind viaind)))
		 `(begin
		    ,@(COMMENT "For non-aggregated returns we just send up immediately.")
		    (call (tok ,return-aggr-and-send retid) toind viaind))
		 )))
      )]


  )
