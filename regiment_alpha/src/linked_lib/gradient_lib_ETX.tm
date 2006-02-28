
;; [2006.01] Ryan Newton

;; Thinking about how to write a better gradient library so that not
;; so much complexity is pushed into the desugar-gradients compiler
;; pass, and more can sit separately in a library.

;; This goes hand in hand with my current effort to improve gradients
;; to use an LQI type metric to build better trees

;; This is a global constant which indicates the length of the "array"
;; storing data objects corresponding to each aggregation we're doing.
;(token GRAD:obj-count ()
;  (stored [count 0])
;  (void))
;; Allocate it at startup.    
;(token node-start () (call GRAD:obj-count))


;; NOTE: This file is linked when the Regiment compiler runs.  Values
;; of parameters that affect this module (retry count) will be read at
;; that time.

; ================================================================================
;; GRAD:return-handler


;; The data handler. <br>
;; When called locally, this triggers aggregation.
;; When called remotely, this builds up the accumulator.
;; If there is no aggregation operator (fold function) provided, it still aggregates; it
;; simply builds *lists* of results that are passed up to parents.  (i.e. cons is default aggregator)


;; .param flag     Called locally or remotely.
;; .param destid   If called remotely, what is the target node for this broadcast?
;; .param val      The value to return up the tree.
;; .param totok    The handler at the root node to invoke when the value gets there.
;; .param viatok   The token identifying the tree to use for aggregation.
;; .param aggrtok  The aggregation token, if available.
;;
;; .param aggr_ID  This is generally the product of the totok and viatok indices.  
;;                 It uniquely identifies "this" aggregation.
`(token (GRAD:return-handler . retid) (flag val totok viatok aggrtok seedval) ;; Bet you wish we had a struct here!
   (stored [ACC (if aggrtok seedval '())]
	   ;; These binds had better stay constant for the duration of this aggregation:
	   [AGGR aggrtok]
	   [SEED seedval]
	   [TO   totok]
	   [VIA  viatok])
	   
   ;; Fire an error if they don't stay constant.
   
   (if (eq? flag ',RHLOCAL)
       ;; When we get the local value, we lump it together:
       (begin 
	 (set! ACC (if AGGR 
		       (subcall AGGR val ACC)
		       (cons val ACC)))
	 
	 ;; Do aggregation right now:
	 (call (tok GRAD:return-aggr-and-send retid))
	 
	 (if AGGR
	     (begin 
	       (token-deschedule (tok GRAD:return-timeout retid))
	       (timed-call ,DEFAULT_RHSEND (tok GRAD:return-timeout retid))
	       ,@(DEBUG_GRADIENTS
		  `(dbg "%d.%d: Setting time-out!!" (my-clock) (my-id)))
	       ,@(DEBUG_GRADIENTS
		  `(dbg "%d.%d: Time-out set: %d" 
			(my-clock) (my-id) (token-scheduled? (tok GRAD:return-timeout retid))))
	       )
	     (void)))
       
       ;; Otherwise, flag = RHREMOTE
       ;; If called remotely, we only proceed if we are the intended destination.
       ;; Now we simply accumulate and wait for the local call.
       (begin   
	 (set! ACC 
	       (if AGGR 
		   (subcall AGGR val ACC) ;; [2005.11.03] Making direct for now
		   (cons val ACC)))
	 
	 (if AGGR
	     (begin 
	       ;;,@(COMMENT "Now we don't reset the timer, but we ENSURE that it's set:")
	       (if (not (token-scheduled? (tok GRAD:return-timeout retid)))
		   (timed-call ,DEFAULT_RHSEND (tok GRAD:return-timeout retid))))
	     (begin
	       ;;,@(COMMENT "For non-aggregated returns we just send up immediately.")
	       (call (tok GRAD:return-aggr-and-send retid))
	       )
	     ))))


;================================================================================
; GRAD:return-timeout

;; The timeout handler. <br>
;; When it fires it invokes the aggregation and sends it up the tree.
;; We only use this when there's an aggregator, otherwise return vals
;; go straight up!
;;
;; <br><br>
;; This takes no arguments because all the necessary dated is stored
;; within the stored vars of the data token object.

`(token (GRAD:return-timeout . retid) ()
   (stored)
   ,@(DEBUG_GRADIENTS `(dbg "%d.%d  Time-out fired!" (my-clock) (my-id)))

   ; [2006.01.17] Don't allow these to stack:
   (token-deschedule (tok GRAD:return-aggr-and-send retid))
   (call (tok GRAD:return-aggr-and-send retid))

   ;,@(COMMENT "Reset the default time-out timer, if there's anything left to aggregate")

   ;; This is an optimization.  We don't set a timer if the aggregation accumulator
   ;; is "empty" that is, if it is equal to the identity element (seed element).
   (if (and (token-present? (tok GRAD:return-handler retid))
	    (not (equal? (ext-ref (tok GRAD:return-handler retid) ACC)
			 (ext-ref (tok GRAD:return-handler retid) SEED))))
       (begin 
	 ,@(DEBUG_GRADIENTS `(dbg "%d.%d: Reset timer again." (my-clock) (my-id)))
	 (token-deschedule (tok GRAD:return-timeout retid))
	 (timed-call ,DEFAULT_RHSEND (tok GRAD:return-timeout retid)))
       (begin ,@(DEBUG_GRADIENTS
		 `(dbg "%d.%d: Nothing in acc, stop time-out timer." (my-clock) (my-id))))))

;================================================================================
; GRAD:return-aggr-and-send

;; The aggregation and communication handler. 
;; <br>
;;
;; This operates only over the data stored in the data token object (see above).
;;
`(token (GRAD:return-aggr-and-send . retid) ()
   (stored)

   ;; First thing first we check to see if the data token exists.
   ;; If not there is no point in firing.
   (if (not (token-present? (tok GRAD:return-handler retid)))
       (void) ;; fizzle -- maybe error?
       ;; Otherwise it's time to aggregate!

       ;; NOTE: This is cheating!!  I'm using pointers and heap
       ;; allocation here.  Not part of the strict model!
       (let ([oldacc (ext-ref (tok GRAD:return-handler retid) ACC)]
	     [_TO     (ext-ref (tok GRAD:return-handler retid) TO)]
	     [_VIA    (ext-ref (tok GRAD:return-handler retid) VIA)]
	     [_AGGR   (ext-ref (tok GRAD:return-handler retid) AGGR)]
	     [_SEED   (ext-ref (tok GRAD:return-handler retid) SEED)])
	 (if (or _AGGR
		 (not (null? oldacc))
		 (begin 
		   (dbg "~a.~a: WARNING: Trying to GRAD:return-aggr-and-send with a null accumulator on non-aggr greturn!"
			(my-clock) (my-id))
		   #f))
	     
	     (begin 
	       ,@(DEBUG_GRADIENTS `(dbg "%d.%d: Aggr-and-send: ~a retid ~a aggrsched? ~a timoutsched? ~a" 
					(my-clock) (my-id) oldacc retid
					(token-scheduled? (tok GRAD:return-aggr-and-send retid))
					(token-scheduled? (tok GRAD:return-timeout retid))))
	       
	       ;; Reset the accumulator, doesn't matter if there's no aggregator:		       
	       (ext-set! (tok GRAD:return-handler retid) ACC 
			 (if _AGGR _SEED (cdr oldacc)))
	       
	       ;; Next, do we have the via token?
	       (if (not (token-present? _VIA))
		   ;; NOTE: FIZZLE SEMANTICS.
		   ;; That is, if we get a local return before the trees there.  Then we just fizzle.
		   ;; One could imagine buffering here, but that gets complex.
		   (begin 
		     ,@(DEBUG_GRADIENTS
			`(dbg "Warning: Didn't have the via token %d at node %d (FIZZLE)" _VIA (my-id))))
		   ;; Now we look at the via tree for this aggregation. Have we reached the root of the tree?	
		   (let ((parent_pointer ;(ext-ref _VIA ,STORED_PARENT_ARG)))
			  (ext-ref _VIA 0))) ;; Parent pointer is in slot ZERO.
		     (if (not parent_pointer)
			 (begin ;,@(DEBUG_GRADIENTS `(dbg "ERRR: fell off the via tree: %d at node %d" _VIA (my-id)))
				(void))

			 (if (eq? ',NO_PARENT parent_pointer)
			     (begin
			       ;,@(COMMENT "Reached tree root, calling to token.")
			       ,@(DEBUG_GRADIENTS 
				  `(dbg "~a.~a: At ROOT of tree, invoking ~a with ~a" 
					(my-clock) (my-id) _TO (if _AGGR oldacc (car oldacc))))
			       
			       (call _TO (if _AGGR oldacc (car oldacc)))
			       )

			     ;; Otherwise, send it on up to the parent:
			     ;; TODO: Should use "send_to"/ucast form here, but haven't implemented yet:
			     (begin 
			       ,@(DEBUG_GRADIENTS
				  `(dbg "%d.%d: Returning up tree %d, parent %d to %d acc %d" 
					(my-clock) (my-id) _VIA parent_pointer (tok GRAD:return-handler retid) oldacc))
			       
			       (call (tok GRAD:retry-upsend retid)
				     parent_pointer ;; destid
				      ',RHREMOTE     ;; flag
				      (if _AGGR oldacc (car oldacc)) ;; val
				      _TO _VIA _AGGR _SEED
				      ',(+ 1 (etx-max-retries)))
			       ))))))
	     (void))

	 ;; After we've launched that message, if we're in
	 ;; non-aggregation mode, we just keep sending whatever we've got.
 	 (if (and (not _AGGR)
 		  (not (null? (cdr oldacc)))
 		  (not (token-scheduled? (tok GRAD:return-aggr-and-send retid))))
 	     (begin 
 	       ,@(DEBUG_GRADIENTS `(dbg "%d.%d: Still have stuff left, continue aggregating..." (my-clock) (my-id)))
 	       (call (tok GRAD:return-aggr-and-send retid) ))
 	     (void))

	 )))

;========================================

;; [2006.01.30] This just runs a loop to retry the up-transmission for greturns.
`(token (GRAD:retry-upsend . retid) (dest flag val to via aggr seed COUNT)
   ;(setlabel "TO <~a> ~a" dest COUNT)
   (if (ucast-wack dest (tok GRAD:return-handler retid) flag val to via aggr seed)
       ;; Good, it was delivered.
       ;;TODO: (if (simalpha-visualize-gradients)
       (highlight-edge dest (rgb 200 0 0))
       (if (<= COUNT 0)
	   (void) ;; Fizzle, out of retries.
	   ;; Otherwise, retry it after a chunk of time:
	   (timed-call ,(etx-retry-delay)
		       (tok GRAD:retry-upsend retid) dest flag val to via aggr seed (- COUNT 1)))))




;========================================================================================================================
