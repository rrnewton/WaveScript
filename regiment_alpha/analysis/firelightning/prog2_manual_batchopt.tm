

;; This is some token machine code written to test out my simple lightning/forest-fire simulation.

;; It makes a little local neighborhood around every heat event.

(parameters 
  ;; This turns on the fire-simulation globally.  Can't be undone.
  [dummy-param (install-firelightning)]
  ;[simalpha-placement-type 'connected]

  [simalpha-realtime-mode #f]
  [simalpha-dbg-on #f]

  [default-slow-pulse (* 5 60 1000)] ;; 5 min
  [default-fast-pulse (*    3 1000)] ;; 3 sec

;  [desugar-gradients-mode 'inlined]
;  [simalpha-channel-model 'lossless]

  [simalpha-dbg-on #t]

  ;[sim-num-nodes 250]
  ;[sim-num-nodes 30]
  ;[sim-timeout 2000000]
  [sim-timeout 3600000] ;; An hour.

  )


;; globaltree : 300000 ; 5 min
;; return: 3000, three sconds

(token SOC-start ()
  (dbg "Launching global tree!")
  (call root))

(token root ()
  (gemit globaltree)
  (timed-call 300000 root))

;; Rapidly spread throughout the whole network.
(token globaltree ()
  (grelay))

(token node-start ()
  (call temp)
  (call loop))

;; Periodically reads and stores the temperature.
(token temp ()
  (stored [t 'uninitialized]) ;; Stored temperature.
  (set! t (sync-sense 'temp))
  ;(dbg "~a: Reading temp... ~a" (my-id) t)
  (timed-call 3000 temp))

;; Runs periodically and emits a local neighborhood when a node goes over threshold.  
;; (Could do better signal processing and wait until the node is over threshold for some period of time.)
`(token loop ()
   (stored [last-sent #f]) ;; Time of last gradient sent.
   
    (if (> (ext-ref temp t) ,(varied-param))
	
	(begin (leds on red)
	       ;; Only *reemit* the gradient every 21 seconds:
	       (if (or (not last-sent)
		       (>= (- (my-clock) last-sent) 21000))
		   (begin (set! last-sent (my-clock))
			  (dbg "~a: Detection, form local neighborhood." (my-id))
			  (gemit (tok local_nbrhood (my-id)))))
	       )
	(leds off red))
    ;; When there's DEFINITELY a fire the node turns blue.
    (if (> (ext-ref temp t) 160)
	(leds on blue)
	(leds off blue))
    (setlabel "T: ~a" (round-to 2 (ext-ref temp t)))

    ;; Check temp gradients every 3 seconds.
    (timed-call 3000 loop)
    )

;; This is the gradient that's only emitted every 21 seconds.
(token (local_nbrhood . subid) ()
  ;; Send out one hop.
  (if (= (ghopcount) 0) (grelay)
      (call read-return (my-clock))))

;; This returns data for several timesteps.
`(token read-return (start-time)
  (dbg "  ~a: Considering neighbor: ~a" (gorigin) (my-id))
  (if (> (ext-ref temp t) ,(varied-param))
      (greturn 1
	       (to local_return)
	       (via (tok local_nbrhood subid))
					;(seed 0)
					;(aggr plus)
	       ))
  ;; If we're still within that batched time-window, return data.
  (if (< (+ (my-clock) 3000) (+ start-time 21000))
      (timed-call 3000 read-return))
  )

(token plus (a b) (+ a b))

(token local_return (v)
  (stored (YAY #f)
	  (max 0))
  (dbg "~a: YAY: ~a at ~a" (my-id) v (my-clock))
  (set! YAY v)
  (if (> YAY max) (set! max YAY))
  (if (>= v 1)
      (begin
	(leds on green)
	(greturn (my-id)
		 (to SOC-return-handler)
		 (via globaltree))))
  )
