;;;; .title A Simple Lightning/Forest-fire Sim.
;;;; .author Ryan Newton

;;;; [2006.02.01] <br> 
;;;; Matt and discussed a simple lightning/forest-fire simulation that
;;;; would stress Regiment and provide for some interesting programs.


;----------------------------------------------------------------------
;; This function maintains the global lightning state and also reads
;; off sensor values.  <br><br>
;; 
;; It's a curried function whose outmost layer is called only to
;; produce a new world.  The second layer is invoked every time the
;; time changes so as to maintain the simulation state.  (However,
;; this is optional, it could very well only update when a sensor
;; reading is requested.)  The time-step must be
;; greater-than-or-equal-to the one inputted last.  However they
;; needn't be consecutive. <br><br>
;; 

;; The inner lambda is only called when the simulator needs to read an
;; actual sensor value.  That is the type of this function is
;; something like the following (where effects are written in the arrows): <br>
;;
;; ()   --{Newsim}--> 
;; Time --{UpdateState}--> 
;; (type, id, x, y)    --> 
;;  SensorReading
(define (firelightning-sensor)

  ;; This is the statically allocated state used by the lightning sim.
  ;; Could refactor this to encapsulate it later:
  (let ([last-time #f]
	;;; Other state....
	;; Current set of fire objects.
	[fires '()]

	;; Constants:  TODO: FIX THESE UP, MAKE THEM MORE REAL:
	;; Need to make this much more sophisticated.
	[lightning-rate .00004] ;; Probability per millisecond of lightning.
	;[lightning-rate 0.5] ;; Probability per millisecond of lightning.
	[fire-spread-rate .2] ;; Again, per millisecond.
	[fire-width 3000] ;; The fire is a ring 500m thick.  It "burns out" in the center.
	[fire-age 60000] ;; Total life in milleseconds.
	[fire-temp 300]  ;; Degrees in celcius.
	       
	)
    (reg:define-struct (fire x y t rad))
    (define exp_const (exp (- lightning-rate)))
    (define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))

    (lambda (t)
      (if (not last-time) (set! last-time t))
      (if (< t last-time)
	  (error 'firelightning-sensor
		 "can't go backwards in time from t=~a to t=~a." last-time t))
      (if (= t last-time)
	  (void)
	  (let ([delta (fx- t last-time)])
	    ;(printf " delt:~a\n" delta)
	    ;; Update State...
	    ;; Propogate existing fires.
	    (set! fires
		  (filter (lambda (f)
			(if (> (fire-t f) fire-age)
			    ;; This fires dead!
			    #f
			    (begin 
			      (set-fire-rad! f (+ (fire-rad f) (* delta fire-spread-rate)))
			      #t)))
		    fires))

	    (if (not (null? fires))
		(printf "    Fires: ~a\n" fires))

	    ;; See if there are new lightning strikes.
	    (for-each (lambda (strike-time)
			(let ([strike-x (random world-xbound)]
			      [strike-y (random world-ybound)])
			  ;(printf "  LIGHTNING!! ~a of ~a ~a\n" i numstrikes strike-time)
			  (let ([newfire (make-fire strike-x strike-y strike-time 0)])  ;; Initial radius zero		   
			    (draw-mark (list strike-x strike-y))
			    (set! fires (cons newfire fires)))))
	      ;(poisson-span last-time t lightning-rate)
	      (fake-poisson last-time t lightning-rate)
	      )
	    (set! last-time t)
	    ))

      ; ========================================
      ;; ACTUAL SENSING KERNEL:  Supports 'temp and 'light.
      ;; Done updating state, now create a function for reading sensor values:
      (lambda (type id x y)
	
	(case type
	  [(temp) 
	   (let ((temp 0))
	     (for-each (lambda (f)
			 ;; Measure distance from us to the center of the fire.
			 (let ((dist (sqrt (+ (^ (- x (fire-x f)) 2)
					      (^ (- y (fire-y f)) 2)))))
			   ;(printf "Dist : ~a \n" dist)
			   ;(sim-setlabel (format "~a" dist))
			   ;; Are we within the circle:
			   (when (< dist (fire-rad f))
			     ;; Are we within the "eye", however.
			     (unless (> (- (fire-rad f) fire-width) dist)
			       (set! temp (+ temp fire-temp))))))
	       fires)
	     temp)]
	  [(light) 9999] ;; TODO
	  [else (error 'firelightning-sensor
		       "unsupported sensor type: ~a" type)])))))


;; This only works if the base-rate is small.
(define (fake-poisson start end base-rate)
  (let ([acc '()])
    (for t = start to end
	 (when (< (random 1.0) base-rate)
	   (set! acc (cons t acc))))
    (reverse! acc)))

;----------------------------------------------------------------------
;; Poisson processes: but can be very inefficient for large spans.
;; .param start - Start of spam for which events are generated.
;; .param end - End of time span.
;; .param base-rate - Average rate of events per time unit.
;; .returns A list of event times.
(define (poisson-span start end base-rate)
  (define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))
  (let* ([delta (- end start)]
	 [mu (* delta base-rate)]
	 [exp_const (exp (- mu))])
    ;; We iteratively roll the chances of their being 0, 1, 2... lightning strikes over the elapsed interval.
    (let loop ([numstrikes 0] [prob exp_const] [probsum exp_const])
      (printf "PROB: ~a sum:~a iter:~a const:~a delta:~a mu:~a\n" prob probsum numstrikes exp_const  delta mu)
      (if (< (random 1.0) probsum)
	  ;; We've got the correct number of strikes, return a list of event times:
	  (let ((ls '()))
	    (for i = 1 to numstrikes 
		 (let ([event-time (+ (random delta) start)])
		   (set! ls (cons event-time ls))))
	    ls)
	  ;; Otherwise, we failed, move on to the next scenario:
	  (let* ([newnum (fx+ 1 numstrikes)]
		 [newprob 
		  ;;(* prob (fl+ 1.0 (/ mu newnum)))
		  ;;(* prob (/ (+ mu numstrikes 1) (add1 numstrikes)))
		  (/ (* (^ mu (add1 numstrikes)) exp_const) (fact (add1 numstrikes)))
		  ])
					;(loop newnum newprob (+ probsum newprob))
	    (loop newnum 0.0 (* prob (fl+ 1.0 (/ mu newnum))))
	    )
	  ))))


;; Poisson processes: with reasonable efficiency


;----------------------------------------------------------------------
;; Install the lightning sim as the default sim:
(define (install-firelightning)
  (simalpha-sense-function-constructor firelightning-sensor)

  ;; Set the world size, 10KM square:
  ;; Set both of these for now, lame:
  (set! world-xbound 5000)
  (set! world-ybound 5000)
  (simalpha-world-xbound 5000)
  (simalpha-world-ybound 5000)

  ;; Inner/Outer Radius for radios is 300/500 meters:
  (simalpha-inner-radius 300)
  (simalpha-outer-radius 500)

  (simalpha-channel-model 'linear-disc)
  (simalpha-placement-type 'gridlike)
  (simalpha-failure-model 'none)
  )
