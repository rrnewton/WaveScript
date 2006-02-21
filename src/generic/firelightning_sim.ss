;;;; .title A Simple Lightning/Forest-fire Sim.
;;;; .author Ryan Newton

;;;; [2006.02.01] <br> 
;;;; Matt and discussed a simple lightning/forest-fire simulation that
;;;; would stress Regiment and provide for some interesting programs.


;;; Constants:  TODO: FIX THESE UP, MAKE THEM MORE REAL:
	;; Need to make this much more sophisticated.
(define lightning-rate .00001) ;; Probability per millisecond of lightning.
	;[lightning-rate 0.5] ;; Probability per millisecond of lightning.
(define fire-spread-rate .001) ;; Again, per millisecond.
(define fire-width 3000) ;; The fire is a ring 500m thick.  It "burns out" in the center.
(define fire-max-age 300000) ;; Total life in milleseconds.
(define fire-temp 200)  ;; Degrees in celcius.

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
	)
    (reg:define-struct (fire x y t rad gobj))
;    (define exp_const (exp (- lightning-rate)))
;    (define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))

    (printf "Constructing fire sim...\n")

    (lambda (t)
      (define worldx (simalpha-world-xbound))
      (define worldy (simalpha-world-ybound))
      (define quartx (/ (simalpha-world-xbound) 4))
      (define quarty (/ (simalpha-world-ybound) 4))

      (if (not last-time) (set! last-time t))
      (if (< t last-time)
	  (error 'firelightning-sensor
		 "can't go backwards in time from t=~a to t=~a." last-time t))
      (if (= t last-time)
	  (void)
	  (let ([delta (fx- t last-time)])
	    ;(printf " delt:~a\n" delta)
	    ;; Update State...
	    ;; Propogate existing fires, while filtering dead fires.
	    (set! fires
		  (filter (lambda (f)
			(if (> (- t (fire-t f)) fire-max-age)
			    ;; This fires dead!
			    (begin 
			      ;; Delete just the mark and keep the circle:
			      (IF_GRAPHICS (delete-gobj (cdr (fire-gobj f))))
			      ;(delete-gobj ((car (fire-gobj f)) 0)) ;; This would delete the circle also.
			      #f)
			    (begin 
			      (set-fire-rad! f (+ (fire-rad f) (* delta fire-spread-rate)))
			      (IF_GRAPHICS ((car (fire-gobj f)) (fire-rad f))) ;; Update the circle on the screen.
			      #t)))
		    fires))
	    (unless (null? fires)
	      (logger 1 t '_ 'GROUND-TRUTH 
		      `[fires ,(map (lambda (f) (list (fire-x f) (fire-y f) (fire-t f) (fire-rad f))) fires)]))

	    ;(if (not (null? fires)) (printf "    Number Fires: ~a\n" (length fires)))

	    ;; TEMP: FIXME:
	    ;; Currently making it so there's one fire at a time for easy analysis.
	    (when (null? fires)
	    ;; See if there are new lightning strikes.
	    (for-each (lambda (strike-time)
			;; [2006.02.19] Restricting the fires to the inner portion of the square.  
			;; Can't have them falling outside of the network.			
			(let ([strike-x (+ quartx (random (/ worldx 2)))]
			      [strike-y (+ quarty (random (/ worldy 2)))])
			  ;(printf "  LIGHTNING!! ~a of ~a ~a\n" i numstrikes strike-time)
			  (let ([newfire (make-fire strike-x strike-y strike-time 0  ;; Initial radius zero
						    ;; The graphical object for this fire is a strange thing.  
						    ;; Consing the center mark and the adjustable circle together.
						    (IF_GRAPHICS (cons (draw-circle `(,strike-x ,strike-y) 0)
								       (draw-mark (list strike-x strike-y)))
								 '()))]
				[mark (IF_GRAPHICS  '())])
			    (printf " New fire!: ~a\n" (list (fire-x newfire) (fire-y newfire) (fire-t newfire)))
			    (set! fires (cons newfire fires))
			    )))
	      (let ([strikes 
					;(poisson-span last-time t lightning-rate)
					;(fake-poisson last-time t lightning-rate)
		     (poisson-span-optimized1 last-time t lightning-rate)])
		;; HACK: If we haven't had any strikes by 10 seconds, just throw one in. 
		;; It's annoying for me to run simulations and wait for a strike.
		(if (and (>= t 10000) (null? fires) (null? strikes))
		    (list t) strikes)
		))	    
	    )
	    (set! last-time t)
	    ))
      ;; Done updating state, now create a function for reading sensor values:

      ; ========================================
      ;; ACTUAL SENSING KERNEL:  Supports 'temp and 'light sensors.
      (lambda (type id x y)
	(case type
	  [(temp) 
	   (let ((temp 0))
	     (for-each (lambda (f)
			 ;; Measure distance from us to the center of the fire.
			 (let ((dist (sqrt (+ (^ (- x (fire-x f)) 2)
					      (^ (- y (fire-y f)) 2)))))
			   (set! temp
				 ;; [2006.02.12] Changing this so the fire-temp is a ceiling:
				 (min fire-temp
				      (+ temp (compute-fire-heat dist (fire-rad f)))))))
	       fires)
	     ;; Our sensor has constant gaussian noise attached to it, stddev 5 degrees:
	     ;(+ temp (* 5 (gaussian)))
	     ;; TEMP: using integer temperature:
	     (inexact->exact (floor temp))
	     )]
	  [(light) 9999] ;; TODO
	  [else (error 'firelightning-sensor
		       "unsupported sensor type: ~a" type)])))))


(define (compute-fire-heat dist radius)
  (let ([dist (exact->inexact dist)]
	[radius (exact->inexact radius)])
  ;; This function determines the rate that heat falls off
  ;; with distance from a fire of a given radius.          <br>
					;(sim-setlabel (format "~a" dist))
  ;; Are we within the circle?
  (if (< dist radius)
      ;; Are we within the "eye", however?
      (if (> (- radius fire-width) dist)
	  0 ;; It's perfectly cool once it's burned out.  A little silly.
	  fire-temp)
      ;; If we're outside the fire, compute our heat reading as follows.
      (if (zero? radius) 0
	  (* fire-temp
	     ;; Treat the fire as a spherical shell, thin the radiated energy
	     ;; over the larger spherical shell.      
	     ;  (fl/ (fl* radius radius)  (fl* dist dist))

	     ;; No, let's try an exponential decay instead:
	     ;  (^ .95 (- dist radius))

	     ;; Nah, let's actually make it a constant "halo" area.
	     ;(min 1.0 (fl/ 100.  (+ 100. (^ (- dist radius) 2))))

	     ;; Ok, also trying just a 1/n falloff:
	     (if (> (- dist radius) 350) 0.
		 (min 1.0 (fl/ 1.  (+ 1.0 (fl* 0.5 (- dist radius))))))
	     
	     )
	  ))))

;; This only works if the base-rate is small.
(define (fake-poisson start end base-rate)
  (let ([acc '()])
    (for t = start to end
	 (when (< (random 1.0) base-rate)
	   (set! acc (cons t acc))))
    (reverse! acc)))

;----------------------------------------------------------------------
;; Poisson processes: but can be very inefficient for large spans.
;; And worse, this implementation can have failures of numeric
;; accuracy as well when the timespan gets large.
;; 
;; .param start - Start of spam for which events are generated.
;; .param end - End of time span.
;; .param base-rate - Average rate of events per time unit.
;; .returns A list of event times.
(define poisson-span 
  (letrec  ([fact (lambda (n) (if (zero? n) 1 (* n (fact (sub1 n)))))])
    (lambda (start end base-rate)
      (let* ([delta (exact->inexact (- end start))]
	     [mu (* delta base-rate)]
	     [exp_const (exp (- mu))])
	;; We iteratively roll the chances of their being 0, 1, 2... events over the elapsed interval.
	(let loop ([numstrikes 0] [prob exp_const] [probsum exp_const])
	  ;(printf "PROB: ~a sum:~a iter:~a const:~a delta:~a mu:~a\n" prob probsum numstrikes exp_const  delta mu)
	  (if (< (random 1.0) probsum) ;; If we fall under the accumulated probability mass...
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
	      ))))))


;; Poisson processes: with reasonable efficiency
(define poisson-span-optimized1
  ;; This constant sets the maximum time-span we'll tolerate.  If the
  ;; span is bigger than this we do a divide-and-conquer.
  (letrec ([max_span_size 100]
	   [fact (lambda (n) (if (zero? n) 1 (* n (fact (sub1 n)))))])
    (lambda (start end base-rate)      
      (let divide-and-conquer ([start start] [end end])       
	(define delta (- end start))	
	(if (> delta max_span_size)
	    ;; Here we recursively split up the time-span into something bite-sized:
	    (let ([mid (+ (/ delta 2) start)])
	      (append  (divide-and-conquer start mid)
		       (divide-and-conquer mid end)))
	    ;; If it's small enough, we then roll our numbers and produce some events:
	    (poisson-span start end base-rate))))))

;----------------------------------------------------------------------
;; Install the lightning sim as the default sim.  <br>
;; As well as plugging in the correct sensing stub, this will set the
;; dimensions of the world and the radio parameters correctly.
(define (install-firelightning)
  (simalpha-sense-function-constructor firelightning-sensor)

  ;; Set the world size, square:
  ;; Set both of these binds for now, lame:
  (set! world-xbound 5000)
  (set! world-ybound 5000)
  (simalpha-world-xbound 5000)
  (simalpha-world-ybound 5000)
  (sim-num-nodes 250)

;  (set! world-xbound 1500)
;  (set! world-ybound 1500)
;  (simalpha-world-xbound 1500)
;  (simalpha-world-ybound 1500)
;  (sim-num-nodes 30)

  ;; Inner/Outer Radius for radios is 300/500 meters:
  (simalpha-inner-radius 300)
  (simalpha-outer-radius 500)

  (simalpha-channel-model 'linear-disc)
  (simalpha-placement-type 'gridlike)
  (simalpha-failure-model 'none)
  )


;================================================================================

;;; Test fire drop-off. 

;; This is just a visual test.  Check by eye that it looks like what you want.
(define (test-fire-heat)
  ;; Currying:
  (define (: f . args) (lambda (x) (apply f (append args (list x)))))
  
  ; First the drop off from a small fire, rad 10.
  (gnuplot (map (lambda (i) (list i (compute-fire-heat i 10))) (map (: * 3) (iota 100))) 'boxes)
  ; Next, rad 100
  (gnuplot (map (lambda (i) (list i (compute-fire-heat i 100))) (map (: * 2) (iota 500))) 'boxes)
  ; Then rad 1000
  (gnuplot (map (lambda (i) (list i (compute-fire-heat i 1000))) (map (: * 5) (iota 1000))) 'boxes)
  )
