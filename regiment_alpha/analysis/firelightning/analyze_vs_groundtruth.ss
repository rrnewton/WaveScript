#! /bin/sh
#| 
exec regiment i --script "$0" ${1+"$@"};
|#

;; Uncomment this only to use Check Syntax in PLT on this file, not to run it:
(begin 
  (require "../../src/plt/iu-match.ss")
  (require "../../src/generic/constants.ss")
  (require "../../src/plt/helpers.ss"))

;;;; .title Analyze Dead-simple fire monitoring vs. groundtruth.

;;;; TODO: POSSIBLE OTHER CONTROLS: <gradient-mode> <retry-delay> <max-retries>

;;;;<br>  Usage: ./<script-name> <options>
;;;;<br>  Options: 
;;;;<br>     -l <logfile>
;;;;<br>     -o <resultsfile>
;;;;<br>     -param <n>   Set the value of 'varied-param'.  In this case it changes the local temp threshold.
;;;;<br>     -noise <n>   Set the level of heat-noise-magnitude, this is stddev in heat readings

;;;;<br><br> 
;;;; This script processes logfiles produced by deadsimple_alarm.rs (or prog0.rs), and it uses 
;;;; a heuristic algorithm of my devising to detect fire events from the temperature readings.
;;;; It then compares these detections vs. ground truth and detects the lag.

;----------------------------------------

;(define curlogfile (format "./deadsimple_~a.log.gz" (current-time)))
(define curlogfile (format "./deadsimple.log.gz"))
(define resultsfile 'uninit)
(define worldseed 'uninit)

(define THE_CLUSTER_THRESHOLD 10)


(define (number->integer x) (inexact->exact (floor x)))

(random-seed (current-time))

(define-syntax mvfirst
  (syntax-rules ()
    [(_ e) (call-with-values (lambda () e) (lambda args (car args)))]))

;; This is the main procedure, either run or analyze:
(define (main flag)
  (case flag

    [(analyze)
     (let ()
       (define logport (open-input-file 
			curlogfile 
			(if (equal? "gz" (extract-file-extension curlogfile))
			    '(compressed) '())))
       (define logstream (begin (printf "\nOpenning log file as stream...\n")
				(reg:read-log logport 'stream)))
       
       ;; type: list of (id (x y))
       (define nodes (begin (printf "Analyzing performance vs. groundtruth...\n")
			    (map cdr 
			      (let loop ((s logstream))
				(if (stream-empty? s)
				    (error 'analyze_deadsimple_vs_groundtruth "no world description in log")
				    (match (stream-car s)
				      [(,t ,id NEWWORLD ,binds ...)
				       (cadr (assq 'nodes binds))]
				      [,else (loop (stream-cdr s))]))))))

       


       (define ground (stream-filter (lambda (x) (eq? 'GROUND-TRUTH (caddr x))) logstream))
       (define returned (stream-filter (lambda (x) (eq? 'SOCRETURN (caddr x))) logstream))
       
       (define resultslog 
 (begin (printf "We build a very simple model based on the returned data.\n")
		(printf "Directing output to file: ~a\n" resultsfile)
		(open-output-file resultsfile 'replace)))

       ;; An estimate contains: confidence, time, position, node-ids reporting.
       (reg:define-struct (estimate conf t pos nodes))

       ;; This is the length of time that a datapoint is considered valid.
       ;; We lose data sometimes, so this number is relevent.
       (define point-life 5000) ; milliseconds

       ;; This is the maximum amount of time during which we'll
       ;; consider two fire estimates to be talking about the "same"
       ;; fire.  That is, if we have no data for less than this amount
       ;; of time, and then have an estimate in a similar place as
       ;; before, we don't create a new detection event.
       (define identity-window 15000)
       
       (define (centroid pts)
	 ;(if (list? (car pts)) (set! pts (map list->vector pts)))
	 (let-match ([#(,accx ,accy ,sum)
		      (foldl (match-lambda (#(,i ,_ ,t) #(,accx ,accy ,sum))
			       (let ((pos (cadr (assq i nodes))))
				 (vector (+ accx (* t (car pos)))
					 (+ accx (* t (cadr pos)))
					 (+ t sum))))
			#(0.0 0.0 0) pts)])
	   (list (/ accx sum) (/ accy sum))))

       (define (node-distances ids)
	 (let loop ([nodes (map (lambda (id) (cadr (assq id nodes))) ids)]
		    [acc ()])
	   (if (null? nodes) acc
	       (let ((n1 (car nodes)))
		 (loop (cdr nodes)
		       (append (map (lambda (n2) (posdist n1 n2))
				 (cdr nodes))
			       acc))))))

       ;; This is supposed to cluster nodes into connected clumps the
       ;; same way the in-network Regiment program will.
       ;; Tested by looking at my GUI and it seems to work correctly.
       ;; Takes a list of node-ids.  Returns a list of (id (x y)).
       (define (cluster-nodes ids)
	 (define (any-match? nds1 nds2)
	   (let ([pos1* (map cadr nds1)]
		 [pos2* (map cadr nds2)])
	     (ormap (lambda (p1)
		      (ormap (lambda (p2) (< (posdist p1 p2) 
					     ;(simalpha-outer-radius)
					     500
					     ))
			     pos2*))
		    pos1*)))
	 (let ([nodes (map (lambda (id) (assq id nodes)) ids)])
	   (let outerloop ([clusters (map list nodes)])
	     (if (null? clusters) '()
		 ;; We take the first cluster, and run down the line
		 ;; seeing who it clicks with. If no one, then we're done with it.
		 (let* ([head (car clusters)]
			[inserted? #f]
			[newclusters 
			 (let innerloop ([rest (cdr clusters)])
			   (cond 
			    [(null? rest) '()]
			    [(any-match? head (car rest)) 
			     (set! inserted? #t)
			    ;; Merge these clusters
			     (cons (append head (car rest)) (cdr rest))]
			    [else (cons (car rest) (innerloop (cdr rest)))]
			    ))])
		   (if inserted?
		       (outerloop newclusters)
		       (cons head (outerloop newclusters))
		       ))))))
       
       (define (vectcar v) (vector-ref v 0))

       (define message-count 0)

       (define protoestimates
	 ;; TRACKS ONLY ONE FIRE CURRENTLY:
	 ;; This produces a stream of estimates.
	 (let predictloop ((rets returned) (points '()))
	   ;; Add the new data point:
	   (if (stream-empty? rets) 
	       '()
	       (match (stream-car rets)
		 [(,clock ,__ SOCRETURN [val #(,id ,nodeclock ,temp)])
		  ;; Expire old points and remove the existing entry for this node.
		  (let ([newpoints
			 (cons (vector id clock temp)
			       (filter (match-lambda (#(,i ,c ,t))
					 (and (not (= i id))
					      (not (> (- clock c) point-life))))
				 points))])
		    ;; Lookup function for newpoints:
		    (define (id->datapoint id)
		      (let loop ((ls newpoints))
			(if (= id (vector-ref (car ls) 0))
			    (car ls)
			    (loop (cdr ls)))))

;		    (printf "ACTIVE PTS: ~a \n" (length newpoints))

		    (let ([definites (filter (match-lambda (#(,i ,c ,t)) (> t 150)) newpoints)])
		      (if (not (null? definites))
			  ;; We are one hundred percent confident in the existence of a 
			  ;; fire once we have any newpoints over 150C.
			  (begin 			    
			    (printf "~a: SURETY\n" clock )
			    (stream-cons (make-estimate 100 clock (centroid definites) (map vectcar definites))
					 (predictloop (stream-cdr rets) newpoints)))

			  ;; Attempt to imitate the clustering that would happen in the in-network algorithm.
			  (let ([sum-cluster 
				 (lambda (clst)
				   (foldl (match-lambda (#(,i ,c ,t) ,acc) (+ t acc)) 0 
					  (map (lambda (nd) (id->datapoint (car nd))) clst)))])
			    (stream-cons 

			     (let* (;; Quadratic in number of nodes.
					;[dist-matrix (node-distances (map vectcar newpoints))]
				    [clusters (cluster-nodes (map vectcar newpoints))]
				    [cluster-sums (map sum-cluster clusters)]
				    [maxsum (apply max cluster-sums)]
				    [maxind (list-find-position maxsum cluster-sums)]
				    [maxclust (list-ref clusters maxind)]
				    )
			       (printf "~a: ~a\n" clock 
				       (sort (lambda (a b) (> (car a) (car b)))
					     (map (lambda (c s) (cons s (length c))) clusters cluster-sums)))

			       (if (> maxsum THE_CLUSTER_THRESHOLD) ;; Our threshold.
				   ;; The cluster must be sufficiently tight for us to count this as a detection.
					;(if (< (average dist-matrix) 500)
				   ;; Put in a random heuristic for the confidence value!
				   (make-estimate (* .9 (- 100 (/ 100 (exp (/ (length maxclust) 2)))))
						  clock (centroid (map id->datapoint (map car maxclust)))
						  (map car maxclust))
				   ;; Otherwise we estimate that there's no fire.
				   (make-estimate 0 clock #f (map vectcar newpoints))
				   ))

			     (predictloop (stream-cdr rets) newpoints))))))]
		 [,other (error 'predictloop "bad output from query: ~a" other)]))
	   )) ; end estimates
       
       ;; Sampling detail.  The estimates are only based on
       ;; time-points when we *received data* from the network.  We
       ;; need to insert negative (no-fire) points in any large
       ;; time-gaps here.
       (define estimates
	 (stream-cons (make-estimate 0 0 #f #f) ;; Insert an initial "off" point in the beginning.
	  (let fillgaps ([last #f] [strm protoestimates])
	    (cond
	     [(stream-empty? strm) '()]
	     [(and last 
		   (> (- (estimate-t (stream-car strm)) (estimate-t last))
		      5000 ;point-life
		      ))
	      (printf "INSERTING OFF: ~a \n" (+ (estimate-t last) 1))
	      ;; Insert an "off" for this gap.
	      (stream-cons (make-estimate 0 
					  ;; Insert in midpoint??
					  ;(quotient (+ (estimate-t last) (estimate-t (stream-car strm)))  2)
					  (+ (estimate-t last) 1)
					  #f #f)
			   (stream-cons (stream-car strm)
					(fillgaps (stream-car strm) (stream-cdr strm))))]
	     [else (stream-cons (stream-car strm) (fillgaps (stream-car strm) (stream-cdr strm)))]))))

       ;; Now we transform the stream of estimates into a stream of discrete detections.
       ;; CURRENTLY WORKS FOR ONE FIRE AT A TIME:
       ;; TODO: Needs improvement:
       ;; TODO: use identity-window
       (define detected-events
	 ;; last tracks the last estimate that was from an ON state.
	 (let detectloop ((last #f) (state #f) (estimates estimates))
	   (cond
	    [(stream-empty? estimates) '()]
	    ;; If there was a fire detected this might be an "up" edge.
	    [(and (estimate-pos (stream-car estimates))
		  ;(> (estimate-conf (stream-car estimates)) 10) ;; Minimum confidence.
		  )
	     (let ([head (stream-car estimates)])
	       (if (and ;last 
			;(< (- t (estimate-t last)) 5000) ;; If we already detected this one recently, don't fire again.
			;(> (- conf 10) lastconf)		    
		        state ;; We're already in ON mode, thus no UP-EDGE
		    )
		   (detectloop head #t (stream-cdr estimates))
		   (begin
		     ;(inspect head)
		     (stream-cons head (detectloop head #t (stream-cdr estimates)))
		     )
		   ))]
	    ;; Otherwise 
	    [else (printf "                OFF STATE ~a\n" (estimate-t (stream-car estimates)))
	     (detectloop last #f (stream-cdr estimates))])))
       
       ;; Threw in an extra delay here because otherwise it scrolls all the way forward to first ground event.
       (define real-events 
	 (delay (let detectloop ((last #f) (ground ground))
		  (if (stream-empty? ground) '()
		      (match (stream-car ground)
			[(,newtime ,id GROUND-TRUTH [fires ,fires])
			 (let ((new (filter (match-lambda ((,x ,y ,t ,r))
					      (or (not last)
						  (not (member `(,x ,y) last))))
				      fires)))
			   (stream-append new
					  (detectloop (map (match-lambda ((,x ,y ,t ,r)) `(,x ,y)) fires)
						      (stream-cdr ground))))])))))
       
       ;; Now compare the estimates with the ground truth
;       (let analyze ((t 0) (estimates estimates) (ground ground))
;	 (if (null? ground)
;	     '()
;	     (match (car ground)
;	       [(,t ,id GROUND-TRUTH [fires ,fires])(m
;		(let 

       ;; Analyze lag-till-detection.
       (printf "Computing lag times in detection.\n")
       (fprintf resultslog "# This was data generated on ~a.\n" (date-and-time))
       (fprintf resultslog "# These are the time-lags for fire detection in this run.\n")

;       (fprintf resultslog "# Current Param Settings:\n")
;       (regiment-print-params "#  " resultslog)
       (fprintf resultslog "\n# Data:  false-positives  false-negatives  estimation-lag  Estimated(0)-or-Definite(1)\n")
       (flush-output-port resultslog)

       ;(printf "ACTUAL: ~a\n" (mvfirst (stream-take 3 real-events)))

       ;; Now pull all those streams and pump out the results.
       ;; MAKE SURE THIS LOOP REMAINS IN TAIL POSITION:
	 ;; NOTE: Currently assume no "missed" fires.  The fires get
	 ;; big enough that they cover nodes and then detection is
	 ;; guaranteed.
       (let ([fileposacc 0]  ;; Keep track of the file-pos in spite of broken-ness.
	     [filepos (file-position logport)])
       (let loop ([detects detected-events] 
		  [actual real-events]
		  )
	 (let ([newpos (file-position logport)])
	   (if (> filepos newpos) ;; If old is greater than new:
	       (set! fileposacc (+ fileposacc 2684354560)))
	   (set! filepos newpos))

	 (printf "----> File position: ~a \n" (comma-number (+ fileposacc filepos)))
	 (if (stream-empty? actual)
	     (begin ;; No more fire events.
	       ;; Close open files.
	       (close-output-port resultslog))
	     (begin ;; Consider the next actual event:
	       (printf "  Actual event: ~a\n" (stream-car actual))


	       ;; TODO: FIXME
	       ;; REQUIRE GEOGRAPHIC PROXIMITY WITHIN THE DETECTION.

	       ;; We need a fix on when the next actual fire is happening after this:
	       ;; If there is no next fire we call it +inf.0.
	       ;;
	       ;; We currently enforce a boundary between fires.  There's only one at a 
	       ;; time, and we're not allowed to detect the last fire after the new one 
	       ;; has started.
	       (let ([next-t (if (stream-empty? (stream-cdr actual)) +inf.0
				 (caddr (stream-car (stream-cdr actual))))])
                 (match (stream-car actual)		 
		 [(,x ,y ,t ,r) 
		  (let inner ((falsepos 0) (detects detects))
		    ;; If we hit the end without detecting anything, that's a false negative.
		    (if (or (stream-empty? detects) ;; Either there's no next detection,
			    ;; Or the detection has already missed the current fire.  It's onto the next.
			    (>= (estimate-t (stream-car detects)) next-t))
			;; This printed data point just communicates a false-negative, nothing else.
			(begin (fprintf resultslog "~a ~a ~a ~a  # lag on top of t=~a\n" 
					(pad-width 2 falsepos)
					(pad-width 2 1)
					(pad-width 10 -100000)
					0
					(comma-number (number->integer t)))
			       (printf "Undetected fire at t = ~a\n" t)
			       (flush-output-port resultslog)
			       ;; With that false-negative recorded, we move on to the next fire.
			       (loop detects (stream-cdr actual)))
			(let ([head (stream-car detects)])
			  ;; If the detection happened after the fire, it might be valid.
			  (if (and (>= (estimate-t head) t)
				   ;; But if the fire's already died, we give the system 2 more seconds,
				   ;; and then we say "No, you missed it" to any further detections.
				   (not (> (- (estimate-t head) t) (+ 2000 fire-max-age))))
			      (let ([lag (- (estimate-t head) t)])
				(printf "  Estimated detection: ~a\n" head)
				(fprintf resultslog "~a ~a ~a ~a # lag on top of t=~a\n" 
					 (pad-width 2 falsepos)
					 (pad-width 2 0)
					 (pad-width 10 (number->integer lag))
					 (if (= 100 (estimate-conf head))
					     1 0)
					 (comma-number (number->integer t)))
				(flush-output-port resultslog)
				(printf "Lag time: ~a\n" lag)
				(loop (stream-cdr detects) (stream-cdr actual)))
			      (begin 
				(printf "    (Ignored detection: ~a)\n" head)
				(inner (add1 falsepos) (stream-cdr detects)))
			      ))))]))))))
     )] ;; End "analyze" implementation.
    ))

;; Body of the script:

(define (thescript script-args)
(let ([run-modes '()])
(let loop ((x (map string->symbol script-args)))
  (match x
    [() ;(inspect (vector run-modes curlogfile))
     ;(set! curlogfile (format "./logs/deadsimple_30n_1hr_thresh~a_noise~a.log.gz" 
     ;(varied-param) heat-noise-magnitude))     
     (if (null? run-modes)
	 (void);(begin (main 'run) (main 'analyze))
	 (for-each main run-modes))]

    [(-param ,n ,rest ...)
     (printf "Setting varied-param to ~a\n" n)
     (varied-param (string->number (symbol->string n))) (loop rest)]
    [(-noise ,n ,rest ...) 
     (printf "Setting temperature-noise to stddev ~a\n" n)
     (set! heat-noise-magnitude (string->number (symbol->string n))) (loop rest)]

    [(-l ,file ,rest ...) (set! curlogfile (symbol->string file)) (loop rest)]
    [(-o ,file ,rest ...) (set! resultsfile (symbol->string file)) (loop rest)]    
    [(-w ,file ,rest ...) (set! worldseed (car (file->slist (symbol->string file)))) (loop rest)]

    ;; Actions:
    [(,action ,rest ...) (guard (memq action '(run analyze)))
     (set! run-modes (snoc action run-modes)) (loop rest)]
    [,other (error 'analyze_deadsimple_vs_groundtruth "bad arguments: ~s\n" other)]))))


(thescript (command-line-arguments))
