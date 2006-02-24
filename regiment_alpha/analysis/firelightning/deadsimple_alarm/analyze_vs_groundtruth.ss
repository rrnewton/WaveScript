#! /bin/sh
#| 
exec regiment i --script "$0" ${1+"$@"};
|#

;;;; .title Analyze Dead-simple fire monitoring vs. groundtruth.

;;;; Usage: ./<script-name> <outputname> <gradient-mode> <retry-delay> <max-retries>
;;;;   <br><br>

;;;; This script uses the deadsimple_alarm.rs query to read *all* the
;;;; temperature readings in the network (over a threshold).  The
;;;; script then takes...............


;----------------------------------------
;; Load the compiler.
;; UNNECESSARY, now I run with regiment i
#; (let ((objectfile (format "~a/src/build/~a/compiler_chez.so" (getenv "REGIMENTD") (machine-type))))
  (if (file-exists? objectfile) 
      (load objectfile)
      (load (string-append (getenv "REGIMENTD") "/src/compiler_chez.ss"))))

;(define curlogfile (format "./deadsimple_~a.log.gz" (current-time)))
(define curlogfile (format "./deadsimple.log.gz"))

(define (number->integer x) (inexact->exact (floor x)))

(random-seed (current-time))

(define-syntax mvfirst
  (syntax-rules ()
    [(_ e) (call-with-values (lambda () e) (lambda args (car args)))]))

;; This is the main procedure, either run or analyze:
(define (main flag)
  (case flag
    [(run)
     ;; Run the simulation.
     ;; TODO: If we can, run it on a separate thread.  It's only executed for its
     ;; writes to the logfile.     
     (let ([devnull (open-output-file "/dev/null" 'append)]
	   [real-console (console-output-port)])
       (printf "Running Simulation in Regiment... (progress will show as vtimes)\n")
       (parameterize ([current-output-port devnull]
					;[console-output-port devnull]
		      )
	 (let loop ((engine 
		     (make-engine 
		      (lambda () 
			(load-regiment (++ REGIMENTD "/demos/firelightning/deadsimple_alarm.rs")
				       '[simulation-logger curlogfile]
				       ;;'[sim-timeout 500000]
				       )))))
	   ;; Print progress using globally stored vtime:
	   (if (simalpha-current-simworld)
	       (fprintf real-console " ~a" (simworld-vtime (simalpha-current-simworld)))
	       (display #\. real-console))
	   (flush-output-port real-console)
	   (engine 75000000 ;536870911
		   (lambda (time-remaining val) (void)) ;; Execute purely for effect.
		   loop))

	 (printf "Simulation finished.\n")
	 (close-output-port devnull)
	 (collect 4)))]

    [(analyze)
     (let ()
       (define logport (open-input-file 
			curlogfile 
			(if (equal? "gz" (extract-file-extension curlogfile))
			    '(compressed) '())))
       (define logstream (begin (printf "\nOpenning log file as stream...\n")
				(reg:read-log logport 'stream)))
       
       (define nodes (begin (printf "Analyzing performance vs. groundtruth...\n")
			    (let loop ((s logstream))
			      (if (stream-empty? s)
				  (error 'analyze_deadsimple_vs_groundtruth "no world description in log")
				  (match (stream-car s)
				    [(,t ,id NEWWORLD ,binds ...)
				     (cadr (assq 'nodes binds))]
				    [,else (loop (stream-cdr s))])))))

       (define ground (stream-filter (lambda (x) (eq? 'GROUND-TRUTH (caddr x))) logstream))
       (define returned (stream-filter (lambda (x) (eq? 'SOCRETURN (caddr x))) logstream))
       
       (define resultslog 
	 (begin (printf "We build a very simple model based on the returned data.\n")
		(printf "Directing output to file: ~a\n" resultsfile)
		(open-output-file resultsfile 'replace)))

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
	 (let-match ([#(,accx ,accy ,sum)
		      (foldl (match-lambda (#(,i ,_ ,t) #(,accx ,accy ,sum))
			       (let ((pos (cadr (assq i nodes))))
				 (vector (+ accx (* t (car pos)))
					 (+ accx (* t (cadr pos)))
					 (+ t sum))))
			#(0.0 0.0 0) pts)])
	   (list (/ accx sum) (/ accy sum))))

       (define (vectcar v) (vector-ref v 0))

       (define estimates
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

		    (let ([definites (filter (match-lambda (#(,i ,c ,t)) (> t 150)) newpoints)])
		      (if (not (null? definites))
			  ;; We are one hundred percent confident in the existence of a 
			  ;; fire once we have any newpoints over 150C.
			  (stream-cons (make-estimate 100 clock (centroid definites) (map vectcar definites))
				       (predictloop (stream-cdr rets) newpoints))
			  
			  ;; Check if there's a critical mass.
					;		    (let ([clusters (clump 500 newpoints)])
					;		      (let ([sums (map (lambda (clust) 
					;					 (foldl (match-lambda (#(,i ,c ,t) ,acc) (+ t acc)) clust))
					;				    clusters)])
					;			(for-each (lambda (pts sum)

			  ;; SIMPLE SIMPLE for now:
			  (let ([sum (foldl (match-lambda (#(,i ,c ,t) ,acc) (+ t acc)) 0 newpoints)])
			    (stream-cons 
			     (if (and ;(> (length newpoints) 1)
				  (> sum 10))
				 ;; Put in a random heuristic for confidence!
				 (make-estimate (* .9 (- 100 (/ 100 (exp (/ (length newpoints) 2)))))
						clock (centroid newpoints) (map vectcar newpoints))
				 ;; Otherwise we estimate that there's no fire.
				 (make-estimate 0 clock #f (map vectcar newpoints)))
			     (predictloop (stream-cdr rets) newpoints))))))]
		 [,other (error 'predictloop "bad output from query: ~a" other)]))
	   )) ; end estimates

       ;; Now we transform the stream of estimates into a stream of discrete detections.
       ;; CURRENTLY WORKS FOR ONE FIRE AT A TIME:
       ;; TODO: Needs improvement:
       ;; TODO: use identity-window
       (define detected-events
	 (let detectloop ((last #f) (estimates estimates))
	   (cond
	    [(stream-empty? estimates) '()]
	    [(estimate-pos (stream-car estimates))
	     (let ((t (estimate-t (stream-car estimates))))
	       (if (and last (< (- last t) 5000))
		   (detectloop t (stream-cdr estimates))
		   (stream-cons (stream-car estimates) 
				(detectloop t (stream-cdr estimates)))))]
	    [else (detectloop #f (stream-cdr estimates))])))
       
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
       (fprintf resultslog "# This was data generated on ~a.\n" (date))
       (fprintf resultslog "# These are the time-lags for fire detection in this run.\n")

;       (fprintf resultslog "# Current Param Settings:\n")
;       (regiment-print-params "#  " resultslog)
       (fprintf resultslog "\n# Data:  false-positives  estimation-lag  Estimated(0)-or-Definite(1)\n")
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
	   (if (> filepos newpos)
	       (set! fileposacc (+ fileposacc 2684354560)))
	   (set! filepos newpos))

	 (printf "----> File position: ~a \n" (comma-number (+ fileposacc filepos)))
	 (if (stream-empty? actual)
	     (begin ;; No more fire events.
	       ;; Close open files.
	       (close-output-port resultslog))
	     (begin 
	       (printf "  Actual event: ~a\n" (stream-car actual))
	       (match (stream-car actual)
		 [(,x ,y ,t ,r) 
		  (let inner ((falsepos 0) (detects detects))
		    (if (stream-empty? detects)
			(begin (fprintf resultslog "-1\n")
			       (flush-output-port resultslog))
			(let ([head (stream-car detects)])
			  (if (>= (estimate-t head) t)
			      (let ([lag (- (estimate-t head) t)])
				(printf "  Estimated detection: ~a\n" head)
				(fprintf resultslog "~a ~a ~a  # lag on top of t=~a\n" 
					 (pad-width 2 falsepos)
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
			      ))))])))))
	)] ;; End "analyze" implementation.
    ))

;; Body of the script:

(let ([run-modes '()])
(let loop ((x (map string->symbol (command-line-arguments))))
  (match x
    [() ;(inspect (vector run-modes curlogfile))
     ;(set! curlogfile (format "./logs/deadsimple_30n_1hr_thresh~a_noise~a.log.gz" 
     ;(varied-param) heat-noise-magnitude))     
     (if (null? run-modes)
	 (void);(begin (main 'run) (main 'analyze))
	 (for-each main run-modes))]

    [(-param ,n ,rest ...) (varied-param (string->number (symbol->string n))) (loop rest)]
    [(-noise ,n ,rest ...) (set! heat-noise-magnitude (string->number (symbol->string n))) (loop rest)]

    [(-l ,file ,rest ...) (set! curlogfile (symbol->string file)) (loop rest)]
    [(-o ,file ,rest ...) (set! resultsfile (symbol->string file)) (loop rest)]    
    ;; Actions:
    [(,action ,rest ...) (guard (memq action '(run analyze)))
     (set! run-modes (snoc action run-modes)) (loop rest)]
    [,other (error 'analyze_deadsimple_vs_groundtruth "bad arguments: ~s\n" other)])))

;(new-cafe)
