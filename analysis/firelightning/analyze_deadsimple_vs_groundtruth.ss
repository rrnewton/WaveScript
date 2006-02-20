#! /bin/sh
#| 
exec chez --script "$0" ${1+"$@"};
|#

;;;; .title Analyze Dead-simple fire monitoring vs. groundtruth.

;;;; Usage: ./<script-name> <outputname> <gradient-mode> <retry-delay> <max-retries>
;;;;   <br><br>

;;;; This script uses the deadsimple_alarm.rs query to read *all* the
;;;; temperature readings in the network (over a threshold).  The
;;;; script then takes...............


;----------------------------------------
;; Load the compiler.
(let ((objectfile (format "~a/src/build/~a/compiler_chez.so" (getenv "REGIMENTD") (machine-type))))
  (if (file-exists? objectfile) 
      (load objectfile)
      (load (string-append (getenv "REGIMENTD") "/src/compiler_chez.ss"))))

(define curlogfile "./deadsimple.log")

(random-seed (current-time))

;; Run the simulation.

;; Keep some data top-level for debugging.  This is just a script afterall!
(define nodes)
(define returned)
(define ground)
(define estimates)
(define detected-events)
(define real-console)

;; TODO: If we can, run it on a separate thread.  It's only executed for its
;; writes to the logfile.


(define (main flag)
  (case flag
    [(run)
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

     (printf "\nOpenning log file as stream...\n")
     ;; Note this takes advantage of Chez's ability to mutate undefined top level symbols:
     (set! logstream (reg:read-log curlogfile 'stream))
       
     (printf "Analyzing performance vs. groundtruth...\n")
     
     (time (set! all (stream-take-all logstream)) ;; inefficient
	   )

     (set! nodes (let loop ((l all))
		   (match l
		     [() (error 'analyze_deadsimple_vs_groundtruth "no world description in log")]
		     [((,t ,id NEWWORLD ,binds ...) . ,_)
		      (cadr (assq 'nodes binds))]
		     [(,_ . ,rest) (loop rest)])))
     (set! ground (filter (lambda (x) (eq? 'GROUND-TRUTH (caddr x))) all))
     (set! returned (filter (lambda (x) (eq? 'SOCRETURN (caddr x))) all))
	 
     (printf "We build a very simple model based on the returned data.\n")

     (let ()     
       (define resultslog (open-output-file "results.dat" 'replace))

       ;(reg:define-struct (datum t d))
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

       (set! estimates
	 (let ([points '()]
	       [output '()])
	   ;; TRACKS ONLY ONE FIRE CURRENTLY:
	   ;; This produces a stream of estimates.
	   (let predictloop ((rets returned))
	     ;; Add the new data point:
	     (if (null? rets)
		 (reverse! output)
		 (match (car rets)
		   [(,clock ,__ SOCRETURN [val #(,id ,nodeclock ,temp)])
		    ;; Expire old points and remove the existing entry for this node.
		    (set! points
			  (filter (match-lambda (#(,i ,c ,t))
				    (and (not (= i id))
					 (not (> (- clock c) point-life))))
			    points))
		    (set! points (cons (vector id clock temp) points))

		    (let ([definites (filter (match-lambda (#(,i ,c ,t)) (> t 150)) points)])
		      (if (not (null? definites))
			  ;; We are one hundred percent confident in the existence of a 
			  ;; fire once we have any points over 150C.
			  (set! output (cons (make-estimate 100 clock (centroid definites) (map vectcar definites)) output))
			  
			  ;; Check if there's a critical mass.
			;		    (let ([clusters (clump 500 points)])
			;		      (let ([sums (map (lambda (clust) 
			;					 (foldl (match-lambda (#(,i ,c ,t) ,acc) (+ t acc)) clust))
			;				    clusters)])
			;			(for-each (lambda (pts sum)

			  ;; SIMPLE SIMPLE for now:
			  (let ([sum (foldl (match-lambda (#(,i ,c ,t) ,acc) (+ t acc)) 0 points)])
			    (set! output 
				  (cons 
				   (if (and ;(> (length points) 1)
					    (> sum 10))
				       ;; Put in a random heuristic for confidence!
				       (make-estimate (* .9 (- 100 (/ 100 (exp (/ (length points) 2)))))
						      clock (centroid points) (map vectcar points))
				       ;; Otherwise we estimate that there's no fire.
				       (make-estimate 0 clock #f (map vectcar points)))
				   output)))))
		    (predictloop (cdr rets))]
		   [,other (error 'predictloop "bad output from query: ~a" other)]))
	     ))) ; end estimates
	   
       ;; Now we transform the stream of estimates into a stream of discrete detections.
       ;; CURRENTLY WORKS FOR ONE FIRE AT A TIME:
       ;; TODO: Needs improvement:
       ;; TODO: use identity-window
       (set! detected-events
	 (let detectloop ((last #f) (estimates estimates))
	   (cond
	    [(null? estimates) '()]
	    [(estimate-pos (car estimates))
	     (let ((t (estimate-t (car estimates))))
	       (if (and last (< (- last t) 5000))
		   (detectloop t (cdr estimates))
		   (cons (car estimates) (detectloop t (cdr estimates)))))]
	    [else (detectloop #f (cdr estimates))])))

       (set! real-events
	 (let detectloop ((last #f) (ground ground))
	   (match ground
	    [() '()]
	    [((,newtime ,id GROUND-TRUTH [fires ,fires]) ,rest ...)
	     (let ((new (filter (match-lambda ((,x ,y ,t ,r))
				  (or (not last)
				      (not (member `(,x ,y) last))))
			  fires)))
	       (append new
		       (detectloop (map (match-lambda ((,x ,y ,t ,r)) `(,x ,y)) fires)
				   (cdr ground))))])))

       ;; Now compare the estimates with the ground truth
;       (let analyze ((t 0) (estimates estimates) (ground ground))
;	 (if (null? ground)
;	     '()
;	     (match (car ground)
;	       [(,t ,id GROUND-TRUTH [fires ,fires])(m
;		(let 

       (print-length 500)
       (printf "Estimated detections: \n")
       (pretty-print detected-events)
       (printf "Actual events: \n")
       (pretty-print real-events)

       ;; Analyze lag-till-detection.
       (printf "Computing lag times in detection.\n")
       (fprintf resultslog "# This was data generated on ~a.\n" (date))
       (fprintf resultslog "# These are the time-lags for fire detection in this run.\n")
       (fprintf resultslog "\n# Current Param Settings:\n")
       (regiment-print-params "#  " resultslog)
       (let loop ((detects detected-events) (actual real-events))
	 (unless (null? actual)
	   (match (car actual)
	     [(,x ,y ,t ,r) 
	      (let inner ((detects detects))
		(if (null? detects)
		    (fprintf resultslog "-1\n")
		    (if (>= (estimate-t (car detects)))
			(begin 			  
			  (fprintf resultslog "~a\n" (- (estimate-t (car detects)) t))
			  (loop (cdr detects) (cdr actual)))
			;; TODO: increment false positives!
			(inner (cdr detects))
			)))])))
       
       ;; Close open files.
       (close-output-port resultslog)

       )]))

;; Body of the script:

(let loop ((x (map string->symbol (command-line-arguments))))
  (match x
    [() (void)]
    [(-l ,file ,rest ...) (set! curlogfile (symbol->string file)) (loop rest)]
    [(run ,rest ...) (main 'run) (loop rest)]
    [(analyze ,rest ...) (main 'analyze) (loop rest)]
    [,other (error 'analyze_deadsimple_vs_groundtruth "bad arguments: ~s\n" other)]))

(new-cafe)
