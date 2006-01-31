#! /bin/sh
#| 
exec scheme --script "$0" `pwd` ${1+"$@"};
|#

;; Usage: ./script <outputname> <gradient-mode> <retry-delay> <max-retries>
;; All the arguments are optional, but can only be omitted from the right.


;; This plots how many nodes we hear from as link quality declines.
;; X-axis: average connectivity of network.
;; Y-axis: number of nodes in the network heard from

;; This script runs an experiment which produces a .data file.
;; The .data file is really an executable scheme file which plots the data.
;; Run it with: "regiment i foo.data"


;; Two different wais to run it:
;; ----------------------------------------
;; Takes about ___ to run on Faith (2.4ghzOpteron) currently [2006.01.26].
(define trials 5)
(define outer 15)
(define nodes 30)

;; Takes about ___ to run on Faith (2.4ghzOpteron) currently [2006.01.26].
;(define trials 1)
;(define outer 10)
;(define nodes 100)

;; ----------------------------------------

;; Load the compiler.
(load (string-append (getenv "REGIMENTD") "/src/compiler_chez.ss"))
(define out (open-output-file 
	     (if (> (length (command-line-arguments)) 1)
		 (list-ref (command-line-arguments) 1)
		 "reception_rate_vs_link_quality.dat") 'replace))

;; Set params:
(simalpha-write-sims-to-disk #f) ;; Better performance, allows simultaneous runs.

(desugar-gradients-mode ;; 'inlined, 'linked, or 'etx
 (if (> (length (command-line-arguments)) 2)
     (string->symbol (list-ref (command-line-arguments) 2))
     'etx))
(etx-retry-delay 
 (if (> (length (command-line-arguments)) 3)
     (string->number (list-ref (command-line-arguments) 3))
     100))
(etx-max-retries
 (if (> (length (command-line-arguments)) 4)
     (string->number (list-ref (command-line-arguments) 4))
     5))

(printf "Analyzing in gradient mode '~a ~a.\nOutput to ~a.\n"
	(desugar-gradients-mode)
	(if (eq? (desugar-gradients-mode) 'etx)
	    (format ", retry-delay ~a, max-retries ~a" (etx-retry-delay) (etx-max-retries))
	    "")
	out)

;; This is the query that we use to test how many nodes we hear from in the aggregation.
(define prog
  `(letrec ([ones (rmap (lambda (_) 1) world)]
	    [count (rfold + 0 ones)]
	    ;;[stamped (smap2 (lambda (t c) (tuple t c)) time count)]
	    ;[stamped (smap (lambda (c) (tuple c)) count)]
	    )
     ;stamped
     count
     ))


;; Results is of the form ([<float> <float>] ...)
(define results
  ;; Here we increment the inner radius from zero up to match the outer
  ;; radius.  This changes the percentage of nodes that have "perfect"
  ;; vs. "fuzzy" reception.
  (map (lambda (rad)
	 (printf ";; Running with inner radius ~s/15\n" rad)
	 (silently 
	  (parameterize ([sim-timeout 3000]
			 [sim-num-nodes nodes]
			 [simalpha-placement-type 'gridlike]
			 [simalpha-channel-model 'linear-disc]
			 [simalpha-outer-radius outer]
			 [simalpha-inner-radius rad]
			 )
	    (let ([comped (run-compiler prog)])
	      (let ([mult-trials
		     (map (lambda (_)
			    (let ((res (run-simulator-alpha comped
							    'srand (current-time))))
			      (let ((avgcon (print-connectivity)))
				(list avgcon 
				      (exact->inexact (average res))
				      (exact->inexact (/ (simalpha-total-messages) (sim-num-nodes)))
				      ))))
		       ;; Do multiple iterations with each setting:
		       (iota trials))])
		(list (average (map car mult-trials))
		      (average (map cadr mult-trials))
		      (average (map caddr mult-trials))
		      ))))))
    (iota 0 (add1 outer))))

;; Going to just dump the data to a gnuplot-style data file instead of this:
;(fprintf out ";; This was data generated on ~a.\\n" (date))
;(fprintf out ";; The graph shows average connectivity (as inner radius\\n")
;(fprintf out ";; is reduced from 15 to 0) vs. number of received messages.\\n")
;(fprintf out "(printf \"Plotting AvgConnectivity against NodesHeard. \\n\")")
;(fprintf out "(printf \"Trials per datapoint ~a, nodes ~a. \\n\")" trials nodes)
;(pretty-print `(gnuplot ',results) out)

(fprintf out "# This was data generated on ~a.\n" (date))
(fprintf out "# The graph shows average connectivity (as inner radius\n")
(fprintf out "# is reduced from 15 to 0) vs. number of received messages.\n")
(fprintf out "# Trials per datapoint ~a, nodes ~a.\n" trials nodes)

(fprintf out "\n# Current Param Settings:\n")
(regiment-print-params "#  " out)

(fprintf out "\n# Columns: AverageConnectivity,  AverageNodesHeardFrom\n")

(for-each (lambda (row) (for-each (lambda (n) (fprintf out "~s " n)) row) (newline out))
  results)

(close-output-port out)

;(inspect results)
;(gnuplot (map list (iota 10 6)
;	      (map exact->inexact (map average results))))

;(gnuplot results)
(exit)
