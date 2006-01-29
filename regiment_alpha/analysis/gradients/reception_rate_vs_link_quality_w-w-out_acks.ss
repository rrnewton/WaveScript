#! /bin/sh
#| 
exec scheme --script "$0" `pwd` ${1+"$@"};
|#

;; This plots how many nodes we hear from as link quality declines.
;; X-axis: average connectivity of network.
;; Y-axis: number of nodes in the network heard from

;; This script runs an experiment which produces a .data file.
;; The .data file is really an executable scheme file which plots the data.
;; Run it with: "regiment i foo.data"

;; Takes about ___ to run on Faith (2.4ghzP4) currently [2006.01.26].
(define trials 10)
(define outer 15)
(define nodes 30)

;; Takes about ___ to run on Faith (2.4ghzP4) currently [2006.01.26].
;(define trials 1)
;(define outer 10)
;(define nodes 100)

(load (string-append (getenv "REGIMENTD") "/src/compiler_chez.ss"))
(define out (open-output-file "reception_rate_vs_link_quality_w-w-out_acks.data" 'replace))

(define prog
  `(letrec ([ones (rmap (lambda (_) 1) world)]
	    [count (rfold + 0 ones)]
	    ;;[stamped (smap2 (lambda (t c) (tuple t c)) time count)]
	    ;[stamped (smap (lambda (c) (tuple c)) count)]
	    )
     ;stamped
     count
     ))


(define results
  ;; This we change the inner ring from zero out to match the outer
  ;; radius.  This changes the percentage of nodes that have "perfect"
  ;; vs. "fuzzy" reception.
  (map (lambda (rad)
	 (printf ";; Running with inner radius ~s/15\n" rad)
	 (silently 
	  (parameterize ([sim-timeout 2000]
			 [sim-num-nodes nodes]
			 [simalpha-placement-type 'gridlike]
			 [simalpha-channel-model 'linear-disc]
			 [simalpha-outer-radius outer]
			 [simalpha-inner-radius rad]
			 )
	    (let ([comped (run-compiler prog)])
	      (let ([mult-trials
		     (map (lambda (_)
			    (let ((res (run-simulator-alpha comped)))
			      (let ((avgcon (print-connectivity)))
				(list avgcon (exact->inexact (average res)))
				)))
		       ;; Do multiple iterations with each setting:
		       (iota trials))])
		(list (average (map car mult-trials))
		      (average (map cadr mult-trials))))))))
    (iota 0 (add1 outer))))

(fprintf out ";; This was data generated on ~a.\n" (date))
(fprintf out ";; The graph shows average connectivity (as inner radius\n")
(fprintf out ";; is reduced from 15 to 0) vs. number of received messages.\n")
(fprintf out "(printf \"Plotting AvgConnectivity against NodesHeard. \n\")")
(fprintf out "(printf \"Trials per datapoint ~a, nodes ~a. \n\")" trials nodes)
(pretty-print `(gnuplot ',results) out)
(close-output-port out)

;(inspect results)
;(gnuplot (map list (iota 10 6)
;	      (map exact->inexact (map average results))))


;(gnuplot results)
(exit)
     
