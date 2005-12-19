#! /bin/sh
#| 
exec scheme --script "$0" `pwd` ${1+"$@"};
|#


;(define trials 10)
;(define outer 15)
;(define nodes 30)

(define trials 1)
(define outer 10)
(define nodes 100)

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
	    (let ((comped (run-compiler prog)))
	      (let ((mult-trials
		     (map (lambda (_)
			    (let ((res (run-simulator-alpha comped)))
			      (let ((avgcon (print-connectivity)))
				(list avgcon (exact->inexact (average res)))
				)))
		       ;; Do multiple iterations with each setting:
		       (iota trials))))
		(list (average (map car mult-trials))
		      (average (map cadr mult-trials))))))))
    (iota 0 (add1 outer))))

(fprintf out ";; This was data generated on ~a.\n" (date))
(fprintf out ";; The graph shows average connectivity (as inner radius\n")
(fprintf out ";; is reduced from 15 to 0) vs. number of received messages.\n")
(pretty-print `(gnuplot ',results) out)

;(inspect results)
;(gnuplot (map list (iota 10 6)
;	      (map exact->inexact (map average results))))


(gnuplot results)
     
