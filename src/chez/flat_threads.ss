;;[2004.05.24]
;; This file represents a simple interface that I use from 

;; Interface:
;; (run-flat-threads <thunklist>)
;; (run-flat-threads <thunklist> <time-out>)
;;   Runs a bunch of thunks in parallel until some number of seconds
;; have elapsed


(define this-unit-description "simple interface for parallel computations")

;; Using hefty granularity right now.
;; This defines the number of engine-ticks given each 
;; thread in each time-slice.
(define flat-threads-granularity 1000)

(define (run-flat-threads thks . time)
  (let ((timeout (if (null? time) #f 
		     (+ (real-time) (* 1000 (car time))))))
    (let loop ([engs (map make-engine thks)]
	       [acc '()])
      (cond
       [(and timeout (> (real-time) timeout)) 'Threads_Timed_Out]
       [(and (null? acc) (null? engs)) 'All_Threads_Returned]
       [(null? engs)  (loop (reverse acc) '())]
       [else 
	((car engs) flat-threads-granularity
	 (lambda (remaining ret) 
	   ;; When an engine returns we just move on without it.
	   ;(error 'run-simulation "engine shouldn't return.  Values were: ~n~s~n" ret))
;	   (fprintf (current-error-port)
;	    "Engine #~s returned!: ~s~n" (length acc) ret)
	   (loop (cdr engs) acc))
	 (lambda (nexteng)
	   (loop (cdr engs) (cons nexteng acc))))]
      ))))

(include "generic/flat_threads.tests")

(define test-this (default-unit-tester this-unit-description these-tests))
