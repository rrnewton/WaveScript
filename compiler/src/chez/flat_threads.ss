;;[2004.05.24]
;; This file represents a simple interface that I use from 

;; Interface:
;; (run-flat-threads <thunklist>)
;; (run-flat-threads <thunklist> <time-out>)
;;   Runs a bunch of thunks in parallel until some number of seconds
;; have elapsed


;; [2004.06.02] WEIRD.  I'm having a problem where this file will load
;; if I strip the module wrapper, but when I put it back the file
;; hangs on load.
;;   The module system is just defunct now... I think I was having
;; similar problems earlier.

;(module flat_threads_foob (run-flat-threads yield-thread
;		      these-tests test-this)

(define this-unit-description "simple interface for parallel computations")

(define yield-thread engine-block)

;; Using hefty granularity right now.
;; This defines the number of engine-ticks given each 
;; thread in each time-slice.
(define flat-threads-granularity 10)

(define (run-flat-threads thks . time)
  (let ((timeout (if (null? time) #f 
		     (+ (real-time) (* 1000 (car time))))))
    (let loop ([engs (map make-engine thks)]
	       [acc '()])
      (cond
       [(and timeout (> (real-time) timeout)) 
	(fprintf (current-error-port)
		 "~n  !!! run-flat-threads ended by time-out.~n")
	'Threads_Timed_Out]
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

(define these-tests 
  (append (include "generic/flat_threads.tests")

    ;; This one depends on particular ordering/timing, only valid for
    ;; this implementation:
    '([ "Ten threads complete at different times."
     (let ((s (open-output-string)))
	(parameterize ([current-output-port s])
;	(printf "running test~n")
;	(flush-output-port)
  	  (run-flat-threads	   
	   (let loop ((n 0) (acc '()))
	     (if (= n 10)
		 (reverse acc)
		 (loop (add1 n)
		       (cons (lambda () 
			       (let loop ((acc (* 1000 n)))
				 (if (zero? acc) 
				     (display n)
				     (loop (sub1 acc)))))
			     acc)))))
	  (get-output-string s)))
	"0123456789"])))

(define test-this (default-unit-tester this-unit-description these-tests))

;#!eof;(define these-tests ())(define test-this ()))#!eof

;) ;; End module

