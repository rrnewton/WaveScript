;;[2004.05.26]

;; This version is for use with the SchemeWidgetLibrary.

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
       [(and timeout (> (real-time) timeout)) 'Simulation_Done]
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

;;======================================================================

(define these-tests
  `(
    [ '1 1 ] 
    [ "Ten threads complete at different times."
     (let ((s (open-output-string)))
	(parameterize ([current-output-port s])
;	(printf "running test~n")
;	(flush-output-port)
  	  (run-flat-threads
	   (map (lambda (n)
					;		(printf "making thread: ~a~n" n)
		  (lambda () 
					;		  (printf "running thunk: ~a~n" n)
		    (let loop ((acc (* 1000 n)))
		      (if (zero? acc) 
			  (display n)
			  (loop (sub1 acc))))))
		(iota 10)))
	  (get-output-string s)))
	"0123456789"]

    [ (let ((s (open-output-string)))
	(parameterize ([current-output-port s])
	  (run-flat-threads 
	   (list (lambda () (let loop () (loop))))
	   .5) 99)) 99]

    [ "One thread waits for another." 
     (let ((v #f)
	    (s (open-output-string)))
	(parameterize ([current-output-port s])
	   (run-flat-threads
	    (list (lambda ()
		    (let loop () 
		      (if v (display "DONE")
			  (loop))))
		  (lambda ()
		    (let loop ((acc 10000))
		      (if (> acc 0)
			  (loop (sub1 acc))
			  (set! v #t)))))))
	(get-output-string s))
      "DONE" ]
))



(define test-this (default-unit-tester this-unit-description these-tests))
