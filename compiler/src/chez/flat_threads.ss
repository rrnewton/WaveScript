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

(define this-unit-description "flat_threads.ss: Simple interface for parallel computations.")

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


;; [2004.06.17] Now I modify this to return an engine.
;; We can't nest engines, but I can make a psuedo-engine that runs a
;; round-robin on some child engines.
(define (run-flat-threads-engine thks . time)
  (let ((timeout (if (null? time) #f 
		     (+ (real-time) (* 1000 (car time))))))    
    (lambda (ticks succ fail)	       
      (let loop ([ticks ticks] [succ succ] [fail fail]
		 [engs (map make-engine thks)]
		 [acc '()])
	(cond
	 ;; Timeout because of real time:
	 [(and timeout (> (real-time) timeout))
	  (fprintf (current-error-port)
		   "~n  !!! run-flat-threads ended by time-out.~n")
	  (succ ticks 'Threads_Timed_Out)]
	 ;; Timeout because out of ticks.
	 [(<= ticks 0)
	  (fail (lambda (ticks succ fail)			    
		  (loop ticks succ fail engs acc)))]
		   ;; Succeed because all engines returned.
	 [(and (null? acc) (null? engs)) 
		    (succ ticks 'All_Threads_Returned)]
	 ;; Done with this pass, loop back around:
	 [(null? engs)  (loop ticks succ fail (reverse acc) '())]
	 ;; *Otherwise* actually run a child engine...
	 [else 
	  ((car engs) flat-threads-granularity
	   ;; Child engine finished, it's outof the race.
	   (lambda (remaining ret)
	     (loop (+ (- ticks flat-threads-granularity) remaining)
		   succ fail (cdr engs) acc))
	   ;; Engine still not done, queue it up and go to the next.
	   (lambda (nexteng)
	     (loop (- ticks flat-threads-granularity)
		   succ fail (cdr engs) (cons nexteng acc))))]
	 )))))


;; This is more complexity to save one line:
'(define (run-flat-threads-engine thks . time)
  (let ((timeout (if (null? time) #f 
		     (+ (real-time) (* 1000 (car time))))))    
    (letrec ((theeng
	      (lambda (engs acc)
		(lambda (ticks succ fail)
;;		  (let loop ([ticks ticks] [succ succ] [fail fail]
;			     [engs (map make-engine thks)]
;			     [acc '()])
		  (cond
		   ;; Timeout because of real time:
		   [(and timeout (> (real-time) timeout))
		    (fprintf (current-error-port)
			     "~n  !!! run-flat-threads ended by time-out.~n")
		    (succ ticks 'Threads_Timed_Out)]
		   ;; Timeout because out of ticks.
		   [(<= ticks 0) (fail (theeng engs acc))]
		   ;; Succeed because all engines returned.
		   [(and (null? acc) (null? engs)) 
		    (succ ticks 'All_Threads_Returned)]
		   ;; Done with this pass, loop back around:
		   [(null? engs)  ((theeng (reverse acc) '()) ticks succ fail)]
		   ;; *Otherwise* actually run a child engine...
		   [else 
		    ((car engs) flat-threads-granularity
		     ;; Child engine finished, it's outof the race.
		     (lambda (remaining ret)
		       ((theeng (cdr engs) acc)
			(+ (- ticks flat-threads-granularity) remaining)
			succ fail))
		     ;; Engine still not done, queue it up and go to the next.
		     (lambda (nexteng)
		       ((theeng (cdr engs) (cons nexteng acc))
			(- ticks flat-threads-granularity)
			succ fail)))]
		   )))))
      (theeng (map make-engine thks) '())
      )))


;; Now instead of thunks we use continuation handlers...
;; (define (run-flat-kthreads cont-procs . time) ...

(define (ten-threads-test runfun)
  `(let ((s (open-output-string)))
     (parameterize ([current-output-port s])
					;	(printf "running test~n")
					;	(flush-output-port)
		   ,(runfun
		     '(let loop ((n 0) (acc '()))
			(if (= n 10)
			    (reverse acc)
			    (loop (add1 n)
				  (cons (lambda () 
					  (let loop ((acc (* 1000 n)))
					    (if (zero? acc) 
						(display n)
						(loop (sub1 acc)))))
					acc)))))
		   (get-output-string s))))

(define these-tests 
  (append (include "generic/flat_threads.tests")

    ;; This one depends on particular ordering/timing, only valid for
    ;; this implementation:
    `(
      [ "Ten threads complete at different times."
	,(ten-threads-test (lambda (ls) `(run-flat-threads ,ls)))
	"0123456789"]

      [ "Ten threads complete at different times <Engine Version>"
	,(ten-threads-test (lambda (ls) 
			     `(let loop ((eng (run-flat-threads-engine ,ls)))
				(eng 100
				     (lambda (remaining val) val)
				     loop))))
	"0123456789"]
      )))

(define test-this (default-unit-tester this-unit-description these-tests))
(define testflatthreads test-this)
;) ;; End module

