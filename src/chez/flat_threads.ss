;;[2004.05.24]
;; This file represents a simple interface that I use from 

;; Interface:
;; (run-flat-threads <thunklist>)
;; (run-flat-threads <thunklist> <time-out>)
;;   Runs a bunch of thunks in parallel until some number of seconds
;; have elapsed
;; Returns either the symbol 'All_Threads_Returned or 'Threads_Timed_Out.

;; [2004.06.02] WEIRD.  I'm having a problem where this file will load
;; if I strip the module wrapper, but when I put it back the file
;; hangs on load.
;;   The module system is just defunct now... I think I was having
;; similar problems earlier.

;(chez:module flat_threads_foob (run-flat-threads yield-thread
;		      these-tests test-this)

(define this-unit-description "flat_threads.ss: Simple interface for parallel computations.")

(define yield-thread engine-block)

;; Using hefty granularity right now.
;; This defines the number of engine-ticks given each 
;; thread in each time-slice.
(define flat-threads-granularity 10)

;; run-flat-threads :: Thunks ->? TimeOut -> All_Threads_Returned | Threads_Timed_Out
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
       [(null? engs)  
	;; Print progress indicator..
	;(display #\.)
	(loop (reverse acc) '())]
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


;; [2004.06.17] Now I modify this to return an engine.  We can't nest
;; engines, but I can make a psuedo-engine that runs a round-robin on
;; some child engines.

;; [2004.06.18] Oops!  This needs to use CPU time or *something*, the
;; timeout system isn't going to work when it's an engine!!  And maybe
;; it shouldn't have a timeout system.

;; Takes:  Thunks, ?Timeout 
;; Returns: Engine
;; ... which returns: 'All_Threads_Returned | 'Threads_Timed_Out
(define (run-flat-threads-engine thks . time)
  (let ([totaltime (if (null? time) #f (* 1000 (car time)))]
	[perdisp (periodic-display 1000)])
    (lambda (ticks succ fail)	       
      (let loop ([ticks ticks] [succ succ] [fail fail]
		 [engs (map make-engine thks)]
		 [acc '()])	
	(if totaltime
	    (perdisp "~n  Total time left on clock: ~s~n" totaltime))
	(cond
	 ;; Timeout because of real time:
	 [(and totaltime (<= totaltime 0)) ;(and timeout (> (real-time) timeout))
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
	  ;; We have to charge ourselves for the time we spend in the child engine:
	  (let ((start-time (real-time)))
	    ((car engs) flat-threads-granularity
	     ;; Child engine finished, it's outof the race.
	     (lambda (remaining ret)
	       ;; Substract elapsed time from totaltime on our timer:
	       (if totaltime 
		   (set! totaltime (- totaltime (- (real-time) start-time))))
	       (loop (+ (- ticks flat-threads-granularity) remaining)
		     succ fail (cdr engs) acc))
	     ;; Engine still not done, queue it up and go to the next.
	     (lambda (nexteng)
	       ;; Substract elapsed time from totaltime on our timer:
	       (if totaltime 
		   (set! totaltime (- totaltime (- (real-time) start-time))))
	       (loop (- ticks flat-threads-granularity)
		     succ fail (cdr engs) (cons nexteng acc)))))]
	 )))))


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
  (append (include "generic/testing/flat_threads.tests")

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
				     (lambda (remaining val) 
				       (if (memq val '(All_Threads_Returned Threads_Timed_Out))
					   (void)
					   (error 'ten-threads-engine-test
						  "run-flat-threads-engine had better return ~a"
						  "either All_Threads_Returned or Threads_Timed_Out!!")))
				     loop))))
	"0123456789"]

      )))

(define test-this (default-unit-tester this-unit-description these-tests))
(define testflatthreads test-this)
;) ;; End module


#; (define p '(let ([s (open-output-string)])
  (parameterize ([current-output-port s])
    (let loop ([eng
                (run-flat-threads-engine
                  (let loop ([n 0] [acc '()])
		    (disp "outer" n acc)(flush-output-port)
                    (if (= n 10)
                        (reverse acc)
                        (loop (add1 n)
                              (cons (lambda ()
                                      (let loop ([acc (* 1000 n)])
					(disp "inner" n)(flush-output-port)
                                        (if (zero? acc)
                                            (display n)
                                            (loop (sub1 acc)))))
                                    acc)))))])
      (eng 100 (lambda (remaining val) val) loop))
    (get-output-string s))))