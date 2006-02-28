;;[2004.05.26]

;; This version is for use with the SchemeWidgetLibrary.

;(chez:module flat_threads (run-flat-threads yield-thread
;		      these-tests test-this)

(define yield-thread thread-yield)

;; Using hefty granularity right now.
;; This defines the number of engine-ticks given each 
;; thread in each time-slice.
(define flat-threads-granularity 10)

(define (run-flat-threads thks . time)   
  (let ((timeout (if (null? time) #f (* 1000 (car time)))))
;    (disp "starting flatthreads")(flush-output-port)

    (let* ([channel (thread-make-msg-queue 'flat-threads-wait-queue)]
	   [return-thread #f]
	   [threads 
	    (map (lambda (thk) 
		   (thread-fork 
		    (lambda () 
;		      (disp "running thread")(flush-output-port)
		      (thk) 
		      (thread-send-msg channel 'Thread_Done))
		    flat-threads-granularity))
		 thks)]
	   [thread-ids (map thread-number threads)]
	   [timeout-thread 
	    (if timeout
		(thread-fork 
		 (lambda ()
;		   (disp "timer starting")(flush-output-port)
		   (thread-sleep (inexact->exact (round timeout)))
;a		   (disp "timer went off")(flush-output-port)
		   (thread-send-msg channel 'Threads_Timed_Out))		      
		 flat-threads-granularity)
		#f)])
      
 ;     (disp "SETTING UP THREADS") (flush-output-port)
      
      (let loop ((counter (length threads)))
;	(disp "loop " counter) (flush-output-port)
	(if (zero? counter)
	    (begin (if timeout-thread
		       (thread-kill timeout-thread))
		   'All_Threads_Returned)
	    (case (thread-receive-msg channel)
	      [(Thread_Done) (loop (sub1 counter))]
	      [(Threads_Timed_Out)
	       ;; Some might be already dead and this might error:
	       (for-each thread-kill thread-ids)
	       'Threads_Timed_Out]))))))
	
;=======================================================================

(define these-tests (include "generic/flat_threads.tests"))

(define test-this (default-unit-tester 
		    "swl_flat_threads.ss: simple interface for parallel computations"
		    these-tests))
(define testswlflatthreads test-this)

;) ;; End module