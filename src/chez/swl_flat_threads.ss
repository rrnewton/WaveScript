;;[2004.05.26]

;; This version is for use with the SchemeWidgetLibrary.

(define this-unit-description 
  "swl_flat_threads.ss: simple interface for parallel computations")

;; Using hefty granularity right now.
;; This defines the number of engine-ticks given each 
;; thread in each time-slice.
(define flat-threads-granularity 1000)

(define (run-flat-threads thks . time)   
  (let ((timeout (if (null? time) #f (* 1000 (car time)))))
    (call/cc
     (lambda (return)       
       (let* ([channel (thread-make-msg-queue)]
	      [threads 
	       (map (lambda (thk) 
		      (thread-fork 
		       (lambda () (thk) (thread-send-msg channel 'Thread_Done))
		       flat-threads-granularity))
		    thks)]
	      [thread-ids (map thread-number threads)]
	      [return-thnk	
	       (lambda ()
		 (let loop ((counter (length threads)))
		   (if (zero? counter)
		       (return 'All_Threads_Returned)
		       (begin (thread-receive-msg channel)
			      (loop (sub1 counter))))))])	
	 (if timeout
	     (begin 
	       (thread-fork return-thnk flat-threads-granularity)
	       (thread-sleep timeout)
	       (return 'Threads_Timed_Out))
	     (return-thnk)))))))
	
;;======================================================================

(include "generic/flat_threads.tests")

(define test-this (default-unit-tester this-unit-description these-tests))
