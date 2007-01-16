
(module flat_threads mzscheme
  (require (lib "include.ss")
           (prefix rn: "../generic/util/helpers.ss"))
  (provide run-flat-threads yield-thread
           test-this these-tests)
  
  (define (yield-thread) (sleep))
  
  (define (run-flat-threads thks . time)   
    (let ((timeout (if (null? time) #f (car time))))
      ;    (disp "starting flatthreads")(flush-output-port)
      
      (let* ([channel (make-channel)]
             [return-thread #f]
             [threads 
              (map (lambda (thk) 
                     (thread 
                      (lambda () 
                        ;		      (disp "running thread")(flush-output-port)
                        (thk) 
                        (channel-put channel 'Thread_Done))
                      ))
                   thks)]
             [timeout-thread 
              (if timeout
                  (thread 
                   (lambda ()
                     ;		   (disp "timer starting")(flush-output-port)
                     (sleep (inexact->exact (round timeout)))
                     ;a		   (disp "timer went off")(flush-output-port)
                     (channel-put channel 'Timed_Out)))		      
                  #f)])
        
        ;     (disp "SETTING UP THREADS") (flush-output-port)
        
        (let loop ((counter (length threads)))
          ;	(disp "loop " counter) (flush-output-port)
          (if (zero? counter)
              (begin (if timeout-thread
                         (kill-thread timeout-thread))
                     'All_Threads_Returned)
              (case (channel-get channel)
                [(Thread_Done) (loop (sub1 counter))]
                [(Timed_Out)
                 ;; Some might be already dead and this might error:
                 (for-each kill-thread threads)
                 'Threads_Timed_Out]))))))
  
  
  '(define (run-flat-threads thnks . timeout)
     (let ((threads (map thread thnks)))
       (printf "Threads: ~s~n" threads)
       (if (null? timeout)
           (begin
             ;; Wait on them all:
             (for-each 
              (lambda (t)
                (printf "Waiting thread: ~s~n" t)
                (flush-output)
                (thread-wait t)
                (printf "Done thread: ~s~n" t)
                (flush-output)
                )
              
              threads)
             'All_Threads_Returned)
           (begin 
             (sleep (car timeout))
             'Threads_Timed_Out)
           )))
  
  (define these-tests  (include (build-path up "generic" "testing" "flat_threads.tests")))
  
  (define test-this (rn:default-unit-tester 
                     "flat_threads.ss: simple parallel computation system for PLT"
                     these-tests))
  
  )


;(require flat_threads) (test-this)
