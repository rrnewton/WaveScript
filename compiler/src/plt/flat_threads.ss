
(module flat_threads mzscheme
  (provide run-flat-threads)
  
  (define (run-flat-threads thnks . timeout)
    (let ((threads (map thread thnks)))
      (if (null? timeout)
          (begin
            ;; Wait on them all:
            (for-each thread-wait threads)
            'All_Threads_Returned)
          (begin 
            (sleep (car timeout))
            'Threads_Timed_Out)
          )))
  
  )
  

