
(module flat_threads mzscheme
  (require (lib "include.ss")
           (prefix rn: "helpers.ss"))
  (provide run-flat-threads test-this these-tests)
  
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
  
  (include "../generic/flat_threads.tests")
  
  (define test-this (rn:default-unit-tester 
                     "flat_threads.ss: simple parallel computation system for PLT"
                     these-tests))
  
  )
  

