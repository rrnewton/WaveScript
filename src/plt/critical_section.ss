
(define *global-semaphore* (make-semaphore 1))

(define-syntax critical-section
  (syntax-rules ()
    [(_ expr ...)
     (begin (semaphore-wait *global-semaphore*)
            (let ((result (begin expr ...)))
              (semaphore-post *global-semaphore*)
              result))]))


(define (test)
  (thread (lambda ()
            (let loop ((i 0))
              (if (< i 1000)
                  (begin 
                    (begin ;critical-section
                     (display "a")
                     (display "A"))
                    (loop (+ 1 i)))))))
  (thread (lambda ()
            (let loop ((i 0))
              (if (< i 1000)
                  (begin (begin ;critical-section
                          (display "b")
                          (display "B"))
                         (loop (+ 1 i))))))))
