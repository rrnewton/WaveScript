
(module engine mzscheme
  (provide make-engine
;           (rename timed-apply SECRETapp)
           (rename timed-apply #%app)
           mileage)
  
  (define start-timer #f)
  (define stop-timer #f)
  (define decrement-timer #f)
  (let ((clock 0) (handler #f))
    (set! start-timer
          (lambda (ticks new-handler)
            (set! handler new-handler)
            (set! clock ticks)))
    (set! stop-timer
          (lambda ()
            (let ((time-left clock))
              (set! clock 0)
              time-left)))
    (set! decrement-timer
          (lambda ()
            (if (> clock 0)
                (begin
                  (set! clock (- clock 1))
                  (if (= clock 0) (handler)))))))
  
  (define make-engine
    (let ((do-complete #f) (do-expire #f))
      (define timer-handler
        (lambda ()
          (start-timer (call/cc do-expire) timer-handler)))
      (define new-engine
        (lambda (resume)
          (lambda (ticks complete expire)
            ((call/cc
              (lambda (escape)
                (set! do-complete
                      (lambda (ticks value)
                        (escape (lambda () (complete ticks value)))))
                (set! do-expire
                      (lambda (resume)
                        (escape (lambda ()
                                  (expire (new-engine resume))))))
                (resume ticks)))))))
      (lambda (proc)
        (new-engine
         (lambda (ticks)
           (start-timer ticks timer-handler)
           (let ((value (proc)))
             (let ((ticks (stop-timer)))
               (do-complete ticks value))))))))
  
  (define-syntax timed-apply
    (syntax-rules ()
      ((_ exp ... )
       (begin (decrement-timer)
              (#%app exp ...)))))
  
  (define mileage
    (lambda (thunk)
      (let loop ((eng (make-engine thunk)) (total-ticks 0))
        (eng 50
             (lambda (ticks value)
               (+ total-ticks (- 50 ticks)))
             (lambda (new-eng)
               (loop new-eng (+ total-ticks 50))))))))

#;(begin 
   (require engine)
   
   (define fibonacci
     (lambda (n)
       (if (< n 2)
           n
           (+ (fibonacci (- n 1))
              (fibonacci (- n 2))))))
   
   (define round-robin
     (lambda (engs)
       (if (null? engs)
           '()
           ((car engs) 1
                       (lambda (ticks value)
                         (cons value (round-robin (cdr engs))))
                       (lambda (eng)
                         (round-robin
                          (append (cdr engs) (list eng))))))))
   
   (round-robin
    (map (lambda (x)
           (make-engine
            (lambda ()
              (fibonacci x))))
         '(10 5 2 8 3 7 6 2)))
   
   ; parallel or
   (define-syntax por
     (syntax-rules ()
       ((_ x ...)
        (first-true
         (list (make-engine (lambda () x)) ...)))))
   
   (define first-true
     (lambda (engs)
       (if (null? engs)
           #f
           ((car engs) 1
                       (lambda (ticks value)
                         (or value (first-true (cdr engs))))
                       (lambda (eng)
                         (first-true
                          (append (cdr engs) (list eng))))))))
   
   
   (por 1 2)
   (por ((lambda (x) (x x)) (lambda (x) (x x)))
        (fibonacci 10))
   
   )