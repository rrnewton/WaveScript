
(define make-standalone-test
  (lambda (tests)
    `(let ([failed
             (lambda (expect receive)
               (display " ...FAILED! ") (newline)
               (display "Expected value was: ")
               (display expect)(newline)
               (display "Instead recieved: ")
               (display receive)(newline))])
       ,(let loop ([n 0] [tests tests])
          (if (null? tests)
              `(begin (display "All tests completed successfully.")
                      (newline) (newline))
              (let ([realval (eval (car tests))])
                `(begin (display ,n) (display ": ")
                        (pretty-print ',(car tests))
                        (let ([v ,(car tests)])
                          (if (equal? v ',realval)
                              ,(loop (add1 n)(cdr tests))
                              (failed realval v))))))))))

(define test-noexec
  (lambda ()
    (printf "yay~n")))

;===============================================================================

(define tests_regiment '())

;===============================================================================

'(define tests_new
  `( ,@tests_new_misc
     ,@tests_derived-prims
     ;,@tests_callcc
     ))

'(define tests_old
  `( ,@tests_noclosure  ;67
     ,@tests_quick      ;93
     ,@tests_medium     ;31
     ,@tests_slow       ; 9
     ))

(define tests
					;  `( ,@tests_old
					;     ,@tests_new))
  `( ,@tests_noclosure
     ,@tests_regiment
     ))
  
;===============================================================================

(define tests_noexec
  `( ))

;===============================================================================

'(define large-test
  `(vector
     ,@tests_noclosure))

'(define huge-test
  `(vector
     ,@tests_noclosure
     ,@tests_quick))

'(define giant-test
  `(vector ,@tests_old))

'(define colossal-test
  `(vector ,@tests))

;===============================================================================

(define write-test-file
  (lambda ()
    (define gensym
      (let ((counter 0))
        (lambda ()
          (set! counter (add1 counter))
          (string->symbol (string-append "g" (number->string counter))))))
    (let ([out (open-output-file "test_dump.ss" 'replace)])
      (write '(display "Starting tests...") out)(newline out)
      (write '(newline) out)(newline out)
      (write '(define start_time (real-time)) out)(newline out)
      (write '(newline) out)(newline out)(newline out)
      (let loop ([tests tests] [sym-acc '()])
        (if (null? tests)
            (begin
              (write `(define test_results (list ,@sym-acc)) out)
              (newline out)(newline out)
              (write `(display "Done running tests.") out)(newline out)
              (write `(newline) out)(newline out)
              (write `(display "Time was ") out)(newline out)
              (write '(display (- (real-time) start_time)) out)(newline out)
              (write '(display " milleseconds") out)(newline out)
              (write `(newline) out)(newline out)
              (newline out)
              (close-output-port out))
            (begin
              (let ([sym (gensym)])
                (write `(define ,sym ,(car tests)) out)
                (newline out)(newline out)
                (loop (cdr tests) (cons sym sym-acc)))))))))