;;;; .title Testing functions.

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

(define tests_regiment 
  '(
    (circle-at 30 40 50) ;; Location 30,40 radius 50

    (rmap nodeid world)
    (rfold + 0 (rmap nodeid world))        

; [2005.09.27] Disabling temporarily: 
;    (rmap (lambda (r) (rfold + 0 r))
;	  (rrcluster (rfilter even? (rmap nodeid world))))

    ))

;===============================================================================

;; [2007.05.16] Not using right now:
#;
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

;===============================================================================


;; [2004.06.11] This runs compiler tests for the whole system, then
;; runs all the unit tests.
;;
;; [2005.02.26] Changing it so that it assumes the files under test
;; are already loaded, and just calls their unit testers by name.
(define (test-units . args)
  (printf "~n;; Performing all unit tests:~n~n")
  (if (andmap (lambda (pr) (newline) (newline) (apply (cadr pr) args))
	      (reverse (reg:all-unit-tests)))
      (begin (printf "\n PASSED ALL TESTS.\n") #t)
      (if (and (top-level-bound? 'REGIMENT-BATCH-MODE)
	       (top-level-value 'REGIMENT-BATCH-MODE))
	  (exit 1)
	  #f)))


;; [2006.01.24] Adding this "metatester" to reparameterize the system
;; in various major ways and run the unit tests in each configuration.
(define (super-test . args)
  (if (not (and 
	    ;; First lets work through our different gradient implementations:
	    (printf "\n;====================================================================================================\n")
	    (printf ";; TESTING W/ STATIC GRADIENTS.\n")
	    (desugar-gradients-mode 'static)
	    (apply test-units args)

	    (printf "\n;====================================================================================================\n")
	    (desugar-gradients-mode 'dynamic)
	    (apply test-units args)

	    (printf "\n;====================================================================================================\n")
	    (desugar-gradients-mode 'etx)
	    (apply test-units args)

	    (printf "\n;====================================================================================================\n")
	    (printf ";; SUPER TEST COMPLETED.\n")
	    ))
      (error 'super-test "One of the batches of unit tests failed.")))

(define (tu . args) (apply test-units 'verbose 'quiet args)) ;; shorthand
   
;===============================================================================

#;
(define tests
					;  `( ,@tests_old
					;     ,@tests_new))
  `( ,@tests_noclosure
     ,@tests_regiment
     ))
