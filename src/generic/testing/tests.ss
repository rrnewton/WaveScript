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


;; This runs all the system tests AND unit tests.
(define (test-everything . args)
  (and (apply test-units 'verbose 'quiet args)
       ;; Finlly run all the compiler system tests.
       (printf "~n;; Testing the whole system on the compiler test cases:~n")
       (test-all) (newline)  (newline)))

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
(define (te . args) (apply test-everything 'verbose 'quiet args)) ;; shorthand

;; Replacing this with much simpler system above:
#;(define (test-units . args)
  (printf "~n;; Performing all unit tests:~n")    
  ;; Pass flags ('quiet 'verbose) onward to the unit tester:
  (let ([test-it (case-lambda
		  [() (apply test-this (filter symbol? args)) (newline)]
		  [(tester) (apply tester (filter symbol? args)) (newline)])]
	[loudload (lambda (s) (printf "~n;; Loading ~a for unit testing... ~n" s) 
			      (load s))])
    (case current_interpreter
      [(chezscheme)

       (test-it compilertest)
       (test-it test-verify-regiment)
	;; Missing eta prims!
       (test-it test-rename-vars)
        ;; Missing remove unquoted constant
       (test-it test-static-elaborate)
        ;; Missing reduce primitives.
        ;; Missing remove complex constant
;       (test-it test-verify-stage2)  ;; Empty presently!
        ;; Missing uncover free.
        ;; Missing lift-letrec
        ;; Missing lift-letrec-body
       (test-it test-remove-complex-opera)
       (test-it test-verify-core)
       (test-it test-classify-names)
        ;; Missing add heartbeats
        ;; Missing add control flow
        ;; Missing add places
        ;; Missing 
       (test-it test-analyze-places)
        ;; Missing add routing
       (test-it test-deglobalize)
       (test-it test-cleanup-token-machine)

       (test-it test-desugar-gradients)

       (test-it test-cps-tokmac)
        ;; Missing verify token machine
       (test-it test-haskellize-tokmac)


;       (loudload "compiler.ss") (test-it) (newline)
;;;       (loudload "generic/analysis.ss") (test-it) (newline)
;       (loudload "generic/pass00_verify-regiment.ss") (test-it) (newline)
;       (loudload "generic/pass02_rename-vars.ss") (test-it) (newline)
;       (loudload "generic/pass04_static-elaborate.ss") (test-it) (newline)
;       (loudload "generic/pass05_verify-stage2.ss") (test-it) (newline)
;       (loudload "generic/pass09_remove-complex-opera.ss") (test-it) (newline)
       ;  (load "pass01_rename-var.ss") (test-it) (newline)
 ;      (loudload "generic/pass10_verify-core.ss") (test-it) (newline)
;       (loudload "generic/pass11_classify-names.ss") (test-it) (newline)
 ;      (loudload "generic/pass15_analyze-places.ss") (test-it) (newline)
  ;     (loudload "generic/pass16_deglobalize.ss") (test-it) (newline)
  ;     (loudload "generic/pass17_cleanup-token-machine.ss") (test-it) (newline)
;       (loudload "generic/pass18_cps-tokmac.ss") (test-it) (newline)
;       (loudload "generic/pass19_haskellize-tokmac.ss") (test-it) (newline)

       ;; Must be loaded to see inside the module.
       (loudload "chez/simulator_nought.ss") (test-it) (newline)

       (if (top-level-bound? 'SWL-ACTIVE)
	   (begin 
	     (printf "~n SWL DETECTED.  TESTING GRAPHICAL MODULES:~n")
	     (loudload "chez/swl_flat_threads.ss")
	     (let () ;(import flat_threads)
	       (test-it) (newline))

	     (loudload "chez/graphics_stub.ss") (test-it) (newline)
	     (loudload "chez/simulator_nought_graphics.ss") (test-it) (newline)
	     )
	   (begin 
	     (loudload "chez/flat_threads.ss") 
;	     (import flat_threads)
	     (test-it) (newline)))]
      [(mzscheme)

       (load/use-compiled "main_plt.ss") (test-it) (newline)
       
       (test00) (newline)
       ;  (load "pass01_rename-var.ss") (test-it) (newline)
       (test08) (newline)
       (test10) (newline)
       (testsim) (newline)
       (testgsim) (newline)

       (error 'test-everything "RYAN FINISH THIS")]
      )
    ))
   
;===============================================================================

(define tests
					;  `( ,@tests_old
					;     ,@tests_new))
  `( ,@tests_noclosure
     ,@tests_regiment
     ))
