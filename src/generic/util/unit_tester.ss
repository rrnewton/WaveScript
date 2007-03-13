;; Note: becuase of recursive dependencies, this file is included into
;; helpers.ss rather than being its own module.

;(module unit_tester mzscheme
;  (require "../../plt/iu-match.ss" "../../plt/chez_compat.ss")
 
;;[2004.06.13] Making this not allow an error to match against unspecified!
(define (lenient-compare? o1 o2)
  (or (eq? o1 o2)
      ;; Strings are not deep structures according to eq-deep,
      ;; So we compare them with equal?
      (and (string? o1) (equal? o1 o2))
      (and (eq? o1 'unspecified) (not (eq? o2 'error)))
      (and (eq? o2 'unspecified) (not (eq? o1 'error)))))

;; This provides a weird sort of interface to a deep equal.  It walks
;; down the tree, applying the input comparator at every intermediate
;; node, only proceeding downward on negative comparisons.
;; [2004.07.21] - Fixed it's behaviour against dotted pairs.
(define eq-deep 
  (lambda (eq)
    (lambda (obj1 obj2)
      (let loop ((o1 obj1) (o2 obj2))
	(cond
	 [(eq o1 o2) #t]
	 [(and (list? o1) (list? o2))
	  (if (= (length o1) (length o2))
	      (andmap loop o1 o2)
	      #f)]
	 [(and (pair? o1) (pair? o2)) ;; Kinda silly to have both these.
	  (and (loop (car o1) (car o2)) ;; the above should save stack space though..
	       (loop (cdr o1) (cdr o2)))]
	 [(and (vector? o1) (vector? o2))
	  ; Treat same as lists:
	  (loop (vector->list o1) (vector->list o2))]
	 [else #f])))))

(define tester-eq? (eq-deep lenient-compare?))
(define tester-equal? (eq-deep lenient-compare?))


;; [2004.04.21] I've started using the (ad-hoc) convention that every
;; file should define "these-tests" and "test-this" for unit testing.
;; This is inspired by the drscheme philosophy of every file being an
;; executable unit...  But it *was* unbearable to duplicate this
;; little tester code across every file 
;; 
;; [2004.05.24] Replacing the default tester with a better one.
;; [2004.06.03] Adding optional preprocessor function
;; [2004.07.21] Added a 'quiet flag.  
;; [2005.02.06] Made the quiet flag also suppress warnings.
;; [2005.09.24] Making failed tests retry optionally, run with flag 'retry
;;              This is for nondeterministic tests that merely have a high 
;;              probability of success.  'retry can be specified either at 
;;              tester-construction time or test-time.
;; [2005.09.25] Modifying the tester to return true or false based on 
;;              whether all tests pass.
;; Forms:
;;  (default-unit-tester message these-tests)
;;  (default-unit-tester message these-tests equalfun)
;;  (default-unit-tester message these-tests equalfun preprocessor)

;; [2005.02.24] Working around weird PLT bug:
(define voidproc (lambda args (void)))

  
(define default-unit-tester
  (lambda (message these-tests . extras)

    ;; Print widths:
    ;; TODO: I should make these adjustable parameters.
    (define TESTWIDTH 70)
    (define ORACLEWIDTH 30)
    (define INTENDEDWIDTH 20)

    ;; Default values of tester-construction time parameters:
    (let ([teq? tester-equal?]
	  [preprocessor (lambda (x) x)]
	  [retry-failures #f]
	  [enabled #t])
    ;; Go through tester construction-time additional arguments: 
    (let arg-loop ([ls extras] [procsseen 0])
      (cond
       [(null? ls) (void)]
       ;; This is a little lame, first proc is equality function, second is preprocessor:
       [(procedure? (car ls))
	(if (= 0 procsseen)
	    (set! teq? (car ls))
	    (if (= 1 procsseen)
		(set! preprocessor (car ls))
		(error 'default-unit-tester "Too many proc arguments!: ~a" (car ls))))
	(arg-loop (cdr ls) (add1 procsseen))]
       [(memq (car ls) '(disable disabled))
	(set! enabled #f)
	(arg-loop (cdr ls) procsseen)]
       [(eq? (car ls) 'retry) 
	(set! retry-failures #t)
	(arg-loop (cdr ls) procsseen)]
       [else (error 'default-unit-tester "Unknown argument or flag: ~a" (car ls))]))
	
    ;; Now we construct the actual tester procedure:
    (let ((testerproc 
      (let ([entries
	      ;; This canonicalizes them so that they're all four-long:
	   (map 
	    (lambda (entry)
	      (match entry 
		     [(,test ,result)      `(#f   () ,test ,result)]
		     [(,msg ,test ,result) `(,msg () ,test ,result)]
		     [(,msg ,moreargs ... ,test ,result)
		      `(,msg ,moreargs ,test ,result)]
		     [else (error 'default-unit-tester 
				  " This is a bad test-case entry!: ~s~n" entry)]))
	       these-tests)])
    (lambda args 
    (call/cc
     (lambda (return)
       (match (memq 'get args)
	 [#f (void)]
	 [(get ,n ,_ ...) (guard (integer? n))
	  (return (list-ref entries n))]
	 [,else (return entries)])
       (when (or (memq 'print-tests args) (memq 'print args))
	   (for-eachi (lambda (i test)
			(if (string? (car test))
			    (printf "~a: ~a\n" i (car test))))
		      entries)
	   (return (void)))

	 (let (;; Flag to suppress test output.  This had better be passed
	       ;; *after* any comparison or preprocessor arguments.
	       [quiet (or (memq 'quiet args)
			  (memq 'silent args)
			  (memq 'q args)
			  (not (or (memq 'verbose args)
				   (memq 'v args))))]
	       ;; Flag to print test descriptions as well as code/output.
	       [titles (not (or (memq 'silent args)
				(memq 'nodescrips args)))]
	       [retry-failures (or retry-failures ;; If already set above, or..
				   (memq 'retry args))]
;	       [descriptions (map car entries)]
;	       [tests (map caddr entries)]
;	       [intended (map cadddr entries)]
	       [success #t]
	       [tests-to-run (filter number? args)]
	       [suppressed-test-output (open-output-string)]
	       )

	   ;; This (long) sub-procedure executes a single test:
	   (let ([execute-one-test
	       (lambda (num entry)
;		 (IFCHEZ (collect) ;; [2006.02.18] Let's do a bit of collection between tests to reduce pauses.
		 (match entry
		   [(,descr ,extraflags ,expr ,intended)
		    ;(printf "extraflags! ~a\n"  extraflags)
		    (fluid-let ([retry-failures (or retry-failures (memq 'retry extraflags))])
		    
		 (let retryloop ((try 0))
		   (flush-output-port)
	       ;; This prints a name, or description for the test:
	       (if (and titles descr) (printf "   ~s~n" descr))

	       (display-constrained `(,num 10) "  " `(,expr ,TESTWIDTH)
				    " -> ")
	       (if (procedure? intended)
		   (display-constrained "Satisfy oracle? " 
					`(,intended ,ORACLEWIDTH) ": ")
		   (display-constrained `(,intended ,INTENDEDWIDTH) ": "))
	       
	       (flush-output-port)
	       (let ([result 
		      (let/ec escape-eval
			 ;; Clear output cache for each new test:
			 (set! suppressed-test-output (open-output-string))
			 (with-error-handlers (lambda args 						
						;; Should format the output, but don't want to cause *another* error
						;; and thereby go into an infinite loop.
						;; Could reparameterize the error-handler... TODO
						(printf "default-unit-tester, got ERROR: ~n  ~s~n"
							args)
						;(if (car args) (printf "~s: " (car args)))
						;(printf "~a~n" (apply format (cdr args)))
						)
					      (lambda () (escape-eval 'error))
					      (lambda () 
						(if quiet						    
						      (with-warning-handler 
						       (lambda (who str . args) 
							 (fprintf suppressed-test-output "Warning in ~a: ~a\n" 
								  who (apply format str args)))
						       (lambda ()
							 (parameterize ([current-output-port suppressed-test-output])
						    ;;========================================
						    ;; RUN THE TEST:
							   (eval (preprocessor expr)))))
						      (eval (preprocessor expr)))
						    ;;========================================
						)))])
;	       (newline)
		(if (or (and (procedure? intended) ;; This means its an oracle
			     ;; If we get an error result, don't run the oracle proc!
			     ;; Oracle proc might not be ready to handle 'error result:
			     (and (not (eq? result 'error)))
			    (intended result))
			(teq? intended result)) ;; Otherwise its an expected answer
		   ;; This test was a success:
		   (begin
		     (printf "PASS~n"))
		   ;; This test was a failure:
		   (if (and retry-failures ;; But if we're in retry mode try again...
			    (< try (default-unit-tester-retries))
			    (not (eq? result 'error))) ;; We don't retry an error!
		       (begin (printf "fail:  But retrying... Retry #~a\n" try)
			      (retryloop (add1 try)))
		       ;; Otherwise just print a notification of the failure and bind it to a global var:
		       (begin 
			  (set! success #f)
			  (newline)
			  (if (procedure? intended)
			      (printf "FAIL: Expected result to satisfy procedure: ~s~n" intended)
			      (begin 
				(printf "FAIL: Expected: ~n")			  
				(pretty-print intended)))
			  (printf "~n      Received: ~n")
			  (write result)
;			  (display-constrained `(,intended 40) " got instead " `(,result 40))  
			  (printf "~n~nFor Test: ~n")
			  (pretty-print expr)
			  (newline)
			  ;(eval `(define failed-unit-test ',expr))
			  (define-top-level-value 'unit-test-received result)
			  (define-top-level-value 'unit-test-expected intended)
			  (define-top-level-value 'failed-unit-test expr)
			  (define-top-level-value 'default-unit-tester-output (get-output-string suppressed-test-output))

			  (printf "Violating test bound to global-variable, try (eval failed-unit-test)\n")
			  (printf "Expected and received also bound to globals, consider: ")
			  (printf "(diff unit-test-expected unit-test-received)\n")
			  (printf "If test output was suppressed, you may wish to inspect it: ")
			  (printf "(display default-unit-tester-output)\n")

			  ;; Regiment specific.  If we're in batch mode print the error output.
                          (when (and (top-level-bound? 'REGIMENT-BATCH-MODE)
                                     (top-level-value 'REGIMENT-BATCH-MODE))
                            (printf "\nBecause we're in batch mode, printing unit test output here:\n")
                            (printf "======================================================================\n")
                            (eval '(display default-unit-tester-output))
                            )
                          
                          ;; Use the continuation to escape:
			  (return #f)
			  ))))))]))]) ;; end execute-one-test

	   ;; Main body of tester:
	  (if titles 
	      (begin (printf ";; Testing module: ~s~n" message)
		     (if quiet (printf ";; (with test output suppressed)~n"))
		     ))
	  (flush-output-port)
	  (let* ((len (length entries))
		 ;; If we have out of range indices, we ignore them:
		 (tests-to-run (filter (lambda (i) (< i len)) tests-to-run))
		 (entries 
		  (if (null? tests-to-run) entries
		      (map (lambda (i) (list-ref entries i)) tests-to-run)))
		 (indices (if (null? tests-to-run)
			     (iota len)
			     tests-to-run)))
	    (for-each execute-one-test
		      indices
		      entries))
	  ;; If we made it this far, we've passed all the tests, return #t:
	  #t
	  ))))))
    )) ;; End testerproc let binding

    ;; Regiment specific:
    (if enabled
	;; Add this unit-tester to the global list:     
	(reg:all-unit-tests (cons (list message testerproc) (reg:all-unit-tests))))	  
	  
    ;; Finally, return the test-executor procedure:  
    testerproc))))

(define (reg:counttests) ;;shorthand
  (apply + (map (lambda (x) (length ((cadr x) 'get))) (reg:all-unit-tests))))

;) ;; End Module

