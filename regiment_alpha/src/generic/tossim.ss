;; TOSSIM - Simulator for NesC 

;; [2005.11.14] This file uses Tossim as a simulator.  
;; It aims to ape the behavior of simulator alpha.
;; In fact it responds to the same parameters as simulator alpha.

;; TODO: At some point I should rename these shared parameters "sim-" instead of "simalpha-".

;; Current working parameters:

;; sim-timeout


; =================================================================================

;; This is the equivalent of "tm-to-list" for nesc:
(define (run-via-nesc tm . opts)
  (define (mask s)
    (let ((pos (pregexp-match-positions "TMPRNT: " s)))
      (if (not pos) (error 'run-via-nesc "inconsistent!")
	  (let ((start (cdar pos)))
	    (substring s start (string-length s))))))
  (define params ())
  ;; Sift through extra options.  Params are for us, everything else
  ;; is for the assembler.
  (set! opts (filter (lambda (x)
		       (match x 
			 [(params (,p ,v) ...) (set! params `((,p ,v) ...)) #f]
			 [,else #t])) opts))
  ;; Pump out some stack frames for each param. ;)
  (let loop ((params params))
    (if (not (null? params))
	(parameterize ([(caar params) (cadar params)])
	  (loop (cdr params)))
	(fluid-let ((pass-names
		     (append (remq 'flatten-tokmac (remq 'emit-nesc pass-names))
			     '(flatten-tokmac emit-nesc))))
	  (match (apply assemble-tokmac tm opts)
	    [(emit-nesc-language ,p)
	     (if (eqv? 0 (emit-nesc-language p))
		 (parameterize ((current-directory (string-append (getenv "REGIMENTD") "/src/haskell/")))
		   (define time (let ((t (sim-timeout)))
				  (if (not t) ""
				      (format "-t=~a"
					      (if (inexact? t) (inexact->exact (floor t))
						  (if (integer? t) (quotient t 1000)
						      (error 'run-via-nesc "bad timeout: ~s" t)))))))
		   ;; Convert milleseconds to seconds:
		   (define stagger (quotient (sim-startup-stagger) 1000))
		   (define command (format "./build/pc/main.exe -b=~a ~a -r=simple ~a | grep TMPRNT"
					   stagger time (sim-num-nodes)))
		   (fprintf (current-error-port) "\n\n  <RUNNING_NESC_CODE_IN_TOSSIM>\n")
		   (fprintf (current-error-port) "Command: ~s\n" command)
		   (fprintf (current-error-port) 
			    ";=======================================================================\n")
		   (flush-output-port)
		   (let ((result
			  (let-match ((  (,in ,out ,id)   (process command)))
			    (let loop ((line (read-line in)) (acc ()))
			      (if (or (not line) (eof-object? line))
				  (reverse! acc)
				  (let ((outline (mask line)))
				    (display outline) (newline)
				    (flush-output-port)4
				    (loop (read-line in) (cons outline acc))))))))  
					;(system "./build/pc/main.exe -b=1 -t=3 -r=simple 10 | grep TMPRNT")))
		     (fprintf (current-error-port)
			      ";=======================================================================\n")
		     (flush-output-port)
		     result))
		 (error 'run-via-nesc "error on NesC build."))]
	    [,other (error 'run-via-nesc "did not assemble to emit-enesc-language program: \n~s" other)]
	    )))))

; =================================================================================

(define emit/compile-nesc
  (lambda (prog)
    (match prog
      [#(,mstr ,cstr ,hstr)
       (parameterize ((current-directory (string-append (getenv "REGIMENTD") "/src/haskell")))
       (printf "~nDumping token machine into directory: ~s~n" (current-directory))
       (let ([modF    (open-output-file (string-append emit-nesc-modname "M.nc") 'replace)]
	     [confF   (open-output-file (string-append emit-nesc-modname ".nc") 'replace)]
	     [headerF (open-output-file (string-append emit-nesc-modname ".h") 'replace)])
	 (display mstr modF)
	 (display cstr confF)
	 (display hstr headerF)	
	 (close-output-port modF)
	 (close-output-port confF)
	 (close-output-port headerF))
       (printf "~nBinding top-level function (run_tm) to compile&assemble this token machine.~n")
       (define-top-level-value 'run_tm
	 (lambda ()
	   (printf "Running token machine in directory: ~s~n~n from str: ~n~s~n~n" 
		   (current-directory) 
		   (substring mstr 0 (min 200 (string-length mstr))))

	   ;; Use the NesC compiler:
	   (system/echoed "exec make pc")))
       ;; For now also go ahead and run it
       (run_tm))]
      [,other (error 'emit-nesc-language "Bad program: ~s" other)])))


; =================================================================================

;; We don't include this in the test suite that is run automatically.
;; The user must run this manually.
(define (test-nesc . opts)
  (define defparams `(params [,sim-num-nodes 1] [,sim-startup-stagger 0] [,sim-timeout 1000]))
    (apply (default-unit-tester "Testing NesC emission and code simulation."
       `(
	 ["NesC: printing with printf"
	  (run-via-nesc '(tokens (SOC-start () (printf "123\n") (printf "abc\n")))
			',defparams)
	  ("123" "abc")]
	 ["NesC: a conditional"
	  (run-via-nesc '(tokens (SOC-start () (if #t (printf "yay\n"))))
			',defparams)
	  ("yay")]
	 
	 ["NesC: do a simple local call."
	  (run-via-nesc '(tokens (SOC-start () (call tok1))
				 (tok1 () (printf "cheers\n")))
			',defparams)
	  ("cheers")]

	 ["NesC: compute factorial of 6 in tail recursive fashion."
	  (run-via-nesc 
	   '(tokens (SOC-start () (call fib 6 1))
		    (fib (x acc) (if (= x 0) 
				     (printf "%d\n" acc) 
				     (call fib (- x 1) (* x acc)))))
	   ',defparams)
	  ("720")]

	 )) opts))



