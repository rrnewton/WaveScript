;; repl.ss

;; Darn, this would be a great place for OOP, in particular, inheritance.

;; [2004.06.11] This module represents a simple read-eval-print
;; interface into the compiler and simulator.


;; "run" had better take a stream.

(define-regiment-parameter repl-stream-depth
   20 (lambda (n)
	(if (integer? n) n
	    (error 'repl-stream-depth " this parameter can only be set to numbers"))))

(define (repl-builder startup cleanse compiler run)
  (define TIMEOUT 10.0)
  (disp "REPL BUILDER")

  (lambda()
    (printf "Type exit to leave repl.~n")
    (startup)
    (let loop ()
      (printf "~n>> ")
      (let ((prog (read)))
	(cleanse)
	(if (not (eq? prog 'exit))
	    (let ((tokmac (compiler prog)))
	      (printf "~nCompiled:~n")
	      (printf   "--------~n")
	      (pretty-print tokmac)
	      
	      (printf "~nBuilding Simulator Program...~n")
	      (let ((converted (if (token-machine? tokmac)
				   (build-simulation (compile-simulate-nought (token-machine->program tokmac)))
				   tokmac)))
		(printf "~nSimulating....~n")	   
		(cleanse-world)		
		(let ([result (run converted TIMEOUT)])
		  (if (not (live-stream? result))
		      ;; NON-STREAM RESULT:
		      result
		      		     
		      (begin 
			(printf "Simulation result is a stream, reading...~n")
		      (let streamloop ([i 0] [stream result] [acc '()])
			(cond			 
			 [(> i (repl-stream-depth)) (printf "~n That's enough.~n")]
			 [(eq? stream 'threads_timed_out)
			  (printf "~n Threads timed out.~n")]
			 [(stream-empty? stream) 
			  (printf "Stream Ended.~n")
			  (printf "All returned values were: ~n" )
			  (pretty-print (reverse acc))]
			 [else 
;			  (disp "ADVANCING STREAM" stream)
			  
			  (let ([head (stream-car stream)])
;			    (disp "GOT HEAD" head)
			    (display-constrained (list i 20) ": " 
						 (list head 60))
			    (newline)
					;(printf "~s:  ~s~n" i (stream-car stream))
			    (streamloop (add1 i) (stream-cdr stream) (cons head acc)))])))		      
		      )))
	      (loop)))))))

