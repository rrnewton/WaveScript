;; repl.ss

;; Darn, this would be a great place for OOP, in particular, inheritance.

;; [2004.06.11] This module represents a simple read-eval-print
;; interface into the compiler and simulator.


;; "run" had better take a stream.

(define (repl-builder startup cleanse compiler run)
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
		(let ([result (run converted 2.0)])
		  (if (not (stream? result))		     
		      result
		      (let streamloop ([i 0] [stream result])
			(cond			 
			 [(> i 10) (printf "~n That's enough.~n")]
			 [(eq? stream 'threads_timed_out)
			  (printf "~n Threads timed out.~n")]
			 [(stream-empty? stream) (newline)] ;(printf "Stream Ended.~n")]
			 [else 
			  (display-constrained (list i 20) ": " (list (stream-car stream) 60))
			  (newline)
					;(printf "~s:  ~s~n" i (stream-car stream))
			  (streamloop (add1 i) (stream-cdr stream))])
			))))
	      (loop)))))))


