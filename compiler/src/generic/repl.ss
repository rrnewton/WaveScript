;; repl.ss

;; [2004.06.11] This module represents a simple read-eval-print
;; interface into the compiler and simulator.

(define (repl-builder startup cleanse run)
  (lambda()
    (printf "Type exit to leave repl.~n")
    (startup)
    (let loop ()
      (printf "~n>> ")
      (let ((prog (read)))
	(cleanse)
	(if (not (eq? prog 'exit))
	    (let ((tokmac (run-compiler prog)))
	      (printf "~nCompiled:~n")
	      (printf   "--------~n")
	      (pretty-print tokmac)
	      
	      (printf "~nBuilding Simulator Program...~n")
	      (let ((converted (compile-simulate-nought (cadadr tokmac))))
		
		(printf "~nSimulating....~n")	   
		(cleanse-world)
		(let streamloop ([i 0] 
				 [stream (run (build-simulation converted) 2.0)])
		  (disp "Round the streamloop: " stream)
		  (cond
		   [(null? stream) (prinf "Stream Ended.~n")]
		   [(procedure? stream) (streamloop i (stream))]
		   [(pair? stream) (printf "~s:  ~s~n" i (car stream))
		    (streamloop (add1 i) (cdr stream))]
		   [else (error 'repl-builder "Bad stream!: ~s" stream)])
		  )))))
      (loop))))

(define text-repl  (repl-builder void void run-simulation))
