;; repl.ss

;; [2004.06.11] This module represents a simple read-eval-print
;; interface into the compiler and simulator.



(define (repl)
  (printf "Type exit to leave repl.~n")
  (let loop ()
    (printf "~n>> ")
    (let ((prog (read)))
      (if (not (eq? prog 'exit))
	  (let ((tokmac (run-compiler prog)))
	    (printf "~nCompiled:~n")
	    (printf   "--------~n")
	    (pretty-print tokmac)
	    
	    (printf "~nBuilding Simulator Program...~n")
	    (let ((converted (compile-simulate-nought (cadadr tokmac))))

	      (printf "~nSimulating....~n")	   
	      (cleanse-world)
	      (let ((result (run-simulation (build-simulation converted) 2.0)))
		(if (and (list? result) (= 1 (length result)))
		    (printf "~n~s~n" result)
		    (printf "~n~s~n" result)))
	      (loop)))))))


	

