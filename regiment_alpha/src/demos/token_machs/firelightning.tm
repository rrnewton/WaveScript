
;; This is some token machine code written to test out my simple lightning/forest-fire simulation.

;; This turns on the fire-simulation globally.  Can't be undone.
(parameters 
  [dummy-param (install-firelightning)]
  [sim-timeout 30000])


(token node-start ()
  (call loop 25))

(token loop (n)
  (if (> n 0)
      (begin 
	(printf "Yay ~a <<<~a>>>\n" (my-clock) (sync-sense 'temp))	
	(timed-call 1000 loop (- n 1)))))



