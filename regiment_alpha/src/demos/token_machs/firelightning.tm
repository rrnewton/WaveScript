
;; This is some token machine code written to test out my simple lightning/forest-fire simulation.

;; This turns on the fire-simulation globally.  Can't be undone.
(parameters 
  [dummy-param (install-firelightning)]
  [sim-num-nodes 200]
  [sim-timeout 1000000])


(token node-start ()
  (call loop 90))

(token loop (n)
  (if (> n 0)
      (begin 
	(printf "Yay ~a <<<~a>>>\n" (my-clock) (sync-sense 'temp))
	(if (> (sync-sense 'temp) 0)
	    (leds on red)
	    (leds off red))
	(timed-call 1000 loop (- n 1)))))



