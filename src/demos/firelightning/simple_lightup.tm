
;; This is some token machine code written to test out my simple lightning/forest-fire simulation.

;; This turns on the fire-simulation globally.  Can't be undone.
(parameters 
  [dummy-param (install-firelightning)]
  ;[simalpha-placement-type 'connected]

  [simalpha-realtime-mode #t]
  ;[sim-num-nodes 250]
  [sim-num-nodes 30]
  [sim-timeout 2000000])


(token node-start ()
  (call loop 1000))

(token loop (n)
  (if (> n 0)
      (begin 
	;(printf "Yay ~a <<<~a>>>\n" (my-clock) (sync-sense 'temp))
	(let ((temp (sync-sense "temp")))
	  (if (> temp 10)
	      (leds on red)
	      (leds off red))
	  (if (> temp 190)
	      (leds on green)
	      (leds off green))
	  (setlabel "T: ~a" (round-to 2 temp)))
	(timed-call 1000 loop (- n 1)))))
