
;; run time functions for simulator code:
(module alpha_lib
	(
	 current-simobject
	 led-toggle-state
	 neighbors
	 sendmsg
	 sim-light-up
	 sim-leds
	 ;sim-dist
	 sim-loc
	 sim-locdiff
	 simulator-soc-return
	 simulator-soc-finished
	 check-store
	 ;alpha-it ;; shorthand
	 )
	
	(include "generic/alpha_lib.ss"))
