
;; run time functions for simulator code:
(module alpha_lib
	(
	 current-simobject

	 retrieve-token
	 add-token
	 evict-token	 

	 neighbors
	 sendmsg
	 sim-light-up
	 sim-print-queue
	 sim-leds
	 ;sim-dist
	 sim-loc
	 sim-locdiff
	 simulator-soc-return
	 simulator-soc-finished
	 check-store
	 ;alpha-it ;; shorthand

	 ;; Simple functions that compute sensor values:
	 sense-dist-from-origin
	 sense-sine-wave
	 sense-noisy-rising
	 sense-random-1to100

	 test-this these-tests test-alphalib
	 )

	(import scheme)
	(import simulator_alpha_datatypes)

	;; Consider compiling simulator code in opt 3 when it's stable.
;	(eval-when (compile load eval)
;	  (optimize-level (IFDEBUG 2 2)))

	(include "generic/alpha_lib.ss"))
