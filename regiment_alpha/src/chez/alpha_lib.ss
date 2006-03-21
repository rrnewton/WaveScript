
;; run time functions for simulator code:
#;(chez:module alpha_lib
	(

	 get-connectivity get-node get-simobject ;; Utilities for coercion:
	 attempt-message-transmission

	 current-simobject

	 retrieve-token
	 add-token
	 evict-token	 
	 evict-all-tokens

	 neighbors
	 sendmsg

	 sim-print-queue
	 sim-light-up
	 sim-highlight-edge
	 sim-draw-mark
	 sim-leds
	 sim-setlabel
	 ;sim-dist
	 sim-loc
	 sim-locdiff
	 simulator-soc-return
	 simulator-soc-finished
	 check-store
	 ;alpha-it ;; shorthand

	 test-this these-tests test-alphalib
	 )

	(import scheme)
;temp;	(import simulator_alpha_datatypes)

	;; Consider compiling simulator code in opt 3 when it's stable.
;	(eval-when (compile load eval)
;	  (optimize-level (IFDEBUG 2 2)))

	(include "../generic/alpha_lib.ss"))


(include "../generic/alpha_lib.ss")
