

(chez:module simulator_alpha
	(run-simulator-alpha rerun-simulator-alpha clean-simworld!
	 compile-simulate-alpha csa ; shorthand
	 test-this these-tests

	 simalpha-total-messages
	 print-stats print-node-stats 
	 print-connectivity plot-connectivity
	 fresh-simulation
	 simalpha-draw-world	 

	 testalpha 

	 )

(import scheme)
(import (except topsort-module test-this these-tests))
(import regiment_helpers)
;temp; (import simulator_alpha_datatypes)
(import alpha_lib_scheduler_simple)

;; Consider compiling simulator code in opt 3 when it's stable.
;(eval-when (compile load eval)
;  (optimize-level (IFDEBUG 2 2)))

;; [2005.11.05] This fills in the implementation-specific casing for the generated code:
;; Could just be identity function, but wrapping in a module should give better performance.
(define (build-genned-code-module node-code)
  `(begin	      
     (chez:module genned-code (node-code) 
       (import scheme)
       (import simulator_alpha_datatypes)
       (import alpha_lib)
       (import alpha_lib_scheduler_simple)
       ,node-code)
     (import genned-code)))

;; We are loaded from the root directory, not the chez subdirectory.
;(include "generic/simulator_nought.examples.ss")
(include "generic/simalpha_ui.ss")
(include "generic/simalpha_rollworld.ss")
(include "generic/simulator_alpha.ss")

)

