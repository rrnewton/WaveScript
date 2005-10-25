
(define-syntax foo
  (syntax-rules ()
    [(_) bar]))

(module simulator_alpha
	(run-simulator-alpha
	 compile-simulate-alpha csa ; shorthand
	 test-this these-tests

	 print-connectivity fresh-simulation
	 simalpha-draw-world	 

	 testalpha testsalpha

	 posdist
	 )

(import scheme)
(import (except topsort-module test-this these-tests))
(import simulator_alpha_datatypes)
(import alpha_lib_scheduler_simple)

;; We are loaded from the root directory, not the chez subdirectory.
;(include "generic/simulator_nought.examples.ss")
(include "generic/simulator_alpha.ss")

)

