#cs ;; Case Sensitivity
(module pass24_desugar-let-stored mzscheme
  (require (lib "trace.ss"))
  (require (lib "include.ss"))
  (require (all-except (lib "list.ss") filter))  
  (require "iu-match.ss")

  (require "constants.ss"
	   "prim_defs.ss")
  (require (all-except "grammar_checker.ss" test-this these-tests id))
  (require (all-except "tml_generic_traverse.ss" test-this these-tests id))
  (require (all-except "helpers.ss" test-this these-tests id))
  (require (all-except "regiment_helpers.ss" test-this these-tests id))
  ;; Some of the unit tests run the simulator:
  (require (all-except "simulator_alpha.ss" test-this these-tests))

  (include (build-path  "generic" "pass24_desugar-let-stored.ss"))
  
  (provide (all-defined))
  )

;(require pass24_desugar-let-stored)
