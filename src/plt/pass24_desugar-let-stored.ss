#cs ;; Case Sensitivity
(module pass24_desugar-let-stored mzscheme

;  (require-for-syntax "constants.ss")
;  (require constants)

  (require (lib "include.ss"))
  (require "constants.ss")
  (require "iu-match.ss")
  (require (all-except "helpers.ss" test-this these-tests))

  ;; Some of the unit tests run the simulator:
  (require (all-except "simulator_alpha.ss" test-this these-tests))

  (require (all-except (lib "list.ss") filter))
  
  (require (lib "trace.ss"))

  (include (build-path  "generic" "pass24_desugar-let-stored.ss"))
  
  (provide (all-defined))
  )

;(require pass24_desugar-let-stored)
