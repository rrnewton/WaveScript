#cs ;; Case Sensitivity

(module pass23_desugar-gradients mzscheme
  (require (lib "include.ss"))
  (require (lib "trace.ss"))
  (require "constants.ss")
  (require "iu-match.ss")
  (require (all-except "grammar_checker.ss" test-this these-tests))
  (require (all-except "tml_generic_traverse.ss" test-this these-tests))
  (require (all-except "helpers.ss" test-this these-tests))
  
  (include (build-path  "generic" "pass23_desugar-gradients.ss"))
  
  (provide (all-defined))
  )

;(require pass23_desugar-gradients)
