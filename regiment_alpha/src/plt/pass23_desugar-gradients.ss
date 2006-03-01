#cs ;; Case Sensitivity

(module pass23_desugar-gradients mzscheme
  (require (lib "include.ss")
           (lib "trace.ss")
           "constants.ss"
	   "prim_defs.ss"
           "iu-match.ss"
           (all-except "grammar_checker.ss" test-this these-tests)
           (all-except "tml_generic_traverse.ss" test-this these-tests)
           (all-except "helpers.ss" test-this these-tests)
           (all-except "regiment_helpers.ss" test-this these-tests)
           ;"logfiles.ss"
           "source_loader.ss"
           )
  
  (include (build-path  "generic" "pass23_desugar-gradients.ss"))
  (include (build-path  "generic" "pass23_desugar-gradients_ETX.ss"))
  (include (build-path  "generic" "pass23_desugar-gradients_verbose.ss"))
  (include (build-path  "generic" "pass23_desugar-gradients_simple.ss"))
  
  (provide (all-defined))
  )

;(require pass23_desugar-gradients)
