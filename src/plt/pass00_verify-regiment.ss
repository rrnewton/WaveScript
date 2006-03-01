
(module pass00_verify-regiment  mzscheme  
  (require (lib "include.ss")           
           "iu-match.ss"
           "prim_defs.ss"
           (all-except "grammar_checker.ss" test-this these-tests)
           (all-except "helpers.ss" test-this these-tests)
           (all-except "regiment_helpers.ss" test-this these-tests)
           (all-except "hm_type_inference.ss" test-this these-tests) 
	   "constants.ss")
  (include (build-path "generic" "pass00_verify-regiment.ss"))
  
  ;; Insure provision of verify-regiment:
  (provide verify-regiment 
	   these-tests test-this test00 tests00)  
  )
