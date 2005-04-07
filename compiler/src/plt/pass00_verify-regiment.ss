
(module pass00_verify-regiment  mzscheme  
  (require (lib "include.ss")           
           "iu-match.ss"
           "helpers.ss"
	   "constants.ss")
  (include (build-path "generic" "pass00_verify-regiment.ss"))
  
  ;; Insure provision of verify-regiment:
  (provide verify-regiment 
	   these-tests test-this test00 tests00)  
  )
