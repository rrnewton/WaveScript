
(module pass00  mzscheme  
  (require (lib "include.ss")           
           "iu-match.ss"
           "helpers.ss")
  (include (build-path ".." "generic" "pass00.ss"))
  
  ;; Insure provision of verify-regiment:
  (provide verify-regiment 
	   these-tests test-this test00 tests00)  
  )
