
(module pass10_verify-core mzscheme

  (require (lib "include.ss"))
  
  (require "iu-match.ss")
  (require "helpers.ss")

  (include (build-path ".." "generic" "pass10_verify-core.ss"))
  
;  (provide verify-core)
  (provide verify-core 
	   test-this these-tests test10 tests10)
)
