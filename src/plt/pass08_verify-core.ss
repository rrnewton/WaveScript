
(module pass08_verify-core mzscheme

  (require (lib "include.ss"))
  
  (require "iu-match.ss")
  (require "helpers.ss")

  (include (build-path ".." "generic" "pass08_verify-core.ss"))
  
;  (provide verify-core)
  (provide verify-core 
	   test-this these-tests test08 tests0)
)
