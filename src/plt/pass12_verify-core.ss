
(module pass12_verify-core mzscheme

  (require "iu-match.ss"
	   (lib "include.ss")
	   (all-except "helpers.ss" test-this these-tests)
	   (all-except "regiment_helpers.ss" test-this these-tests))

  (include (build-path "generic" "pass12_verify-core.ss"))
  
;  (provide verify-core)
  (provide verify-core 
	   test-this these-tests test12 tests12)
)
