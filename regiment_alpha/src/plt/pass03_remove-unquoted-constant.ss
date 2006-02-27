
(module pass03_remove-unquoted-constant mzscheme
  (require (lib "include.ss")
           "iu-match.ss"
	   (all-except "helpers.ss" test-this these-tests)
           (all-except "regiment_helpers.ss" test-this these-tests))
  
  (include (build-path "generic" "pass03_remove-unquoted-constant.ss"))
  
  (provide (all-defined))
  ;	(provide rename-var 
  ;		 test-this these-tests test01 tests01)
  )
