
(module pass03_remove-unquoted-constant mzscheme
  (require (lib "include.ss")
           "iu-match.ss"
           "helpers.ss")
  
  (include (build-path ".." "generic" "pass03_remove-unquoted-constant.ss"))
  
  (provide (all-defined))
  ;	(provide rename-var 
  ;		 test-this these-tests test01 tests01)
  )
