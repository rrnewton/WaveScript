
(module pass04_reduce-primitives mzscheme
  (require (lib "include.ss")
           "iu-match.ss"
           "helpers.ss")
  
  (include (build-path ".." "generic" "pass04_reduce-primitives.ss"))
  
  (provide (all-defined))
  ;	(provide rename-var 
  ;		 test-this these-tests test01 tests01)
  )
