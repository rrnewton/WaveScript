(module pass10_deglobalize mzscheme

  (require (lib "include.ss"))
  
  (require "iu-match.ss")
  (require "helpers.ss")

  (require (lib "trace.ss"))

  (include (build-path ".." "generic" "pass10_deglobalize.ss"))
  
;  (provide deglobalize
;;	   test-this these-tests test10 tests10)
  (provide (all-defined))
  )

;(require pass10_deglobalize)