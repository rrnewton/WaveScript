(module pass16_deglobalize mzscheme

  (require (lib "include.ss"))
  (require "constants.ss")
  (require "iu-match.ss")
  (require "helpers.ss")

  (require (lib "trace.ss"))

  (include (build-path ".." "generic" "pass16_deglobalize.ss"))
  
;  (provide deglobalize
;;	   test-this these-tests test12 tests12)
  (provide (all-defined))
  )

;(require pass16_deglobalize)