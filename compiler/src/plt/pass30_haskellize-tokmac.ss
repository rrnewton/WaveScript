

(module pass30_haskellize-tokmac mzscheme

;  (require-for-syntax "constants.ss")
;  (require constants)

  (require (lib "include.ss"))
  (require "constants.ss")
  (require "iu-match.ss")
  (require "helpers.ss")

  (require (lib "trace.ss"))

  (include (build-path ".." "generic" "pass30_haskellize-tokmac.ss"))
  
;  (provide deglobalize
;;	   test-this these-tests test12 tests12)
  (provide (all-defined))
  )

;(require pass30_haskellize-tokmac)