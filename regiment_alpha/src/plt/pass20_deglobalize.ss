

(module pass20_deglobalize mzscheme

  (require (lib "include.ss"))
  (require "constants.ss")
  (require "iu-match.ss")
  (require (all-except "helpers.ss" test-this these-tests))

  (require (lib "trace.ss"))

  (include (build-path  "generic" "pass20_deglobalize.ss"))
  
;  (provide deglobalize
;;	   test-this these-tests test12 tests12)
  (provide deglobalize test20 tests20 test-deglobalize tests-deglobalize 
           ;; Temporarily exposing.
           delazy-bindings)
  )

;(require pass20_deglobalize)
