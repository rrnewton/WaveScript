

(module pass20_deglobalize mzscheme

  (require (lib "include.ss")
	   (lib "trace.ss")
	   "constants.ss"
           "iu-match.ss"
           "hashtab.ss"
	   "prim_defs.ss"
           (all-except "tsort.ss" test-this these-tests)
           (all-except "tml_generic_traverse.ss" test-this these-tests)
           (all-except "helpers.ss" test-this these-tests)
           (all-except "regiment_helpers.ss" test-this these-tests))

  (include (build-path  "generic" "pass20_deglobalize.ss"))
  
;  (provide deglobalize
;;	   test-this these-tests test12 tests12)
  (provide deglobalize test20 tests20 test-deglobalize tests-deglobalize 
           ;; Temporarily exposing.
           delazy-bindings)
  )

;(require pass20_deglobalize)