

(module pass30_haskellize-tokmac mzscheme

;  (require-for-syntax "plt_constants.ss")
;  (require constants)

  (require (lib "include.ss"))
  (require "plt_constants.ss")
  (require "iu-match.ss"
	   "prim_defs.ss")
  (require (all-except "helpers.ss" test-this these-tests))
  (require (all-except "regiment_helpers.ss" test-this these-tests))

  (require (lib "trace.ss"))

  (include (build-path  "generic" "pass30_haskellize-tokmac.ss"))
  
;  (provide deglobalize
;;	   test-this these-tests test12 tests12)
  (provide (all-defined))
  )

;(require pass30_haskellize-tokmac)
