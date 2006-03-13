#cs ;; Case Sensitivity

(module pass25_rename-stored mzscheme
  (require (lib "include.ss"))
  (require "plt_constants.ss"
	   "prim_defs.ss")
  (require "iu-match.ss")
;  (require (all-except "grammar_checker.ss" test-this these-tests))
  (require (all-except "tml_generic_traverse.ss" test-this these-tests))
  (require (all-except "helpers.ss" test-this these-tests))
  (require (all-except "regiment_helpers.ss" test-this these-tests))

  (require (all-except (lib "list.ss") filter)) 

  (include (build-path "generic" "pass25_rename-stored.ss"))
  
  (provide (all-defined))
  )

;(require pass25_rename-stored)
