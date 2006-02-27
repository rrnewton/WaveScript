
(module tml_generic_traverse  mzscheme  
  (require (lib "include.ss")           
           "iu-match.ss"
	   ;(lib "compat.ss")
           (all-except "helpers.ss" test-this these-tests filter)
           (all-except "regiment_helpers.ss" test-this these-tests filter)
           )
  
  (include (build-path "generic" "tml_generic_traverse.ss"))
  
  ;; Insure provision of verify-regiment:
  (provide tml-generic-traverse
           tml-simple-pass
           tml-free-vars
           test-this test-generic-traverse)
  )
