
(module tml_generic_traverse  mzscheme  
  (require (lib "include.ss")           
           "iu-match.ss")
  (include (build-path "generic" "tml_generic_traverse.ss"))
  
  ;; Insure provision of verify-regiment:
  (provide tml-generic-traverse)
  )
