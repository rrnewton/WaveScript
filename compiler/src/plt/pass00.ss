
(module pass00  mzscheme  
  (require (lib "include.ss")           
           "iu-match.ss"
           "helpers.ss")
  (include (build-path ".." "generic" "pass00.ss"))
  
  (provide (all-defined))
  ;; Insure provision of verify-regiment:
  ; (provide verify-regiment (all-defined-except verify-regiment))  
  )
