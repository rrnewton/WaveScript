#cs ;; Case Sensitivity

(module pass25_rename-stored mzscheme
  (require (lib "include.ss"))
  (require "constants.ss")
  (require "iu-match.ss")
  (require "helpers.ss")

  (require (all-except (lib "list.ss") filter)) 

  (include (build-path "generic" "pass25_rename-stored.ss"))
  
  (provide (all-defined))
  )

;(require pass25_rename-stored)
