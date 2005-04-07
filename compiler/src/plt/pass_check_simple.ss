
(module pass_check_simple mzscheme

  (require (lib "include.ss"))
  
  (require "iu-match.ss")
  (require "helpers.ss")

  (include (build-path  "generic" "pass_check_simple.ss"))
  
  (provide verify-core)
)
