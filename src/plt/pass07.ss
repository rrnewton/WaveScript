
(module pass07 mzscheme

  (require (lib "include.ss"))
  
  (require "iu-match.ss")
  (require "helpers.ss")

  (include (build-path ".." "generic" "pass07.ss"))
  
  (provide verify-core)
)
