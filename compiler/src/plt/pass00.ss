
(module pass00  mzscheme

  (require (lib "include.ss"))
  
  (require "iu-match.ss")
  (require "helpers.ss")

  (include (build-path ".." "generic" "pass00.ss"))
  
  (provide verify-regiment)
)
