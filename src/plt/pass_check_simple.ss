
(module pass_check_simple mzscheme

  (require (lib "include.ss")
	   "iu-match.ss"
	   "helpers.ss"
	   "regiment_helpers.ss")

  (include (build-path  "generic" "pass_check_simple.ss"))
  
  (provide verify-core)
)
