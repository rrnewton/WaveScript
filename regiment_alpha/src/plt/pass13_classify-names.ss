(module pass13_classify-names mzscheme

  (require (lib "include.ss")
	   (lib "pretty.ss")
	   (lib "trace.ss")
	   "iu-match.ss"
	   "constants.ss"
	   (all-except "helpers.ss" test-this these-tests)
	   (all-except "regiment_helpers.ss" test-this these-tests))

  (include (build-path  "generic" "pass13_classify-names.ss"))
  
  (provide (all-defined))
  )

