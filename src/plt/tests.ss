
;(module tests mzscheme  
  (require (lib "include.ss")           
;           "iu-match.ss"
;           "helpers.ss")
	   )

  (include (build-path  "generic" "tests_noclosure.ss"))
  (include (build-path  "generic" "tests.ss"))
  
  ;; Insure provision of verify-regiment:
;  (provide tests tests_noclosure)
;  )
