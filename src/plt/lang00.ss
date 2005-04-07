
(module lang00 mzscheme  
  (require (lib "include.ss")           
	   (lib "pretty.ss")
           "iu-match.ss"
           "helpers.ss"
	   "language-mechanism.ss"
	   )

  (include (build-path "generic" "lang00.ss"))
  
  (provide (all-defined))
  )
