
(module language-mechanism mzscheme  
  (require (lib "include.ss")           
	   (lib "pretty.ss")
           "iu-match.ss"
           "helpers.ss"
	   )
  
  (include (build-path ".." "generic" "language-mechanism.ss"))

  ;; This is lame, but I'm doing it for PLTs module system.
  ;(define base-language (eval 'base-language))
  
  (provide (all-defined))
  )

