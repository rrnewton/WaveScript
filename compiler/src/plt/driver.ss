
;; This should depend on 'pass-names' being defined, but I don't know
;; how to do this correctly.

(module drivers mzscheme  
  (require (lib "include.ss")           
           "iu-match.ss"
           "helpers.ss"
;	   pass-names)
	   )

  (include (build-path ".." "generic" "driver.ss"))
  
  (provide test-one test-all
;; These are in the interface, but aren't used yet for this project:
;	   tracer game-eval analyze-all test-all-but 
;	   print-file remaining-pass-names
	   )
  )

