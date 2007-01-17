;; This helps PLT Scheme compile the whole directory as a collection.

(module info (lib "infotab.ss" "setup")
  (define name "Complete Regiment System")

  ;; These are all the files that SHOULD NOT be compiled as part of the package.
  (define compile-omit-files '(
			       "main.ss"
			       "main_chez.ss"
			       ))
  (define compile-subcollections  '(				    
				    ("plt")
				    ("generic")

				    ))
  )
