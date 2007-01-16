;; This helps PLT Scheme compile the whole directory as a collection.

(module info (lib "infotab.ss" "setup")
  (define name "PLT Regiment - Generic Code")

  ;; These are all the files that SHOULD NOT be compiled as part of the package.
  (define compile-omit-files '("shortcuts.ss"
			       ))
  (define compile-subcollections  '(("util")
				    ("grammars")
				    ("sim")
				    ("passes")
				    ("passes" "normalize_source")
				    ("passes" "normalize_query")
				    ("passes" "static_elaborate")
				    ("passes" "deglobalize")
				    ("passes" "tokmac_bkend")
				    ("passes" "nesc_bkend")
				    ("passes" "wavescope_bkend")				    
				    ;("testing")
				    ))
  )
