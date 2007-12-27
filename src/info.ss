;; This helps PLT Scheme compile the whole directory as a collection.

(module info (lib "infotab.ss" "setup")
  (define name "Complete Regiment System")

  ;; These are all the files that SHOULD NOT be compiled as part of the package.
  (define compile-omit-files '(
			       "config.ss"
			       "main.ss"
			       "main_chez.ss"
			       "main_larceny.ss"
;			       "main_plt.ss"
			       "common_loader.ss"
			       "match_larceny.ss"
			       "regiment.ss"
			       "regiment_script.ss"
			       "regiment_pltscript.ss"
			       "regiment_script.threaded.ss"
			       "reg_grammar.ss"
			       "scrap.ss"
			       "supertest.ss"
			       "tests.ss"
			       ))
  (define compile-subcollections  '(				    
				    ("plt")
				    ("generic")

				    ))
  )
