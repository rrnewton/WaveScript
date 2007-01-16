(module info (lib "infotab.ss" "setup")
  (define name "Various Utilities Used in Regiment")

  (define compile-omit-files '(
			       ;; Haven't factored helpers yet:
			       "helpers_general.ss"
			       "helpers_tml.ss"

			       ;; Used only by Chez: 
			       "slib_hash.ss"
			       "slib_hashtab.ss"
			       
			       ;; 
			       "streams.ss"
			       "unit_tester.ss"
			       ))
  )
