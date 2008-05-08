(module info (lib "infotab.ss" "setup")
  (define name "Various Utilities Used in Regiment")

  (define compile-omit-files '(
			       ;; Haven't factored helpers yet:
			       "helpers_general.ss"
			       "helpers_tml.ss"

			       ;; These aren't complete, stand-alone modules:
			       "streams.ss"
			       "unit_tester.ss"
			       ))
  )
