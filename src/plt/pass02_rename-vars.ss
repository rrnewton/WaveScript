
(module pass02_rename-vars mzscheme
	(require (lib "include.ss")
                  "iu-match.ss"
		  (all-except "helpers.ss" test-this these-tests)
                  (all-except "regiment_helpers.ss" test-this these-tests))

	(include (build-path "generic" "pass02_rename-vars.ss"))

	(provide rename-var 
		 test-this these-tests test01 tests01)
)
