
(module pass01 mzscheme
	(require (lib "include.ss")
                  "iu-match.ss"
                  "helpers.ss")

	(include (build-path ".." "generic" "pass01.ss"))

	(provide rename-var 
		 test-this these-tests test01 tests01)
)
