
(module pass02_rename-vars mzscheme
	(require (lib "include.ss")
                  "iu-match.ss"
                  "helpers.ss")

	(include (build-path ".." "generic" "pass02_rename-vars.ss"))

	(provide rename-var 
		 test-this these-tests test01 tests01)
)
