
(module pass01_rename-var mzscheme
	(require (lib "include.ss")
                  "iu-match.ss"
                  "helpers.ss")

	(include (build-path ".." "generic" "pass01_rename-var.ss"))

	(provide rename-var 
		 test-this these-tests test01 tests01)
)
