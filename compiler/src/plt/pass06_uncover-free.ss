
(module pass06_uncover-free mzscheme
	(require (lib "include.ss")
                  "iu-match.ss"
                  "helpers.ss")

	(include (build-path ".." "generic" "pass06_uncover-free.ss"))

	(provide (all-defined))
;	(provide rename-var 
;		 test-this these-tests test01 tests01)
)