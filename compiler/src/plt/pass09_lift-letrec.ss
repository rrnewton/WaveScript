
(module pass07_lift-letrec mzscheme
	(require (lib "include.ss")
                  "iu-match.ss"
                  "helpers.ss")

	(include (build-path ".." "generic" "pass07_lift-letrec.ss"))

	(provide (all-defined))
;	(provide rename-var 
;		 test-this these-tests test01 tests01)
)
