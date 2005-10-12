
(module pass09_lift-letrec mzscheme
	(require (lib "include.ss")
		 "iu-match.ss"
		 (all-except "helpers.ss" test-this these-tests))

	(include (build-path "generic" "pass09_lift-letrec.ss"))

	(provide (all-defined))
;	(provide rename-var 
;		 test-this these-tests test01 tests01)
)
