
(module pass08_uncover-free mzscheme
	(require (lib "include.ss")
                  "iu-match.ss"
                  (all-except "helpers.ss" test-this these-tests))

	(include (build-path "generic" "pass08_uncover-free.ss"))

	(provide (all-defined))
;	(provide rename-var 
;		 test-this these-tests test01 tests01)
)
