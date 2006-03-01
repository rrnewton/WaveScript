
(module pass08_uncover-free mzscheme
	(require (lib "include.ss")
                  "iu-match.ss"
                  "prim_defs.ss"
		  (all-except "helpers.ss" test-this these-tests)
                  (all-except "regiment_helpers.ss" test-this these-tests))

	(include (build-path "generic" "pass08_uncover-free.ss"))

	(provide (all-defined))
;	(provide rename-var 
;		 test-this these-tests test01 tests01)
)
