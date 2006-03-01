
(module pass06_remove-complex-constant mzscheme
	(require (lib "include.ss")
                  "iu-match.ss"
		  "prim_defs.ss"
		  (all-except "helpers.ss" test-this these-tests)
                  (all-except "regiment_helpers.ss" test-this these-tests)
                  (all-except "hm_type_inference.ss" test-this these-tests)
                  )

	(include (build-path "generic" 
			     "pass06_remove-complex-constant.ss"))

	(provide (all-defined))
;	(provide rename-var 
;		 test-this these-tests test01 tests01)
)
