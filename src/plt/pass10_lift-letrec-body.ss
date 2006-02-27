
(module pass10_lift-letrec-body mzscheme
	(require (lib "include.ss")
                  "iu-match.ss"
		  (all-except "helpers.ss" test-this these-tests)
                  (all-except "regiment_helpers.ss" test-this these-tests)
                  (all-except "hm_type_inference.ss" test-this these-tests))

	(include (build-path  "generic" "pass10_lift-letrec-body.ss"))

       	(provide (all-defined))
;	(provide rename-var 
;		 test-this these-tests test01 tests01)
)
