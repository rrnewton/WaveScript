
(module pass11_remove-complex-opera mzscheme
	(require (lib "include.ss")
                  "iu-match.ss"
                  "constants.ss"
		  "prim_defs.ss"
		  (all-except "helpers.ss" test-this these-tests)
                  (all-except "regiment_helpers.ss" test-this these-tests)
                  (all-except "hm_type_inference.ss" test-this these-tests))

	(include (build-path "generic" "pass11_remove-complex-opera.ss"))

       	(provide (all-defined))
;	(provide rename-var 
;		 test-this these-tests test01 tests01)
)


;; (require  pass07_remove-complex-opera)
