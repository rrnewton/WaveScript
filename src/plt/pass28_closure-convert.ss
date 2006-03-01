
(module pass28_closure-convert mzscheme

;  (require-for-syntax "constants.ss")
;  (require constants)

  (require (lib "include.ss"))
  (require (lib "list.ss"))
;  (require (lib "list.ss"))
;  (require (lib "trace.ss"))
  (require "constants.ss"
	   "prim_defs.ss")
  (require "iu-match.ss")
  (require (all-except "tml_generic_traverse.ss" filter test-this these-tests))
  (require (all-except "grammar_checker.ss" filter test-this these-tests))
  (require (all-except "helpers.ss" filter test-this these-tests))
  (require (all-except "regiment_helpers.ss" filter test-this these-tests))

  (require (lib "trace.ss"))

;  (define (make-list n x) (map (lambda (_) x) (iota n)))  
  
  (include (build-path  "generic" "pass28_closure-convert.ss"))
  
;  (provide (all-defined))
  (provide closure-convert test28 tests28 test-this these-tests
           test-closure-convert tests-closure-convert
           mvlet destructure-tokbind
           unique-name-counter deep-assq deep-assq-all
           )
  )

;(require pass28_closure-convert)
;(test28 'qv)
