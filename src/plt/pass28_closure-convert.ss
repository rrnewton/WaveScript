
(module pass28_closure-convert mzscheme

;  (require-for-syntax "constants.ss")
;  (require constants)

  (require (lib "include.ss"))
  (require (lib "list.ss"))
;  (require (lib "trace.ss"))
  (require "constants.ss")
  (require "iu-match.ss")
  (require (all-except "helpers.ss" filter))

  (require (lib "trace.ss"))

  (define (make-list n x)
    (map (lambda (_) x) (iota n)))
  
  (include (build-path  "generic" "pass28_closure-convert.ss"))
  
;  (provide (all-defined))
  (provide closure-convert test28 tests28 test-closure-convert tests-closure-convert
           mvlet destructure-tokbind
           unique-name-counter deep-assq
           )
  )

;(require pass28_closure-convert)
