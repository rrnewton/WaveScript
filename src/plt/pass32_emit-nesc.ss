
(module pass32_emit-nesc mzscheme
  (require (lib "include.ss")
           (lib "list.ss")
           (lib "trace.ss"))
  
;  (require (lib "trace.ss"))

  (require "constants.ss"
	   "hashtab.ss"
	   "prim_defs.ss"
           "cheztrace.ss"
	   "iu-match.ss")
  (require (all-except "tml_generic_traverse.ss" test-this these-tests)
           (all-except "grammar_checker.ss" test-this these-tests)
	   (all-except "helpers.ss" filter test-this these-tests id)
	   (all-except "regiment_helpers.ss" filter test-this these-tests id))
 
  (define (id x) x)
  (include (build-path  "generic" "pass32_emit-nesc.ss"))
  
  (provide 
   (all-defined)
   ;emit-nesc test32
           )
  )

;(require pass32_emit-nesc)

