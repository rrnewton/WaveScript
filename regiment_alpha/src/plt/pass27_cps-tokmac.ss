
(module pass27_cps-tokmac mzscheme

;  (require-for-syntax "constants.ss")
;  (require constants)

  (require (lib "include.ss"))
  (require (lib "list.ss"))
;  (require (lib "trace.ss"))
  (require "constants.ss")
  (require "iu-match.ss")
  (require (all-except "helpers.ss" filter test-this these-tests))
  (require (all-except "tml_generic_traverse.ss" test-this these-tests))
  (require (lib "trace.ss"))
  
  (include (build-path  "generic" "pass27_cps-tokmac.ss"))
  
;  (provide (all-defined))
  (provide cps-tokmac test27 tests27 test-cps-tokmac tests-cps-tokmac testcps-expr
           mvlet destructure-tokbind
           )
  )

;(require pass27_cps-tokmac)

#;(cps-tokmac '(cps-tokmac-lang '(program (bindings) (nodepgm (tokens)))))

#;(cps-tokmac
 '(cps-tokmac-lang
   '(program
     (bindings )
     (nodepgm 
      (tokens
       [tok1 id () (bindings) (stored) (printf "foo~n")]
       )))))


#;(cps-tokmac
 '(cps-tokmac-lang
   '(program
     (bindings )
     (nodepgm 
      (tokens
       [tok1 id () (bindings) (stored) (printf "foo~n")]
       [tok2 id (x y) (+ x y)]
       )))))

