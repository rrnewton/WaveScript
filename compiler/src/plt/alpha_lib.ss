
(module alpha_lib mzscheme
  (require 
   "iu-match.ss"
   (lib "include.ss")
   (lib "pretty.ss")
   (lib "list.ss")
   (all-except (lib "compat.ss") define-structure flush-output-port) 
   "constants.ss"  
   (all-except "helpers.ss" test-this these-tests filter)
   (all-except "basic_graphics.ss" test-this these-tests)
   (all-except "graphics_stub.ss" test-this these-tests) 
   "simulator_alpha.ss" ;; Would like to remove this dependency eventually.
   )

  (include "../generic/alpha_lib.ss")
  (provide (all-defined))
 )

(require alpha_lib)
;(run-alpha-sim)