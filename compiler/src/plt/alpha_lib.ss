
(module alpha_lib mzscheme
  (require 
;   "iu-match.ss"
   (lib "include.ss")
   (lib "pretty.ss")
;           (lib "load.ss" "slibinit")
;           (lib "compat.ss") ;; gives us define-structure
   "constants.ss"
   (all-except "basic_graphics.ss" test-this these-tests)
   (all-except "graphics_stub.ss" test-this these-tests) 
   "simulator_alpha.ss" ;; Would like to remove this dependency eventually.
   )

  (include "../generic/alpha_lib.ss")
  (provide (all-defined))
 )

(require alpha_lib)
