
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
   (all-except "pass21_cleanup-token-machine.ss" test-this these-tests)
   "simulator_alpha.ss" ;; Would like to remove this dependency eventually.
   )
  (provide (all-defined)
           (all-from "simulator_alpha.ss"))
  
  (include (build-path "generic" "alpha_lib.ss"))
  (include (build-path "generic" "alpha_lib_scheduler.ss"))
  (include (build-path "generic" "alpha_lib_scheduler_simple.ss"))
 )


;    (require alpha_lib)

;    (alpha-repl)
    
;    (run-alpha-sim 'simple)
    
;    (begin (require alpha_lib) (time (run-alpha-sim 'simple 10.0)))


;    (begin (require alpha_lib) (t) (run-alpha-sim))


;    (begin (require "alpha_lib.ss") (time (run-alpha-sim 15.0)))
