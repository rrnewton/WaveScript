#cs ;; Case Sensitivity
(module alpha_lib mzscheme
  (require 
   "iu-match.ss"
   (lib "include.ss")
   (lib "pretty.ss")
   (lib "list.ss")
   (all-except (lib "compat.ss") reg:define-struct flush-output-port) 
   "constants.ss"  
    "hashtab.ss"
    (all-except "helpers.ss" test-this these-tests filter)
    (all-except "regiment_helpers.ss" test-this these-tests filter)
    (all-except "pass21_cleanup-token-machine.ss" test-this these-tests)
      ;; Would like to remove this dependency eventually:

   (all-except "simulator_alpha_datatypes.ss") ;run-alpha-simple-scheduler)
   
   ;(all-except "alpha_lib_scheduler_simple.ss")
   )

  (IF_GRAPHICS (require (all-except "basic_graphics.ss" test-this these-tests)))
  (IF_GRAPHICS (require (all-except "graphics_stub.ss" test-this these-tests)))

  (provide 

	 get-connectivity get-node get-simobject ;; Utilities for coercion:         
         attempt-message-transmission

   	 current-simobject

	 retrieve-token
	 add-token
	 evict-token	 
	 evict-all-tokens


         
	 neighbors
	 sendmsg

	 sim-print-queue
	 sim-light-up
         sim-highlight-edge
	 sim-draw-mark
	 sim-leds
         sim-setlabel
	 ;sim-dist
	 sim-loc
	 sim-locdiff         
	 simulator-soc-return
	 simulator-soc-finished
	 check-store
	 ;alpha-it ;; shorthand

         
	 test-this these-tests test-alphalib   
;           (all-from "simulator_alpha.ss"))
   )

   
  (include (build-path "generic" "alpha_lib.ss"))
 )


;    (require alpha_lib)

;    (alpha-repl)
    
;    (run-alpha-sim 'simple)
    
;    (begin (require alpha_lib) (time (run-alpha-sim 'simple 10.0)))


;    (begin (require alpha_lib) (t) (run-alpha-sim))


;    (begin (require "alpha_lib.ss") (time (run-alpha-sim 15.0)))



;(require alpha_lib)
;(define (t) (start-alpha-sim node-code 'simple 10.0))
