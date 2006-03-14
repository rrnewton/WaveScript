
(module simulator_nought_graphics mzscheme
  (require "iu-match.ss"
           (lib "include.ss") 
	   (lib "compat.ss") 
	   (lib "pretty.ss"))
  
  (require "../generic/constants.ss"
           (all-except "helpers.ss" id flush-output-port)
           (all-except "flat_threads.ss" test-this these-tests)
           (all-except "tsort.ss" test-this these-tests)            
           "basic_graphics.ss"
           (all-except "graphics_stub.ss" these-tests test-this)
;           (prefix textsim: "simulator_nought.ss")
           (all-except "simulator_nought.ss" this-unit-description 
                       unfold-list test-this these-tests wrap-def-simulate
                       csn t1 
;                       example-nodal-prog0 example-nodal-prog1 example-nodal-prog2
;                       example-nodal-prog99 example-nodal-output0
                       )
          )
  
  (require (prefix textsim: "simulator_nought.ss"))
;  (disp "WOO" world-xbound draw-procs)
   
;  (define object-graph textsim:object-graph)
;  (define (make-default-hash-table) (make-hash-table))   
 
  (define sleep-me sleep)
  
;  (require "graphics_stub.ss")
;  (disp "yeah" draw-proc draw-procs)
  (include (build-path "generic" "simulator_nought_graphics.ss"))

  (provide (all-defined)
           (all-from "../generic/constants.ss")
           (all-from "flat_threads.ss")
           (all-from "helpers.ss")
           (all-from "basic_graphics.ss")
           (all-from "graphics_stub.ss")
           (all-from "simulator_nought.ss")
   )

  )

;(require simulator_nought_graphics)
;(test-this 'quiet 'verbose)

