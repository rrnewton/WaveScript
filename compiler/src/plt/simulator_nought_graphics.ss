
(module simulator_nought_graphics mzscheme
  (require (lib "iu-match.ss")
           (lib "include.ss")
           (lib "compat.ss")                      
           (all-except "helpers.ss" id flush-output-port)
           (all-except "flat_threads.ss" test-this these-tests)
           (all-except "tsort.ss" test-this these-tests)
           (all-except "simulator_nought.ss" this-unit-description 
                       unfold-list test-this these-tests wrap-def-simulate
                       csn t1 
;                       example-nodal-prog0 example-nodal-prog1 example-nodal-prog2
;                       example-nodal-prog99 example-nodal-output0
                       )
           "basic_graphics.ss"
           "graphics_stub.ss"           
           )
  
  (provide (all-defined) 
           ;object-graph all-objs
;           init-graphics close-graphics
           ;change-color!
           (all-from "helpers.ss")
           (all-from "basic_graphics.ss")
           (all-from "graphics_stub.ss")
           (all-from "simulator_nought.ss"))
;  (define (make-default-hash-table) (make-hash-table))
    
  (define hashtab-get hash-table-get)
  (define hashtab-set! hash-table-put!)

  (define sleep-me sleep)
  
  (include "../generic/simulator_nought_graphics.ss")
  )

(require simulator_nought_graphics)
(test-this)