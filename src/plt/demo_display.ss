

(module demo_display mzscheme	
  (provide processors_temp test-this ;these-tests
           ;; from graphics_stub
           draw-procs draw-proc draw-edge 
           ;; from basic_graphics
           init-graphics close-graphics
	   window-width window-height the-win
           )

  (require (lib "include.ss"))      
  (require "basic_graphics.ss"
           "graphics_stub.ss"
           "helpers.ss")

  (include (build-path ".." "generic" "demo_display.ss"))
 
  ) ;; End module 


(require demo_display)
(require (lib "graphics.ss" "graphics"))
(define (t)
  (init-graphics)
  (draw-procs processors_temp))
