

(module demo_display mzscheme	
  (provide processors)

  (require (lib "include.ss"))      
  (require "graphics_stub.ss")

  (include (build-path ".." "generic" "demo_display.ss"))
   
  (draw-procs processors)

  ) ;; End module 

