


(module graphics_stub mzscheme	
  (provide init-graphics draw-procs width height)

  (require (lib "graphics.ss" "graphics"))

  (define the-win '())

  ;; Init.
  (define (init-graphics)
    (open-graphics)
    (set! the-win (open-viewport "RegionStreams Demo" width height))
    (printf "Running graphical interface to simple simulator.\n"))

  (define width 400)
  (define height 400)

  (define (draw-procs procs)
    ((draw-viewport the-win) (make-rgb 0 0 0))
    (for-each (lambda (pr)          
                ((draw-solid-ellipse the-win) 
                 (make-posn (car pr) (cadr pr))
                 5 5 (make-rgb 1 0 0)))
              procs))  
    
)

