(module temp mzscheme

(require "graphics_stub.ss")
;(require "simulator_nought_graphics.ss")
;(test-this 'verbose)
;(close-graphics)

;(define (g) (load "temp.ss"))
(printf "temp loaded~n")

  (fluid-let ((foob 'newb))
    (disp 'yay))  
)