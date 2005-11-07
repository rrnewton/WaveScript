
(module basic_graphics mzscheme
  (require (lib "include.ss")
           ;(lib "compat.ss")
           "helpers.ss"
           (prefix plt: (lib "graphics.ss" "graphics")))
;  (provide init-graphics close-graphics	window-width window-height the-win)
  (provide (all-defined))
  
  ;; This defines window-width and window-height presets;.
  (include (build-path "generic" "basic_graphics.ss"))
  
;  (define the-win #f) 
  (define (init-graphics) 
    (plt:open-graphics)
    (set! the-win (plt:open-viewport "Basic graphics for simulator." 
                                 window-width window-height))
    )
  (define (close-graphics) 
    (if (not the-win) (error 'close-graphics 
                             "basic_graphics.ss: the graphics system is not open!"))
    (plt:close-viewport the-win)
    (set! the-win #f) 
    (plt:close-graphics))
  
  )

;(require basic_graphics)
