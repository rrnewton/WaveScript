

  (define the-win #f)
  (define the-canvas #f)

  (define width 400)
  (define height 400)

  ;; Init.
  (define (init-graphics)
    (printf "Running graphical interface to simple simulator.\n")
    (set! the-win (create <toplevel> with (title: "Region Streams Demo")))
    (set! the-canvas (create <canvas> the-win))
;	  with
;	  (background-color: (make <rgb> 215 215 255)))

    (show (create <label> the-win with (title: "FOOBTRON")))
    (show the-win)
    (show the-canvas)
    )


'  (define (draw-procs procs)
    ((draw-viewport the-win) (make-rgb 0 0 0))
    (for-each (lambda (pr)          
                ((draw-solid-ellipse the-win) 
                 (make-posn (car pr) (cadr pr))
                 5 5 (make-rgb 1 0 0)))
              procs))

