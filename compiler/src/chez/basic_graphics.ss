;; [2004.05.24]
;; This implements my simple "basic graphics" interface, which I will
;; document somewhere as soon as it's done.  Erg, you'd think this
;; would be part of slib?

(module basic_graphics (window-height window-width
			init-graphics close-graphics 			
			rgb? rgb rgb-red rgb-green rgb-blue
			;; current-drawing-color 
			;; current-filling-color
			;; current-background-color
			;; draw-ellipse ....
			)

;; This defines window-width and window-height presets:
(include "../generic/basic_graphics.ss")

(define the-win #f)
(define the-canvas #f)

(define-structure (rgb red green blue))
(define rgb make-rgb)

(define current-drawing-color (make-rgb 0 255 0))
(define current-filling-color #f)
(define current-background-color (make-rgb 200 200 200))

;; Here's where we keep objects that are to be drawn:
(define object-buffer '())
;; And here's where we keep the objects that are on the screen:
(define screen-buffer '())

;; Init.
(define (init-graphics)
;  (printf "Running graphical interface to simple simulator.~n")
  (if the-win 
      (printf "Graphics already open!!~n")
      (begin
	(set! the-win (create <toplevel> with 
			      (title: "Region Streams Demo")))
	(set! the-canvas (create <canvas> the-win
				 with
				 (width: window-width) ;(in->pixels 5))
				 (height: window-height) ;(in->pixels 5))
				 (background-color: (make <rgb> 215 215 255))))
	(show the-win)
	(show the-canvas)  
	)))

(define (close-graphics)
  (destroy the-canvas)
  (destroy the-win)
  (set! the-canvas #f)
  (set! the-win #f))

(define (clear-buffer)
  (for-each destroy object-buffer)
  (set! object-buffer '())
  )

;; This may optionally destroy the object buffer as well:
(define (paint-buffer)
  ;; This requires them be seperate in memory:
  (for-each destroy screen-buffer)
  (for-each show object-buffer)
  (set! screen-buffer object-buffer)
  (set! object-buffer '())
  )

(define draw-ellipse
  (let ((drawit (lambda (x1 y1 x2 y2 draw fill)		  
		  (let ((circ (create <oval> the-canvas x1 y1 x2 y2)))
		    (set-outline-color! circ 
                      (make <rgb> 
			(rgb-red draw) (rgb-green draw) (rgb-blue draw)))
		    (if fill (set-fill-color! 
			      circ 
			      (make <rgb> (rgb-red fill) (rgb-green fill) (rgb-blue fill))))
		    ;(show circ)
		    (hide circ)
		    (set! object-buffer (cons circ object-buffer))
		    ))))
  (case-lambda
   [(x1 y1 x2 y2) (drawit x1 y1 x2 y2 current-drawing-color current-filling-color)]
   [(x1 y1 x2 y2 c1) (drawit x1 y1 x2 y2 c1 current-filling-color)]
   [(x1 y1 x2 y2 c1 c2) (drawit x1 y1 x2 y2 c1 c2)])))


'(let* ([top (create <toplevel> with (title: "Canvas Example"))]
	[start-text "Click button 1 in canvas below"]
	[label (create <label> top with (title: start-text))])
  (define-class (<example-canvas> parent) (<canvas> parent)
    (ivars (x1 #f) (y1 #f) (rect #f))
    (inherited)
    (inheritable)
    (private)
    (protected)
    (public
      [mouse-press (x y mods)
       (event-case ((modifier= mods))
         (([left-button])
          (set! x1 x)
          (set! y1 y)
          (set-title! label "Hold down button 1 and drag")
          (set! rect (create <rectangle> self x1 y1 x1 y1)))
         (else (send-base self mouse-press x y mods)))]
      [mouse-motion (x y mods)
       (event-case ((modifier= mods))
         (([left-button])
          (when rect (set-coords! rect (min x x1) (min y y1) (max x x1) (max y y1))))
         (else (send-base self mouse-motion x y mods)))]
      [mouse-release (x y mods)
       (event-case ((modifier= mods))
         (([left-button]) (set! rect #f) (set-title! label start-text))
         (else (send-base self mouse-release x y mods)))]))
  (let ([canvas
         (create <example-canvas> top
           with
           (background-color: (make <rgb> 215 215 255)))])
    (show label)
    (show canvas)))


);; End module