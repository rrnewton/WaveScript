
;; [2004.05.23]

;; This will depend on the "basic_graphics" abstraction, but will not
;; respect the abstraction.  It's kinda silly, right now I have two
;; layers of astraction that I prolly don't need.  Both the
;; "basic_graphics" for basic drawing primitives, and "graphics_stub"
;; for high level routines used by my system.  SO, if this continues
;; to be unneccessary I will prolly move this file to the src/generic/
;; subdirectory and just use the basic_graphics interface.

(load "basic_graphics.ss")

(define processor_screen_objs '())
;(define links '())

;; This function bears an onus to destroy old screen objects and post up new ones.
;; It might be called whenever the processor set changes.
(define (draw-procs procs)
  (for-each destroy procesor_screen_objs)
  (set! processor_screen_objs
  (map (lambda (pr)          
;	      (draw-ellipse (car pr) (cadr pr) 
;			    (+ (/ width 50) (car pr)) (+ (/ height 50) (cadr pr))
;			    (rgb 0 0 0) (rgb 200 10 10))
	      (let ((circ (create <oval> the-canvas 
				  (car pr) (cadr pr) 
				  (+ (/ width 50) (car pr)) (+ (/ height 50) (cadr pr))
				  )))
		(set-fill-color! circ (make <rgb> 200 10 10))
		(show circ)
		circ))
       procs)
;  (paint-buffer)
  ))

;; This being caled involves
(define (draw-links procs)
  

  

;(init-graphics)


(define (test)
(let* ([top (create <toplevel> with (title: "Canvas Example"))]
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
)