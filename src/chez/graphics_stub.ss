
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

