
;; [2004.05.23]

;; This will depend on the "basic_graphics" abstraction, but will not
;; respect the abstraction.  It's kinda silly, right now I have two
;; layers of astraction that I prolly don't need.  Both the
;; "basic_graphics" for basic drawing primitives, and "graphics_stub"
;; for high level routines used by my system.  SO, if this continues
;; to be unneccessary I will prolly move this file to the src/generic/
;; subdirectory and just use the basic_graphics interface.

;; DEPENDS: on world-xbound and world-ybound from simulator_nought.ss, 
;; it uses these to scale the world coordinates to the display.

;(load "basic_graphics.ss")

(module graphics_stub (draw-procs draw-proc draw-edge
		       these-tests test-this)
	(import basic_graphics)

(define processor-screen-objs '())
(define edge-screen-objs '())

(define processor-screen-radius 25.)

;;===============================================================================
;; Utils:

;; Returns a fixnum or flonum
(define scale2d 
  (let ((prep (lambda (x)
		(if (not (integer? x))
		    (exact->inexact x)
		    x))))
    (lambda (pos box1 box2)
					;  (disp "SCALING" (list pos box1 box2))
      (match (list pos box1 box2)
	     [((,x ,y) (,a1 ,b1 ,a2 ,b2) (,c1 ,d1 ,c2 ,d2))
	      (values 
	       (prep (+ (* (/ (- x a1) (- a2 a1)) (- c2 c1)) c1))
	       (prep (+ (* (/ (- y b1) (- b2 b1)) (- d2 d1)) d1)))]
	     [,otherwise (error 'scale2d "bad arguments: ~s ~s ~s"
				pos box1 box2)]))))
    
;;===============================================================================

;(define links '())

;; This function bears an onus to destroy old screen objects and post up new ones.
;; It might be called whenever the processor set changes.
;; This one DOES SHOW the processors.
(define (draw-procs procs)
  (DEBUGMODE
   (if (not the-win) (error 'draw-procs "graphics window is not initialized"))
   (for-each (lambda (proc)
	       (if (not (and (list? proc)
			     (= (length proc) 2)
			     (number? (car proc))
			     (number? (cadr proc))))
		          (error 'draw-procs
				 "Invalid processor coordinates: ~s among processors ~n~s~n" 
				 proc procs)))
	     procs))
  (for-each destroy processor-screen-objs)
  (set! processor-screen-objs '())
  (for-each draw-proc procs)
  (for-each show processor-screen-objs)
  processor-screen-objs)

;; This *does not show* the screen object that it creates.
(define (draw-proc pr)  
  (mvlet ([(x y) (scale2d 
		  pr (list 0 0 world-xbound world-ybound)
		     (list 0 0 window-width window-height))])
	 
#;	 (disp "DRAWING AT:"
	       (- x processor-screen-radius)
	       (- y processor-screen-radius)
	       (+ x processor-screen-radius)
	       (+ y processor-screen-radius))

	 (let ((circ (create <oval> the-canvas 
;			     50 50 
;			     100 100)))
	       (- x processor-screen-radius)
	       (- y processor-screen-radius)
	       (+ x processor-screen-radius)
	       (+ y processor-screen-radius))))
;			     (flonum->fixnum (- x processor-screen-radius))
;			     (flonum->fixnum (- y processor-screen-radius))
;			     (flonum->fixnum (+ x processor-screen-radius))
;			     (flonum->fixnum (+ y processor-screen-radius)))))
	   (set-fill-color! circ (make <rgb> 200 10 10))
;	   (show circ)
	   (set! processor-screen-objs
		 (cons circ processor-screen-objs))
	   circ)))

;; This being caled involves
(define (draw-edge pos1 pos2)
  (let ([box1 (list 0 0 world-xbound world-ybound)]
	[box2 (list 0 0 window-width window-height)])
    (match (list pos1 pos2)
	   [((,a ,b) (,c ,d))
	    (mvlet ([(x1 y1) (scale2d (list a b) box1 box2)]
		    [(x2 y2) (scale2d (list c d) box1 box2)])
		   (let ((line (create <line> the-canvas x1 y1 x2 y2)))
		     	   (set! edge-screen-objs
				 (cons line edge-screen-objs))
			   line))]
	   [,otherwise (error 'draw-edge "bad-input: ~s" edge)])))

;(define (draw-edge ....
 
;(init-graphics)

;;===============================================================================

;; Get the tests for this unit:
(include "generic/graphics_stub.tests")

(define test-this (default-unit-tester this-unit-description these-tests))

) ;; End module.
