;; chez/graphics_stub.ss
;; Implements the GRAPHICS_STUB interface, described in "generic/graphic_stub.ss".
;; Should provide the same functionality as plt/graphics_stub.ss

;; TODO IMPLEMENT GET-STATE



;; [2004.05.23]

;; This will depend on the "basic_graphics" abstraction, but will not
;; respect the abstraction.  It's kinda silly, right now I have two
;; layers of astraction that I prolly don't need.  Both the
;; "basic_graphics" for basic drawing primitives, and "graphics_stub"
;; for high level routines used by my system.  SO, if this continues
;; to be unneccessary I will prolly move this file to the src/generic/
;; subdirectory and just use the basic_graphics interface.

;; REQUIRES: on world-xbound and world-ybound from simulator_nought.ss, 
;; it uses these to scale the world coordinates to the display.

;(load "basic_graphics.ss")

(module graphics_stub (draw-procs draw-proc draw-edge change-color! ;set-color!
				  these-tests test-this )
  (import basic_graphics)

;; CONSTANTS:
;(define Starting-Node-Color (make <rgb> 200 10 10))
(define Starting-Node-Color (make <rgb> 130 130 130))


(define processor-screen-objs '())
(define edge-screen-objs '())

;; Include definitions common to the chez and plt versions:
(include "generic/graphics_stub.ss")
;; Down in the INITIALIZATION section of this file I mutate some of
;; the state from here...

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

(define (change-color! ob c)
  (set-fill-color! ob
		   (make <rgb>
		     (rgb-red c)
		     (rgb-green c)
		     (rgb-blue c))))		     

(define (get-state sym ob)
  (case sym
    [(color) (get-fill-color ob)]
    [(loc) (get-coords ob)] ;; WHAT TYPE DOES THIS RETURN??
    [else (error 'chez/graphics_stub.ss "unknown state property: ~s" sym)]))

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

	 (let ((circ (create <oval> the-win
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
	   (set-fill-color! circ Starting-Node-Color)
;	   (show circ)
	   (set! processor-screen-objs
		 (cons circ processor-screen-objs))
	   circ)))

;; This being caled involves
(define (draw-edge pos1 pos2)
  (let ([box1 (list 0 0 world-xbound world-ybound)]
	[box2 (list 0 0 window-width window-height)])
    (DEBUGMODE
     (if (not (and (list? pos1) (= 2 (length pos1))
		   (list? pos2) (= 2 (length pos2))))
	 (error 'chez/graphics_stub.draw-edge
		"Invalid input positions: ~s ~s" pos1 pos2)))

;    (let ([a (car pos1)] [b (cadr pos1)]
;	  [c (car pos1)] [d (cadr pos1)])
    (mvlet ([(x1 y1) (scale2d pos1 box1 box2)]
	    [(x2 y2) (scale2d pos2 box1 box2)])
	   (let ((line (create <line> the-win x1 y1 x2 y2)))
	     (set! edge-screen-objs
		   (cons line edge-screen-objs))
	     line))))


;;===============================================================================

;; Get the tests for this unit:
(include "generic/graphics_stub.tests")

(define test-this (default-unit-tester this-unit-description these-tests))

;;===============================================================================
;; INITIALIZATION:

;; This is imported from the above.  Make sure that it is a flonum,
;; just so we don't end up with rationals:
(set! processor-screen-radius (exact->inexact processor-screen-radius))

) ;; End module.
