;; chez/graphics_stub.ss
;; Implements the GRAPHICS_STUB interface, described in "generic/graphic_stub.ss".
;; Should provide the same functionality as plt/graphics_stub.ss

;; THERE IS NO ABSTRACTION BOUNDARY BETWEEN THIS AND BASIC_GRAPHICS.SS

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

(module graphics_stub (draw-network draw-proc draw-edge draw-mark draw-circle
				  clear-buffer delete-gobj
				  Starting-Node-Color
				  change-color! ;set-color!
				  ;get-state ;; [2004.11.13] including
				  change-text!
				  ;; temporarily exposed:
				  processor-screen-radius
				  
				  these-tests test-this 
				  test-graphics-stub)
  (import basic_graphics)

  ;; CONSTANTS:
;(define Starting-Node-Color (make <rgb> 200 10 10))
(define Starting-Node-Color (make <rgb> 130 130 130))
(define Default-Edge-Color  (make <rgb> 10 10 10))

;; GLOBAL BINDINGS:

;; These three keep track of the SWL widgets.  Could use a hash.
;; Basically we use these to clear the screen by destroying everything mentioned in these lists.
(define processor-screen-objs '())
(define edge-screen-objs '())
(define other-objs '())

;; Include definitions common to the chez and plt versions:
(include "generic/graphics_stub.ss")
;; Down in the INITIALIZATION section of this file I mutate some of
;; the state from here...

;===============================================================================
;; Utils:

;; Returns a fixnum or flonum, 
;; Maps a coordinate in one box to the anologous coordinate in another.
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


;; This makes use of the global constants for world and screen bounds,
;; and uses them to convert a coordinate.
(define (coord:sim->screen pr)
  (scale2d pr (list 0 0 world-xbound world-ybound)
  	      (list 0 0 window-width window-height)))

;; [2004.06.22]
;; The values returned by the draw-* procedures in this file are my
;; "graphics-objects" or gobjs used by the simulator.  They can be
;; uniformly destroyed with this procedure:
(define (delete-gobj g)
  (if (list? g) (map destroy g) (destroy g)))

;; Deletes all the existing SWL widgets on the canvas..
(define (clear-buffer)
  (for-each destroy other-objs)
  (for-each destroy processor-screen-objs)
  (for-each destroy edge-screen-objs) 
  (set! other-objs '())
  (set! processor-screen-objs '())
  (set! edge-screen-objs '())
  ;; If the simulation has pointers over here, get rid of em.
  (and (top-level-bound? 'all-objs)
       all-objs
       (for-each (lambda (simob)
		   (set-simobject-gobj! simob #f))
		 all-objs)))

;================================================================================

;; We don't keep track of which objects need to be redrawn, that's all
;; handled by tcl/tk.
(define (change-color! ob c)
  (set-fill-color! ob
		   (make <rgb>
		     (rgb-red c)
		     (rgb-green c)
		     (rgb-blue c))))

(define change-text! set-title!)

(define (get-state sym ob)
  (case sym
    [(color) (get-fill-color ob)]
    [(loc) (get-coords ob)] ;; WHAT TYPE DOES THIS RETURN??
    [else (error 'chez/graphics_stub.ss "unknown state property: ~s" sym)]))

;(define links '())

;; This function bears an onus to destroy old screen objects and post up new ones.
;; It might be called whenever the processor set changes.
;; This one DOES SHOW the processors.
;; Input: list of pairs (coordinates).
(define (draw-network procs ids)
  (DEBUGMODE ;; Invariant checking:
   (if (not the-win) (error 'draw-network "graphics window is not initialized"))
   (for-each (lambda (proc)
		(if (not (and (list? proc)
			      (= (length proc) 2)
			      (number? (car proc))
			      (number? (cadr proc))))
		    (error 'draw-network
			   "Invalid processor coordinates: ~s among processors ~n~s~n" 
			   proc procs)))
	     procs))
  ;; This is not an efficient use of the SWL library:
  ;; We don't clear the entire buffer, we're just drawing new procs and edges 
  ;; and we leave whatever else was drawn on the screen.
  (begin 
    (for-each destroy processor-screen-objs)
    ;(for-each destroy edge-screen-objs)
    ;(set! edge-screen-objs '())
    (set! processor-screen-objs '()))

  ;; Draw nodes:
  (let ((results (map draw-proc procs ids)))
    (for-each show processor-screen-objs) 
    results))


;; This *does not show* the screen object (gobj) that it creates.
;; Screen objects are SWL objects.  Their represention is opaque to us.
;; [2005.11.07] Now this returns a vector with four graphics: circle + three led boxes
(define (draw-proc pr nodeid)
  (mvlet ([(x y) (coord:sim->screen pr)])
  (let ([boundx1 (- x processor-screen-radius)]
	[boundy1 (- y processor-screen-radius)]
	[boundx2 (+ x processor-screen-radius)]
	[boundy2 (+ y processor-screen-radius)]
	[third (/ (* 2 processor-screen-radius) 3)])
    (let ((circ (create <oval> the-win
;			     50 50 
;			     100 100)))
			     boundx1 boundy1 boundx2 boundy2)))
;			     (flonum->fixnum (- x processor-screen-radius))
;			     (flonum->fixnum (- y processor-screen-radius))
;			     (flonum->fixnum (+ x processor-screen-radius))
;			     (flonum->fixnum (+ y processor-screen-radius)))))	  

	   (let ([box1 (create <rectangle> the-win 
			       boundx1                 (+ boundy1 (* .8 third))
			       (+ boundx1 third)       (- boundy2 (* .8 third)))]
		 [box2 (create <rectangle> the-win 
			       (+ boundx1 third)       (+ boundy1 (* .8 third))
			       (+ boundx1 (* 2 third)) (- boundy2 (* .8 third)))]
		 [box3 (create <rectangle> the-win 
                               (+ boundx1 (* 2 third)) (+ boundy1 (* .8 third))
			       (+ boundx1 (* 3 third)) (- boundy2 (* .8 third)))]
		 [defled (make <rgb> 0 50 50)]
		 [text (create <canvas-text> the-win (+ boundx1 (* 0.1 processor-screen-radius))
			                             (- boundy1 (* 0.3 processor-screen-radius))
			       with (title: (format "~s" nodeid )))]
		 [text2 (create <canvas-text> the-win (+ boundx1 (* 0.1 processor-screen-radius))
			                             (+ boundy2 (* 0.35 processor-screen-radius))
						     with (title: "") ;(format "~s" nodeid ))
						          (fill-color: (make <rgb> 0 100 0)))]
		 )
	     (show text)
	     (show text2)

	     ;(set-foreground-color! text2 (make <rgb> 255 0 0))
	     ;(set-color! text2 (make <rgb> 255 0 0))

	     (set-fill-color! box1 defled)
	     (set-fill-color! box2 defled)
	     (set-fill-color! box3 defled)
	   
	   (set-fill-color! circ Starting-Node-Color)
;	   (show circ)
	   (set! processor-screen-objs
		 (append (list circ box1 box2 box3 text text2)
			 processor-screen-objs))
	   (vector circ box1 box2 box3 text text2))))))

;; This returns nothing.
(define (draw-mark pr . color)
  (set! color (if (null? color) (make-rgb 0 0 0) (car color)))
  (mvlet ([(x y) (coord:sim->screen pr)])
    (let ((len 10)) ;; shouldn't be constant.
      (let ([l1 (create <line> the-win (- x len) (- y len) (+ x len) (+ y len))]
	    [l2 (create <line> the-win (- x len) (+ y len) (+ x len) (- y len))]
	    [c  (make <rgb> (rgb-red color) (rgb-green color) (rgb-blue color))])
	(set-fill-color! l1 c)
	(set-fill-color! l2 c)
	(set! other-objs (cons l1 (cons l2 other-objs)))
	(list l1 l2)))))

;; This draws an empty circle:
(define (draw-circle pr rad)
  (mvlet ([(x y) (coord:sim->screen pr)]
	  [(radx rady) (coord:sim->screen (list rad rad))])
	 (let ([obj (create <oval> the-win
			    (- x radx) (- y rady)
			    (+ x radx) (+ y rady))])
	   (set! other-objs (cons obj other-objs))
	   obj)))

;; This draws a new edge and adds that graphics object to the global list, 
;; as well as returning it.
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
	   (let ((line (create <line> the-win x1 y1 x2 y2
			       with (fill-color: Default-Edge-Color))))
	     (set! edge-screen-objs
		   (cons line edge-screen-objs))
	     line))))


;================================================================================

;; Get the tests for this unit:
(include "generic/graphics_stub.tests")

(define test-this (default-unit-tester "GraphicsStub: common graphics interface for PLT and chez" 
		    these-tests))
(define test-graphics-stub test-this)

;================================================================================
;; INITIALIZATION:

;; This is imported from the above.  Make sure that it is a flonum,
;; just so we don't end up with rationals:
(set! processor-screen-radius (exact->inexact processor-screen-radius))

) ;; End module.
