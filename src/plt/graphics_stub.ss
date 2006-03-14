;; plt/graphics_stub.ss
;; Implements the GRAPHICS_STUB interface, described in "generic/graphic_stub.ss".
;; Should provide the same functionality as chez/graphics_stub.ss

;; This uses "proc" to refer to "processor", as in "node", which is a bit confusing.

(module graphics_stub mzscheme	
  (provide draw-network draw-proc draw-edge draw-mark draw-circle init-graphics 
           change-color! get-state 
           these-tests test-this testgraphics
	   clear-buffer
           )
  
  (require (all-except "helpers.ss" test-this these-tests)
	   "iu-match.ss"
           (prefix plt: (lib "graphics.ss" "graphics"))
           (lib "include.ss")
           (all-except "basic_graphics.ss")
;           (all-except "simulator_nought.ss" test-this these-tests) ;; For world-xbound, ybound
           "../generic/constants.ss" ;; For world-xbound, ybound
           )
  ;; [2006.02.27] No longer have a common file here:
;  (include (build-path "generic" "graphics_stub.ss"))

  (define default-proc-color (make-rgb 200 20 20))
  (define default-edge-color (make-rgb 0 0 0))
  
  ;=============================================================
  
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

  (define (coord:sim->screen pr)
    (scale2d pr (list 0 0 world-xbound world-ybound)
	     (list 0 0 window-width window-height)))
  
  ;=============================================================

  (define (clear-buffer) ((plt:clear-viewport the-win)))

  (define (draw-network procs)
    (map draw-proc procs))
    
    ;    ((draw-viewport the-win) (make-rgb 0 0 0))
;    (for-each (lambda (pr)          
;                ((plt:draw-solid-ellipse the-win)
;                 (plt:make-posn (car pr) (cadr pr))
;                 5 5 (plt:make-rgb 1 0 0)))
;              procs))
  
  ;; This uses PLT's built-in hash tables directly:
  ;; [2004.06.05] The proc-table maps a "gobj" (gensymed symbol) to an
  ;; association list of properties, currently including 'loc and
  ;; 'color.
  (define proc-table (make-hash-table))
  ;; This one supports properties 'src and 'dst for the source and
  ;; destination positions.  Also 'color.
  (define edge-table (make-hash-table))

  ;; Internal helper:
  (define rasterize-proc 
    (let ([halfrad (/ processor-screen-radius 2.0)])
    (lambda (pr color)
      (let ((color (plt:make-rgb (/ (rgb-red   color) 255.0)
			       (/ (rgb-green color) 255.0)
			       (/ (rgb-blue  color) 255.0))))
      (let-values ([(x y) (scale2d 
			   pr (list 0 0 world-xbound world-ybound)
			   (list 0 0 window-width window-height))])
		  (let ((gobj (gensym)))
		    ((plt:draw-solid-ellipse the-win)
		     (plt:make-posn (- x halfrad) (- y halfrad))
		     processor-screen-radius
		     processor-screen-radius
		     color)
		    gobj))))))

  (define (draw-mark pr color)
    (mvlet ([(x y) (coord:sim->screen pr)])
	   (let ((len 10) ;; shouldn't be constant.
		 (liner (plt:draw-line the-win))
		 (color (plt:make-rgb (/ (rgb-red   color) 255.0)
				      (/ (rgb-green color) 255.0)
				      (/ (rgb-blue  color) 255.0))))
	     (liner (plt:make-posn (- x len) (- y len)) (plt:make-posn (+ x len) (+ y len)) color)
	     (liner (plt:make-posn (+ x len) (- y len)) (plt:make-posn (- x len) (+ y len)) color)	     
	     (let ([l1 (gensym)]
		   [l2 (gensym)])
	       (list l1 l2)))))

(define (draw-circle pr rad)
  (mvlet ([(x y) (coord:sim->screen pr)]
	  [(radx rady) (coord:sim->screen (list rad rad))])
	 (let ((gobj (gensym)))
	   ((plt:draw-solid-ellipse the-win) (plt:make-posn x y)
	    radx
	    rady
	    (plt:make-rgb 0 0 0))
	   gobj)))

  ;; Internal helper:
  (define (rasterize-edge pos1 pos2 color)
    (let ((color (plt:make-rgb (/ (rgb-red   color) 255.0)
			       (/ (rgb-green color) 255.0)
			       (/ (rgb-blue  color) 255.0))))
      (DEBUGMODE
       (if (not (and (list? pos1) (= 2 (length pos1))
		     (list? pos2) (= 2 (length pos2))))
	   (error 'plt/graphics_stub.rasterize-edge
		  "Invalid input positions: ~s ~s" pos1 pos2)))       
      (let ([box1 (list 0 0 world-xbound world-ybound)]
	    [box2 (list 0 0 window-width window-height)])
	(let-values ([(x1 y1) (scale2d pos1 box1 box2)]
		     [(x2 y2) (scale2d pos2 box1 box2)])
		    (let ((gobj (gensym)))
		      ((plt:draw-line the-win) 
		       (plt:make-posn x1 y1) (plt:make-posn x2 y2) color)
		      gobj)))))
    
    (define (draw-proc pr)  
      (let ((gobj (rasterize-proc pr default-proc-color)))
	(hash-table-put! proc-table gobj `([loc ,pr]
                                           [color ,default-proc-color]))
	gobj))
    
    (define (draw-edge pt1 pt2) 
      (let ((gobj (rasterize-edge pt1 pt2 default-edge-color)))
	(hash-table-put! edge-table gobj `([src ,pt1] [dst ,pt2]))
	gobj))


    ;; Gets the location property for the object and draws a new circle there:
    (define (change-color! ob c)
      (let ((props (hash-table-get proc-table ob)))
      (let ((loc (assq 'loc props))
	    (col (assq 'col props)))
	(DEBUGMODE (if (not loc) 
		       (error 'graphics_stub.change-color! 
			      "processor graphics object had no 'loc property.")))
	;; Set the property for future reference:
	(if col (set-car! (cdr col) c)
	    (hash-table-put! proc-table ob
			     (cons `(color ,c) props)))
	(rasterize-proc (cadr loc) c)
	(void))))
    
    (define (get-state sym ob)
      (let ((props (hash-table-get 
		    proc-table ob
		    (lambda ()
		      (hash-table-get edge-table ob)))))
	(let ((entry (assq sym props)))
	  (DEBUGMODE (if (not entry) 
			 (error 'plt/graphics_stub.get-state
				"Couldn't find symbol in property list: ~s ~s" 
				sym props)))
          (cadr entry))))
   
  
  (define these-tests
    `(["You should see a circle"
       (begin (require "basic_graphics.ss")
              (init-graphics)
              (let ((x (draw-proc '(10 20))))
;                (display proc-table)(newline)
;                (printf "~s~n" (get-state 'color x))
                (sleep 0.3)
                (close-graphics)
                (list (get-state 'color x)
                      (get-state 'loc x))
                ))
       unspecified]

      ["You should see a line"
       (begin (require "basic_graphics.ss")
              (init-graphics)
              (let ((x (draw-edge '(10 20) '(50 50))))
                (sleep 0.3)                
                (close-graphics)
                x))
       unspecified]

      ["You should see three \"processors\""
       (begin (require "basic_graphics.ss")
              (init-graphics)
              (let ((x (draw-network  '((10 20) (50 50) (30 10)) )))
                (sleep 0.3)
                (close-graphics)
                x))
       unspecified]
      ))
  
  ;; This test is disabled right now because it's messed up and will only run in the plt/ directory.
  ;; It can still be run manually but will not be included in the collected unit tests for the system.
  (define test-this (default-unit-tester "graphics interface for simulator" these-tests 'disabled))
  (define testgraphics test-this)
  );;End module

;(require graphics_stub)
