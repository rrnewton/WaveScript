

(module graphics_stub mzscheme	
  (provide draw-procs draw-proc draw-edge init-graphics change-color!)
  
  (require (lib "iu-match.ss")
           (prefix plt: (lib "graphics.ss" "graphics"))
           (lib "include.ss")
           "basic_graphics.ss"
           "simulator_nought.ss" ;; For world-xbound, ybound
           )
  
  (include "../generic/graphics_stub.ss")
  
  ;;============================================================
  
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
  
  ;;============================================================
  
  (define (draw-procs procs)
    (map draw-proc procs))
    
    ;    ((draw-viewport the-win) (make-rgb 0 0 0))
;    (for-each (lambda (pr)          
;                ((plt:draw-solid-ellipse the-win)
;                 (plt:make-posn (car pr) (cadr pr))
;                 5 5 (plt:make-rgb 1 0 0)))
;              procs))
  
  ;; This uses PLT's built-in hash tables directly:
  (define edge-table (make-hash-table))
  (define proc-table (make-hash-table))
  
  (define (rasterize-proc pr . color)
    (let ((color (if (null? color)
                     (plt:make-rgb .8 .1 .1)
                     (plt:make-rgb (/ (rgb-red   (car color)) 255.0)
                                   (/ (rgb-green (car color)) 255.0)
                                   (/ (rgb-blue  (car color)) 255.0)))))
    (let-values ([(x y) (scale2d 
                         pr (list 0 0 world-xbound world-ybound)
                         (list 0 0 window-width window-height))])
      (let ((gobj (gensym)))
        ((plt:draw-solid-ellipse the-win) (plt:make-posn x y)
                                          processor-screen-radius
                                          processor-screen-radius
                                          color)
        gobj))))
  
  (define (draw-proc pr)  
    (let ((gobj (rasterize-proc pr)))
      (hash-table-put! proc-table gobj pr)         
      gobj))
  
  (define (draw-edge pt1 pt2)     
    (void))
  
  (define (change-color! ob c)
    (let ((proc (hash-table-get proc-table ob)))
      (rasterize-proc proc c)
      (void)))
  
  );;End module

;(require graphics_stub)