

(module graphics_stub mzscheme	
  (provide draw-procs draw-proc draw-edge)
  
  (require (lib "iu-match.ss")
           (prefix plt: (lib "graphics.ss" "graphics"))
           (lib "include.ss")
           "basic_graphics.ss")

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
;    ((draw-viewport the-win) (make-rgb 0 0 0))
    (for-each (lambda (pr)          
                ((plt:draw-solid-ellipse the-win)
                 (plt:make-posn (car pr) (cadr pr))
                 5 5 (plt:make-rgb 1 0 0)))
              procs))   

  ;; This uses PLT's built-in hash tables directly:
  (define edge-table (make-hash-table))
  (define proc-table (make-hash-table))
  
  (define (draw-proc proc)
    (let ((gobj (gensym)))
      ((plt:draw-solid-ellipse the-win)
       (plt:make-posn (- (car pr) processor-screen-radius) 
                      (- (cadr pr) processor-screen-radius))
       processor-screen-radius
       processor-screen-radius
       (plt:make-rgb 1 0 0))
      (hash-table-put! proc-table gobj proc)
      gobj))

  (define (draw-edge ed) (void))  
  
  )

