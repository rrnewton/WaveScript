;;;; .title A Simple Lightning/Forest-fire Sim.
;;;; .author Ryan Newton

;;;; [2006.02.01] <br> 
;;;; Matt and discussed a simple lightning/forest-fire simulation that
;;;; would stress Regiment and provide for some interesting programs.


;; This is the global state used by the lightning sim.  Could refactor
;; this to encapsulate it later.
(define foo 0)

;; This function maintains the global lightning state.  It's updated
;; at every time the world is recomputed.  The inputted time-step must
;; be greater than the one inputted last.  However the needn't be
;; consecutive.
(define firelightning-timestepper 
  (let ([last-time #f])
    (lambda (t)
      (if (< t last-time)
	  (error 'firelightning-timestepper "can't go backwards in time from t=~a to t=~a." last-time t))
      
      (void))))

;; This is the sensing function itself.
(define firelightning-sense-function
  
  (void))


;; Install the lightning sim as the default sim:
(simalpha-sense-function firelightning-sense-function)
(simalpha-sense-timestepper firelightning-timestepper)
