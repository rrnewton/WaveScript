
;;;; This simple demo program fires local events when the sensor value
;;;; goes over a certain threshold.  

(parameters 
  [simalpha-channel-model 'lossless]
  [simalpha-placement-type 'connected]
  [simalpha-failure-model  'none]
  ;[simalpha-sense-function-constructor sense-noisy-rising]
  [simalpha-sense-function-constructor sense-random-1to100]
  [sim-timeout 2000])


;; Main program

;; Fire the event when any 1-100 reading is greater than 99.  Should
;; return a smattering of events from across the network.

(rwhen-any (lambda (v) (> (tupref 1 2 v) 99))
	   (rmap (lambda (n) (tuple (nodeid n) (sense n))) world)
	   )
