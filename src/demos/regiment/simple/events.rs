
;;;; This simple demo program fires local events when the sensor value
;;;; goes over a certain threshold.  

(parameters 
  [simalpha-channel-model 'lossless]
  [simalpha-placement-type 'connected]
  [simalpha-failure-model  'none]
  ;[simalpha-sense-function-constructor sense-noisy-rising]
  [simalpha-sense-function-constructor sense-random-1to100]
  [sim-num-nodes 30]
  [simalpha-consec-ids #t]
  [default-slow-pulse 1000]
  [default-fast-pulse 100]
  [sim-timeout 7000])

;; Main program

;; Fire the event when any 1-100 reading is greater than 90.  Should
;; return a smattering of events from across the network.

(rwhen-any 
 (lambda (#(_ t)) (> t 90))
 ;(lambda (v) (> (tupref 1 2 v) 99))
 (rmap (lambda (n) (tuple (nodeid n) (sense "default" n))) world)
	   )
