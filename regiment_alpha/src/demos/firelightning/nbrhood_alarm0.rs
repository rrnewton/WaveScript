
(parameters 
  [dummy-param (install-firelightning)]

  [simalpha-realtime-mode #t]
  [simalpha-channel-model 'lossless]
  ;[simalpha-placement-type 'gridlike]
  [simalpha-failure-model  'none]
  ;[simalpha-sense-function sense-noisy-rising]
  ;[simalpha-sense-function sense-random-1to100]
  ;`[sim-timeout 2000]
  )

(define _tempthreshold 20)

;; All nodes over a local temperature threshold.
(define heat-events
  (rfilter (lambda (n) (> (sense 'temp n) _tempthreshold))
	   world))

(define local-avgs 
  (rfilter 
   
   area))

TODO FINISH

(define local-avgs
  (rmap (lambda (r) (average (rmap (lambda (n) (sense 'temp n)) r)))
	locales))

(define alarms
  (rwhen-any (lambda (avg) (> avg _tempthreshold))
	     local-avgs))

;; Main query:
alarms
