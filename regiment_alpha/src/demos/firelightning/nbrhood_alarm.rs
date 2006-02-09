
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

(define _lightthreshold 20)
(define _tempthreshold 40)

;; All nodes over a light threshold:
(define light-triggered
  (rfilter (lambda (n) (> (sense 'light n) _lightthreshold))
	   world))

;; The neighborhoods around all light-triggered.
(define area (rkhood light-triggered 2))
(define locales (cluster area))
;(define heads (rmap anchor-in locales))

(define local-avgs
  (rmap (lambda (r) (average (rmap (lambda (n) (sense 'temp n)) r)))
	locales))

(define alarms
  (rwhen-any (lambda (avg) (> avg _tempthreshold))
	     local-avgs))

;; Main query:
alarms
