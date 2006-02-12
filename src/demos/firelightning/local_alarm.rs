
(parameters 
  [dummy-param (install-firelightning)]

  [simalpha-realtime-mode #t]
  [simalpha-channel-model 'lossless]
  ;[simalpha-placement-type 'gridlike]
  [simalpha-failure-model  'none]
  ;[simalpha-sense-function sense-noisy-rising]
  ;[simalpha-sense-function sense-random-1to100]
  [sim-timeout 80000]
  )

(define _threshold 30)

;; This is our data space, it consists of (nodeid, temp) pairs
(define data
  (rmap (lambda (n) (tuple (nodeid n) (sense 'temp n)))
	world))

;(define alarm
;  (rwhen-any (lambda (tup) (> (tupref 1 2 tup) _threshold)) temps))

;; Main query:
;alarm
data
