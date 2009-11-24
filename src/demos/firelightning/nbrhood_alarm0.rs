
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
(define (temp n) (sense 'temp n))
(define (abovethresh n) (> (temp n) _tempthreshold))
(define (count r) (rfold (lambda (_ acc) (+ 1 acc)) 0 r))

;; All nodes over a local temperature threshold.

(define heat-events (rfilter abovethresh world))

(define (local-results n)
  (let ((hood (khood 1.5 (node->anchor n))))
    (count hood)))

;; Main query:

(rfilter (lambda (c) (> c 1))
	 (rmap local-results heat-events))

