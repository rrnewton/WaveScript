;;;; .title A basic test of the Regiment flatten operation

(parameters 
  [simalpha-channel-model 'lossless]
  [simalpha-placement-type 'gridlike]
  [simalpha-failure-model  'none]
  [simalpha-dbg-on #f]
  ;[simalpha-sense-function-constructor sense-noisy-rising]
  ;[simalpha-sense-function-constructor sense-random-1to100]
  [sim-timeout 10000])


(define a (light-up (khood (anchor-at 50 10) 1)))
(define b (light-up (khood (anchor-at 30 40) 1)))

(define theunion (runion a b))

;; Main:

;(tuple a b)
theunion
