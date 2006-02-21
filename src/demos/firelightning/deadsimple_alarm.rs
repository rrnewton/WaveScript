



;; This can provide us a base-line.  It uses *no* fancy Regiment
;; features, it merely sends all the temperature values over a certain
;; threshold to the base station.

(parameters 
  [dummy-param (install-firelightning)]
  ;[simalpha-realtime-mode #t]
  ;[simalpha-channel-model 'lossless]
  ;[simalpha-placement-type 'gridlike]
  [simalpha-failure-model  'none]
  [simalpha-dbg-on #f]
  ;[simalpha-sense-function sense-noisy-rising]
  ;[simalpha-sense-function sense-random-1to100]
  ;[sim-timeout 2000000]
  ;[sim-timeout 86400000] ;; A full day. 86 Million milli's
  [sim-timeout 3600000] ;; An hour.
  )

(define (head #(a b)) a)
(define (tail #(a b)) b)

(define threshold 3)

;(define foo (tail (tuple 99 999)))

;; Main

(define (read n)  
  (tuple (nodeid n) (sense 'clock n) (sense 'temp n)))

(define (filt #(_ __ temp)) (> temp threshold))

(rfilter filt (rmap read world))

;; Region 'a = Area (Node, 'a)
;; world = Region ()
;; khood : Node -> Region ()
;; rmap : (a->b) -> Region a -> Region b
