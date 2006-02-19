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
  [sim-timeout 500000]
  )

(define (head #(a b)) a)
(define (tail #(a b)) b)

(define threshold 5)

;(define foo (tail (tuple 99 999)))

;; Main

(rfilter
 (lambda (tup) (> (tail tup) threshold))
 (rmap (lambda (n) (tuple (nodeid n) (sense 'temp n)))
       world))

;; Region 'a = Area (Node, 'a)
;; world = Region ()
;; khood : Node -> Region ()
;; rmap : (a->b) -> Region a -> Region b
