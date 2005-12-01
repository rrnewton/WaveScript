
;;;; This demo implements the Anchor Free Localization algorithm.
;;;; UNFINISHED.


; Interesting.  Getting multiple return paths for a simple
; anchor-maximizing when lossy channels are used.

(parameters 
  [simalpha-realtime-mode #t]

  [simalpha-channel-model 'lossless]
  [simalpha-placement-type 'gridlike]
  [simalpha-failure-model  'none]
  ;[simalpha-sense-function sense-noisy-rising]
  ;[simalpha-sense-function sense-random-1to100]
  [simalpha-sense-function sense-dist-from-origin]
  [sim-timeout 2000])


;; Main program:

(anchor-maximizing sense 0)

(define 