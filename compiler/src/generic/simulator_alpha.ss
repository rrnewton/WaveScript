
;; simulator_alpha.ss
;;  -Ryan Newton [2005.02.25]
;===============================================================================

;; This will be a second attempt simulator.
;; However, it will support only core tml (no gradients).

;; It will have a single thread of control and a queue of simulator
;; events sorted by virtual clock times.

;; Later, it may serve as a place to test scheduling algorithms so
;; that we may actually implement the atomic action model.



