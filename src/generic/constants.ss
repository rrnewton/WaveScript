;; [2004.07.02]
;; Hmm, I considered making this a while ago.  But I never know
;; whether to keep constants module-local or lift them out like this.
;; I'm just going to settle for an ad-hoc strategy, anything that
;; needs to (or might need to) be used in more than one module will
;; get lifted up here.


;; Used primarily by pass12_annotate-heartbeats:
;;===================================================
;; The slow-pulse is used for region formation.
(define slow-pulse 1.0)
;; The fast-pulse is used for folding.
(define fast-pulse 10.0)

;; Used primarily by Simulator_nought.ss:
;;===================================================
;; These are the virtual coordinate bounds of the world.
(define world-xbound 60)
(define world-ybound 60)
(define radius 20) ;; And the comm radius.
(define numprocs 20) ;; And the total # processors.

(define SPECIAL_RETURN_TOKEN 'RETTT)

;; In milliseconds
(define return-window-size 500)