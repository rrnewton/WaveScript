;; [2004.07.02]
;; Hmm, I considered making this a while ago.  But I never know
;; whether to keep constants module-local or lift them out like this.
;; I'm just going to settle for an ad-hoc strategy, anything that
;; needs to (or might need to) be used in more than one module will
;; get lifted up here.

;; [2005.02.24]
;; Now I'm keeping some parameters (which are actually not constant) in here as well.


;; Used primarily by pass12_add-heartbeats:
;;===================================================
;; The slow-pulse is used for region formation.
(define slow-pulse 1000) ;; MILLISECONDS
;; The fast-pulse is used for folding.
(define fast-pulse 100)  ;; MILLISECONDS

;; Used primarily by pass14_add-places:
;; (and by pass15_add-routing
;;===================================================
(define unknown-place '?) ;'X?)
(define noplace '_)

;; Used primarily by Simulator_nought.ss:
;;===================================================
;; These are the virtual coordinate bounds of the world.
(define world-xbound 60)
(define world-ybound 60)
(define radius 20) ;; And the comm radius.
(define numsimnodes 30) ;; And the total # processors.

(define SPECIAL_RETURN_TOKEN 'RETTT)

;; Id number for the base-station / Source-of-Control (SOC)
(define BASE_ID 0)

;; In milliseconds, this is effectively the epoch size.  
;; Nodes aggregate and resend at this frequency.
(define return-window-size 500)

;; Used primarily by Simulator_alpha.ss:
;;===================================================

(define token-store-size 1000) ;; Store up to 1000 token-objs per node.


;;======================================================================

(define regiment-parameters (make-parameter '()))
(define-syntax define-regiment-parameter
  (syntax-rules () 
    [(_ name args ...)
     (define name 
       (begin (regiment-parameters (cons (quote name) (regiment-parameters)))
	      (make-parameter args ...)))]))

;; This parameter determines whether the compiler should output extra debugging info.
(define-regiment-parameter regiment-verbose #f)

;; This one optionally provides a target for logging simulation data.
(define-regiment-parameter simulation-logger #f)

(define-regiment-parameter regiment-consec-ids #t)
