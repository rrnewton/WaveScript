
;; This file may be loaded more than once.
;; MORE (and detailed) configuration options can be found in constants.ss

;; The IFDEBUG/DEBUGMODE toggles are like a #define that toggles debug code for the whole compiler.
;; This is not a very appropriate place for this definition, but it's the most convenient
;; so that it can be seen from everywhere.
;; <br><br>
;; Uncomment one line for debug mode, the other to deactivate it.
;(define-syntax IFDEBUG (syntax-rules () [(_ debon deboff) debon]))  ;; ON
(define-syntax IFDEBUG (syntax-rules () [(_ debon deboff) deboff])) ;; OFF


;; [2006.08.28] This is the "#define" for building WAVESCOPE related code.
;; When turned off, the system should build Regiment without WaveScope extensions.
(define-syntax IFWAVESCOPE (syntax-rules () [(_ on off) on] [(_ on) on]))  ;; ON
;(define-syntax IFWAVESCOPE (syntax-rules () [(_ on off) off] [(_ on) (void)])) ;; OFF

;(define-syntax WAVESCOPE (syntax-rules () [(_ expr ...) (IFWAVESCOPE (list expr ...) ())]))


;; [2006.09.11] This configures the scheme compiler when loading regiment.
;;   0 -- mode for debugging 
;;   2 -- good performance but still safe
;;   3 -- unsafe optimizations, used for long running simulations.
(define REGOPTLVL 3)
;; Note that this is separate from IFDEBUG above.
