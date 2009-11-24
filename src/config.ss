
;; This file contains configuration options that users of the
;; WaveScript compiler might want to change.

;; This file may be loaded more than once.
;; MORE (and detailed) configuration options can be found in globals.ss

;; The IFDEBUG/DEBUGMODE toggles are like a #define that toggles debug code for the whole compiler.
;; This is not a very appropriate place for this definition, but it's the most convenient
;; so that it can be seen from everywhere.
;; <br><br>
(define-syntax IFDEBUG 
  (lambda (x)
    ;===============================;
    (define DEFAULT_IS_DEBUG_MODE #f) ;; <-- CHANGE DEFAULT HERE
    ;===============================;
    (define (on)  (syntax-case x () [(_ debon deboff) #'debon]))
    (define (off) (syntax-case x () [(_ debon deboff) #'deboff]))
    (cond
     [(getenv "REGDEBUGMODE") => 
      (lambda (m) 
	(if (or (equal? m "0") (equal? m "OFF") (equal? m ""))
	     (off) (on)))]
     [else (if DEFAULT_IS_DEBUG_MODE (on) (off))])))

;; [2006.08.28] This is the "#define" for building WAVESCOPE related code.
;; When turned off, the system should build Regiment without WaveScope extensions.
(define-syntax IFWAVESCOPE
  (let ()
    ;===============================;
    (define DEFAULT_WAVESCOPE-REGIMENT_MODE "WAVESCRIPT") ;; <-- CHANGE DEFAULT HERE
    ;===============================;
    (define (ws x)  (syntax-case x () [(_ ws reg) #'ws]   [(_ ws) #'ws]))
    (define (reg x) (syntax-case x () [(_ ws reg) #'reg]  [(_ ws) #'(begin)]))
    (define (set m)
      ;(printf "  ENVVAR: ~s\n" m)
      (cond 
       [(equal? m "WAVESCRIPT") ws]
       [(equal? m "WAVESCOPE")  ws]
       [(equal? m "WS")         ws]
       [(equal? m "REGIMENT")  reg]
       [(equal? m "REG")       reg]
       [(equal? m "BOTH")
	(lambda (x) (syntax-case x () [(_ ws reg) #'(begin ws reg)] [(_ ws) #'ws]))]
       [else (error 'IFWAVESCOPE "unknown value for environment var REGIMENT_OR_WAVESCRIPT: ~s" m)]
       ))
    (cond
     [(getenv "REGIMENT_OR_WAVESCRIPT") => (lambda (m) (set m))]
     [else (set DEFAULT_WAVESCOPE-REGIMENT_MODE)]
     ;[else (error 'IFWAVESCOPE "environment var not set")]
     )))


;; [2006.09.11] This configures the scheme compiler when loading regiment.
;; NOTE: this controls the optimization level for the COMPILER, and
;; also for the token machine simulator or the Scheme WS backend.
;; But it does *not* control the number of optimizations applied by
;; the compiler to the generated code.  
;;
;; Look at the parameter ws-optimizations-enabled for that.
;;
;;   0 -- mode for debugging 
;;   2 -- good performance but still safe
;;   3 -- unsafe optimizations, used for long running simulations.
(define (REGOPTLVL)
  (cond
   [(getenv "REGOPTLVL") => (lambda (str) (read (open-string-input-port str)))]
   [else 2]  ;; <-- CHANGE DEFAULT HERE
   ))
;; Note that this is separate from IFDEBUG above.


;; [2006.11.24] This is just a temporary thing so I can watch how fast things load.
(define VERBOSE-LOAD #f)

