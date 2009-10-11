

;; [2009.10.10] This file establishes the connection between the
;; Scheme process that runs the compiler and performs high-level
;; management of the runitme, and the control module.

;; Presently, everything runs in the same process.  In the future, the
;; runtime, compiler, and control module could all be in separate
;; processes if desired.

(printf " --- LOADING UP COMPILER.\n")


(let* ([startd (current-directory)]
       [src-dir (string-append (getenv "REGIMENTD") "/src")]
       [src (string-append src-dir "/regiment.ss")])
  (parameterize ([current-directory src-dir]
		 [command-line `(,src ,startd "nothing")])
    (printf "INDIR ~s\n" (current-directory))
    (load src)))


;;==============================================================================
(printf " --- BRIDGING CONTROL MODULE AND RUNTIME\n")

(define (WSQ_BeginTransaction id)
  (printf ".....In WSQ_BeginTransaction.....\n")
  )

(define (WSQ_EndTransaction)
  (printf ".....In WSQ_EndTransaction....\n")
  )

;;==============================================================================

(define WSQ_BeginTransaction-entry
  (let ([x (foreign-callable WSQ_BeginTransaction (int) void)])
    (lock-object x) (foreign-callable-entry-point x)))

(define WSQ_EndTransaction-entry
  (let ([x (foreign-callable WSQ_EndTransaction () void)])
    (lock-object x) (foreign-callable-entry-point x)))






(define (foo n)
  (printf " >> THIS IS MY SCHEME FUNCTION ~a\n" n))
(define foo-entry 
  (let ([x (foreign-callable foo (int) void)])
    (lock-object x) (foreign-callable-entry-point x)))
(printf "Set foo-entry ~a\n" foo-entry)
#;
(let ([x (foreign-callable foo (int) void)])
  (lock-object x)
  (define-top-level-value 'foo-entry
    (foreign-callable-entry-point x))
  (printf "Set foo-entry ~a\n" (top-level-value 'foo-entry)))
(define test 3939)
;(define-top-level-value 'test 9383)

;;==============================================================================

(exit 0)
