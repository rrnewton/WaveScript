

(eval-when (compile eval load) 
  ;(optimize-level 3)
  (optimize-level 2)
  (collect-trip-bytes (* 20 1048576)) ;; collects 47 times in ~3 sec
  )

(include "chez_threaded_utils.ss")
(import threaded_utils)


;(define test-depth 27)
(define test-depth 23)

(define (test)
  (let ()
    (define (tree n)
      (if (fxzero? n) 1
          (pcall fx+ (tree (fx- n 1)) (tree (fx- n 1)))))
    (printf "Run using parallel add-tree via pcall mechanism:\n")
    (printf "\n~s\n\n" (time (tree test-depth)))
    ;(par-status)
    ))


(printf "Starting simple test\n")
;(collect-notify #t)

(define (go n )
  (collect 4)
  (init-par (string->number (or (getenv "NUMTHREADS") n)))
  (test)
  (shutdown-par)
  )

(go "8")
(go "4")
(go "2")
(go "1")

;; After threads shutdown I should be able to collect!
(collect 4)
;(printf "Collected successfully...\n")


(exit)
