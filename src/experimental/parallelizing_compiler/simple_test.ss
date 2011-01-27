
;; Chez Scheme script.

(eval-when (compile eval load) 
;  (optimize-level 3)
  (optimize-level 2)
;  (collect-trip-bytes (* 20 1048576)) ;; collects 47 times in ~3 sec
  )

;(include "chez_threaded_utils.ss")
;(import threaded_utils)
(import (par5))
;(import (par6))

(print-gensym #f)
(print-graph #t)

;(define test-depth 5)
(define test-depth (string->number (or (getenv "TESTDEPTH") "33")))
;(define test-depth 27)
;(define test-depth 23)
;(define test-depth 25)
;(define test-depth 42)
;(define test-depth 40) ;; fib of 40... is that 165580141 yes?


#;
(define (test)
  (let ()
    (define (tree n)
      (if (fxzero? n) 1
          (pcall fx+ (tree (fx- n 1)) (tree (fx- n 1)))))
    (printf "Run using parallel add-tree via pcall mechanism:\n")
    (printf "\n~s\n\n" (time (tree test-depth)))
    ;(par-status)
    ))

;; Fib instead of bin tree:
;;
;; Note (fib 42): Cilk could do 4.2 seconds 4 threads on wasp with a serial time of 2.37 seconds.
;; My HCilk on haskell could do 6.6 seconds on wasp.
;;    Right now this is taking around 31.8 seconds for me at opt-level 3... 48 seconds one thread.
;;    5.5 seconds serial...
;; I can do prints on the steals and its not doing many steals at all... as one would expect.
;; the problem is that the workers aren't using 100% cpu... they're messing with mutexes constantly.
(define (test)
  (let ()
    (define (fib n)
      (if (fx< n 2) 1
          (pcall fx+ (fib (fx- n 1)) (fib (fx- n 2)))
	  ;(pcall fx+ (fib (fx- n 2)) (fib (fx- n 1)))
	  ))
    (printf "Run using parallel add-tree via pcall mechanism:\n")
    (printf "\n~s\n\n" (time (fib test-depth)))
    ;(par-status)
    ))

(printf "Starting simple test\n")
;(collect-notify #t)

(define (go n )
  (printf "Next test...\n")
  ;; It's somewhat impossible to know when previous threads have ACTUALLY shut down...
  ;(let loop ((i 1000000)) (unless (zero? i) (loop (sub1 i)))) (collect 4)
  (init-par (string->number (or (getenv "NUMTHREADS") n)))
  (test)
  (printf "Try to shutdown...\n")
  (shutdown-par)
  )

;(go "8")
;(go "4")
(go "2")
;(go "1")

;; After threads shutdown I should be able to collect!
;(collect 4)
;(printf "Collected successfully...\n")

(define (serial n)
#;
  (if (fxzero? n) 1
      (fx+ (serial (fx- n 1)) (serial (fx- n 1))))
  (if (fx< n 2) 1  ;; Fib instead.
      (fx+ (serial (fx- n 1)) (serial (fx- n 2))))
      )
(printf "Serially: \n")
(time (serial test-depth))

(exit)
