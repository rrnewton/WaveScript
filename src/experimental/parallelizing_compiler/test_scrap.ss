

;  (init-par (string->number (or (getenv "NUMTHREADS") "2")))

#;
  (let ()
    (define (tree n)
      (if (fxzero? n) 1
          (pcall fx+ (tree (fx- n 1)) (tree (fx- n 1)))))
    (printf "Run using parallel add-tree via pcall mechanism:\n")
    (printf "\n~s\n\n" (time (tree test-depth)))
    (par-status))

#;
  (let ()
    (define (tree n)
      (if (fxzero? n) 1
          (apply fx+ (par (tree (fx- n 1)) (tree (fx- n 1))))))
    (printf "Run using parallel add-tree w/ LIST intermediate values:\n")
    (printf "\n~s\n\n" (time (tree test-depth)))
    (par-status))
#;
  (let ()
    (define (tree n)
      (if (fxzero? n) 1
          (call-with-values (lambda () (parmv (tree (fx- n 1)) (tree (fx- n 1)))) fx+)))
    (printf "Run using parallel add-tree w/ MULTIPLE values:\n")
    (printf "\n~s\n\n" (time (tree test-depth)))
    (par-status))

#;
  (let ()
    (define (tree n)
      (if (fxzero? n) 1
          (fx+ (tree (fx- n 1)) (tree (fx- n 1)))))
    (printf "Run sequential (non-par) version:\n")
    (printf "\n~s\n\n" (time (tree test-depth)))
    (par-status))
 
#;
(define _
  (begin 

  (init-par (string->number (or (getenv "NUMTHREADS") "2")))
  (printf "Run using parallel add-tree via pcall mechanism:\n")
  (let loop ((n 1000000)) (or (zero? n) (loop (sub1 n))))
  (let ()
    (define (tree n)
      (if (zero? n) 1
          (pcall + (tree (sub1 n)) (tree (sub1 n)))))
    (printf "\n~s\n\n" (time (tree 23)))
    (par-status))

    ))

