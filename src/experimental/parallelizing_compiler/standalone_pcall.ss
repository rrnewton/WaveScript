;; SEPARATE STANDALONE VERSION FOR TESTING -- 
;; Note, this is duplicated code from threaded_utils.ss

;; Now doing a hack to get rid of thunk allocation.

;; ================================================================================
;;  Implementation of 'par' facility.
;;  Important entrypoints:
;;   (par e1 e2) compute expressions in parallel and return in a list.
;; ================================================================================

(eval-when (compile eval load) 
  (optimize-level 3)
  (collect-trip-bytes (* 20 1048576)) ;; collects 47 times in ~3 sec
  )

(module ()

(define-syntax ASSERT
  (lambda (x)
    (syntax-case x ()
      [(_ expr) #'(or expr (error 'ASSERT "failed: ~s" (IFCHEZ #'expr (format-syntax #'expr))))]
      ;; This form is (ASSERT integer? x) returning the value of x.
      [(_ fun val) #'(let ([v val])
		       (if (fun v) v			   
			   (error 'ASSERT "failed: ~s\n Value which did not satisfy above predicate: ~s" 
				  (IFCHEZ #'fun (format-syntax #'fun))
				  v)))]
      )))

  (define test-depth 25) ;; Make a tree with 2^test-depth nodes.

  (define vector-build
    (lambda (n f)
      (let ([v (make-vector n)])
        (do ([i 0 (fx+ i 1)])
            ((= i n) v)
          (vector-set! v i (f i))
          ))))
  
  ;;================================================================================


  ;; [2010.10.31] NIXING FOR NOW ... updating latest version and I don't want the duplicated code.




  ;;================================================================================

  (init-par (string->number (or (getenv "NUMTHREADS") "2")))

  (let ()
    (define (tree n)
      (if (fxzero? n) 1
          (pcall fx+ (tree (fx- n 1)) (tree (fx- n 1)))))
    (printf "Run using parallel add-tree via pcall mechanism:\n")
    (printf "\n~s\n\n" (time (tree test-depth)))
    (par-status))

) 

