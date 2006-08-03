(define (dft2 x)
  (define (w-powers n)
    (let* ([pi (* (acos 0.0) 2)]
           [delta (/ (* -2.0i pi) n)]
           [half (quotient n 2)]
           [out (make-vector half)])      
      (do ([n 0 (+ n 1)] [x 0.0 (+ x delta)])   ((= n half) out)
        (vector-set! out n (exp x)))))
   (define (evens v)
    (let* ([half (quotient (vector-length v) 2)]
           [out (make-vector half)])
      (do ((i 0 (+ i 1)))  ((= i half) out)
        (vector-set! out i (vector-ref v (* i 2))))))
   (define (interlace x y)
     (let* ([double (+ (vector-length x) (vector-length y))]
            [out (make-vector double)])
       (do ((i 0 (+ i 1)))  ((= i (vector-length x)) out)
         (vector-set! out (* 2 i)       (vector-ref x i))
         (vector-set! out (+ 1 (* 2 i)) (vector-ref y i))
         )))
   (define (subvector v ind l)
     (let ([out (make-vector l)])
       (do ((i 0 (+ i 1)))
         ((= i l) out)
         (vector-set! out i (vector-ref v (+ ind i))))))
   (define (split v)
     (let ([half (quotient (vector-length v) 2)])
       (values (subvector v 0 half) 
               (subvector v half half))))
   (define (vmap2 f v1 v2)
     (let ([out (make-vector (vector-length v1))])
       (do ((i 0 (+ 1 i))) ((= i (vector-length v1)) out)
         (vector-set! out i (f (vector-ref v1 i) (vector-ref v2 i))))))
   (define (vmap3 f v1 v2 v3)
     (let ([out (make-vector (vector-length v1))])
       (do ((i 0 (+ 1 i))) ((= i (vector-length v1)) out)
         (vector-set! out i (f (vector-ref v1 i)
                               (vector-ref v2 i)
                               (vector-ref v3 i))))))
  (define (butterfly x w)
    (let-values ([(front back) (split x)])
      (values (vmap2 + front back)
              (vmap3 (lambda (f b w) (* (- f b) w))
                     front back w))))
  (define (rfft x w)
    (if (= (vector-length x) 2)
        (let ((x0 (vector-ref x 0)) (x1 (vector-ref x 1)))
          (vector (+ x0 x1) (- x0 x1)))
        (let-values ([(front back) (butterfly x w)])
          (let ([w (evens w)])
            (interlace (rfft front w) (rfft back w))))))
  (rfft x (w-powers (vector-length x))))





;; [2006.08.03] This method seems to pass the obvious memory leak tests that I can think of.

(load-shared-object (format "i3le/test.o"))

(define test (foreign-procedure "test" (fixnum) fixnum))
(define test2 (foreign-procedure "test2" (scheme-object) scheme-object))
(define query (foreign-procedure "query_type" (scheme-object) void))
(define print_exact (foreign-procedure "print_exact" (scheme-object) void))
(define print_inexact (foreign-procedure "print_inexact" (scheme-object) void))
;(print_exact 1+1i)
;(print_inexact 15+64i)


(define (explode-complex-vector v)
  (let* ([out (make-vector (fx* 2 (vector-length v)))])
    (do ([i 0 (fx+ 1 i)])
	((= i (vector-length v)) out)
      (let ([c (vector-ref v i)])
	(vector-set! out (fx* 2 i)         (cfl-real-part c))
	(vector-set! out (fx+ 1 (fx* 2 i)) (cfl-imag-part c))))))

(define (implode-complex-vector v)
  (let* ([out (make-vector (fx/ (vector-length v) 2))])
    (do ([i 0 (fx+ 1 i)])
	((= i (vector-length out)) out)
      (vector-set! out i (fl-make-rectangular 
			  (vector-ref v (fx* 2 i))
			  (vector-ref v (fx+ 1 (fx* 2 i))))))))


;(define set-dft-plan   (foreign-procedure "set_fftw_plan_dft_1d" (fixnum) void))
;(define clear-dft-plan (foreign-procedure "clear_fftw_plan_dft_1d" () void))


;; Based on the example from the chez scheme users guide:
(define make-dft-plan 
  (let ([malloc-guardian (make-guardian)]
	[do-malloc (foreign-procedure "make_fftw_plan_dft_1d" (integer-32) integer-32)]
	[do-free (foreign-procedure "free_fftw_plan_dft_1d" (integer-32) void)])
    (define foobar (lambda ()
       ;; first, invoke the collector
       (collect)
       ;; then free any storage that has been dropped
       (let f ()
	 (let ((x (malloc-guardian)))
	   (when x
	     ;(printf "Killing fftw plan! ~a\n" x)(flush-output-port)
	     (do-free (unbox x))
	     (f))))))
    (collect-request-handler
     foobar)
    (lambda (size)
      ;; allocate and register the new storage
      (let ([x (box (do-malloc size))]) ;; A boxed int.
        (malloc-guardian x)
        x))))

(define dft-1d
  (let ([execplan (foreign-procedure "s_fftw_execute" (scheme-object integer-32) scheme-object)])
    (lambda (v planbox)
      (let ([vec (explode-complex-vector v)])
	(execplan vec (unbox planbox))
	(implode-complex-vector vec))
      )))


(define (compare-ffts)
  (printf "\n======================\n" )
  (printf "Test #1: One big array\n" )
  (let* ([vec (list->vector (map exact->inexact (iota 262144)))]
	 [plan (make-dft-plan (vector-length vec))])
    (printf "Made plan: ~a\n" plan)
    (let* ([a (time (dft2 vec))]
	   [b (time (dft-1d vec plan))])      
      (printf "\nBiggest Elementwise Diff between results: ~a\n\n" 
	      (apply max 
		     (map cfl-magnitude-squared 
		       (map - (vector->list a) (vector->list b)))))))
  (printf "\n==============================\n" )
  (printf "Test #2: Many size 1024 arrays\n" )
  (let* ([vec (list->vector (map exact->inexact (iota 1024)))]
	 [plan (make-dft-plan (vector-length vec))])
    (printf "Made plan: ~a\n" plan)
    (time (rep 1000 (dft2 vec)))
    (time (rep 1000 (dft-1d vec plan)))))

(compare-ffts)
