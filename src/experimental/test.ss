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














(load-shared-object (format "i3le/test.o"))

(define test (foreign-procedure "test" (fixnum) fixnum))

(define test2 (foreign-procedure "test2" (scheme-object) scheme-object))

(define query (foreign-procedure "query_type" (scheme-object) void))

(define print_exact (foreign-procedure "print_exact" (scheme-object) void))
(define print_inexact (foreign-procedure "print_inexact" (scheme-object) void))

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

;(print_exact 1+1i)

;(print_inexact 15+64i)


;fftw_plan_dft_1d

(define set-dft-plan   (foreign-procedure "set_fftw_plan_dft_1d" (fixnum) void))
(define clear-dft-plan (foreign-procedure "clear_fftw_plan_dft_1d" () void))


(define make-dft-plan   (foreign-procedure "make_fftw_plan_dft_1d" (integer-32) integer-32))
(define free-dft-plan (foreign-procedure "free_fftw_plan_dft_1d" (integer-32) void))

(define dft-1d
  (let (;[initplan (foreign-procedure "s_fftw_plan_dft_1d" (fixnum) void)]
	[execplan (foreign-procedure "s_fftw_execute" (scheme-object) void)])
;    (lambda (N)
      (let ();[plan (initplan N)])
	(lambda (v)
	  (let ([vec (explode-complex-vector v)])
	    (execplan vec)
	    (implode-complex-vector vec))
	  ))))

;(define vec #(1. 2. 3. 4. 5. 6. 7. 8.))
;(define vec (list->vector (map exact->inexact (iota 1024))))
(define vec (list->vector (map exact->inexact (iota 262144))))

(define plan (make-dft-plan 1024))
(printf "Made plan: ~a\n" plan)
(free-dft-plan plan)
(printf "Killed plan!\n")


#|
(set-dft-plan 262144)

(define a (time (dft2 vec)))
(define b (time (dft-1d vec)))
(printf "\nBiggest Diff: ~a\n\n" 
	(apply max 
	       (map cfl-magnitude-squared 
		 (map - (vector->list a) (vector->list b)))))

(set-dft-plan 1024)

(define vec (list->vector (map exact->inexact (iota 1024))))
(define a (time (rep 1000 (dft2 vec))))
(define b (time (rep 1000 (dft-1d vec))))

;(printf "Result: ~a\n" (dft-1d vec))
|#