
;;;; .title FFTW wrapper for Chez Scheme
;;;; .author Ryan Newton

;;;; [2006.08.03] This method seems to pass the obvious memory leak tests that I can think of.

;(chez:module fftw (make-dft-plan dft-1d compare-ffts)

;(load-shared-object (format "~a/build/~a/fftw.o" (getenv "REGIMENTD") (machine-type)))


(load-shared-object "libgslcblas.so")
;(load-shared-object "libgsl.so")
;(load-shared-object "libm.so")

(load-shared-object "libblas.so.3")

(load-shared-object "libcblas.so.3")

(load-shared-object "liblapack.so.3")

; gcc -Wl --as-needed  --enable-new-ldtags -lgsl -lgslcblas -lm -shared -o foo.so
;; Amazing, that gcc command actually worked.
(load-shared-object "./foo.so")

(define gsl_matrix_calloc (foreign-procedure "gsl_matrix_calloc" (iptr iptr) uptr))
(define gsl_matrix_alloc (foreign-procedure "gsl_matrix_alloc" (iptr iptr) uptr))
(define gsl_matrix_get   (foreign-procedure "gsl_matrix_get" (uptr iptr iptr) double-float))
(define gsl_matrix_set   (foreign-procedure "gsl_matrix_set" (uptr iptr iptr double-float) void))

(define mat (gsl_matrix_calloc 10 10))
(printf "Get: ~s\n" (gsl_matrix_get mat 1 1))
(printf "Set: ~s\n" (gsl_matrix_set mat 1 1 3.4))
(printf "Get: ~s\n" (gsl_matrix_get mat 1 1))

;GSLMatrix(ComplexSingle)
;GSLMatrix(Complex)

;(load-shared-object (format "~a/fftw.o" (machine-type)))

;(define test (foreign-procedure "test" (fixnum) fixnum))
;(define test2 (foreign-procedure "test2" (scheme-object) scheme-object))
;(define query (foreign-procedure "query_type" (scheme-object) void))
;(define print_exact (foreign-procedure "print_exact" (scheme-object) void))
;(define print_inexact (foreign-procedure "print_inexact" (scheme-object) void))
;(print_exact 1+1i)
;(print_inexact 15+64i)

; (define (explode-complex-vector v)
;   (let* ([out (make-vector (fx* 2 (vector-length v)))])
;     (do ([i 0 (fx+ 1 i)])
; 	((= i (vector-length v)) out)
;       (let ([c (vector-ref v i)])
; 	(if (not (cflonum? c)) (set! c (exact->inexact c)))
; 	(vector-set! out (fx* 2 i)         (cfl-real-part c))
; 	(vector-set! out (fx+ 1 (fx* 2 i)) (cfl-imag-part c))))))
; (define (implode-complex-vector v)
;   (let* ([out (make-vector (fx/ (vector-length v) 2))])
;     (do ([i 0 (fx+ 1 i)])
; 	((= i (vector-length out)) out)
;       (vector-set! out i (fl-make-rectangular 
; 			  (vector-ref v (fx* 2 i))
; 			  (vector-ref v (fx+ 1 (fx* 2 i))))))))


; ;(define set-dft-plan   (foreign-procedure "set_fftw_plan_dft_1d" (fixnum) void))
; ;(define clear-dft-plan (foreign-procedure "clear_fftw_plan_dft_1d" () void))

; ;; Based on the example from the chez scheme users guide:
; (define fftw:make-dft-plan 
;   (let ([malloc-guardian (make-guardian)]
; 	[do-malloc (foreign-procedure "make_fftw_plan_dft_1d" (integer-32) uptr)]
; 	[do-free (foreign-procedure "free_fftw_plan_dft_1d" (uptr) void)])
;     (define foobar (lambda ()
;        ;; first, invoke the collector
;        (collect)
;        ;; then free any storage that has been dropped
;        (let f ()
; 	 (let ((x (malloc-guardian)))
; 	   (when x
; 	     ;(printf "Killing fftw plan! ~a\n" x)(flush-output-port)
; 	     (do-free (unbox x))
; 	     (f))))))
;     (collect-request-handler
;      foobar)
;     (lambda (size)
;       ;; allocate and register the new storage
;       (let ([x (box (do-malloc size))]) ;; A boxed int.
;         (malloc-guardian x)
;         x))))

; (define fftw:dft-1d
;   (let ([execplan (foreign-procedure "s_fftw_execute" (scheme-object uptr) scheme-object)])
;     (lambda (v planbox)
;       (let ([vec (explode-complex-vector v)])
; 	(execplan vec (unbox planbox))
; 	(implode-complex-vector vec))
;       )))

; ;; Compares fftw against list-based native scheme version:
; (define (compare-ffts)
;   (define iota
;     (case-lambda
;       [(n) (iota 0 n)]
;       [(i n) (if (= n 0) '()
; 		 (cons i (iota (+ i 1) (- n 1))))]))
;   (printf "\n======================\n" )
;   (printf "Test #1: One big array\n" )
;   (let* ([vec (list->vector (map exact->inexact (iota 262144)))]
; 	 [plan (make-dft-plan (vector-length vec))])
;     (printf "Made plan: ~a\n" plan)
;     (let* ([a (time (dft-list vec))]
; 	   [b (time (dft-1d vec plan))])      
;       (printf "\nBiggest Elementwise Diff between results: ~a\n\n" 
; 	      (apply max 
; 		     (map cfl-magnitude-squared 
; 		       (map - (vector->list a) (vector->list b)))))))
;   (printf "\n==============================\n" )
;   (printf "Test #2: Many size 1024 arrays\n" )
;   (let* ([vec (list->vector (map exact->inexact (iota 1024)))]
; 	 [plan (make-dft-plan (vector-length vec))])
;     (printf "Made plan: ~a\n" plan)
;     (time (rep 1000 (dft-list vec)))
;     (time (rep 1000 (dft-1d vec plan)))))

; ;) ;; End module

; ;(compare-ffts)

; ;(import (add-prefix fftw: fftw))
; ;(import (add-prefix fftw fftw:))

