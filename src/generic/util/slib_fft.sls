#!r6rs

;; This is lifted from SLIB: 
(library (ws util slib_fft)     
  (export slib:fft slib:fft-1)
  (import (rnrs) 
	  (except (rnrs r5rs (6)) delay force) 
	  (ws compat compat)
	  (ws util helpers) (ws globals))

;; What is the cleanest way to turn on unsafe optimizations just for this module?
;(IFCHEZ (eval-when (compile eval load) (define-top-level-value 'origoptlvl (optimize-level)) (optimize-level 3)) (begin))

(define (fft:shuffle&scale new ara n scale)
  (define lgn (bitwise-length (+ -1 n)))
  (if (not (eqv? n (expt 2 lgn)))
      (error 'fft "array length not power of 2: ~s" n))
  (do ((k 0 (fx+ 1 k)))
      ((fx= k n) new)
    (vector-set! new (bit-reverse lgn k) (* (vector-ref ara k) scale))))

(define (dft! ara n dir)
  (define lgn (bitwise-length (+ -1 n)))
  (define pi2i (* (make-rectangular 0 8)
		  (atan 1)))
  (do ((s 1 (+ 1 s)))
      ((> s lgn) ara)
    (let* ((m (expt 2 s))
	   (w_m (exp (* dir (/ pi2i m))))
	   (m/2-1 (+ (quotient m 2) -1)))
      (do ((j 0 (+ 1 j))
	   (w 1 (* w w_m)))
	  ((> j m/2-1))
	(do ((k j (+ m k)))
	    ((>= k n))
	  (let* ((k+m/2 (+ k m/2-1 1))
		 (t (* w (vector-ref ara k+m/2)))
		 (u (vector-ref ara k)))
	    (vector-set! ara k     (+ u t) )
	    (vector-set! ara k+m/2 (- u t) )))))))

;;@args array
;;@var{array} is an array of @code{(expt 2 n)} numbers.  @code{fft}
;;returns an array of complex numbers comprising the
;;@dfn{Discrete Fourier Transform} of @var{array}.
(define (slib:fft ara)
  (define n (vector-length ara))
  (define new (make-vector n (vector-ref ara 0)))
  (dft! (fft:shuffle&scale new ara n 1) n 1))

;;@args array
;;@code{fft-1} returns an array of complex numbers comprising the
;;inverse Discrete Fourier Transform of @var{array}.
(define (slib:fft-1 ara)
  (define n (vector-length ara))
  (define new (make-vector n (vector-ref ara 0)))
  (dft! (fft:shuffle&scale new ara n (/ n)) n -1))

(define (bit-reverse k n)
  (do ((m (if (negative? n) (bitwise-not n) n) (bitwise-arithmetic-shift m -1))
       (k (+ -1 k) (+ -1 k))
       (rvs 0 (bitwise-ior (bitwise-arithmetic-shift rvs 1) (bitwise-and 1 m))))
      ((negative? k) (if (negative? n) (bitwise-not rvs) rvs))))

) ;; End module.

