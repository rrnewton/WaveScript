

;;;; .title A simple consensus interface to discrete 1d complex FFTs.
;;;; .author Ryan Newton


;; This exposes a SIMPLE 1d discrete fourier transform.

;; It wraps either a native scheme version, or the linked fftw
;; library.  In the case of FFTW, it uses a simple memoization scheme
;; for building plans.  (I don't want to complicate the interface by
;; including plan-creation.)

(module fft mzscheme

  (require "../constants.ss"
           "scheme_fft.ss"
	   "slib_fft.ss"
	   "helpers.ss")
  (provide ;make-dft-plan 
           dft
	   inverse-dft)
  (chezimports slib_fft
	       scheme_fft)
  
  ;; Slib's inverse dft is the only one I've got right now in pure scheme:
  (define inverse-dft slib:fft-1)
  
  ;; Set to the basic scheme version:
  (define basic-dft 
    (lambda (x)
      (cond [(vector? x) 
	     (unless (integer? (/ (log (vector-length x)) (log 2)))
	       (error 'basic-dft "can't do fft on window of length that's not power of two: ~s" (vector-length x)))
	     (list->vector (dft-list (vector->list x)))]
	    [(list? x) (dft-list x)]
	    [else (error 'dft "didn't receive list or vector: ~a\n" x)])))
;  (define make-dft-plan (lambda (n) 'dummy-plan))

  ;; We do late-binding here.  Don't load the extension until we call dft.
  (define (dft vec)
    ;(set! dft basic-dft)
    (set! dft slib:fft)
    
    ;; If we're using chez and have FFTW available, use that instead:
    (IFCHEZ 
     (let ([fftw-file (format "~a/src/build/~a/fftw.so" (REGIMENTD) (machine-type))])
       ;(inspect fftw-file)
       ;(inspect (file-exists? fftw-file))

       (when (file-exists? fftw-file)
	 (printf "Loading FFTW extension.\n")
	 (load fftw-file)
	 ;;(import (add-prefix fftw fftw:))
	 (set! dft 
	       ;; Simple memoization scheme:
	       (let ([plans '()])
		 (lambda (v)
		   (let ([cell (assq (vector-length v) plans)])
		     (let ([plan 
			    (if cell (cdr cell)
				(let ([newplan (fftw:make-dft-plan (vector-length v))])
				  (printf "Adding FFTW plan for complex vector length ~a.\n" (vector-length v))
				  (set! plans (cons (cons (vector-length v) newplan) plans))
				  newplan))])
		       (fftw:dft-1d v plan))))))
					;(set! make-dft-plan fftw:make-dft-plan)
	 ))
     (void))
    ;; Now with the correct binding established, invoke dft:
    (dft vec))
  )
