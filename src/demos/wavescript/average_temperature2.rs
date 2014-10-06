;; [2005.11.29] Causes an fx+ error right now.

;; This performs the ever-popular "average temperature" query.
;; It uses default sampling rates.

(parameters [sim-timeout 3000]
	    [simalpha-channel-model 'lossless]
	    [simalpha-failure-model  'none]
	    [simalpha-zeropad-args #t];'warning] ;; Sync-sensing necessitates continuations.
	    [simalpha-sense-function-constructor sense-sine-wave])

;; Main program:

(define avg 
  (lambda (r)
    (letrec ([readings (rmap (lambda (x) (tuple x 1)) r)]
	     [aggr (lambda (x y)
		     (tuple (+ (tupref 0 2 x) (tupref 0 2 y))
			    (+ (tupref 1 2 x) (tupref 1 2 y))))]
	     [div (lambda (v) 
		(if (= (tupref 1 2 v) 0) 0 ;; Just return zero if there are no samples to avg.
		    (/ (tupref 0 2 v) (tupref 1 2 v))))]
	     [sums (rfold aggr (tuple 0 0) readings)]
	     [result (smap div sums)])
      result)))

(define readings (rmap sense world))

(avg readings)
