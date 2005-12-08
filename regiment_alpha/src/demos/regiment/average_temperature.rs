
;; This performs the ever-popular "average temperature" query.
;; It uses default sampling rates.

(parameters [sim-timeout 3000]
	    [simalpha-channel-model 'lossless]
	    [simalpha-failure-model  'none]
	    [simalpha-zeropad-args #t];'warning] ;; Sync-sensing necessitates continuations.
	    [simalpha-sense-function sense-sine-wave])

;; Main program:

(letrec ([readings (rmap (lambda (n) (tuple (sense n) 1))
			 world)]
	 [foo 3]
;	 [aggr (lambda (x y)
;		 (tuple (+ (tupref 0 x)
;			   (tupref 0 y))
;			(+ (tupref 1 x)
;			   (tupref 1 y))))]
;	 [div (lambda (v)
;		(if (= (tupref 1 v) 0) 0 ;; Just return zero if there are no samples to avg.
;		    (/ (tupref 0 v) (tupref 1 v))))]
	 ;[sums (rfold aggr (tuple 0 0) readings)]
	 ;[result (smap div sums)]
	 )

  (tuple readings )
  ;result
  )

