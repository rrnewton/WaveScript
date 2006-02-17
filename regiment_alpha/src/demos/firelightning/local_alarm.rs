



(parameters 
  [dummy-param (install-firelightning)]
  ;[simalpha-realtime-mode #t]
  ;[simalpha-channel-model 'lossless]
  ;[simalpha-placement-type 'gridlike]
  [simalpha-failure-model  'none]
  [simalpha-dbg-on #f]
  ;[simalpha-sense-function sense-noisy-rising]
  ;[simalpha-sense-function sense-random-1to100]
  [sim-timeout 500000]
  )

(define _threshold 30)

;; This is our data space, it consists of (nodeid, temp) pairs
(define data
  (rmap (lambda (n) (tuple n (sense 'temp n) (sense 'clock n)))
	world))

(define (node x)  (tupref 0 3 x))
(define (gettemp x)  (tupref 1 3 x))
(define (clock x) (tupref 2 3 x))

;(define twohop (lambda (n) (khood (node->anchor n) 2)))

;; Next select the nodes that just might possibly be too hot.
;; (But it could also be noise.)
(define maybe_hits
  (light-up
   (rfilter (lambda (tup) (> (gettemp tup) 7))
	    data)))

(define flat_locales
  (rrflatten
   (rmap (lambda (t) (khood (node->anchor (tupref 0 3 t)) 1))
	 maybe_hits)))

(define locales (rrcluster flat_locales))

(define selected_readings (rmap (lambda (n) (sense 'temp n)) flat_locales))

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
	     ;[result (smap div sums)]
	     )
      ;result
	     sums
      )))

;(define alarm
;  (rwhen-any (lambda (tup) (> (tupref 1 2 tup) _threshold)) temps))

;; Main query:
;locales
;flat_locales
;(light-up locale) ;maybe_hits)
;(rmap light-up locale)
(rfold + 0 selected_readings)
;(avg selected_readings)

