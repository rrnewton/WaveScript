







(parameters 
  [dummy-param (install-firelightning)]
  ;[simalpha-realtime-mode #t]
  [simalpha-dbg-on #f]

  [default-slow-pulse (* 5 60 1000)] ;; 5 min
  [default-fast-pulse (*    3 1000)] ;; 3 sec

;  [simalpha-channel-model 'lossless]

  ;[sim-timeout 60000] ;; One minute
  ;[sim-timeout 600000] ;; Ten minutes
  [sim-timeout 3600000] ;; An hour.
  ;[sim-timeout 86400000] ;; A full day. 86 Million milli's

  ;; Default value for the threshold. (Over-ridden by analysis script)
  ;[varied-param 3] 
  )

`(define _threshold ,(varied-param)) ; 20)
(define (temp n) (sense 'temp n))
(define (abovethresh n) (> (temp n) _threshold))

(define (count-nbrs r) 
  (rfold + 0 
	 (rmap (lambda (n) 
		 (if (> (temp n) _threshold) 1 0))
	       r)))
#|
do n <- r;
   t = sense Temp n;
   if t>thresh 
      then return 1 
      else return 0;
   rfold + 0; -- switch monads?
|#


;; All nodes over a local temperature threshold.

(define heat-events (rfilter abovethresh world))


(define (local-results n)
  (letrec ((hood (khood (node->anchor n) 1))) ;; 1.5
    (count-nbrs hood)))

;; Main query:

(rfilter (lambda (c) (> c 1))
	 (rmap local-results heat-events))

;(count world)
