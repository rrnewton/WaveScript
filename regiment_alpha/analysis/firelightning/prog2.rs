







(parameters 
  [dummy-param (install-firelightning)]
  ;[simalpha-realtime-mode #t]
  [simalpha-dbg-on #f]

  [default-slow-pulse (* 5 60 1000)] ;; 5 min
  [default-fast-pulse (*    3 1000)] ;; 3 sec

  ;[sim-timeout 60000] ;; One minute
  ;[sim-timeout 600000] ;; Ten minutes
  [sim-timeout 3600000] ;; An hour.
  ;[sim-timeout 86400000] ;; A full day. 86 Million milli's

  ;; Default value for the threshold. (Over-ridden by analysis script)
  ;[varied-param 3] 
  )

(define _tempthreshold 20)
(define (temp n) (sense 'temp n))
(define (abovethresh n) (> (temp n) _tempthreshold))
(define (count r) (rfold (lambda (_ acc) (+ 1 acc)) 0 r))

;; All nodes over a local temperature threshold.

(define heat-events (rfilter abovethresh world))

(define (local-results n)
  (let ((hood (khood 1.5 (node->anchor n))))
    (count hood)))

;; Main query:

(rfilter (lambda (c) (> c 1))
	 (rmap local-results heat-events))

