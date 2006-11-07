

 

;; This can provide us a base-line.  It uses *no* fancy Regiment
;; features, it merely sends all the temperature values over a certain
;; threshold to the base station.

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
  ;[varied-param 20]
  )

;; The varied parameter can be changed from outside the program source before load-time.
`(define threshold ,(varied-param))
;(define threshold 20)

;; Main

(define (read n) (tuple (nodeid n) (sense 'clock n) (sense 'temp n)))

(define (filt #(_ _ temp)) (> temp threshold))

;(tuple test
(light-up (rfilter filt (rmap read world)))
