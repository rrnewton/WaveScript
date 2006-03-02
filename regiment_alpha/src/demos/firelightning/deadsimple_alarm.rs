



;; This can provide us a base-line.  It uses *no* fancy Regiment
;; features, it merely sends all the temperature values over a certain
;; threshold to the base station.

(parameters 
  [dummy-param (install-firelightning)]
  ;[simalpha-realtime-mode #t]
  ;[simalpha-channel-model 'lossless]
  ;[simalpha-placement-type 'gridlike]
  [simalpha-failure-model  'none]
  [simalpha-dbg-on #f]
  ;[simalpha-sense-function sense-noisy-rising]
  ;[simalpha-sense-function sense-random-1to100]

  [default-slow-pulse (* 5 60 1000)] ;; 5 min
  [default-fast-pulse (*    3 1000)] ;; 3 sec

  ;[sim-timeout 2000000]
  ;[sim-timeout 86400000] ;; A full day. 86 Million milli's
  ;[sim-timeout 3600000] ;; An hour.
  ;[sim-timeout 600000] ;; Ten minutes
  [sim-timeout 60000] ;; One minute

  [varied-param 3] ;; Default value for the threshold.
  )

;; The varied parameter can be changed from outside the program source before load-time.
`(define threshold ,(varied-param))
;(define threshold 3)

;; Main

(define (read n) (tuple (nodeid n) (sense 'clock n) (sense 'temp n)))

(define (filt #(_ _ temp)) (> temp threshold))

;(tuple test
 (rfilter filt (rmap read world))
