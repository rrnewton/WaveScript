;;;; This is like the hello world of Regiment:
;;;; It establishes a single spanning tree across the network and uses
;;;; it to aggregate all the sensor readings.

;; Simulation configuration:
(parameters 
    [sim-num-nodes 30]
    [sim-timeout 2000]
    [simalpha-placement-type 'gridlike] ;'random]  ;'connected]    
    [simalpha-channel-model 'linear-disc] ;'lossless]
    [simalpha-failure-model 'none])

;; Main program:

(define readings (rmap (lambda (n) (sense 'temp n)) world))

(define abovethresh (rfilter (lambda (n) (> n 30)) readings))

(rfold + 0 readings)
