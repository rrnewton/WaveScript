
;; Simulation configuration:
(parameters 
    [sim-num-nodes 30]
    [sim-timeout 2000]
    [simalpha-channel-model 'lossless]
    [simalpha-failure-model 'none])

;; This is like the hello world of Regiment:
;; It establishes a single spanning tree across the network and uses
;; it to aggregate all the sensor readings.

(define readings (rmap sense world))

(rfold + 0 readings)
