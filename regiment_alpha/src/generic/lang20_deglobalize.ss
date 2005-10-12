;; [2004.06.09]
;; This is the "language definition" for the output of deglobalize.

;; REQUIRES: on simulator_nought.ss and maybe simulator_nought_grpahics.ss

(define (deglobalize-lang prog)
;  (run-simulation
;   (build-simulation
;    (compile-simulate-nought prog))
    (compile-simulate-alpha prog))
;   2.0))

