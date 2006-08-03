

;(load-shared-object (format "~a/usleep.o" (machine-type)))
(load-shared-object (format "usleep.o"))

(define sleep (foreign-procedure "rrn_usleep" (integer-32) void))
