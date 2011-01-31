

(load-shared-object (format "libc.so.5"))
;(load-shared-object (format "libc.so.6"))
;(load-shared-object (format "libc.so"))  ;; Doesn't work on linux system.

(define sleep 
  (let ((cproc (foreign-procedure "usleep" (integer-32) void)))
    (lambda (x) (cproc (* 1000 x)))))

;(printf "now sleep\n")
;(sleep 1000)
;(printf "awake!\n")