;;; setup.ss
;;; Copyright (c) 1997 Cadence Research Systems

;;; redefine error handler to abort Scheme so that compiles exit from
;;; Scheme with nonzero exit status
(error-handler
   (lambda (who msg . args)
      (fprintf (console-output-port)
               "~%Error~a: ~a.~%"
               (if who (format " in ~s" who) "")
               (parameterize ([print-level 3] [print-length 6])
                  (apply format msg args)))
      (abort)))
