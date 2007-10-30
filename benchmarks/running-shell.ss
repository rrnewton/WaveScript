#!/usr/bin/mzscheme -C


;
;
;
(require (lib "pretty.ss")
         (lib "string.ss")
         "running.ss")


;
;
;
(define (main args)
  (pretty-print
   (run-wavescope-program (read-from-string (cadr args)))))

