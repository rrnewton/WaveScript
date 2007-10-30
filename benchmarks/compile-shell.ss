#!/usr/bin/mzscheme -C


;
;
;
(require (lib "pretty.ss")
         (lib "string.ss")
         "compile.ss")


;
;
;
(define (main args)
  (pretty-print
   (compile-wavescript-program (read-from-string (cadr args)))))

