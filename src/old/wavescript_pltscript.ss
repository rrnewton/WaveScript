#! /bin/bash
#|
# -u -t -r
exec mzscheme -qr "$0" ${1+"$@"}
|#

;(module wavescript_pltscript mzscheme  

  (eval '(define start-dir (current-directory)))
  
  (require "main_plt.ss")

  (DEBUGMODE (printf "Running in DEBUG mode.\n"))
  
  (eval '(define wavescript-origin "PLT"))
  (eval '(define svn-revision 'unknown-svn-rev))

  ;; Bring this out to top level:
;  (eval `(define REGIMEND ,WAVESCRIPTD))

  (apply main (cdr (vector->list (current-command-line-arguments)))) 

;  (provide (all-defined)(all-from  "main_plt.ss")))

