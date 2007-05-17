#! /bin/sh
#|
# -u -t -r
exec mzscheme -qr "$0" ${1+"$@"}
|#

;(module regiment_pltscript mzscheme  

  (eval '(define start-dir (current-directory)))
  
  (require "main_plt.ss")

  (DEBUGMODE (printf "Running in DEBUG mode.\n"))
  
  (eval '(define regiment-origin "PLT"))
  (eval '(define svn-revision 'unknown-svn-rev))

  ;; Bring this out to top level:
;  (eval `(define REGIMEND ,REGIMENTD))

  (apply main (cdr (vector->list (current-command-line-arguments)))) 

;  (provide (all-defined)(all-from  "main_plt.ss")))

