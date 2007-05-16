#! /bin/sh
#|
exec mzscheme -qr "$0" ${1+"$@"}
|#

(module regiment_pltscript mzscheme  
;  (printf "Loading under PLT.\n")

  (eval '(define start-dir (current-directory)))
  
  (require "main_plt.ss")
  
  (eval '(define regiment-origin "PLT"))
  (eval '(define svn-revision 'unknown-svn-rev))
  (apply main (cdr (vector->list (current-command-line-arguments)))) 
  )

