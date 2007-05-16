#! /bin/sh
#|
exec mzscheme -qr "$0" ${1+"$@"}
|#

(printf "Loading under PLT.\n")
(define start-dir (current-directory))

(load/use-compiled "main_plt.ss")

(define regiment-origin "PLT")
(define svn-revision 'unknown-svn-rev)
(apply main (cdr (vector->list (current-command-line-arguments))))









#;
(module regiment_pltscript mzscheme
  (load/use-compiled "main_plt.ss")
  (apply main
   (cdr (vector->list (current-command-line-arguments))))
  )

