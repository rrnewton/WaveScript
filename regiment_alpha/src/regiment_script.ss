#! /bin/sh
#|
exec petite --script "$0" ${1+"$@"}
|#

; /usr/bin/scheme --script

; (load (string-append (getenv "HOME") "/scheme/chez/full_chez.ss"))  
; (load "compiler_chez.ss")

(load "regiment.ss")

; (suppress-greeting #t)
; (scheme-start main)
; (when (top-level-bound? 'command-line-arguments)
;       (apply main (command-line-arguments))
;       (disp "SCRIPT FINISHED" (scheme-script) (command-line-arguments)))
