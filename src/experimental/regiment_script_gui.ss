#! /bin/bash
#|
exec $REGIMENTD/depends/petite_swl --script "$0" ${1+"$@"}
|#

; DOESN'T WORK YET!!!

; /usr/bin/scheme --script

; (load (string-append (getenv "HOME") "/scheme/chez/full_chez.ss"))  
; (load "main_chez.ss")

;(parameterize ([current-directory "~/cur"])
(load (string-append (getenv "REGIMENTD") "/src/regiment.ss"))

; (suppress-greeting #t)
; (scheme-start main)
; (when (top-level-bound? 'command-line-arguments)
;       (apply main (command-line-arguments))
;       (disp "SCRIPT FINISHED" (scheme-script) (command-line-arguments)))
