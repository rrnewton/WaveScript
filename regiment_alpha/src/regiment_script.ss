#! /bin/sh
#|
if [ -f $REGIMENTD/depends/petite ];
then exec $REGIMENTD/depends/petite --script "$0" ${1+"$@"};
else petite --script "$0" ${1+"$@"};
fi
|#

; /usr/bin/scheme --script

; (load (string-append (getenv "HOME") "/scheme/chez/full_chez.ss"))  
; (load "compiler_chez.ss")

;(parameterize ([current-directory "~/cur"])
(load (string-append (getenv "REGIMENTD") "/regiment.ss"))

; (suppress-greeting #t)
; (scheme-start main)
; (when (top-level-bound? 'command-line-arguments)
;       (apply main (command-line-arguments))
;       (disp "SCRIPT FINISHED" (scheme-script) (command-line-arguments)))
