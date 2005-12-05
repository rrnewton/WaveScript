#! /bin/sh
#|
if which chez ;
then chez --script "$0" `pwd` ${1+"$@"};
elif [ -f $REGIMENTD/depends/petite ];
then exec $REGIMENTD/depends/petite --script "$0" `pwd` ${1+"$@"};
else petite --script "$0" `pwd` ${1+"$@"};
fi
|#

; /usr/bin/scheme --script

; (load (string-append (getenv "HOME") "/scheme/chez/full_chez.ss"))  
; (load "compiler_chez.ss")

;; First argument is the directory
;(parameterize ([current-directory "~/cur"])
(parameterize ([current-directory (car (command-line-arguments))])
  (load (string-append (getenv "REGIMENTD") "/src/regiment.ss")))
  
; (suppress-greeting #t)
; (scheme-start main)
; (when (top-level-bound? 'command-line-arguments)
;       (apply main (command-line-arguments))
;       (disp "SCRIPT FINISHED" (scheme-script) (command-line-arguments)))
