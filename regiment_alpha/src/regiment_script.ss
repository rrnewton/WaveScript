#! /bin/sh
#|
if (which chez > /dev/null); 
then exec chez --script "$0" `pwd` ${1+"$@"};
elif [ -f $REGIMENTD/depends/petite ]; 
then exec $REGIMENTD/depends/petite --script "$0" `pwd` ${1+"$@"};
else exec petite --script "$0" `pwd` ${1+"$@"}; 
fi 
|#

; /usr/bin/scheme --script

; (load (string-append (getenv "HOME") "/scheme/chez/full_chez.ss"))  
; (load "main_chez.ss")

;; First argument is the directory
;(parameterize ([current-directory "~/cur"])
(parameterize ([current-directory (car (command-line-arguments))])
  (load (string-append (getenv "REGIMENTD") "/src/regiment.ss")))
  
; (suppress-greeting #t)
; (scheme-start main)
; (when (top-level-bound? 'command-line-arguments)
;       (apply main (command-line-arguments))
;       (disp "SCRIPT FINISHED" (scheme-script) (command-line-arguments)))
