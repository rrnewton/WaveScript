#! /bin/sh
#|
if (which chez_threaded > /dev/null); 
then exec chez_threaded --script "$0" `pwd` ${1+"$@"};
elif [ -f $WAVESCRIPTD/depends/petite ]; 
then exec $WAVESCRIPTD/depends/petite --script "$0" `pwd` ${1+"$@"};
else exec petite --script "$0" `pwd` ${1+"$@"}; 
fi 
|#

; /usr/bin/scheme --script

; (load (string-append (getenv "HOME") "/scheme/chez/full_chez.ss"))  
; (load "main_chez.ss")

;; First argument is the directory
;(parameterize ([current-directory "~/cur"])
(parameterize ([current-directory (car (command-line-arguments))])
  (load (string-append (getenv "WAVESCRIPTD") "/src/wavescript.ss")))
  
; (suppress-greeting #t)
; (scheme-start main)
; (when (top-level-bound? 'command-line-arguments)
;       (apply main (command-line-arguments))
;       (disp "SCRIPT FINISHED" (scheme-script) (command-line-arguments)))
