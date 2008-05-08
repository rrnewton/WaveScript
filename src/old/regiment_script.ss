#! /bin/sh
#|
if (which chez > /dev/null); 
then exec chez --script "$0" `pwd` ${1+"$@"};
elif (which petite > /dev/null); 
then exec petite --script "$0" `pwd` ${1+"$@"}; 
elif [ -f $REGIMENTD/depends/petite ]; 
then exec $REGIMENTD/depends/petite --script "$0" `pwd` ${1+"$@"};
else echo CHEZ SCHEME not found.; exit -1 
fi 
|#

;;;; This script is just an executable wrapper script to regiment.ss

;; First argument is the directory
;(parameterize ([current-directory "~/cur"])
(parameterize ([current-directory (car (command-line-arguments))])
  (load (string-append (getenv "REGIMENTD") "/src/regiment.ss")))
