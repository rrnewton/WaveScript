#! /bin/bash
#|
exec mzscheme -mqt "$0" ${1+"$@"}
|#

(module wsparse_client mzscheme

  (define filename (vector-ref (current-command-line-arguments) 0))
;  (unless (file-exists? filename) (error 'wsparse_client "file does not exist: ~s" filename))
  
  (define-values (inpipe outpipe) (tcp-connect "localhost" 60606))
  
  ;(write "~/wavescript/lib/rewindowGeneral.ws" outpipe)
  (write filename outpipe)
  (flush-output outpipe)

  (define parsed (read inpipe))
  ;(printf "GOT PARSED: ~s\n" (car parsed))
  (print-graph #t) (write parsed)(newline)
  
  (tcp-abandon-port inpipe)
  (tcp-abandon-port outpipe)

  (exit 0)
  )

