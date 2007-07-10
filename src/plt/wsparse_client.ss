#! /bin/bash
#|
exec mzscheme -mqt "$0" ${1+"$@"}
|#

(module wsparse_client mzscheme

  (define filename (vector-ref (current-command-line-arguments) 0))
;  (unless (file-exists? filename) (error 'wsparse_client "file does not exist: ~s" filename))
  
  (define (go)
    (with-handlers ([(lambda (x) #t) fallback])
    (let-values ([(inpipe outpipe) (tcp-connect "localhost" 60606)])
      (write filename outpipe)
      (flush-output outpipe)

      (let ([parsed (read inpipe)])
	(print-graph #t) (write parsed)(newline)
	
	(tcp-abandon-port inpipe)
	(tcp-abandon-port outpipe)

	))))

  (define (fallback _)
    (fprintf (current-error-port) "  \"wsparse_server_tcp.ss\" not running or returned error.")
    (fprintf (current-error-port) "  Invoking wsparse.ss from source.\n")
    (eval '(require (lib "process.ss")))
    (eval `(system ,(format "mzscheme -mqt ~a/src/plt/wsparse.ss ~a --nopretty" 
		    (getenv "REGIMENTD") filename)))
    )

  (go)
  (exit 0)
  )

