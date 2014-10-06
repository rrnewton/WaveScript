#! /bin/bash
#|
exec mzscheme -mqt "$0" ${1+"$@"}
|#

(module wsparse_client mzscheme

;  (printf "CURRENT COMMAND ARGS: ~s\n\n" (current-command-line-arguments))
  
  (define filename (vector-ref (current-command-line-arguments) 0))
;  (unless (file-exists? filename) (error 'wsparse_client "file does not exist: ~s" filename))
  
  (define (go)
    ;(fprintf (current-error-port) "  Opening TCP connection...\n")
    (with-handlers ([(lambda (x) #t) fallback])		   
    (let-values ([(inpipe outpipe) (tcp-connect "localhost" 60606)])

      (if (member "--nopos" (vector->list (current-command-line-arguments)))
	  (write `(no-src-pos ,filename) outpipe)
;	  (write filename outpipe)
	  (write filename outpipe))

      (flush-output outpipe)

      (let ([parsed (read inpipe)])
	(print-graph #t) (write parsed)(newline)
	
	(tcp-abandon-port inpipe)
	(tcp-abandon-port outpipe)

	))))

  (define (fallback _)
;    (fprintf (current-error-port) "ERROR \"wsparse_server_tcp.ss\" not running or returned error.")
    ;; [2007.07.19] NOT GOING  TO DO THIS FROM HERE:
    ;; The caller should retry:
#|
    (fprintf (current-error-port) "  Invoking wsparse.ss from source.\n")
    (eval '(require (lib "process.ss")))
    (eval `(system ,(format "mzscheme -mqt ~a/src/plt/wsparse.ss ~a --nopretty" 
		    (getenv "WAVESCRIPTD") filename)))
|#
    (write #f)
    )


  ;(fprintf (current-error-port) "  Using TCP client...\n")
  (go)
  (exit 0)
  )

