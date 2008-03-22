#! /bin/bash
#|
exec mzscheme -qt "$0" ${1+"$@"}
|#

;; Experimenting with doing a TCP version

(module tcpcat mzscheme 

  (define default-port "9002")
  
  (define (run-server port)
    (define conn (tcp-listen port 100 #t))
    (define (server-loop)
      (printf ";; Waiting for connection on port ~s.\n" port)
      (let-values ([(inpipe outpipe) (tcp-accept conn)])
	(printf ";; Got connection, echoing input:\n")
	(let loop ([fn (read-char inpipe)])
	  ;(printf "Read byte: ")
	  (unless (eof-object? fn)
	    (display fn);(newline)
	    (loop (read-char inpipe))))
	(printf "\n;; Hit EOF, restarting\n")
	(server-loop)))
    (printf ";; Starting server loop...\n")
    (server-loop))

  (define (run-client server port towrite)
    (fprintf (current-error-port) ";; Opening TCP connection to ~s port ~s\n" server port)
    (let-values ([(inpipe outpipe) (tcp-connect server port)])
      (let loop ([xx (read-line towrite)])
	(display ".")
	;(write xx)(newline)
	(unless (eof-object? xx)
	  (display xx outpipe)
	  (newline outpipe)
	  (loop (read-line towrite))))
      (newline)
      (fprintf (current-error-port) ";; Done writing output, shutting down.\n")
      (flush-output outpipe)
      (tcp-abandon-port inpipe)
      (tcp-abandon-port outpipe)
      (exit)))

  (define main
    (case-lambda 
      [()     (main default-port)]
      [(port) (run-server (string->number port))]
      [(port infile) (main "localhost" port infile)]
      [(server port infile)
       (run-client server (string->number port) (open-input-file infile))]))
  
  ;; Look at the args and decide whether to run client or server:  
  (apply main (vector->list (current-command-line-arguments))))
