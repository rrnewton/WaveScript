

;; This is the poor-man's server.

;; Would be straightforward to do this with TCP/IP too:

(module wsparse_server mzscheme 

  (require "regiment_parser.ss")
  (require (lib "process.ss"))
  (require (lib "pretty.ss"))

(define inpipefile "/tmp/wsparse_server_pipe")
(define outpipefile "/tmp/wsparse_server_response")

(current-exception-handler
 (lambda (exn)
   (define msg
     (format "\nWSPARSE error:\n   ~a\n\nException: ~s\n" 
	     (exn-message exn) exn))
   (if (file-exists? inpipefile) (delete-file inpipefile))
   (if (file-exists? outpipefile) (delete-file outpipefile))
   (display msg)
   (display msg (open-output-file "/tmp/wsparse_server.log" 'append))
   ;(mail ryan-email "Failure of supertest.ss" msg)
   (exit 1)))

(if (file-exists? inpipefile) (delete-file inpipefile))
(if (file-exists? outpipefile) (delete-file outpipefile))

(system (format "mkfifo ~a" inpipefile))
(system (format "mkfifo ~a" outpipefile))

(define inpipe (open-input-file inpipefile))
(define outpipe (open-output-file outpipefile 'append))

;; Now just write it to stdout:
;(display (ws-postprocess (reg-parse-file filename)))(newline)
;(pretty-print (ws-postprocess (reg-parse-file filename)))

(printf "Starting server loop...\n")
(let server-loop ([fn (read inpipe)])
  (printf "Handling request: ~s\n" fn)
  (cond 
   [(eof-object? fn) (begin (close-output-port outpipe)
			    (close-input-port inpipe)
			    (delete-file inpipefile)
			    (delete-file outpipefile)
			    (exit 0))]
   [(string? fn) 
    (printf "Responding to request: ~s\n" fn)
    (write (ws-parse-file fn) outpipe)
    (flush-output outpipe)]
   [else (error 'server-loop "received something other than a filename")]
   ))


)

;(require wsparse)
