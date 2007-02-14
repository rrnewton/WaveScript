

;; This is the poor-man's server.

;; Would be straightforward to do this with TCP/IP too:

(module wsparse_server mzscheme 

  (require "regiment_parser.ss")
  (require (lib "process.ss"))
  (require (lib "pretty.ss"))

(define inpipefile "/tmp/wsparse_server_pipe")
(define outpipefile "/tmp/wsparse_server_response")

(define inpipe #f)
(define outpipe #f)

;; Now just write it to stdout:
;(display (ws-postprocess (reg-parse-file filename)))(newline)
;(pretty-print (ws-postprocess (reg-parse-file filename)))

(printf "Starting server loop...\n")
(define (server-loop)  
  (define fn #f)
  (printf "  Deleting pipes.\n")
  
  (if (file-exists? inpipefile)  (delete-file inpipefile))
  (if (file-exists? outpipefile) (delete-file outpipefile))

  (printf "  Making new pipes.\n")

  (system (format "mkfifo ~a" inpipefile))
  (system (format "mkfifo ~a" outpipefile))

  (set! inpipe (open-input-file inpipefile))
  (set! outpipe (open-output-file outpipefile 'append))

  (printf "  Waiting on request.\n")
  (set! fn (read inpipe))

  (printf "\nHandling request: ~s\n" fn)
  (cond 
   [(eof-object? fn) 
    (server-loop)
    
#;    
    (begin (close-output-port outpipe)
	   (close-input-port inpipe)
	   (delete-file inpipefile)
	   (delete-file outpipefile)
	   (exit 0))]
   [(string? fn) 
    (printf "Responding to request: ~s\n" fn)
    (time (write (ws-parse-file fn) outpipe))
    (flush-output outpipe)
    (server-loop)]
   [else (error 'server-loop "received something other than a filename")]
   ))

(define restart-handler
  (lambda (exn)
    (define msg
      (format "\nWSPARSE error:\n   ~a\n\nException: ~s\n" 
	      (exn-message exn) exn))
    
					;(if (file-exists? inpipefile) (delete-file inpipefile))
					;(if (file-exists? outpipefile) (delete-file outpipefile))
    (display msg)
    (display msg (open-output-file "/tmp/wsparse_server.log" 'append))
       
    ;;(mail ryan-email "Failure of supertest.ss" msg)
					;(exit 1)

    (write #f outpipe)
    (flush-output outpipe)
    (printf "Restarting server.\n")
    (parameterize ([current-exception-handler restart-handler])
      (server-loop))
    ))

(current-exception-handler restart-handler)

(server-loop)


)

;(require wsparse)
