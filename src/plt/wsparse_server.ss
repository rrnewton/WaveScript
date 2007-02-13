

;; This is the poor-man's server.

;; Would be straightforward to do this with TCP/IP too:

(module wsparse mzscheme 

  (require "regiment_parser.ss")
  (require (lib "process.ss"))
  (require (lib "pretty.ss"))

(define inpipefile "/tmp/wsparse_server_pipe")
(define outpipefile "/tmp/wsparse_server_response")

(current-exception-handler
 (lambda (exn)
   (delete-file inpipefile)
   (delete-file outpipefile)
   (define msg
     (format "\nWSPARSE error:\n   ~a\n\nException: ~s\n" 
	     (exn-message exn) exn))
   (display msg)
   (display msg (open-output-file "/var/log/wsparse_server.log" 'append))
   ;(mail ryan-email "Failure of supertest.ss" msg)
   (exit 1)))

(if (file-exists? inpipefile) (delete-file inpipefile))
(if (file-exists? outpipefile) (delete-file outpipefile))

(system)

(define inpipe (open-input-file inpipefile))
(define outpipe (open-output-file outpipefile 'append))

;; Now just write it to stdout:
;(display (ws-postprocess (reg-parse-file filename)))(newline)
;(pretty-print (ws-postprocess (reg-parse-file filename)))

(if (member "--nopretty" (vector->list (current-command-line-arguments)))
    (write (ws-parse-file filename))
    (pretty-print (ws-parse-file filename)))

(delete-file inpipefile)
(delete-file outpipefile)

)

;(require wsparse)
