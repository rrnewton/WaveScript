#! /bin/bash
#|
exec mzscheme -qt "$0" ${1+"$@"}
|#

;; Experimenting with doing a TCP version

(module wsparse_server_tcp mzscheme 
  (require "regiment_parser.ss")

  (define conn #f)
  (define inpipe #f)
  (define outpipe #f)
  
  (define (run-server)
    (define _ (set! conn (tcp-listen 60606 100 #t)))
    (define __ (printf "Starting server loop...\n"))
    (define (server-loop)
      (printf "  Waiting for connection.\n")
      (let-values ([(a b) (tcp-accept conn)]) 
        (set! inpipe a)
        (set! outpipe b))
      (let ([fn (read inpipe)])
        (printf "\nHandling request: ~s\n" fn)
        (cond 
          [(eof-object? fn) (server-loop)]
          [(string? fn)
           (let ([parsed (ws-parse-file fn)])
             (printf "Got parse, responding to request: ~s\n" fn)
             ;(printf "PORT CLOSED? ~a\n" (port-closed? outpipe))
             ;; If they close the port too soon we'll get an error here.
             ;; Don't know how to avoid that.  port-closed? doesn't help.
             (time (write parsed outpipe))
             (flush-output outpipe)
             (printf "Wrote response to port\n")
             (tcp-abandon-port inpipe)
             (tcp-abandon-port outpipe)
             (server-loop))]
          [else (error 'server-loop "received something other than a filename: ~s" fn)]
          )))
    (server-loop))

(define restart-handler
  (lambda (exn)
    (define (first)
      (define msg
        (format "\nWSPARSE error:\n   ~a\n\nException: ~s\n" 
                (exn-message exn) exn))
      (display msg)
      (display msg (open-output-file "/tmp/wsparse_server.log" 'append))
      (printf "Cleaning up open connections.\n")
      ;; This is lame:
      (with-handlers ([(lambda (x) #t) second])
        (when conn (tcp-close conn) (set! conn #f))
        (when outpipe
          (printf "Writing error message to outpipe (open? ~s).\n" (not (port-closed? outpipe)))
          (display "ERROR\n\n" outpipe) ;; Erk, what if this produces a broken pipe exception?
          (flush-output outpipe)
          (tcp-abandon-port outpipe) 
          (set! outpipe #f))
        (when inpipe (tcp-abandon-port inpipe) (set! inpipe #f))
        )
      (second))
    (define (second . _)     
      (printf "Restarting server.\n")
      ;(run-server)    
      ;[uncaught-exception-handler restart-handler](run-server)    
      ;(parameterize ([uncaught-exception-handler restart-handler]) (run-server))
      (with-handlers ([(lambda (x) #t) restart-handler]) (run-server))      
      (printf "SHOULD NOT RETURN\n"))    
    (first)
    ))

  #;  
(define restart-handler
  (lambda (exn)
    (define msg
      (format "\nWSPARSE error:\n   ~a\n\nException: ~s\n" 
                (exn-message exn) exn))
      (display msg)
      (display msg (open-output-file "/tmp/wsparse_server.log" 'append))
      (printf "Cleaning up open connections.\n")    
      ;(when inpipe (tcp-abandon-port inpipe) (set! inpipe #f))
      ;(when outpipe (tcp-abandon-port outpipe) (set! outpipe #f))
      (when conn (tcp-close conn) (set! conn #f))
      (printf "Restarting server.\n")
      ;(run-server)    
      ;[uncaught-exception-handler restart-handler](run-server)    
      ;(parameterize ([uncaught-exception-handler restart-handler]) (run-server))
      (with-handlers ([(lambda (x) #t) restart-handler]) (run-server))      
      (printf "SHOULD NOT RETURN\n")
    ))
  
  

;(run-server)  
(with-handlers (;[exn:fail? (lambda (x) (printf "GOT FAIL: ~s\n" x))]
                [(lambda (x) #t) restart-handler]) (run-server))
;(uncaught-exception-handler restart-handler)(run-server)

)