(module reader syntax/module-reader
  wavescript
 
  #:read        ws-read
  #:read-syntax ws-read-syntax

  #:whole-body-readers? #t

  (require "../../wavescript_parser.ss")
  ;(require "main.sls")
  ;(require (scheme pretty))
  (require scheme/pretty)

  (define (ws-read-syntax file port)
    (source-position-tracking #t)
    (let ((expr 
      `((require (except-in rnrs error + - * / or and)
		 "../ws/sim/wavescript_sim_library_push.sls"
		 "../ws/util/streams.sls"
		 scheme/pretty

		 (for-syntax ;"../ws/sim/wavescript_sim_library_push.sls"
			     scheme/pretty)

		 ;(for-meta 2 "../ws/sim/wavescript_sim_library_push.sls")
		 )
	(let ()
	  (begin 
	    ;(printf "Executing module code.  And got sim binding: ~a ~a\n" wsequal? (wscase #f))
	    (reset-wssim-state!)
	    (for-each print
	      (car 
	       (values->list 
		(stream-take 10 ;browse-stream ;wsint:direct-stream
			     (run-stream-query 
			      (WSCOMPILE_MACRO ,(ws-parse-port port file)))))))
	    (void))))))

    
    (if (file-exists? "DEBUG.ss") (delete-file "DEBUG.ss") (void))
    (with-output-to-file "DEBUG.ss"
      (lambda ()
	(pretty-print expr)))
    
    (datum->syntax #f expr)
      )
    )

  (define (ws-read port)
    (syntax->datum (ws-read-syntax "UNKNOWN_FILE" port)))
  )



	;;[p ',(ws-parse-port port file)]
#|
	;; Make all the pairs mutable:
	(define (gross-hack sexp)
	  (cond 
	   [(pair? sexp) (mcons (gross-hack (car sexp))
				(gross-hack (cdr sexp)))]
	   [(vector? sexp)
	    (vector-map gross-hack sexp)]
	   [else sexp]))
       (pretty-print (gross-hack p))
       (for-each pretty-print
	 (mlist->list
	  (mcar (values->list (stream-take 10 (wsint (gross-hack p) '()))))))
|#
