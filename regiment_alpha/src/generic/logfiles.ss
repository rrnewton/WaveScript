;;;; Manipulate log files / simulator traces.
;;;; .author Ryan Newton

;;;; [2006.02.27] <br>
;;;; This (currently) goes along with simulator alpha. 

;----------------------------------------------------------------------
;; This reads in a log file.  Either as a stream or all at once.



(module logfiles mzscheme 
  (require "../plt/iu-match.ss"
           (lib "include.ss")
           ;(all-except (lib "list.ss") filter)
           "../generic/constants.ss"
           ;"hashtab.ss"
           (all-except "../plt/helpers.ss" test-this these-tests)
           ;(all-except "regiment_helpers.ss" test-this these-tests)
           )

  (provide     	
   logger
   reg:read-log
   log-line->human-readable
   ) ;; End provide

  (chezimports )

; =======================================================================  

;;
;; I manually do the delays rather than using stream-cons/stream-append.
;; Streams are not currently an ADT, they're representation is transparent.
;; (They're simply lists with delayed tails.)
(define (reg:read-log file . opts)
  (define valid-options '(stream)) ;; A superset of opts.
  ;; [2006.02.19] This doesn't really seem to work as an optimization.
  (define batch-size 1) ;; Number of lines of input to read at a time.
  ;; NOTE: This didn't increase performance, so I'm leaving it at 1.
  (let ((inport	(if (input-port? file) file
		    (open-input-file
		     file (if (equal? (extract-file-extension file) "gz")
			      'compressed 'uncompressed)))))
    (for-each (lambda (opt) (if (not (memq opt valid-options)) 
				(error 'reg:read-log "invalid option: ~a" opt)))
      opts)
    (if (memq 'stream opts)
	(let ((first (read inport)))
	  (if (vector? first)
	      ;; If the file contains vectors, those are batches of log-lines.
	      ;; We follow that existing granularity in our stream output.
	      (let loop ((x first))
		(if (eof-object? x) '()
		    (append (vector->list x)
			    (delay (loop (read inport))))))
	      ;; Otherwise, the file is a flat set of log-lines.  We batch it on reading.
	      (let loop ((x first) (count batch-size) (acc '()))
		(cond
		 [(eof-object? x) '()]
		 [(= count 0) (append (reverse! acc)
				      (delay (loop (read inport) batch-size '())))]
		 [else (loop (read inport) (sub1 count) (cons x acc))]))))
	;; Otherwise read the whole log file:
	(let loop ([x (read inport)] [acc '()])
	  (if (eof-object? x)
	      (reverse! acc)
	      (loop (read inport)
		    (if (vector? x)
			(append (reverse! (vector->list x)) acc)
			(cons x acc)))))
	)))

;; This has multiple calling forms.                                  <br>
;; .form (log-line->human-readable level ob args)                    <br>
;; .param level -- the priority level (generally 0-5)      
;; .param The object to print, which may be:                         <br>
;;  1) A list: (vtime nodeid Symbol (field val)* )                   <br>
;;     In this case args is ignored.                                 <br>
;;  2) A string, in which case format is called and                  <br>
;;     'args' are filled into the holes.                             <br>
;; .param args -- Args to format, if ob is a string.
;; .returns -- A string representing the line of log output.
(define log-line->human-readable
  (let ()
    (define (column-width w ob)
      (let ((s (format "~a" ob)))
	(if (< (string-length s) w)
	    (string-append s (make-string (- w (string-length s)) #\space))
	    s)))
    (define (print-header level)
      (format "~a{~a} " 
	      (column-width 5
			    (if (simulation-logger-count)
				(begin (simulation-logger-count (+ 1 (simulation-logger-count)))
				       (- (simulation-logger-count) 1))
				"foo"))
					;	     (column-width 4 (number->string current-vtime))
	      (column-width 3 level)
	      ))
    (lambda (level ob args)
      (when (<= level (simulation-logger-level))
	(print-header level)
	(cond
	 [(list? ob) 
	  (string-append
	   (apply string-append 
		  (format "~a ~a ~a ~a -- " (pad-width 6 (car ob))
			  (pad-width 3 (cadr ob))
			  (make-string (fx* 2 (inexact->exact (floor level))) #\space)
			  (caddr ob))
		  (insert-between ", "
				  (map (lambda (pr)
					 (format "~a: ~a" (car pr) (cadr pr)))
				    (cdddr ob))))
	   "\n")]
	 [(null? args) (format "~a\n" ob)]
	 [else (apply format ob args)])
	))))


;======================================================================

;; This is our logger for events in the simulator:                  <br> 
;; .form (logger str args ...)                                      <br>        
;; .form (logger print-level str args ...)                          <br><br>
;;
;; This uses the parameter "simulation-logger", expecting it to be bound to an output port. <br><br>
;;
;; TODO: make this a syntax so that the calls disappear entirely in non-debug mode...  <br><br>
;;
;; The current model for storing SExp logs allows them to be of the following type:    <br> 
;;   LogLine ::= (vtime nodeid Symbol (field val)* )                                   <br>
;;   Log ::= <LogLine>*                        -- A flat file of log-lines             <br>
;;        |  #( <LogLine>* )*                  -- Log-lines chunked/batched by vectors <br><br>
;;
;; And further, log files can either be .log or .log.gz.
(define logger
  (let ()
    ;; This is a bit of added complexity, but I'm going to write out
    ;; chunks of log-file as vectors.  Thus I'm going to buffer the logging.
    ;; UNFINISHED UNFINISHED::
    (define buffered-writer
      (let ([obj-buffer 
	     (if (integer? (simulation-logger-fasl-batched))
		 (make-vector (simulation-logger-fasl-batched) #f)
		 (make-vector 1 #f))]
	    [num-objs 0])
	(case-lambda 
	  [() ;; This is a flush.
	   (buffered-writer (simulation-logger))]
	  [(port) ;; This is a flush.
	   ;(fprintf (current-error-port) "Bufwrite:     FLUSH ~a\n" num-objs)
	   (when (> num-objs 0)
	     (fasl-write 
	      (if (= num-objs (vector-length obj-buffer))
		  ;; If it's full, then blast out the whole thing.
		  obj-buffer
		  ;; Otherwise, print a shortened chunk:
		  (let ([outchunk (make-vector num-objs)])
		    (for i = 0 to (sub1 num-objs)
			 (vector-set! outchunk i (vector-ref obj-buffer i)))
		    outchunk))
	      port)
	     (set! num-objs 0)
	     (vector-fill! obj-buffer #f))]
	  [(obj port)
	   (vector-set! obj-buffer num-objs obj)
	   (set! num-objs (fx+ 1 num-objs))
	   ;(fprintf (current-error-port) "Bufwrite: num curr ~a/~a\n" num-objs  (vector-length obj-buffer))
	   ;; If we're full initiate a flush.
	   (if (= num-objs (vector-length obj-buffer))
	       (buffered-writer port))])))
    
    (define (do-logging input writer)
      ;; Several different ways to invoke the logger:
      (mvlet ([(level ob args)
	       (match input
		 ;; These two forms are basically like a printf:
		 [(,lvl ,str ,args ...)
		  (guard (number? lvl) (string? str))
		  (values lvl str args)]
		 [(,str ,args ...) (guard (string? str))
		  (values 1 str args)]
		 ;; This form is for structured, SEXP output:
		 [(,lvl ,time ,nodeid ,sym ,pairs ...)
		  (guard (number? lvl) (number? time) ;(integer? nodeid) 
			 (symbol? sym) (andmap list? pairs))
		  (values lvl (cons time (cons nodeid (cons sym pairs))) '())]
		 [,else (error 'logger "invalid arguments: ~a" input)]
		 )])
	(let ([port (simulation-logger)])
	  (if (simulation-logger-human-readable)
	      (begin 
		(if (simulation-logger-fasl-batched)
		    (error 'logger "cannot have both human-readable and fasl mode turned on."))
		(display (log-line->human-readable level ob args) port))
	    (parameterize ([print-graph #t]
			   [print-length #f]
			   [print-level #f]
			   [pretty-maximum-lines #f])
	      (if (null? args)
		  (writer ob port)
		  (writer (apply format ob args) port))
	      (if (not (simulation-logger-fasl-batched))
		  (newline port))
	      ;; TEMP TEMP TEMP FIXME:
	      ;(flush-output-port port)
	      )))))

    ;; Body of logger:
    (case-lambda 
      ;; This is a flush, pass it on.
      [() 
       (when (simulation-logger)
	 (buffered-writer (simulation-logger)))]
      [input
       (when (simulation-logger)
	 (do-logging input 
		     ;; Pick a writer based on this parameter.
		     (case (simulation-logger-fasl-batched)
		       [(#f) write]
		       [(#t) fasl-write]
		       [else buffered-writer]
		       )))])))

;; This is just another variant:
;; This has no critical section for now!!! [2005.02.25]
(define-syntax with-logger
  (syntax-rules ()
      [(_ exp ...)
      (if (simulation-logger)
	  (parameterize ([current-output-port (simulation-logger)])
			exp ...))]))

) ;; End module.
