;;;; Manipulate log files / simulator traces.
;;;; .author Ryan Newton

;;;; [2006.02.27] <br>
;;;; This (currently) goes along with simulator alpha. 

;----------------------------------------------------------------------
;; This reads in a log file.  Either as a stream or all at once.
;;
;; I manually do the delays rather than using stream-cons/stream-append.
;; Streams are not currently an ADT, they're representation is transparent.
;; (They're simply lists with delayed tails.)
(define (reg:read-log file . opts)
  (define valid-options '(stream)) ;; A superset of opts.
  ;; [2006.02.19] This doesn't really seem to work as an optimization.
  (define batch-size 1) ;; Number of lines of input to read at a time.
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

;; TODO: MOVE LOGGER HERE!!

