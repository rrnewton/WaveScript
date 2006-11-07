#! /bin/sh
#| 
exec regiment i --script "$0" ${1+"$@"};
|#

(define curlogfile (format "./deadsimple.log.gz"))
(define resultsfile 'uninit)
(define worldseed 'uninit)

(define-syntax mvfirst
  (syntax-rules ()
    [(_ e) (call-with-values (lambda () e) (lambda args (car args)))]))

;; This is the main procedure, do the analysis:
(define (main) 
       (define logport (open-input-file 
			curlogfile 
			(if (equal? "gz" (extract-file-extension curlogfile))
			    '(compressed) '())))

       (define resultslog 
	 (begin (printf "We build a very simple model based on the returned data.\n")
		(printf "Directing output to file: ~a\n" resultsfile)
		(open-output-file resultsfile )))

       (printf "\n\n\nOpenning log file as stream...\n")
       (printf "===================================================================\n")
       (printf "Dumping logfile in simplified format...\n")
       (let loop ((s  (reg:read-log logport 'stream)))
	 (when (= (random 10000) 0) 
	   (printf "~a " (comma-number (file-position logport)))
	   (flush-output-port))
	 (flush-output-port resultslog)
	 (unless (stream-empty? s)
	   (match (stream-car s)
	     [(,t ,id Bcast . ,_)
	      (fprintf resultslog "~s ~s maintenance\n" t id)]
	     [(,t ,id Ucast . ,_)
	      (fprintf resultslog "~s ~s data\n" t id)]
	     [(,t ,_ SOCRETURN (val #(,resls ,tempsum ,count)))
	      (guard (list? resls))
	      (fprintf resultslog "~s ~s return ~s ~s\n" t (vector-ref (car resls) 0) tempsum count)]
	     [(,t ,_ SOCRETURN (val #(,nodeid ,clock ,temp)))
	      (fprintf resultslog "~s ~s return ~s\n" t nodeid temp)]
	     [,else (void)])
	   (loop (stream-cdr s))))
     )

;; Body of the script:

(define (thescript script-args)
  (let ()
    (let loop ((x (map string->symbol script-args)))
      (match x
	[() (main)]
	
	[(-l ,file ,rest ...) (set! curlogfile (symbol->string file)) (loop rest)]
	[(-o ,file ,rest ...) (set! resultsfile (symbol->string file)) (loop rest)]    
	[(-w ,file ,rest ...) (set! worldseed (car (file->slist (symbol->string file)))) (loop rest)]

    [,other (error 'mattlogs "bad arguments: ~s\n" other)]))))


(thescript (command-line-arguments))
