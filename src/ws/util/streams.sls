#!r6rs

;;;; This implements a straightforward stream datatype -- tail-delayed pairs.

;;;; There are also other implementations of streams in different files.


(library (ws util streams)
  (export 
   stream? live-stream?
   stream-empty? stream-cons stream-car stream-cdr
   stream-map stream-filter stream-take stream-take-all 
   iota-stream stream-append browse-stream ;random-stream 
   stream-append-list stream-dump
   test-streams
   ) 
  (import (except (rnrs (6)) error)
	  ;(rnrs r5rs (6))
	  (ws compat compat)
	  (ws globals)
	  (ws util helpers)
	  (ws util reg_macros)
	  (ws util iu-match))

;=======================================================================
;;; Stream functions.
;;;
;;; [2004.06.17] These functions deal with streams that are represented
;;; as a list, promise, or improper list with a promise as its final
;;; cdr-pointer.  That is:                         <br><br>
;;;  Stream  := (item*)                            <br>
;;;           | (item* . promise)                  <br>
;;;           | promise                            <br><br>
;;;
;;; [2005.10.16] Just switched this from head-strict to not.
;;; I should probably switch over to using the standard SRFI-40 stream
;;; implementation at some point. <br><br>
;;;
;;; [2006.02.19] NOTE: Streams are not currently an ADT. Their
;;; representation is transparent.  The user is free to construct
;;; their own tail-delayed lists with whatever strictness pattern
;;; they wish.

;; Is the object potentially a stream?  Can't tell for sure because
;; promises are opaque.
;; [2006.07.28] TODO: this quadratic definition looks unnecessary!  FIXME: 
(define (stream? s)
  (or (list? s)   ;; Is it a proper list?
      ;; Or an improper list that's still being computed?
      (live-stream? s)))
;; A live stream is one not all of whom's values have been computed yet.
(define (live-stream? s) 
  (or (promise? s)
      (and (pair? s) (stream? (cdr s)))))
(define stream-empty? 
  (lambda (s)
    (cond 
     [(null? s) #t]
     [(promise? s) (stream-empty? (force s))]
     [else #f])))

(define-syntax stream-cons
  (syntax-rules ()
    [(_ a b) (cons a (delay b))]))

;; NOTE: Double delay for append:
;; Appends list to stream... not stream to stream!
(define-syntax stream-append-list
  (syntax-rules ()
    [(_ args ... tail) (delay (append args ... (delay tail)))]))

;; Append a stream (which should be finite) to another stream.
(define (stream-append s1 s2)
  (delay 
    (let loop ([s1 s1])
      (cond
       [(stream-empty? s1) s2]
       ;; Optimization: don't lazify something that's already there.
       [(pair? s1) (cons (car s1) 
			 ;; Keep going with it if we can.
			 (if (pair? (cdr s1)) (loop (cdr s1))
			     (delay (loop (cdr s1)))))]
       [else 
	(stream-cons (stream-car s1) 
		     (loop (stream-cdr s1)))]))))
(define stream-car
  (lambda (s)
    (let scloop ((s s))
      (cond
       [(promise? s)
	;; We have no way of mutating the prior cell, so just return this:
	(scloop (force s))]
       [(pair? s) (car s)]
       [(null? s) (error 'stream-car "Stream is null!")]
       [else (error 'stream-car "invalid stream: ~s" s)]))))

(define (stream-cdr s)
  (cond
   [(promise? s)      
    ;; Again, this one isn't structured as a pair, so we can't mutate and extend.
    ;; We just have to leave the promises in place.
    (stream-cdr (force s))]
   [(null? s) (error 'stream-cdr "Stream is null!")]
   [(pair? s)
; [2006.02.19] Why was I forcing this!?
;      (if (promise? (cdr s))
;	  (begin (set-cdr! s (force (cdr s)))
;		 ;; Might need to keep going, a promise may return a promise:
;		 (stream-cdr s))
	  (cdr s)]
   [else (error 'stream-cdr "invalid stream: ~s" s)]))

;; Take N elements from a stream
;; [2006.02.19] Modified to return two values, the second being the
;; remainder of the stream.
;; Tail recursive.
(define stream-take 
  (lambda (n s)
    (let stloop ((n n) (s s) (acc '()))
      (cond
       [(fx= 0 n) (values (reverse! acc) s)]
       [(stream-empty? s)
	(error 'stream-take "Stream ran out of elements before the end!")]
       [else 
	(stloop (fx- n 1) (stream-cdr s)
		(cons (stream-car s) acc))]))))

;; Drop N elements from a stream.
(define stream-drop
  (lambda (n s)
    (let stloop ((n n) (s s))
      (cond
       [(fx= 0 n) s]
       [(stream-empty? s)
	(error 'stream-drop "Stream ran out of elements before the end!")]
       [else (stloop (fx- n 1) (stream-cdr s))]))))

;; Dump an entire stream to a file:
(define stream-dump
  (lambda (stream file)
    (define count 0)
        (let ([port (open-output-file (format "~a" file))])
	       (parameterize ([print-length #f]
			      [print-level #f]
			      [print-graph #f]
			      [ws-print-output-port port]
			      )
		 ;;(IFCHEZ (optimize-level 3) (run-cp0 (lambda (x cp0) x)))
		 (let ([go
			(lambda ()
			  ;; Cannot do progress-dots now because we cannot nest engines.
			  (let loop ()
			    (if (stream-empty? stream)
				(unless (<= (regiment-verbosity) 0)
				  (fprintf (current-error-port) "Finished, dumped ~a stream elements.\n" count))
				(let ([elem (stream-car stream)])
				  (unless (equal? elem '#())
				    (write elem port)(newline port))
				  ;(set! pos (add1 pos))
				  (set! count (add1 count))
				  (set! stream (stream-cdr stream))
				  (loop)
				  )))
			  #;
			  (progress-dots
			   (lambda ()
			     ...)
			   50000000 
			   (lambda ()
			     (unless (<= (regiment-verbosity) 0)
			       (fprintf  (current-error-port) "  POS# ~a dumped...\n" pos))
			     )))])
		   (if (<= (regiment-verbosity) 0)		       
		       (go)
		       (time (go))
		       ))))
    ))

;============================================================
;;; Stream transformers.

;; Read the stream until it runs dry.  Had better be finite.
(define (stream-take-all s)
  (let stloop ((s s) (acc '()))
    (if (stream-empty? s) (reverse! acc)
	(stloop (stream-cdr s) (cons (stream-car s) acc)))))

(define (stream-map f s)
  ;; Don't make the output stream any lazier than the input:
  (let stream-map-loop ((s s))
    (cond 
     [(null? s) '()]
     [(pair? s)
      ;; Note, this is not tail recursive, but for streams the space between
      ;; promises should not be that large! (Or it's not a stream.)
      (cons (f (car s))
	    (stream-map-loop (cdr s)))]
     ;; Don't break a promise (yet):
     [else (delay (stream-map-loop (force s))
		  )])))

(define (stream-filter f s)
  ;; Don't make the output stream any lazier than the input:
  (let stream-filt-loop ((s s))
    (cond 
     [(null? s) '()]
     [(pair? s)
      ;; Note, this is not tail recursive, but for streams the space between
      ;; promises should not be that large! (Or it's not a stream.)
      (if (f (car s))
	  (cons (car s) (stream-filt-loop (cdr s)))
	  (stream-filt-loop (cdr s)))]
     ;; Don't break a promise (yet):
     [else (delay (stream-filt-loop (force s)))])))


;============================================================
;;; Stream constructors.

;; A stream of non-negative integers:
(define iota-stream
  (let loop ((i 0))
    (delay (cons i (loop (add1 i))))))

;============================================================
;;; Stream browsing.

;; By convention this doesn't print the unit value #() in dump mode.
(define browse-stream
  (case-lambda
    [(stream) (browse-stream stream pretty-print)]
    [(stream printer)
     (unless (stream? stream) (error 'browse-stream "This is not a stream: ~s" stream))
  ;; Now that we've got a stream we provide a little command
  ;; prompt and ask the user what we should do with it:
  (unless (<= (regiment-verbosity) 0)
    (printf "\nYou can now control the output stream, commands are:\n")
    (printf "     <n>          print n stream element, advance position\n")
    (printf "     <enter>      same as '1'\n")
    (printf "     print        print current stream element (in full), don't advance\n")
    (printf "     skip <n>     advance the stream, but don't print\n")
					;  (printf "     code         print the query that is executing\n")
    (printf "     dump <file>  dump whole stream to file (better not be infinite!)\n")
    (printf "     dump <fn> <n>   dump up to this many elements\n")
    (printf "     bindump <file>  assumes uint16s, if SigSegs, better be non-overlapping\n")
    (printf "     until <fun>  scrolls forward until an element satisfies the predicate\n")
    (printf "     profile      dump the profile to /tmp/pdump \n")
    (printf "     exit         exit\n\n")
    (flush-output-port (current-output-port)))

  (parameterize ([print-length 100]
		 [print-graph #t]
		 [print-level 5]
		 [print-vector-length #t])
    (let loop ([pos 0])
      (DEBUGASSERT (stream? stream))
      
      (printf "pos#~a: " pos)
      (flush-output-port (current-output-port))
      (let ((line (read-line (current-input-port))))
	(when line 
	  (match (port->slist (open-string-input-port line))
	    [() (guard (stream-empty? stream)) 
	     (printf "\nReached end of stream.\n")]
	    [() (printf "  ") (printer (stream-car stream))
	     (set! stream (stream-cdr stream)) (loop (add1 pos))]
	    [(,n) (guard (integer? n))
	     (mvlet ([(ls strm) (stream-take n stream)])
	       (newline)
	       (for-each (lambda (x)
			   (printf "     POS#~a = " pos)
			   (printer x)
			   (set! pos (add1 pos)))
		 ls)
	       (set! stream strm)
	       (loop pos))]
	    [(,print) (guard (memq print '(p pr pri prin print)))
	     (parameterize ([print-length 10000]
			    [print-level 200])
	       (printf "  ") (printer (stream-car stream)) (loop pos))]
	    [(,skip ,n) (guard (memq skip '(s sk ski skip)))
	     (time 

	      (set! stream (stream-drop n stream))
	      #;	      
	      (mvlet ([(_ strm) (stream-take n stream)])
		(set! stream strm))
	      )
	      (loop (+ pos n))]

          [(,dump ,file ,limit) 
	   (error 'browse-stream "limited dump not implemented")]

	  [(,dump ,file) (guard (memq dump '(d du dum dump)))
	   (stream-dump stream file)]

	  [(profile)  	   
	   (error 'profile "cond expand unimplemented...\n")
#;
	   (cond-expand
	    [chez 
	     (with-output-to-file "/tmp/pdump"
	       (lambda () 
		(parameterize ([print-level #f]
			       [print-length #f]
			       [print-graph #t])
		  (write (profile-dump))))
	      'replace)]
	    [(or plt larceny)
	     (error 'browse-stream "can't dump profile in this Scheme backend.")])]

	  ;; Wavescope-specific.	  
	  [(,bindump ,file) (guard (memq bindump '(b bi bin bind bindu bindum bindump)))
(error 'bindump "cond expand unimplemented...\n")
#;
	   (cond-expand
	    [chez 
	     (let ([failename (format "~a" file)])
	       (wavescript-language `(dump-binfile ,filename ,stream ,pos)))]
	    [else (error 'bindump "only implemented under Chez scheme")])]

	  [(,until ,predtext) (guard (memq until '(u un unt unti until)))
	   (let ([pred (reg:top-level-eval predtext)])
	     (unless (procedure? pred)
	       (error 'browse-stream "until must take a procedure: ~s" pred))
	     (let scrollloop ()
	       (if (stream-empty? stream)
		   (error 'browse-stream::until 
			  "reached end of stream before finding element satisfying predicate ~s" pred)
		   (let ([elem (stream-car stream)])
		     (if (pred elem)
			 (begin    
			   (printf " Found element satisfying predicate ~s:\n\n" pred)
			   (printf "     POS#~a = " pos)
			   (printer elem)
			   (newline)
			   (loop pos))
			 (begin 
			   (set! pos (add1 pos))
			   (set! stream (stream-cdr stream))
			   (scrollloop))
			 )))))]
	  
	  [(exit) (void)]
	  [,other 
	   (printf "Bad input.\n") (loop pos)]
	  )))
      ))]))

(define-testing test-streams 
  (default-unit-tester "streams.ss: Implementation of streams as lazy lists." 
  `(
    [(mvlet ([(x _) (stream-take 5 iota-stream)]) x)
     (0 1 2 3 4)]
    [(mvlet ([(x _) (stream-take 3 `(1 2 . ,(delay '(3))))]) x)
     (1 2 3)]

    [(mvlet ([(x _) (stream-take 10 (stream-filter even? iota-stream))]) x)
     (0 2 4 6 8 10 12 14 16 18)]
     
    
;; Having problems with errors in drscheme.
;    [(stream-take 5 `(1 2 . ,(delay '(3))))      error]
;    [(stream-cdr '()) error]
;    [(stream-cdr (delay 1)) error]
    [(stream-cdr (delay '(1))) ()]
    [(stream-car (delay '(1))) 1]
    ["stream-map"
     (mvlet ([(ls _) (stream-take 3 (stream-map add1 '(1 2 3)))]) ls)
     (2 3 4)]
    ["stream-map"
     (mvlet ([(ls _) (stream-take 3 (stream-map add1 
		        (stream-cons 1 (stream-cons 2 (stream-cons 3 (delay '()))))))]) ls)
     (2 3 4)]
    ["stream-filter" 
     (stream-take-all (stream-filter odd?
       (stream-cons 1 (stream-cons 2 (stream-cons 3 (delay '()))))))
     (1 3)]

    ["stream-append: Shouldn't hit the error."
     (stream-car 
      (stream-cdr 
       (stream-append (delay (append '(1 2) (delay (error 'test ""))))
		      '(3 4 5))))
     2]

    ["stream-append: Should hit the error."
     (stream-car 
      (stream-cdr 
       (stream-append (delay (cons 1 (delay (error 'test ""))))
		      '(3 4 5))))
     error]

    )))


) ; End module
  
  