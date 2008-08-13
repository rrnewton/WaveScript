#!r6rs

;;;; This implements a straightforward stream datatype -- thunks called repeatedly.

(library (ws util imperative_streams) 
  (export
   stream? 
   stream-empty-token list->stream
   stream-map stream-filter stream-take! stream-take-all!
   iota-stream browse-stream 
					
   stream-append stream-append-list

   test-imperative_streams
   )
  (import (rnrs) (rnrs eval) 
	  ;(ws common)
	  (ws compat compat)
	  (ws globals)
	  (ws util helpers)
	  (ws util reg_macros)
	  (ws util iu-match)
	  )


(define (stream? s) (or (procedure? s) (null? s)))

#;
(define-syntax stream-cons
  (syntax-rules ()
    [(_ a b) (let ([fired #f]) 
	       (lambda () 
		 (if fired (b) 
		     (begin (set! fired #t) a))))]))

;;====================================================================================================

(define (list->stream ls)  
  (lambda ()
    (if (null? ls)
	stream-empty-token
	(let ([x (car ls)])
	  (set! ls (cdr ls))
	  x))))

;; Append a stream (which should be finite) to another stream.
(define (stream-append s1 s2)
  (let ([switch #f])
    (lambda ()
      (if switch
	  (s2)
	  (let ([x (s1)])
	    (if (eq? x stream-empty-token)
		(begin (set! switch #t) (s2))
		x))))))
  
(define (stream-append-list ls s2)
  (let ([switch ls])
    (lambda ()
      (if (null? ls)
	  (s2)          
	  (let ([x (car ls)])
            (set! ls (cdr ls))
            x)))))

;; Take N elements from a stream
;; Tail recursive.
(define stream-take! 
  (lambda (n s)
    (let stloop ([n (fx- n 1)] [x (s)] [acc '()])
      (cond
       [(eq? x stream-empty-token)
	(error 'stream-take! "Stream ran out of elements before the end!")]
       [(fx= 0 n) (reverse! (cons x acc))]
       [else (stloop (fx- n 1) (s) (cons x acc))]))))

;; Drop N elements from a stream.
(define stream-drop
  (lambda (n s)
    (let stloop ((n n))           
      (cond
       [(fx= 0 n) (void)]
       [(eq? (s) stream-empty-token)
	(error 'stream-drop "Stream ran out of elements before the end!")]
       [else (stloop (fx- n 1))]))))

;============================================================
;;; Stream transformers.

;; Read the stream until it runs dry.  Had better be finite.
(define (stream-take-all! s)
  (let stloop ([x (s)] [acc '()])
    (if (eq? x stream-empty-token)
	(reverse! acc)
	(stloop (s) (cons x acc)))))

(define (stream-map f s) (lambda () (f (s))))

(define (stream-filter f s)
  (lambda () 
    (let loop ([x (s)])
      (cond 
       [(eq? x stream-empty-token)
	stream-empty-token]
       [(f x) x]
       [else (loop (s))]
       ))))

(define stream-empty-token (gensym "End-Of-Stream"))


;============================================================
;;; Stream constructors.

;; A stream of non-negative integers:
(define (iota-stream)
  (let ((i -1))
    (lambda ()
      (set! i (fx+ 1 i))
      i)))

;============================================================
;;; Stream browsing.

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
;    (printf "     print        print current stream element (in full), don't advance\n")
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
      (let ((line (read-line (current-input-port))))
	(when line 
	  (match (port->slist (open-string-input-port line))
	    [() (guard (eq? stream stream-empty-token))
	     (printf "\nReached end of stream.\n")]
	    [() (printf "  ") 
	     (printer (stream))	     
	     ;(inspect (stream))
	     (loop (add1 pos))]
	    [(,n) (guard (integer? n))
	     (let ([ls (stream-take! n stream)])
	       (for-each (lambda (x)
			   (printf "     POS#~a = " pos)
			   (printer x)
			   (set! pos (add1 pos)))
		 ls)	       
	       (loop pos))]

	    [(,skip ,n) (guard (memq skip '(s sk ski skip)))
	     (time (stream-take! n stream))
	      (loop (+ pos n))]

	    [(,dump ,file ,limit) 
	     (error 'browse-stream "limited dump not implemented")]

	  [(,until ,predtext) (guard (memq until '(u un unt unti until)))
	   (let ([pred (eval predtext)])
	     (unless (procedure? pred)
	       (error 'browse-stream "until must take a procedure: ~s" pred))
	     (let scrollloop ()
	       (if (eq? stream stream-empty-token)
		   (error 'browse-stream::until 
			  "reached end of stream before finding element satisfying predicate ~s" pred)
		   (let ([elem (stream)])
		     (when (eq? stream stream-empty-token)
		       (error 'browse-stream::until 
			      "reached end of stream before finding element satisfying predicate ~s" pred))
		     (if (pred elem)
			 (begin    
			   (printf " Found element satisfying predicate ~s:\n\n" pred)
			   (printf "     POS#~a = " pos)
			   (printer elem)
			   (newline)
			   (loop pos))
			 (begin 
			   (set! pos (add1 pos))			  
			   (scrollloop))
			 )))))]
	  
	  [(exit) (void)]
	  [,other 
	   (printf "Bad input.\n") (loop pos)]
	  )))
      ))
     ]))



;(stream-take 10 (stream-map add1 (stream-filter odd? (iota-stream))))
;(2 4 6 8 10 12 14 16 18 20)

(define test-imperative_streams
  (default-unit-tester "imperative_streams.ss: Implementation of streams as thunks" 
    `(
    [(',stream-take! 5 (',iota-stream))
     (0 1 2 3 4)]
    [(',stream-take! 10 (',stream-filter even? (',iota-stream)))
     (0 2 4 6 8 10 12 14 16 18)]

    ["stream-map"
     (',stream-take! 3 (',stream-map add1 (',list->stream '(1 2 3))))
     (2 3 4)]

    ["stream-filter" 
     (',stream-take-all!
      (',stream-filter odd?
	(',list->stream '(1 2 3))))
     (1 3)]

    )))

) ; End module
