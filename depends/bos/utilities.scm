; utilities.scm is -*- Scheme -*-
;
; Bryan's Object System
;
; (C) 1994 Bryan O'Sullivan <bosullvn@maths.tcd.ie>

; GET-ARG gives us much the same facilities as Common Lisp's keyword
; argument gumph.  You can use it in the following manner:
;   (define (mumble a b . args)
;     ...
;     (foo (get-arg args 'arg-name default-value))
;     ...)
;   ...
;   (mumble 1 2 'x 4 'y 7)

;; [2008.02.10] RRN applying some minor changes.

(define (get-arg arg-list arg . default)
  (let loop ([arg-list arg-list])
    (cond
     ((null? arg-list)
      (if (null? default)
	  (error 'get-arg "no such arg: ~s" arg)
	  (car default)))
     ((equal? (car arg-list) arg)  (cadr arg-list))
     (else (loop (cddr arg-list))))))

; Print an object nicely to an output port.

(define (write-object object . output-port)
  (let* ((class (vector-ref object *class-offset*))
	 (length (- (vector-length class) *slot-names-offset*)))
    (apply display (cons "#{Object " output-port))
    (apply display (cons (vector-ref class *name-offset*) output-port))
    (apply display (cons ": " output-port))
    (if (> length 0)
	(begin
	  (set! length (- length 1))
	  (do ((offset 0 (+ offset 1)))
	      ((= offset length))
	    (apply write-char (cons #\( output-port))
	    (apply display (cons (vector-ref class (+ offset *slot-names-offset*))
				 output-port))
	    (apply display (cons ": " output-port))
	    (apply display (cons (vector-ref object (+ offset *slots-offset*))
				 output-port))
	    (apply display (cons ") " output-port)))
	  (apply write-char (cons #\( output-port))
	  (apply display (cons (vector-ref class (+ length *slot-names-offset*))
			       output-port))
	  (apply display (cons ": " output-port))
	  (apply display (cons (vector-ref object (+ length *slots-offset*))
			       output-port))
	  (apply write-char (cons #\) output-port)))
	(apply display (cons "(no members)" output-port)))
    (apply write-char (cons #\} output-port))))

(define (write-class class . output-port)
  (apply display (cons "#{Class " output-port))
  (apply display (cons (vector-ref class *name-offset*) output-port))
  (if (not (eq? class <class>))
      (begin
	(apply display (cons ": " output-port))
	(let loop ((superclasses (vector-ref class *superclasses-offset*)))
	  (if (not (null? superclasses))
	      (if (null? (cdr superclasses))
		  (apply display (cons (vector-ref (car superclasses) *name-offset*)
				       output-port))
		  (begin
		    (apply display (cons (vector-ref (car superclasses) *name-offset*)
					 output-port))
		    (apply write-char (cons #\space output-port))
		    (loop (cdr superclasses))))))))
  (apply display (cons ", " output-port))
  (let ((length (vector-length class)))
    (if (= length *slot-names-offset*)
	(apply display (cons "no members" output-port))
	(begin
	  (set! length (- length 1))
	  (do ((offset *slot-names-offset* (+ offset 1)))
	      ((= offset length))
	    (apply display (cons (vector-ref class offset) output-port))
	    (apply write-char (cons #\space output-port)))
	  (apply display (cons (vector-ref class length) output-port)))))
  (apply write-char (cons #\} output-port)))

(define (class? thing)
  (and (vector? thing)
       (>= (vector-length thing) *slot-names-offset*)
       (list? (vector-ref thing *specialised-methods-offset*))
       (list? (vector-ref thing *superclasses-offset*))
       (string? (vector-ref thing *name-offset*))))

(define (object? thing)
  (and (vector? thing)
       (>= (vector-length thing) *slots-offset*)
       (class? (vector-ref thing *class-offset*))))