;;;; This implements a straightforward stream datatype -- tail-delayed pairs.

;;;; Currently 'include'd by helpers.ss

;;;; TODO: Could consider an implementation that mutates into a list
;;;; as the promises are evaluated.  Good for streams that are
;;;; processed multiple times.

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
(define (stream? s) (or (procedure? s) (null? s)))

(define-syntax stream-cons
  (syntax-rules ()
    [(_ a b) (let ([fired #f]) 
	       (lambda () 
		 (if fired (b) 
		     (begin (set! fired #t) a))))]))

;;a====================================================================================================

;; NOTE: Double delay for append:
;; Appends list to stream... not stream to stream!
; (define-syntax stream-append-list
;   (syntax-rules ()
;     [(_ args ... tail) (delay (append args ... (delay tail)))]))

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

;; Take N elements from a stream
;; Tail recursive.
(define stream-take 
  (lambda (n s)
    (let stloop ([n (fx- n 1)] [x (s)] [acc '()])
      (cond
       [(eq? x stream-empty-token)
	(error 'stream-take "Stream ran out of elements before the end!")]
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
(define (stream-take-all s)
  (let stloop ([x (s)] [acc '()])
    (if (eq? x stream-empty-token)
	(reverse! acc)
	(stloop (s) (cons x acc)))))

(define (stream-map f s) (lambda () (f (s))))

(define (stream-filter f s)
  (lambda () 
    (let loop ([x (s)])
      (if (f x) x
	  (loop (s))))))

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

;(stream-take 10 (stream-map add1 (stream-filter odd? (iota-stream))))
;(2 4 6 8 10 12 14 16 18 20)
