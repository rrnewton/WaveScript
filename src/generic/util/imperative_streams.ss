;;;; This implements a straightforward stream datatype -- thunks called repeatedly.

(module imperative_streams mzscheme
  (require "../constants.ss"
	   "helpers.ss"
	   "reg_macros.ss"
	   ;"../../plt/chez_compat.ss"
	   "../../plt/iu-match.ss"
	   )
  (provide 
   stream? live-stream? stream-empty? stream-cons stream-car stream-cdr
   stream-map stream-filter stream-take stream-take-all 
   iota-stream stream-append browse-stream ;random-stream 
   stream-append-list

   test-streams
   )
  (chezimports)


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

) ; End module
