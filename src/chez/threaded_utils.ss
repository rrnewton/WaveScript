
;; [2006.07] This contains some miscellaneous stuff related to
;; threads.  Only relevent to the pthread-based version of Chez
;; Scheme.

(chez:module threaded_utils
    (stream-parmap 
     make-bq enqueue! dequeue!   bq-i bq-vec bq-mutex bq-ready bq-room

     init-par  ;; Run initialization code (fork threads, etc)
     par       ;; Evaluate expressions in parallel, return list of values
     par-list  ;; Evaluate a list of thunks
     par-map   ;; Apply function to list in parallel

     ;async-par ;; A version of 'par' that returns immediately.
     ;sync      ;; The corresponding call to wait for an async-par to finish.
     )
  
  (import chez_constants)

;; A multicore version of stream-map:
;;
;; NOTE: The computations have to be heavyweight for the parallelism
;; to beat the extra communication cost.
(define (stream-parmap f s)
  ;; Could be made slightly more efficient by not using generic queues.
  (define outq (make-bq 1))
  (define inq  (make-bq 1))
  (define finished #f)
  ;; This thread will compute the function f
  (define (worker)
    (enqueue! inq (f (dequeue! outq)))
    (if (not finished) (worker)))
  ;; Create the initial suspension.  When popped it will peek-2
  (delay (cond
	  [(stream-empty? s) '()]
	  [(stream-empty? (stream-cdr s)) (list (f (stream-car s)))]
	  ;; Initialize by sending the first element to the worker.
	  [else 
	   (fork-thread worker)
	   (enqueue! outq (stream-car s))
	   ;; Now begin our main loop to move through the stream.
	   (let loop ((s (stream-cdr s)))
	     (cond
	      ;; If we've hit the end we just wait for the one in-flight to finish:
	      [(stream-empty? s) (let ([x (dequeue! inq)]) (set! finished #t) (list x))]
	      [else (enqueue! outq (stream-car s))
		    (cons (dequeue! inq) (delay (loop (stream-cdr s))))]))])))



;(define par-map map)
;(define (par-list . thunks)  (map (lambda (th) (th)) thunks))

;; ================================================================================
(define-syntax par
  (syntax-rules ()
    [(par e ...) (par-list (lambda () e) ...)]
    ))

;; A bit inefficient:
(define (par-map f ls) (apply par-list (map (lambda (x) (lambda () (f x))) ls)))

;; Inefficient version, forks threads on demand:
(begin 
  (define (init-par num-cpus) (void))

  (define (par-list . thunks)
    ;; Could be made slightly more efficient by not using generic queues.
    (define q* (map (lambda (th) 
		      (let ([q (make-bq 1)])
			(fork-thread (lambda () (enqueue! q (th))))
			q))
		 thunks))
    (fprintf (current-error-port) "\n  Forking ~s threads.\n" (length thunks))
    (map dequeue! q*)))

#;
;; Better, maintains worker threads and a job queue.
;; UNFINISHED
(begin 
  (define thread-pool '())
  (define (init-par num-cpus) 
    (do ([i 0 (fx+ i 1)]) ([= i num-cpus] (void))      
      (set! thread-pool (cons 
			 (MAKEWORKER )
			 thread-pool)
	    )))

  ;(define (par-map f ls) (apply par-list (map (lambda (x) (lambda () (f x))) ls)))
  (define (par-list . thunks)
    ;; If no threads are available, compute here.
    
    (map dequeue! q*)))


;; TODO: What other algorithms might be effective?  Work stealing?



;=============================================================================
;;; Bounded queues, from http://www.scheme.com/csug7/threads.html#./threads:h7

;; TODO: This has a stack behavior... should make it a Queue!!

(define-record bq (i)
  ([vec (make-vector i)]
   [mutex (make-mutex)]
   [ready (make-condition)]
   [room (make-condition)]))
(define enqueue!
  (lambda (bq item)
    (let ([mutex (bq-mutex bq)])
      (with-mutex mutex
        (let loop ()
          (when (zero? (bq-i bq))
            (condition-wait (bq-room bq) mutex)
           ; we reacquire the mutex when we wake up, but some other
           ; thread may beat us to the punch
            (loop)))
        (let ([i (- (bq-i bq) 1)])
          (vector-set! (bq-vec bq) i item)
          (set-bq-i! bq i)
          (condition-signal (bq-ready bq)))))))
(define dequeue!
  (lambda (bq)
    (let ([mutex (bq-mutex bq)])
      (with-mutex mutex
        (let loop ()
          (when (= (bq-i bq) (vector-length (bq-vec bq)))
            (condition-wait (bq-ready bq) mutex)
           ; we reacquire the mutex when we wake up, but some other
           ; thread may beat us to the punch
            (loop)))
        (let ([i (bq-i bq)])
          (let ([item (vector-ref (bq-vec bq) i)])
            (set-bq-i! bq (+ i 1))
            (condition-signal (bq-room bq))
            item))))))


) ;; End module.
