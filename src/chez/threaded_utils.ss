
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
     ;WAITING
     )
  
  (import chez_constants)



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




;(define par-map map)
;(define (par-list . thunks)  (map (lambda (th) (th)) thunks))

;; ================================================================================
(define-syntax par
  (syntax-rules ()
    [(par e ...) (par-list (lambda () e) ...)]
    ))

;; A bit inefficient, defined in terms of par-list:
(define (par-map f ls) (apply par-list (map (lambda (x) (lambda () (f x))) ls)))

;; Inefficient version, forks threads on demand:
;; ACK: segfaults under stress:
#;
(begin 
  (define (init-par num-cpus) (void))
  (define (shutdown-par) (void))

  (define (par-list . thunks)
    ;; Could be made slightly more efficient by not using generic queues.
    (define q* (map (lambda (th) 
		      (let ([q (make-bq 1)])
			(fork-thread (lambda () (enqueue! q (th))))
			q))
		 thunks))
    (fprintf (current-error-port) "\n  Forking ~s threads.\n" (length thunks))
    (map dequeue! q*)))


;; Better, maintains worker threads and a job queue.
;; NOT GONNA WORK: DELETEME
#;
(begin 
  ;; This is a pool of job queues waiting for work.
  (define thread-pool #f)
  (define mut (make-mutex))
  (define not-finished #t)
  (define (make-worker)
    (define inq (make-bq 1)) ;; Incoming work
    (define outq (make-bq 1)) ;; Outgoing results
    (define (go) 
      ;; Add ourselves back to the thread-pool:
      (with-mutex mut 
	(set! thread-pool (cons outq thread-pool)))
      (let ([work (dequeue! inq)])
	;; This dies when it gets a #f.
	(when work 
	  (enqueue! outq (work))
	  (go))))
    (fork-thread go)
    (vector inq outq))
  
  (define (init-par num-cpus) 
    ;; Clear any existing junk:
    (with-mutex mut 
      (set! thread-pool '())
      (do ([i 0 (fx+ i 1)]) ([= i num-cpus] (void))      
	(set! thread-pool (cons (make-worker) thread-pool)))))
  (define (shutdown-par)
    ;; Any available threads are terminated.
    ;; This doesn't wait for them to really die... there's currently
    ;; no way to do that.
    (with-mutex mut
      (for-each (lambda (q) (enqueue! q #f)) thread-pool)
      (set! thread-pool #f)))

  (define (par-list . thunks)
    (let par-list-loop ([waiting '()]) ;; Computations we're waiting on.
      ;; UNSAFE?: is it safe to do this outside the mutex?
      (if (with-mutex mut (null? thread-pool))       ;; Null is an unboxed value...
	  ;; If no threads are available, start waiting.

	  (dequeue! (car waiting))

	  ;; Otherwise, pull the first available thread and feed it work.
	  (enqueue! (with-mutex mut 
		      (let ([q (car thread-pool)])
			(set! thread-pool (cdr thread-pool))))
		    th)
	  ))
    

    (map (lambda (th)
	   ;; UNSAFE?: is it safe to do this outside the mutex?
	   ;; Null is an unboxed value...
	   (if (with-mutex mut (null? thread-pool))
	       ;; If no threads are available, compute here.
	       (th)
	       ;; Otherwise, pull the first available thread and feed it work.
	       (enqueue! (with-mutex mut 
			   (let ([q (car thread-pool)])
			     (set! thread-pool (cdr thread-pool))))
			 th)
	       )))))


#;
(let ()
  (define (l1 x) (unless (zero? x) (l1 (sub1 x))))
  (define (l2 x) (unless (zero? x) (l2 (sub1 x))))
  ;(time (rep 10000 (par (l1 10000) (l2 10000))))
  (time (par (l1 10000000) (l2 10000000)))
  (time (list (l1 10000000) (l2 10000000)))
  )

;; Better, maintains worker threads and a job queue.
;; [2007.04.04] Occasionally deadlocks currently.
(begin 
  ;; We need to put a request for work in a job queue, but get a ticket
  ;; back that we can use to both (1) wait on the computation's
  ;; completion (2) retrieve the value produced by the computation.
  (define not-finished #t)
  (define tickets (make-bq 10)) ;; Tickets for work.
  
  ;; DEBUG:
  (define WAITING '())

  (define not-computed (gensym "not-computed"))
  (define mut (make-mutex)) ;; Just for printing right now.
  (define (print . args) (with-mutex mut (apply printf args) (flush-output-port)))
  ;(define (print . args) (apply printf args))
  ;(define (print . args) (void))
  (define-record job (work)
    ([mutex (make-mutex)]  ;; TODO: This doesn't really need its own mutex.
     [ready (make-condition)]
     [val  not-computed]))
  (define (get-job-result j)
    ;; DEBUG!!! BRINGING THIS MUTEX OUT HERE:
    (with-mutex (job-mutex j)
      (let ([result (job-val j)])
	(if (eq? not-computed result)
	    (begin ;with-mutex (job-mutex j)
	      (begin ;with-mutex (job-mutex j)
		(condition-wait (job-ready j) (job-mutex j)))
	      ;; Now it must be ready.
	      (job-val j))
	    result))))
  (define (make-ticket jobqueue)
    (lambda (work)
      (let* ([j (make-job work)])
	(enqueue! jobqueue j)	
	j)))
  (define make-worker
    (let ([counter 0])
      (lambda ()
	(define id (begin (set! counter (add1 counter)) counter))
	(define inq (make-bq 1)) ;; Incoming work
	(define (go) 
	  ;; Issue a ticket for the next piece of work.
	  (enqueue! tickets (make-ticket inq))
	  ;; Someone took the ticket, lets get their work:
	  (print "  Thread ~s waiting for work... (also waiting: ~s)\n" id WAITING)
	  (with-mutex mut (set! WAITING (cons id WAITING)))
	  (let ([j (dequeue! inq)]) 
	    (with-mutex mut (set! WAITING (remq id WAITING)))
	    (print "  Thread ~s  got work!! ~s\n" id j)
	    (when not-finished	      
	      (let* ([v ((job-work j))]) ;; DOES THE WORK!!!!  MAY CALL PAR AGAIN!
		;; Inform the client that their job is done.
		(with-mutex (job-mutex j)
		  (set-job-val! j v)
		  (condition-signal (job-ready j)))
		(go)))))
	(fork-thread go))))
  (define (init-par num-cpus) 
    (do ([i 0 (fx+ i 1)]) ([= i num-cpus] (void))      
      (make-worker)))
  (define (shutdown-par) (set! not-finished #f))
  (define (par-list . thunks)
    (let ([jobs (map (lambda (th) 		      
		       (print " SRC: Taking a ticket...\n") 
		       (let ([tick (dequeue! tickets)]) ; Pull a ticket.			 
			 (tick th))) ; Fill it out, get a job object back.
		  thunks)])
      (print " SRC: Jobs out, now collect results\n")
      ;; Now get all those results:
      (map get-job-result jobs)
      ;jobs
      ))
)


;; Trying to get rid of deadlock problems:
#;
(begin 
  ;; We need to put a request for work in a job queue, but get a ticket
  ;; back that we can use to both (1) wait on the computation's
  ;; completion (2) retrieve the value produced by the computation.
  (define not-finished #t)
  (define tickets '()) ;; Tickets for work.
  
  ;; DEBUG:
  (define WAITING '())

  (define not-computed (gensym "not-computed"))
  (define mut (make-mutex)) ;; Just for printing right now.
  (define (print . args) (with-mutex mut (apply printf args) (flush-output-port)))
  ;;(define (print . args) (apply printf args))
  ;;(define (print . args) (void))
  (define-record job (work)
    ([mutex (make-mutex)]  ;; TODO: This doesn't really need its own mutex.
     [ready (make-condition)]
     [val  not-computed]))
  (define (get-job-result j)
    ;; DEBUG!!! BRINGING THIS MUTEX OUT HERE:
    (begin ;with-mutex (job-mutex j)
      (let ([result (job-val j)])
	(if (eq? not-computed result)
	    (begin ;with-mutex (job-mutex j)
	      (with-mutex (job-mutex j)
		(condition-wait (job-ready j) (job-mutex j)))
	      ;; Now it must be ready.
	      (job-val j))
	    result))))
  (define (make-ticket jobqueue)
    (lambda (work)
      (let* ([j (make-job work)])
	(enqueue! jobqueue j)	
	j)))
  (define make-worker
    (let ([counter 0])
      (lambda ()
	(define id (begin (set! counter (add1 counter)) counter))
	(define inq (make-bq 1)) ;; Incoming work
	(define (go) 
	  ;; Issue a ticket for the next piece of work.
	  (with-mutex mut (set! tickets (cons (make-ticket inq) tickets)))
	  ;; Someone took the ticket, lets get their work:
	  (print "  Thread ~s waiting for work... (~s tickets available, also waiting: ~s)\n" 
		 id (length tickets) WAITING)
	  (with-mutex mut (set! WAITING (cons id WAITING)))
	  (let ([j (dequeue! inq)]) 
	    (with-mutex mut (set! WAITING (remq id WAITING)))
	    (print "  Thread ~s  got work!! ~s\n" id j)
	    (when not-finished
	      (let* ([v ((job-work j))])
		;; Inform the client that their job is done.
		(set-job-val! j v)
		(condition-signal (job-ready j))
		(go)))))
	(fork-thread go))))
  (define (init-par num-cpus) 
    (do ([i 0 (fx+ i 1)]) ([= i num-cpus] (void))      
      (make-worker)))
  (define (shutdown-par) (set! not-finished #f))
  (define (par-list . thunks)
    (let ([jobs (map (lambda (th) 		      
		       (print " SRC: Taking a ticket...\n") 
		       (let ([tick (dequeue! tickets)]) ; Pull a ticket. 
			 (tick th))) ; Fill it out, get a job object back.
		  thunks)])
      (print " SRC: Jobs out, now collect results\n")
      ;; Now get all those results:
      (map get-job-result jobs)
      ;jobs
      ))
)




;; Basic work-stealing...???





;=============================================================================
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



) ;; End module.
