
;;;; [2009.02.09]
;;;; For reference, here is a scheme implementation of thread ring using
;;;; cooperative multithreading (continuations).

(let ()
  
  (define N 9)
  ;(define N (string->number (car (command-line-arguments))))

  ;; Double linked lists for queues:
  ;; TODO: First just using singly linked lists.
  (begin
    (define qnull '())
    (define qcons cons)
    (define (qsnoc x dll) (append dll (list x)))
    (define qcar car)
    (define qcdr cdr)
    (define (qrac dll) (car (last-pair dll))))

  ;; Because this is very simple, just using disjoint arrays to store
  ;; the fields rather than an array of structures.
  (define mailboxes (make-vector (add1 N) qnull))
  (define threads   (make-vector (add1 N))) ;; Continuations
  (define enqueued? (make-vector (add1 N) #f)) ;; Is the thread queued?

  ;; Passes data, returns a new queue.
  (define (emit who what queue)
    (vector-set! mailboxes who 
		 (qsnoc what (vector-ref mailboxes who)))
    (if (vector-ref enqueued? who) queue	
	(begin 
	  (vector-set! enqueued? who #t)
	  (qsnoc who queue))))

  (define (spawn fn n)
    (call/1cc 
     (lambda (jumpout)
       (let ((queue (call/cc jumpout)))
	 (printf " Thread ~a woke up, mailbox: ~a, queue ~a\n" n (vector-ref mailboxes n) queue)
	 (let* ((msgs (vector-ref mailboxes n))
		(msg  (qcar msgs)))
	   (vector-set! mailboxes n (qcdr msgs)) ;; Pop the message

	   ;; Process our incoming message
	   (fn n msg queue 
	       (lambda (q2) 
		 (call/cc (lambda (newk) 
			    (set! __ newk)))))
	  
	   ;; If this were more sophisticated, we would need to update
	   ;; our continuation based on where the computation happened
	   ;; to yield.  But in this first draft, it just fires atomically.	   	   
	   )))))

  (define (workfun who msg queue yield)
    (define next (if (fx= N (fx+ 1 who)) 1 (fx+ 1 who)))
    (printf "  Running workfun, passing from ~a to ~a\n" who next)
    (let workerloop ()
      ;; Pass on the same message:
      (let ([nuque (emit next msg queue)])
	(printf "  Yielding new queue ~a\n" nuque)
	(yield nuque)
	(workerloop)))
    )

  ;; This processes the queue of threads with active messages.
  (define (mainloop queue)
    (define __ (printf "Looping, with queue: ~s\n" queue))
    (define who (qcar queue))
    ;; Invoke it, it returns a new queue, updates own continuation.
    (define nuque ((vector-ref threads who) (qcdr queue))) ;; Also pop the queue.
    (printf "   returned new queue ~a\n" nuque)
    (mainloop queue)
   )

  (printf "Running ~a\n" N)
  ;; Spawn the threads:
  (let spawnloop ((n N))
    (unless (zero? n)
      (vector-set! threads n (spawn workfun n))
      (spawnloop (fx- n 1))))
    
  (mainloop (emit 1 'token qnull))
)
