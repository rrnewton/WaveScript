
;;;; [2009.02.09]
;;;; For reference, here is a scheme implementation of thread ring using
;;;; cooperative multithreading (continuations).

(let ()
  
  (define N 3)
  ;(define N (string->number (car (command-line-arguments))))

  ;; Double linked lists for queues:
  ;; TODO: First just using singly linked lists.
  (begin
    (define qnull '())
    (define qnull? null?)
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
  (define already-called (make-vector (add1 N) #f))

  (define globalqueue qnull)

  ;; Passes data, returns a new queue.
  (define (emit! who what)
    (vector-set! mailboxes who 
		 (qsnoc what (vector-ref mailboxes who)))
    (unless (vector-ref enqueued? who)
      (begin 
	(vector-set! enqueued? who #t)
	(set! globalqueue (qsnoc who globalqueue)))))

  ;; This is the continuation for returning to the scheduler, set up later:
;  (define schedk 'uninit)

  ;; Do an initial run of the workfun:
#;
  (define (spawn fn n)
    (call/cc (lambda (return)
	       (fluid-let ((schedk return))
		 (fn n (lambda () (call/cc schedk))))
	       )))
  
  (define (workfun who recv)
    (printf "  ** Calling ~a for first time.. ~a\n" who recv)
    (let workerloop ((msg (recv)))
      (define next (if (fx= N who) 1 (fx+ 1 who)))
      (printf "  ** Done Recv, now passing ~a from ~a to ~a\n" msg who next)
      (emit! next msg)
      (workerloop (recv))))

  (define count 0)

  ;; This processes the queue of threads with active messages.
  (define (mainloop)    
    (define __ (begin 
		 (set! count (add1 count))
		 (when (= count 10) (printf "ENOUGH\n") (exit))
		 (printf "Looping, with queue: ~s, mailboxes ~s\n" globalqueue
			 mailboxes
			 #;
			 (if (qnull? globalqueue)   #f
			   (vector-ref mailboxes (qcar globalqueue))))))
    (define who (qcar globalqueue))
    (set! globalqueue (qcdr globalqueue))
    ;; Invoke it, it returns a new queue, updates own continuation.
    ;((vector-ref threads who) (qcdr globalqueue))
    
    ;; Pop the first message for the selected thread:
    (let* ((msgs (vector-ref mailboxes who))
	   (msg  (qcar msgs)))

      (vector-set! mailboxes who (qcdr msgs)) ;; Pop the message

      ;; If we've been called before, just charge through, otherwise initialize.
      ;; We could use multi-value continuations to get around this weirdness.
      (if (vector-ref already-called who)
	  ((vector-ref threads who) msg) ;; This will transfer control back to scheduler
	  (begin 
	    ;; Run the thread, give it a blocking recv function.
	    ;; This will generate N different continuations for jumping back to scheduler.
	    (vector-set! threads who
	       (call/cc (lambda (backtosched)
			  ((vector-ref threads who)
			   who (lambda ()
				 (call/cc backtosched))))))
	    (printf " INSTALLED new continuation for ~a  \n" who)
	    ;; If we're passing through the first time we still have a message to process.
	    (unless (vector-ref already-called who)
	      (printf "...bounce...\n")
	      (vector-set! already-called who #t)
	      ((vector-ref threads who) msg))
	    
	    ))



      
      
      
; (printf " Thread ~a woke up, mailbox: ~a, queue ~a\n" n 
; 	(vector-ref mailboxes n) globalqueue)

      ;; If this were more sophisticated, we would need to update
      ;; our continuation based on where the computation happened
      ;; to yield.  But in this first draft, it just fires atomically.	   	   
      )
    (printf "   finishd, new queue ~a\n" globalqueue)    
    (mainloop))

  (printf "Running ~a\n" N)
  ;; Spawn the threads:
  (let spawnloop ((n N))
    (unless (zero? n)
      ;(vector-set! threads n (spawn workfun n))
      (vector-set! threads n workfun)
      (spawnloop (fx- n 1))))
    
  (emit! 1 'token)
  (mainloop)
)
