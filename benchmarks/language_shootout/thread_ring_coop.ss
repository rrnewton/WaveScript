
;;;; [2009.02.09]
;;;; For reference, here is a scheme implementation of thread ring using
;;;; cooperative multithreading (continuations).

(eval-when (compile eval load) (optimize-level 3))

(let ()
  
  (define N 503) 
  (define steps (string->number (car (command-line-arguments))))

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
	(set! globalqueue (qsnoc who globalqueue))
	;(printf "   global queue updated ~a\n" globalqueue)
	)))

  ;; This is the continuation for returning to the scheduler, set up later:
;  (define schedk 'uninit)

  ;; Do an initial run of the workfun:
#;
  (define (spawn fn n)
    (call/cc (lambda (return)
	       (fluid-let ((schedk return))
		 (fn n (lambda () (call/cc schedk))))
	       )))

  ;; This processes the queue of threads with active messages.
  (define (mainloop)    
#;
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
    (vector-set! enqueued? who #f) ;; Reenable it
    ;; Invoke it, it returns a new queue, updates own continuation.
    
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
	       (call/1cc (lambda (backtosched)
			  ((vector-ref threads who)
			   who (lambda ()
				 (call/cc backtosched))))))
	    ;(printf " INSTALLED new continuation for ~a  \n" who)
	    ;; If we're passing through the first time we still have a message to process.
	    (unless (vector-ref already-called who)
	     ; (printf "...bounce...\n")
	      (vector-set! already-called who #t)
	      ((vector-ref threads who) msg))
	    )))
    ;(printf "   finishd, new queue ~a\n" globalqueue)    
    (mainloop))

  
  (define (workfun who recv)
    ;(printf "  ** Calling ~a for first time.. ~a\n" who recv)
    (let workerloop ((msg (recv)))
      (define next (if (fx= N who) 1 (fx+ 1 who)))
      ;(printf "  ** Done Recv, message ~a, passing from ~a to ~a\n" msg who next)
      (when (fxzero? msg) (printf "~a\n" who) (exit))
      (emit! next (fx- msg 1))
      (workerloop (recv))))

  (printf "Running ~a threads for ~a steps \n" N steps)
  ;; Spawn the threads:
  (let spawnloop ((n N))
    (unless (zero? n)
      (vector-set! threads n workfun)
      (spawnloop (fx- n 1))))
    
  (emit! 1 steps)
  (mainloop)
)
