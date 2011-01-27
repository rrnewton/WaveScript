
;;;; [2009.02.09]
;;;; For reference, here is a scheme implementation of thread ring using
;;;; cooperative multithreading (continuations).

(eval-when (compile eval load) (optimize-level 2))

(begin ;let ()

  (define-syntax DBG (syntax-rules () [(_ e) (void)]))
  
  (define N 503) 

  ;; Double linked lists for queues:
  ;; TODO: First just using singly linked lists.

  (begin
    (define-record-type queue (fields (mutable hd) (mutable tl) (mutable vec)))
    (trace-define (qnull) (make-queue 0 0 (make-vector 10 'uninit)))
    (define (qnull? q) (error 'qnull? "unimplemented"))
    (define (qsnoc x q) 
      (define tl (fx+ 1 (queue-tl q)))
      ;(if (fx>= t (vector-length )))
      (vector-set! (queue-vec q) 0 x))
    (trace-define (qcar q) (vector-ref (queue-vec q) (queue-hd q)))
    (trace-define (qcdr q) (queue-hd-set! q 0))
    (define (qrac q) (vector-ref (queue-vec q) (queue-tl q))))

#;
  (begin
    (define (qnull) '())
    (define qnull? null?)
    (define (qsnoc x dll) (append dll (list x)))
    (define qcar car)
    (define qcdr cdr)
    (define (qrac dll) (car (last-pair dll))))

  ;; Because this is very simple, just using disjoint arrays to store
  ;; the fields rather than an array of structures.
  (define mailboxes (make-vector (add1 N) (qnull)))
  (define threads   (make-vector (add1 N))) ;; Continuations
  (define enqueued? (make-vector (add1 N) #f)) ;; Is the thread queued?

  (define globalqueue (qnull))

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
  (define schedk #f)
  (define alldone exit)
  (define (receiver n) ;; instantiated for each thread
    (lambda ()
      (define msg 
	;; ODD.  Under chez 7.5 call/1cc goes SLOWER than call/cc here: 
	(call/cc (lambda (k)
		 (DBG (printf " -- recv called.. posting cont ~a for ~a jumping back to sched\n " k n))
		 (vector-set! threads n k)
		 (schedk 'nilmsg) ;; Don't need to send any info to the scheduler
		 )))
      (DBG (printf " @@ Returning control to recv call ~s, message ~s \n " n msg))
      msg
      ))

  ;; Do an initial run of the workfun:
  (define (spawn fn n)
    (call/1cc (lambda (return)
		(begin ;fluid-let ((schedk return))
		  (set! schedk return)
		  (fn n (receiver n))		    
		  )))
    (set! schedk #f)
    ;(printf ".....finished first call ~a\n" n)
    (vector-ref threads n)
    )

  ;; This processes the queue of threads with active messages.
  (define (mainloop)    

    ;(printf "MAINLOOP schedk init ~a\n" schedk)

    ;; Initialize the schedk that we jump back to in the future.
    (unless schedk 
      (call/cc (lambda (k) (DBG (printf " SETTING SCHEDK ~s\n" k)) (set! schedk k)))
      (DBG (printf "jumped back in\n")))

    (DBG (printf "Looping, with queue: ~s, mailboxes ~s\n" globalqueue
			 mailboxes
			 #;
			 (if (qnull? globalqueue)   #f
			   (vector-ref mailboxes (qcar globalqueue)))))

    
    ;; Pop the first message for the selected thread:
    (let* ((who (qcar globalqueue))
	   (msgs (vector-ref mailboxes who))
	   (msg  (qcar msgs)))
      (set! globalqueue (qcdr globalqueue))
      (DBG (printf "    popped queue ~s \n" globalqueue))
      (vector-set! enqueued? who #f) ;; Reenable it
      (vector-set! mailboxes who (qcdr msgs)) ;; Pop the message

      ((vector-ref threads who) msg) ;; This will transfer control back to scheduler

;      (printf "   finishd, new queue ~a\n" globalqueue)
;      (mainloop)
      ;; If we've been called before, just charge throuagh, otherwise initialize.
      ;; We could use multi-value continuations to get around this weirdness.
      )

    )
  
  (define (workfun who recv)
    (DBG (printf "  ** Calling ~a for first time.. ~a\n" who recv))
    (let workerloop ((msg (recv)))
      (define next (if (fx= N who) 1 (fx+ 1 who)))
      (DBG (printf "  ** Done Recv, message ~a, passing from ~a to ~a\n" msg who next))
      (when (fxzero? msg) (printf "~a\n" who) (alldone #t))
      (emit! next (fx- msg 1))
      (workerloop (recv))))


  (printf "Running ~a threads for ~a steps \n" N 
	  (string->number (car (command-line-arguments))))
  ;; Spawn the threads:
  (let spawnloop ((n N))
    (unless (zero? n)
      (vector-set! threads n (spawn workfun n))
      (spawnloop (fx- n 1))))

  (emit! 1 (string->number (car (command-line-arguments))))
  ;; Timing slows it down.

#;
  (time (call/cc (lambda (k) 
		   (set! alldone k)
		   (mainloop))))
  ;(time  (mainloop))
  (mainloop)


)
