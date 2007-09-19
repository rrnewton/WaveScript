
;; [2006.07] This contains some miscellaneous stuff related to
;; threads.  Only relevent to the pthread-based version of Chez
;; Scheme.

;; TODO: Par should really return multiple values... not a list.

(chez:module threaded_utils
    (stream-parmap 
     make-bq enqueue! dequeue!   bq-i bq-vec bq-mutex bq-ready bq-room

     init-par  ;; Run initialization code (fork threads, etc)
     par       ;; Evaluate expressions in parallel, return list of values
     par-list  ;; Evaluate a list of thunks
     par-map   ;; Apply function to list in parallel

     par-status ;; Optional utility to show status of par threads.
     par-reset! ;; Reset counters
     
     ;async-par ;; A version of 'par' that returns immediately.
     ;sync      ;; The corresponding call to wait for an async-par to finish.
     ;WAITING
     ;tickets
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

;; ================================================================================
;; <-[ VERSION 1 ]->

;; Inefficient version, forks threads on demand:
;; ACK: segfaults under stress:
;; [2007.09.14] After about 500 forks...
;; Well, probably it doesn't clean up these threads?
#;
(begin 
  (define (init-par num-cpus) (void))
  (define (shutdown-par) (void))
  (define numforked 0)
  (define (par-status) (printf "Total threads forked: ~s\n" numforked))
  (define (par-reset!) (set! numforked 0))
  (define tickets 'dummy)

  (define (par-list . thunks)
    ;; Could be made slightly more efficient by not using generic queues.
    (define q* (map (lambda (th) 
		      (let ([q (make-bq 1)])
			(set! numforked (add1 numforked))
			(fork-thread (lambda () (enqueue! q (th))))
			q))
		 thunks))
    (fprintf (current-error-port) "\n  Forking ~s threads.\n" (length thunks))
    (map dequeue! q*)))


;; ================================================================================
;; <-[ VERSION 2 ]->

;; Work sharing version.
;; Maintains worker threads and a job queue.
;;
;; [2007.04.04] Occasionally deadlocks currently.
;; Fixed deadlock, but it has the design flaw that it runs out of
;; threads if you do nested pars.
;;
;; Tweaking design: if no tickets are available, we just do it ourselves.
#;
(begin
  
  ;; STATE:
  ;; We need to put a request for work in a job queue, but get a ticket
  ;; back that we can use to both (1) wait on the computation's
  ;; completion (2) retrieve the value produced by the computation.
  (define not-finished #t)
  (define tickets '()) ;; Tickets for work.
  (define mut (make-mutex)) ;; Global: guards tickets and printing.
  (define threads ())
  
  (define par-list-counter 0) ;; protected by mut
  
  ;; DEBUGGING:
  ; (define WAITING '())
  ;;  Pick a print:
  ;   (define (print . args) (with-mutex mut (apply printf args) (flush-output-port)))
  ;   (define (print . args) (apply printf args))
     (define (print . args) (void)) ;; fizzle

  (define not-computed (gensym "not-computed"))

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
	  (with-mutex mut (set! tickets (cons (make-ticket inq) tickets)))
	  ;; Someone took the ticket, lets get their work:
	  ;(print "  Thread ~s waiting for work... (also waiting: ~s)\n" id WAITING)
	  ;(with-mutex mut (set! WAITING (cons id WAITING)))
	  (let ([j (dequeue! inq)]) 
	    ;(with-mutex mut (set! WAITING (remq id WAITING)))
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
    (fprintf (current-error-port) "\n  Initializing PAR system for ~s threads.\n" num-cpus)
    (with-mutex mut
      (do ([i 0 (fx+ i 1)]) ([= i num-cpus] (void))      
	(set! threads (cons (make-worker) threads)))))
  (define (shutdown-par) (set! not-finished #f))
  (define (par-status)
    (printf "Par status:\n  not-finished: ~s\n  mut: ~s\n  tickets: ~s\n  threads: ~s\n  fork-attempts: ~s\n"
	    not-finished mut  tickets threads par-list-counter))
  ;; This should maybe reset more:
  (define (par-reset!) 
    (with-mutex mut (set! par-list-counter 0)))

  (define (par-list . thunks)
    (let ([jobs (map (lambda (th) 		      
		       (print " SRC: Taking a ticket...\n") 
		       (with-mutex mut
			 (set! par-list-counter (add1 par-list-counter))
			 (if (null? tickets)
			     ;; Do it ourselves:
			     (let ([j (make-job #f)])
			       (print " SRC: NO TICKETS, Doing ourselves...\n")
			       (set-job-val! j (th))
			       j)
			     (let ([tick (car tickets)]) ; Pull a ticket. 
			       (set! tickets (cdr tickets))
			       (tick th))))) ; Fill it out, get a job object back.
		  thunks)])
      (print " SRC: Jobs out, now collect results\n")
      ;; Now get all those results:
      (map get-job-result jobs)
      ;jobs
      ))
)



;; ================================================================================
;; <-[ VERSION 3 ]->

;; Work stealing version.
;; Each thread maintains a "stack" of potentially parallel computations (thunks).
;; Idle threads steal frames from this stack.
;;
;; [2007.09.18] This successfully gets a reasonable parallel speedup on the tree test.
(begin
  
  ;; STATE:

  ;; Each thread's stack has a list of frames, from newest to oldest.
  ;; We use a lock-free approach for mutating/reading the frame list.
  ;; Therefore, a thief might steal an old inactive frame, but this poses no problem.
  ;; 
  ;; A thread's "stack" must be as efficient as possible, because
  ;; it essentially replaces the native scheme stack where par calls
  ;; are concerned.  (I wonder if continuations can serve any purpose here.)
  (reg:define-struct (thread-stack id frames))
  
  ;; Frames are locked individually.
  ;; status may be 'available 'grabbed 'done
  (reg:define-struct (frame mut thunks statuses))
  
  ;; There's also a global list of threads:
  (define allstacks #()) ;; This is effectively immutable.
  (define par-finished #f)
  ;; And a mutex for global state:
  (define global-mut (make-mutex))  
  (define par-list-counter 0) ;; how many attempted forks were there

  ;; A new stack has no frames, but has a (hopefully) unique ID:
  (define (new-stack) (make-thread-stack (random 10000) ()))

  ;; A per-thread parameter.
  (define this-stack (make-thread-parameter (new-stack)))

  ;; DEBUGGING:
  ;;  Pick a print:
  ;   (define (print . args) (with-mutex global-mut (apply printf args) (flush-output-port)))
  ;   (define (print . args) (apply printf args))
     (define (print . args) (void)) ;; fizzle

  ;; ----------------------------------------
  ;; Traverse everybody's stack to find work.
  ;; Returns frame, statuspair, workpair.
  (define (steal-work)     
    (let theifloop ()
      (let-values ([(ind frames)
		    (with-mutex global-mut 
		      (let ([ind (random (vector-length allstacks))])
			(values ind (thread-stack-frames (vector-ref allstacks ind)))))])
	(cond
	 ;; No work on this processor, try again. 
	 [(null? frames) 
	  ;(display ind)
	  (theifloop)]
	 [else 
	  ;; NOTE: I HAVE NO IDEA WHETHER THIS IS MORE EFFICIENT THAN JUST DOING A REVERSE ON THE FRAME LIST!
	  ;;
	  ;; Go across the frames in the stack, oldest first, looking for something to steal.
	  ;; This is just a foldr, written with an explicit loop an multiple values.
	  (let-values ([(frm sp wp)
			(let stackloop ([frames frames])
			  (define (grab) 
			    (let-values ([(sp wp) (grab-work (car frames))])
			      (if wp 
				  (values (car frames) sp wp)
				  (values #f #f #f))))
			  (if (null? (cdr frames))
			      ;; Got to the end, try this one.
			      (grab)
			      (let-values ([(frm sp wp) (stackloop (cdr frames))])
				(if wp 
				    (values frm sp wp)
				    (grab)))))])
	    (if wp 
		(begin 
		  (print "Stealing WORK from processor ~s frame: ~s\n" ind frm)
		  (values frm sp wp))
		(theifloop)))]))))

  ;; Traverse a frame to grab a piece of work and mark it "grabbed".
  (define (grab-work frame)
    (with-mutex (frame-mut frame)
      (let grabloop ([thunks (frame-thunks frame)]
		     [statuses (frame-statuses frame)])
	(if (null? thunks) (values #f #f) ;; All finished.
	    (case (car statuses)
	      [(available) (set-car! statuses 'grabbed)
	       (values statuses thunks)]
	      [(grabbed done) (grabloop (cdr thunks) (cdr statuses))])))))

  ;; Do the work and mark it as done.
  (define (do-work frame statuspair workpair) 
    ;; It's already marked as grabbed, so we don't need the mutex to execute:
    (let ([result ((car workpair))])
      ;; We do need the mutex to mutate the pairs:
      (with-mutex (frame-mut frame)
	(set-car! workpair result)
	(set-car! statuspair 'done))))

  (define (make-worker)
    (define stack (new-stack))
    (fork-thread (lambda () 		   
		   (this-stack stack) ;; Initialize stack.		  
		   ;; Steal work forever:
		   (let forever ()
		     (unless par-finished
		       (let-values ([(frm sp wp) (steal-work)])  
			 (when wp (do-work frm sp wp)))		       
		       (forever)
		       ))))
    stack)

  (define (init-par num-cpus) 
    (fprintf (current-error-port) "\n  Initializing PAR system for ~s threads.\n" num-cpus)
    (with-mutex global-mut   
      (set! allstacks (make-vector num-cpus))
      (vector-set! allstacks 0 (this-stack))
      ;; We fork N-1 threads (the original one counts)
      (do ([i 1 (fx+ i 1)]) ([= i num-cpus] (void))
	(vector-set! allstacks i (make-worker)))))
  (define (shutdown-par) (set! par-finished #t))

  (define (par-status) 
    (printf "Par status:\n  par-finished ~s\n  allstacks: ~s\n  stacksizes: ~s\n  fork-attempts: ~s\n"
	    par-finished (vector-length allstacks)
	    (map length (map thread-stack-frames (vector->list allstacks)))
	    par-list-counter))

  ;; This should maybe reset more:
  (define (par-reset!) 
    (with-mutex global-mut (set! par-list-counter 0)))

  ;; What thread are we called from?  Which stack do we add to?...
  (define (par-list . thunks)
    (define frame
      (make-frame (make-mutex) thunks 
		  (map (lambda (_) 'available) thunks)))

    ;; Should use global mutex:
    (set! par-list-counter (add1 par-list-counter))

    ;; From here on out, that frame is ready for business.
    ;; Add a frame to our stack.  NO LOCKS!
    (let ([st (this-stack)])
      (set-thread-stack-frames! st (cons frame (thread-stack-frames st))))

#;
    (with-mutex global-mut
      (print "\nall threads ~s  our thread stack id ~s length ~s\n" 
	     (vector-length allstacks) (thread-stack-id (this-stack)) (length (thread-stack-frames (this-stack))))
      )


    ;; The version below was broken, so trying this simple but inefficient one:
    ;; FIXME: QUADRATICALLY traversing the list from the start each time with grab-work:
    ;;   
    (let workloop ()
      (define (alldone? frame)
	(with-mutex (frame-mut frame)
	  (andmap (lambda (x) (eq? x 'done)) (frame-statuses frame))))
      (let-values ([(sp wp) (grab-work frame)])
	(cond
	 [wp (do-work frame sp wp) (workloop)]
	 [(alldone? frame)
	  ;(print "Frame finished, popping: ~s\n" frame)
	  ;; POP IT.  It's ok that this isn't mutex-protected.
	  (let ([st (this-stack)]) (set-thread-stack-frames! st (cdr (thread-stack-frames st))))
	  (frame-thunks frame)]
	 [else  ;; SPIN until it's done:
	  (workloop)])))

    ;; Now we traverse our frame, handling each thunk.
    ;; Other threads may also steal work from our frame during this process.
    ;; 
    ;; If all work from this frame is assigned, but not completed then we need to block.
#;
    (let workloop ([alldone #t]
		   [status* (frame-statuses frame)]
		   [thunks (frame-thunks frame)])
      (cond
       [(null? thunks) 
	(if alldone 
	    ;; If we're all done we can return the values!
	    (begin 
	      (print "Frame finished, popping: ~s\n" frame)
	      ;; POP IT.  It's ok that this isn't mutex-protected.
	      (let ([st (this-stack)]) (set-thread-stack-frames! st (cdr (thread-stack-frames st))))
	      (frame-thunks frame))
	    ;; FIXME: ACK, for the time being we SPIN until the grabbed work is done.
	    (begin
	      (print " @ SPINNING @\n")
	      (workloop #f (frame-statuses frame) (frame-thunks frame))
	      ))]
       [(eq? 'done (car status*)) (workloop alldone (cdr status*) (cdr thunks))]
       [(eq? 'available (car status*)) 
	    (begin 
	      (do-work frame status* thunks)
	      ;; Now keep going to process the rest of the frame.
	      (workloop alldone (cdr status*) (cdr thunks)))]
       ;; Someone else has it:
       [(eq? 'grabbed (car status*)) (workloop #f (cdr status*) (cdr thunks))]))

;    (map (lambda (th) (th)) thunks)
        
    ))




;; ================================================================================
;;; Little tests:


;; This type of test can get 7.18X speedup on 8-way valor (work-stealing).
#;
(let ()
  (define count (* 100 1000 1000))
  ;(define count (* 500 ))
  ;; Decrement a counter in two threads vs. one.
  (define (l1 x) (unless (zero? x) (l1 (sub1 x))))
  ;(define (l2 x) (unless (zero? x) (l2 (sub1 x))))
  ;(time (rep 10000 (par (l1 10000) (l2 10000))))

  (par-reset!)
  (time (par (l1 count) (l1 count)(l1 count)(l1 count)(l1 count)(l1 count)(l1 count)(l1 count)))
  ;(time (list (l1 count) (l2 count)))
  (par-status)
  )



;; This won't work because of shared code:
;; WAIT it works!
#;
(let ()
  (define (l1 x) (unless (zero? x) (l1 (sub1 x))))
  ;(time (rep 10000 (par (l1 10000) (l2 10000))))
  (time (par (l1 10000000) (l1 10000000)))
  (time (list (l1 10000000) (l1 10000000)))
  )



;; [2007.09.18] Hmm, on this one it displays the "taking turns" behavior (work sharing)
;; [2007.09.18] With work-stealing this gets a MEAGER 1.6X speedup on 8-way valor!!
;;  7684ms 8-way (3611 collecting) vs. 12228ms (2370 collecting)
;; Clearly our implementation of the "parallel stack" isn't that good.
#;
(let ()
  ;; Make 1024 threads:
  (define (tree n)
    (if (zero? n) 1
	(apply + (par (tree (sub1 n)) (tree (sub1 n))))))
  (par-reset!)
  (printf "\n~s\n\n" (time (tree 22)))
  (par-status))



;; Basic work-stealing...???





;=============================================================================
;; A multicore version of stream-map:
;;
;; NOTE: The computations have to be heavyweight for the parallelism
;; to beat the extra communication cost.
;;
;; [2007.09.14] Note, this is for the old stream representation:
;; (tail-delayed pairs), the WS emulator now uses a push-based stream rep.
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
