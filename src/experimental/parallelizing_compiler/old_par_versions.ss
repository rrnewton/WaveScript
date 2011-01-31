

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
  (define (par-status) (fprintf (current-error-port) "Total threads forked: ~s\n" numforked))
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
    (map dequeue! q*))

  #;
  (define-syntax par
    (syntax-rules ()
      [(par e ...) (par-list (lambda () e) ...)]
      ))

  ;; A bit inefficient, defined in terms of par-list:
  ;;(define (par-map f ls) (apply par-list (map (lambda (x) (lambda () (f x))) ls)))
)


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
  ;   (define (print . args) (with-mutex mut (apply printf args) (flush-output-port (current-output-port))))
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
    (fprintf (current-error-port)
             "Par status:\n  not-finished: ~s\n  mut: ~s\n  tickets: ~s\n  threads: ~s\n  fork-attempts: ~s\n"
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

  ;; TEMP: This is inefficient of course:
  (define (parmv-fun th1 th2)
    (let ([ls (par-list (list th1 th2))])
      (values (car ls) (cadr ls))))
  
)



;; ================================================================================
;; <-[ VERSION 3 ]->

;; Work stealing version.
;; Each thread maintains a "stack" of potentially parallel computations (thunks).
;; Idle threads steal frames from this stack.
;;
;; [2007.09.18] This successfully gets a reasonable parallel speedup on the tree test.
#;
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
  (define allstacks '#()) ;; This is effectively immutable.
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
  ;   (define (print . args) (with-mutex global-mut (apply printf args) (flush-output-port (current-output-port))))
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
       [(eq? 'grabbed (car status*)) (workloop #f (cdr status*) (cdr thunks))]))    )

  ;; TEMP: This is inefficient of course:
  (define (parmv-fun th1 th2)
    (let ([ls (par-list (list th1 th2))])
      (values (car ls) (cadr ls))))

  (define-syntax parmv
    (syntax-rules ()
      [(_ a b) (parmv-fun (lambda () a) (lambda () b))]))
)


;; ================================================================================
;; <-[ VERSION 4 ]->


;; Tweaking Work-stealing version to use multiple values and do only 2-way par.
#;
(begin
  
  ;; STATE:

  ;; Each thread's stack has a list of frames, from newest to oldest.
  ;; We use a lock-free approach for mutating/reading the frame list.
  ;; Therefore, a thief might steal an old inactive frame, but this poses no problem.
  ;; 
  ;; A thread's "stack" must be as efficient as possible, because
  ;; it essentially replaces the native scheme stack where par calls
  ;; are concerned.  (I wonder if continuations can serve any purpose here.)
  ;; Note, head is the "bottom" and tail is the "top".  We add to tail.
  (reg:define-struct (shadowstack id head hlock tail tlock frames))
  
  ;; Frames are locked individually.
  ;; status may be 'available 'grabbed 'done
  (reg:define-struct (shadowframe mut status thunkval))

  ;; QUICK HACK, does this do any better:
#;
  (begin (define make-shadowframe vector)
	 (define (shadowframe-mut v)      (vector-ref v 0))
	 (define (shadowframe-status v)   (vector-ref v 1))
	 (define (shadowframe-thunkval v) (vector-ref v 2))
	 (define (set-shadowframe-mut! v x)      (vector-set! v 0 x))
	 (define (set-shadowframe-status! v x)   (vector-set! v 1 x))
	 (define (set-shadowframe-thunkval! v x) (vector-set! v 2 x)))

  ;; There's also a global list of threads:
  (define allstacks '#()) ;; This is effectively immutable.
  (define par-finished #f)
  ;; And a mutex for global state:
  (define global-mut (make-mutex))
  (define par-counter 0) ;; how many attempted forks were there

  ;; A new stack has no frames, but has a (hopefully) unique ID:
  (define (new-stack) 
    (make-shadowstack (random 10000) 
      0            ;; Head pointer.
      #f;(make-mutex) ;; Head mutex.
      0            ;; Tail pointer.
      #f;(make-mutex) ;; Tail mutex.
      (vector-build 50 
	(lambda (_) (make-shadowframe (make-mutex) #f #f)))))

  ;; A per-thread parameter.
  (define this-stack (make-thread-parameter (new-stack)))
  
  ;; Mutated below:
  (define numprocessors #f)

  ;; DEBUGGING:
  ;;  Pick a print:
  ;   (define (print . args) (with-mutex global-mut (apply printf args) (flush-output-port (current-output-port))))
  ;   (define (print . args) (apply printf args))
     (define (print . args) (void)) ;; fizzle

  ;; ----------------------------------------

  ;; Try to do work and mark it as done.
  (define (do-work! frame)
    ;; EXCESSIVE LOCKING
    ;; FIXME: Optimization... do a lock-free check first:
    ;(if (eq? 'available (shadowframe-mut frame))  )
    (and (eq? 'available (shadowframe-status frame))
	 (with-mutex (shadowframe-mut frame)
	   ;; If someone beat us here, we fizzle
	   (and (eq? 'available (shadowframe-status frame))
		(begin 
		  (print "STOLE work! ~s\n" frame)
		  (set-shadowframe-status!   frame 'done)
		  (set-shadowframe-thunkval! frame ((shadowframe-thunkval frame))))))))

  (define (make-worker)
    (define stack (new-stack))
    (fork-thread (lambda () 		   
		   (this-stack stack) ;; Initialize stack.		  
		   ;; Steal work forever:
		   (let forever ()
		     (unless par-finished
		       (let* ([ind (random numprocessors)]
			      [stack (vector-ref allstacks ind)])
			 (let* ([frames (shadowstack-frames stack)]
				[tl     (shadowstack-tail stack)])
			   (let frmloop ([i 0])
			     (if (fx= i tl) 
				 (forever) ;; No work on this processor, try again. 
				 (or (and (do-work! (vector-ref frames i)) (forever))
				     (frmloop (fx+ 1 i)))))))))))
    stack)

  (define (init-par num-cpus) 
    (fprintf (current-error-port) "\n  Initializing PAR system for ~s threads.\n" num-cpus)
    (with-mutex global-mut   
      (set! numprocessors num-cpus)
      (set! allstacks (make-vector num-cpus))
      (vector-set! allstacks 0 (this-stack))
      ;; We fork N-1 threads (the original one counts)
      (do ([i 1 (fx+ i 1)]) ([= i num-cpus] (void))
	(vector-set! allstacks i (make-worker)))))
  (define (shutdown-par) (set! par-finished #t))

  (define (par-status) 
    (printf "Par status:\n  par-finished ~s\n  allstacks: ~s\n  stacksizes: ~s\n  fork-attempts: ~s\n"
	    par-finished (vector-length allstacks)
	    (map shadowstack-tail (vector->list allstacks))
	    par-counter))

  ;; This should maybe reset more:
  (define (par-reset!) (with-mutex global-mut (set! par-counter 0)))

  (define (push! stack thunk)
    (define frame (vector-ref (shadowstack-frames stack) (shadowstack-tail stack)))
    ;; Initialize the frame
    ;(set-shadowframe-mut!      frame  (make-mutex)) ;; TEMPTOGGLE
    (set-shadowframe-thunkval! frame  thunk)
    (set-shadowframe-status!   frame  'available)
    ;(print "PUSH frame\n")
    (set-shadowstack-tail! stack (fx+ (shadowstack-tail stack) 1))
    ;; TODO! Check if we need to realloc the stack!
    ;(when (> ))
    frame)
  (define (pop! stack)
    ;(print "POP frame\n")
    (set-shadowstack-tail! stack (fx- (shadowstack-tail stack) 1)))

#;
  ;; What thread are we called from?  Which stack do we add to?...
  (define (parmv-fun th1 th2)
    (define stack (this-stack))
    ;; Add a frame to our stack.  NO LOCKS!    
    ;; From here on out, that frame is ready for business.
    (define frame (push! stack th2))
    ;; Start processing the first thunk.
    (let ([val1 (th1)])
      ;; Then grab the frame mutex and see if someone did the work for us:
      (with-mutex (shadowframe-mut frame)
	(case (shadowframe-status frame)
	  [(available)
	   (print "  hmm... no one stole our work...\n")
	   (pop! stack) ;; Pop before we even start the thunk.
	   (values val1 (th2))]
	  [(grabbed) 
	   (error 'parmv-fun "should never observe the grabbed state! mutex should prevent this")]
	  [else (DEBUGASSERT (eq? 'done (shadowframe-status frame)))
	   (pop! stack)
	   (values val1 (shadowframe-thunkval frame))]))))

  (define (parmv-helper stack frame val1)
    (with-mutex (shadowframe-mut frame)
      (case (shadowframe-status frame)
	[(available)
	 (pop! stack) ;; Pop before we even start the thunk.
	 (values val1 ((shadowframe-thunkval frame)))]
	[(grabbed) 
	 (error 'parmv-fun "should never observe the grabbed state! mutex should prevent this")]
	[else (DEBUGASSERT (eq? 'done (shadowframe-status frame)))
	      (pop! stack)
	      (values val1 (shadowframe-thunkval frame))])))

  ;; This one makes a thunk only for the second argument:
  (define-syntax parmv
    (syntax-rules ()
      [(_ a b) 
       (let ([stack (this-stack)]
	     [th2   (lambda () b)])
	 (define frame (push! stack th2))
	 (let ([val1 a]) ;; Do the first computation:
	   (parmv-helper stack frame val1) ;; Doesn't seem to make much difference.
	   #;
	   (with-mutex (shadowframe-mut frame)
	     (case (shadowframe-status frame)
	       [(available)
		(pop! stack) ;; Pop before we even start the thunk.
		(values val1 (th2))]
	       [(grabbed) 
		(error 'parmv-fun "should never observe the grabbed state! mutex should prevent this")]
	       [else (DEBUGASSERT (eq? 'done (shadowframe-status frame)))
		     (pop! stack)
		     (values val1 (shadowframe-thunkval frame))]))

	   )
	 )]))

#;
  (define-syntax parmv
    (syntax-rules ()
      [(_ a b) (parmv-fun (lambda () a) (lambda () b))]))

  
  (define (par-list . thunks)
    (if (null? thunks) ()
	(let ()
	  (let plloop ([thunks thunks])
	    (cond 
	     [(null? (cdr thunks)) (list ((car thunks)))]
	     [else
	      (let-values ([(fst rest) (parmv ((car thunks))
					      (plloop (cdr thunks)))])
		(cons fst rest)	  
		)])
	    ))))

  (define pcall 'undefined)
)