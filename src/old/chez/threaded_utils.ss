
;; [2006.07] This contains some miscellaneous stuff related to
;; threads.  Only relevent to the pthread-based version of Chez
;; Scheme.

;; TODO: Par should really return multiple values... not a list.

(module threaded_utils
    (stream-parmap 
     make-bq enqueue! dequeue!   bq-i bq-vec bq-mutex bq-ready bq-room

     init-par     ;; Run initialization code (fork threads, etc)
     shutdown-par ;; Tell workers to stop spinning.
     par       ;; Evaluate expressions in parallel, return list of values
;     par-list  ;; Evaluate a list of thunks
     ;parmv-fun          
    ; (parmv pop! push! this-stack parmv-helper par-list)
     par-map   ;; Apply function to list in parallel

     (pcall find-and-steal-once! wait-on-frame push! pop! mark-pop-release!)
     parmv

     par-status ;; Optional utility to show status of par threads.
     par-reset! ;; Reset counters
     
     ;async-par ;; A version of 'par' that returns immediately.
     ;sync      ;; The corresponding call to wait for an async-par to finish.
     ;WAITING
     ;tickets
     make-shadowframe shadowframe-mut shadowframe-status 
     set-shadowframe-mut! set-shadowframe-status! 
     ;shadowframe-thunkval set-shadowframe-thunkval!
     shadowframe-argval shadowframe-oper set-shadowframe-argval! set-shadowframe-oper!
     
     shadowstack-frames set-shadowstack-frames!
     shadowstack-tail  set-shadowstack-tail!
     shadowstack-head  set-shadowstack-head!
     this-stack

     empty-ivar set-ivar! register-on-ivar ivar-avail? 
     (ivar-apply-or-block mark-pop-release!)
     )
  
  ;(import chez_constants)

(define (format-syntax-nicely x) x)

(define-syntax ASSERT
  (lambda (x)
    (syntax-case x ()
      [(_ expr) #'(or expr (error 'ASSERT (format "failed: ~s" (format-syntax-nicely #'expr))))])))

;; PLT:
#;
  (define (format-syntax-nicely x)
    (format "Syntax ~a, line ~a in ~a" 
	    (syntax->datum x) (plt:syntax-line x) (plt:syntax-source x)))

;; This is an unsafe version that doesn't handle escapes via continuations :
(define-syntax with-mutex
  (syntax-rules ()
    [(_ e0 e1 e2 ...)
     (let ([m e0])
       (mutex-acquire m)
       (let ([x (begin e1 e2 ...)])
         (mutex-release m)
         x))]))


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

;; ================================================================================
;;; < PAR IMPLEMENTATION FOR STRICTLY NESTED PARALLELISM>
;; ================================================================================

(define vector-build
  (lambda (n f)
    (let ([v (make-vector n)])
      (do ([i 0 (fx+ i 1)])
	  ((= i n) v)
	(vector-set! v i (f i))))))

(define-syntax define-inlined
  (syntax-rules ()
    [(_ (f x ...) e ...)
     (define-syntax f 
       (syntax-rules ()
	 [(_ x ...) e ...]))]))

;; ================================================================================
;; <-[ VERSION 5 ]->

;; Using a more restricted "pcall" syntax to try to minimize allocation.
;; Also, this includes Kent's modifications to get rid of allocation.

;; CHANGELOG:
;; 
;; [2010.10.31] Adding support for blocking on ivars. This is a new state for a shadowframe.

(begin
    
  ;; STATE:

  ;; Each thread's stack has a list of frames, from newest to oldest.
  ;; We use a lock-free approach for mutating/reading the frame list.
  ;; Therefore, a thief might steal an old inactive frame, but this poses no problem.
  ;; 
  ;; A thread's "stack" must be as efficient as possible, because
  ;; it essentially replaces the native scheme stack where par calls
  ;; are concerned.  (I wonder if continuations can serve any purpose here.)
  ;; Note, head is the "bottom" and tail is the "top".  We add to tail, steal from head.
  (define-record shadowstack (id head tail frames)) ;; FIXME: head not actually used yet:
  
  ;; Frames are locked individually.
  ;; status may be 'available, 'ivar-blocked, 'stolen, or 'done
  (define-record shadowframe  (mut status oper argval)) ;; argval is both argument and stores result

  ;; There's also a global list of threads:
  (define allstacks '#()) ;; This is effectively immutable.
  (define par-finished 'par-finished-uninit) ;; This gets polled without the lock.
  (define threads-registered 1)
  ;; And a mutex for global state (threads-registered, numprocessors, allstacks...):
  (define global-mut (make-mutex)) 

  (define initial-stack-size 500)

  ;; A new stack has no frames, but has a (hopefully) unique ID:
  (define (new-stack) 
    (make-shadowstack (random 10000) 
      0            ;; Head pointer.
      0            ;; Tail pointer.
      (vector-build initial-stack-size
        (lambda (_) (make-shadowframe (make-mutex) #f #f #f)))))

  ;; A per-thread parameter.
  (define this-stack (make-thread-parameter 'stack-uninit))
  (define numprocessors #f) ;; Mutated below.

  ;; We spin until everybody is awake. (Could perhaps make this gentler by yielding?)
  (define (wait-for-everybody desired)    
    (let wait-for-threads ()
      ;(printf " w~s" threads-registered) (let loop ((i 1000000)) (unless (zero? i) (loop (sub1 i))))
      (unless 
	(fx= threads-registered desired)  ;; unlocked version, polling + monotonicity
  	;(with-mutex global-mut (fx= threads-registered desired))
	(wait-for-threads))))

    ;; DEBUGGING:
    (define DEBUG #t)
  ;;  Pick a print:
    (define (print . args) (with-mutex global-mut (apply printf args) (flush-output-port (current-output-port))))
  ;   (define (print . args) (apply printf args))
  ;   (define (print . args) (void)) ;; fizzle


  ;; ----------------------------------------

  (define (init-par num-cpus)
    (fprintf (current-error-port) "\n  [par] Initializing PAR system for ~s threads.\n" num-cpus)
    (let ((mystack (new-stack)))
      (with-mutex global-mut   
	(ASSERT (eq? threads-registered 1))
	(set! par-finished #f)
	(set! numprocessors num-cpus)
	(set! allstacks (make-vector num-cpus))
	(this-stack mystack)
	(vector-set! allstacks 0 mystack)
	;; We fork N-1 threads (the original one counts too)
	;; Each worker will grab global-mut before they really get started.
	(do ([i 1 (fx+ i 1)]) ([= i num-cpus] (void))
	  (vector-set! allstacks i (make-worker)))))
    
    (wait-for-everybody numprocessors)
    (fprintf (current-error-port) "  [par] Everyone's awake!\n")
    )
  (define (par-reset!) (void))
  (define (shutdown-par)
    (with-mutex global-mut (set! par-finished #t))
    (wait-for-everybody 1)
    (with-mutex global-mut (fprintf (current-error-port) "  [par] Shutdown complete\n"))
    )
  (define (par-status) 
    (with-mutex global-mut
      (fprintf (current-error-port)
	       "  [par] Par status:\n        par-finished ~s\n        allstacks: ~s\n        stacksizes: ~s\n\n"
	       par-finished (vector-length allstacks)
	       (map shadowstack-tail (vector->list allstacks)))))

  ;; ----------------------------------------

  ;; Try to do work and mark it as done.
  (define (steal-work! stack frame)
    #;
    (define (bump)
      ;; How much can we do just holding the lock on the head-frame on this stack?
      (set-shadowstack-head! stack (fx+ 1 (shadowstack-head stack)))  ;; FIXME: need atomic ops
      (when DEBUG (print "BUMPED head on stack ~s to ~s because of status ~s \n" (shadowstack-id stack) (shadowstack-head stack) (shadowframe-status frame)))
      )

    (case (shadowframe-status frame) ;; Check before locking.
      [(available ivar-blocked)   
       (and (mutex-acquire (shadowframe-mut frame) #f) ;; Don't block on it           
            ;;(begin (mutex-acquire (shadowframe-mut frame)) #t) ;; NONBLOCKING VERSION APPEARS TO HAVE A PROBLEM!!?
	    ;; From here on out we've got the mutex:
	    (case (shadowframe-status frame) ;; Double check after waking up.
	      [(available) #t]
	      [(ivar-blocked)
	       (when DEBUG (printf "(TID ~s) ATTEMPT TO STEAL ivar-blocked FRAME, avail ~s: ~s\n" (get-thread-id) (ivar-avail? (shadowframe-argval frame)) frame))
	       (ivar-avail? (shadowframe-argval frame))]  ;; TODO: if blocked on ivar, might want a different return value for steal-work!
	      [else (begin ;(bump) ;; FIXME: Head not used yet.
		      (mutex-release (shadowframe-mut frame)) ;; If someone beat us here, we fizzle
		      #f)]
		    )
	    (begin 
	      (when DEBUG (print "STOLE work! ~s, ID ~s\n" frame (get-thread-id)))
	      ;;(print "STOLE work! ~s \n" frame)

	      (set-shadowframe-status! frame 'stolen) ;; Could have done this atomically with CAS
	      (mutex-release (shadowframe-mut frame)) 
	      ;; Then let go to do the real work, note that this may do further pcall's:
	      (set-shadowframe-argval! frame 	                               
				       ((shadowframe-oper frame) 
				       ;; If stealing an ivar-blocked computation, we need an extra step:
					(if (eq? 'ivar-blocked (shadowframe-status frame))
					    (ivar-val (shadowframe-argval frame))
					    (shadowframe-argval frame))
				        ))
	      ;; Now we *must* acquire the lock (even if we block) in order to set the status to done.
	      ;; [2010.10.31]  Really? Why?  Shouldn't we own it after it's stolen?
	      (mutex-acquire (shadowframe-mut frame)) ;; blocking...
	      (set-shadowframe-status! frame 'done)	   
	      (mutex-release (shadowframe-mut frame))
	      #t)
	      )]
      ;[(stolen done) (with-mutex (shadowframe-mut frame) (bump))] ;; FIXME: Head not used yet.
      ;[(#f) #f])
      [else #f]
    ))

  (define (find-and-steal-once!)
    (let* ([ind (random numprocessors)]
	   [stack (vector-ref allstacks ind)])
	(let* ([frames (shadowstack-frames stack)]
	       [tl     (shadowstack-tail stack)]
	       ;[hd     (shadowstack-head stack)]
	       )
#;  ;; FIXME: head not used yet:
	  (when (fx> tl 0)  ;; If tail==0 there are no frames to steal.
	    (steal-work! stack (vector-ref frames hd)))

	  ;; Testing: This is a silly strategy to scan all of them from the base:
	  ;; Inefficient, but should be correct.
	  (let frmloop ([i 0])
	    (if (fx= i tl)
		#f ;; No work on this processor, try again. 
		(if (steal-work! stack (vector-ref frames i)) ;; NOTE: Wrong number of args to steal-work! here seemed to cause deadlock!!  Add any extra arg, like '99'
		    #t
		    (frmloop (fx+ 1 i))))))))

  ;; Fork a worker thread and return its stack:
  (define (make-worker)
    (define stack (new-stack))
    (fork-thread (lambda ()                
                   (this-stack stack) ;; Initialize stack. 
		   (let ([myid 
			  (with-mutex global-mut ;; Register our existence.
			    (set! threads-registered (add1 threads-registered))
			    threads-registered)])
		     ;; Steal work approximately forever:
		     (let steal-loop ()
		       (find-and-steal-once!)
		       (if par-finished
			   (with-mutex global-mut 
			      (set! threads-registered (sub1 threads-registered))
			      (fprintf (current-error-port) "  [par] worker ~s terminating (TID ~s, ~s remain).\n"
				       myid (get-thread-id) threads-registered))
			   (steal-loop))))))
     stack)

  ;; --------------------------------------------------------------------------------
  ;;  < Experimenting with IVars > 
  ;; --------------------------------------------------------------------------------
  ;; We don't want to change shadowframe, so we just put a magic value in shadowframe-argval
  (define magic1 (gensym "blocked-on-ivar-sym"))
  (define magic2 (gensym "empty-ivar-sym"))

  ;; An ivar really *should* support atomic operations to extend the
  ;; wait list... alas for now it has to have its own mutex:
  (define-record ivar (mut val waiting))

  (define (empty-ivar) (make-ivar (make-mutex) magic2 '()))
  (define (set-ivar! iv val)
    (with-mutex (ivar-mut iv)
      (let ((old (ivar-val iv)))
	(unless (eq? old magic2)
	  (error 'set-ivar! "error ivar should only be assigned once!  Already contains value ~s\n" old))
	(for-each (lambda (fn) (fn val)) (ivar-waiting iv))
	(set-ivar-val! iv val))))
  ;; Apply a function to the ivar when its available:
  (define (register-on-ivar fn iv)
    (with-mutex (ivar-mut iv)
      (let ((val (ivar-val iv)))
	(if (eq? val magic2)
	    (set-ivar-waiting! iv (cons fn (ivar-waiting iv)))
	    (fn val)))))
  ;; Testing for availability is not part of the 
  (define (ivar-avail? iv) (not (eq? (ivar-val iv) magic2)))

  ;; TODO: possibly make these syntax instead of functions:
  (define (push! stack oper val status)
    (let ([frame (vector-ref (shadowstack-frames stack) (shadowstack-tail stack))])
      ;; Initialize the frame
      (set-shadowframe-oper!   frame oper)
      (set-shadowframe-argval! frame val)
      (set-shadowframe-status! frame  status)
      (set-shadowstack-tail! stack (fx+ (shadowstack-tail stack) 1)) ;; bump cursor
      ;; TODO: could check for stack-overflow here and possibly add another stack segment...
      frame))
  (define (pop! stack) (set-shadowstack-tail! stack (fx- (shadowstack-tail stack) 1)))


  ;; Read the ivar or BLOCK the current stack frame if it isn't available.
  ;; UNFINISHED
  (trace-define (read-ivar iv)
    (if (ivar-avail? iv) (ivar-val iv)
      ;; Here the trick is that we use both the Scheme stack, and the
      ;; shadow stack to store the blocked computation.
      ;; We assume that "our" frame is the tail of our own stack:
	(let ([stack (this-stack)]) ;; thread-local parameter
	  (let ([frame (vector-ref (shadowstack-frames stack) (shadowstack-tail stack))])
	    ;; Whether we or someone else is working on it we have the convention of marking 
	    (ASSERT (eq? 'stolen (shadowframe-status frame)))
	    
	    ;; TODO -- problem here, this strategy won't make ivar-blocked computations stealable...
	    ;; The trapped part of the computation in the Scheme stack is never captured and exposed.    
	    ))
      ))



  ;; --------------------------------------------------------------------------------	      

  ;; This has an ugly interface.  But the alternative is duplicated code.
  ;; This is shared between ivar-apply-or-block and pcall.
  (define-syntax wait-on-frame
    (syntax-rules ()
      [(_ stack frame release-and-steal mark-pop-release availstatus do-it-ourself retrieve-complete)
       (let waitloop ()
         (define (release-and-steal)
	   (mutex-release (shadowframe-mut frame))		 
	   (find-and-steal-once!) ;; Meanwhile we should go try to make ourselves useful..		  
	   (waitloop))
	 (define (mark-pop-release)
	   (set-shadowframe-status! frame 'stolen) ;; Just in case...
	   (pop! stack) ;; Pop before we even start the thunk.
	   (mutex-release (shadowframe-mut frame)))

	 ;; We're the parent, when we get to this frame, we lock it off from all other comers.
	 ;; Thieves should do non-blocking probes.
	 (mutex-acquire (shadowframe-mut frame))
	 (let ([status (shadowframe-status frame)])
	   (cond
	    [(eq? status availstatus)  do-it-ourself]
	   
	    ;; Oops, they may be waiting to get back in here and set the result, let's get out quick:
	    ;; Let go of this so they can finish and mark it as done.
	    [(eq? status 'stolen)  
	     (release-and-steal) ;; When we're done with that come back and see if our outsourced job is done.
	     ]

	    ;; It was stolen and is now completed:
	    [(eq? status 'done) 
	     (pop! stack)
	     (mutex-release (shadowframe-mut frame))	     
	     retrieve-complete]
	     
	    [else (error 'wait-on-frame (format "invalid status: ~s" (shadowframe-status frame)))]
	    )))
      ]))

  ;; Capturing common sequences of operations to avoid duplication:
  (define (mark-pop-release! stack frame)
    (set-shadowframe-status! frame 'stolen) ;; Just in case...
    (pop! stack) ;; Pop before we even start the thunk.
    (mutex-release (shadowframe-mut frame)))


  ;; Like register-on-ivar but pushes a new stack frame that is blocked on the ivar.
  ;; If the new frame gets pushed this thread will go off and steal work.
  (trace-define (ivar-apply-or-block fn ivar)
    (let () ;((v (ivar-val ivar)))
      ;; FIRST test: before blocking:
      (if (ivar-avail? ivar);(not (eq? v magic2)) ;; Inlined ivar-avail? here.
          (fn (ivar-val ivar)) ; (fn v)
	  (let* ([stack (this-stack)] ;; thread-local parameter
		 [_ (when DEBUG (print "\n\n(TID ~s) BLOCKING ON IVAR\n\n" (get-thread-id)))]
		 [frame (push! stack fn ivar 'ivar-blocked)])
	     (find-and-steal-once!) ;; Let some time pass before polling again.


;;====================================================================================================
#;
      (wait-on-frame stack frame release-and-steal mark-pop-release 'ivar-blocked
		     ;; Scenario 1: Still blocked, poll it ourselves:
			    (begin
			      (when DEBUG (print "(TID ~s) Polling ivar: ~s\n" (get-thread-id) ivar))
			      (if (ivar-avail? ivar) ;; SECOND test: can we unblock?
				  (begin (mark-pop-release) (fn (ivar-val ivar)))
				  (release-and-steal)))
			    ;; Scenario 2: Stolen but finished.
			    (begin 
			      (when DEBUG (ASSERT (ivar-avail? ivar)))
			      (fn (ivar-val ivar)))
			    )
       ;; This is similar to the loop inside pcall:
       (trace-let waitloop ()
         (define (release-and-steal!) ;; Used twice below.
	   (mutex-release (shadowframe-mut frame))		 
	   (find-and-steal-once!) ;; Meanwhile we should go try to make ourselves useful..		  
	   (waitloop))

	 ;; We're the parent, when we get to this frame, we lock it off from all other comers.
	 ;; Thieves should do non-blocking probes.
	 (mutex-acquire (shadowframe-mut frame))
	 (let ([status (shadowframe-status frame)])
	   (cond
	    [(eq? status 'ivar-blocked)
	     (when DEBUG (print "(TID ~s) Polling ivar: ~s\n" (get-thread-id) ivar))
	     (if (ivar-avail? ivar) ;; SECOND test: can we unblock?
		 (begin
		   (mark-pop-release! stack frame)
		   (fn (ivar-val ivar)))
		 ;; Otherwise steal and then come back to it:
		 (release-and-steal!))]

	    ;; IVar-blocked frames will be set to stolen by a thief too (after the ivar is filled).
	    [(eq? status 'stolen) (release-and-steal!)]

	    ;; It was stolen and is now completed:
	    [(eq? status 'done) 
	     (pop! stack)
	     (mutex-release (shadowframe-mut frame))
	     
	     (when DEBUG (ASSERT (ivar-avail? ivar)))
	     (fn (ivar-val ivar))]
	     
	    [else (error 'wait-on-frame (format "invalid status: ~s" (shadowframe-status frame)))]
	    )))


;;====================================================================================================

	     (when DEBUG (print "(TID ~s) IVAR unblocked, returning: ~s\n" (get-thread-id) ivar))
	     ))))




  (define-syntax pcall
    (syntax-rules ()
      [(_ op (f x) e2)
       (let ([stack (this-stack)]) ;; thread-local parameter
         (begin ;let ([op1 op] [f1 f] [x1 x]) ;; Don't duplicate subexpressions:
	   (let ([frame (push! stack f x 'available)])
	     (let ([val1 e2])  ;; Evaluate the second expression on this thread.

#;
	       (wait-on-frame stack frame release-and-steal mark-pop-release 'available
			      ;; [2010.10.31] Oper/argval should be f/x here:
			      (op ;(f x)  ;; possible optimization?
			       ((shadowframe-oper frame) (shadowframe-argval frame))
			       val1)
			      (op (shadowframe-argval frame) val1)
			       )

	       (let waitloop ()
                 (mutex-acquire (shadowframe-mut frame))
                 (case (shadowframe-status frame)
                   [(available)                   
		    (mark-pop-release! stack frame)
                    ;; [2010.10.31] Oper/argval should be f/x here:
                    (op ;(f x)  ;; possible optimization?
                        ((shadowframe-oper frame) (shadowframe-argval frame))
                        val1)]
                   ;; Oops, they may be waiting to get back in here and set the result, let's get out quick:
                   [(stolen) 
                    ;; Let go of this so they can finish and mark it as done.
                    (mutex-release (shadowframe-mut frame))              
                    (find-and-steal-once!) ;; Meanwhile we should go try to make ourselves useful..               
                    (waitloop)] ;; When we're done with that come back and see if our outsourced job is done.
                   ;; It was stolen and is now completed:
                   [else (pop! stack) 
                         (mutex-release (shadowframe-mut frame))
                         (op (shadowframe-argval frame) val1)]))


	       ))))]))


  ;; Returns values in a list
  (define-syntax par
    (syntax-rules ()
      [(_ a b) (pcall list ((lambda (_) a) #f) b)]
      [(_ a b* ...) (pcall cons ((lambda (_) a) #f) (par b* ...))]))

  (define-syntax parmv
    (syntax-rules ()
      [(_ a b) (pcall values ((lambda (_) a) #f) b)]))

  (define (par-map fn ls) 
    (if (null? ls) '()
	(let loop ([ls ls])
	  (if (null? (cdr ls))
	      (list (fn (car ls)))
	      (pcall cons 
		     (fn (car ls))
		     (loop (cdr ls)))))))


  ;;================================================================================

;  (init-par (string->number (or (getenv "NUMTHREADS") "2")))

#;
  (let ()
    (define (tree n)
      (if (fxzero? n) 1
          (pcall fx+ (tree (fx- n 1)) (tree (fx- n 1)))))
    (printf "Run using parallel add-tree via pcall mechanism:\n")
    (printf "\n~s\n\n" (time (tree test-depth)))
    (par-status))

#;
  (let ()
    (define (tree n)
      (if (fxzero? n) 1
          (apply fx+ (par (tree (fx- n 1)) (tree (fx- n 1))))))
    (printf "Run using parallel add-tree w/ LIST intermediate values:\n")
    (printf "\n~s\n\n" (time (tree test-depth)))
    (par-status))
#;
  (let ()
    (define (tree n)
      (if (fxzero? n) 1
          (call-with-values (lambda () (parmv (tree (fx- n 1)) (tree (fx- n 1)))) fx+)))
    (printf "Run using parallel add-tree w/ MULTIPLE values:\n")
    (printf "\n~s\n\n" (time (tree test-depth)))
    (par-status))

#;
  (let ()
    (define (tree n)
      (if (fxzero? n) 1
          (fx+ (tree (fx- n 1)) (tree (fx- n 1)))))
    (printf "Run sequential (non-par) version:\n")
    (printf "\n~s\n\n" (time (tree test-depth)))
    (par-status))
 
)  ;; End version 5


#;
(define _
  (begin 

  (init-par (string->number (or (getenv "NUMTHREADS") "2")))
  (printf "Run using parallel add-tree via pcall mechanism:\n")
  (let loop ((n 1000000)) (or (zero? n) (loop (sub1 n))))
  (let ()
    (define (tree n)
      (if (zero? n) 1
          (pcall + (tree (sub1 n)) (tree (sub1 n)))))
    (printf "\n~s\n\n" (time (tree 23)))
    (par-status))

    ))


;; ================================================================================
;; <-[ VERSION 6 ]->

;; This version will block an entire worker (and use its real continuation) whenever a read happens.
;; It will maintain a queue of unblocked workers looking to rejoin the computation.



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

