
;; [2006.07] This contains some miscellaneous stuff related to
;; threads.  Only relevent to the pthread-based version of Chez
;; Scheme.

;; TODO: Par should really return multiple values... not a list.


(library (par5)
 (export 
     init-par     ;; Run initialization code (fork threads, etc)
     shutdown-par ;; Tell workers to stop spinning.
     par          ;; Evaluate expressions in parallel, return list of values
     par-map      ;; Apply function to list in parallel

     pcall ;(pcall find-and-steal-once! push! pop! mark-pop-release!)
     parmv

     par-status ;; Optional utility to show status of par threads.
     par-reset! ;; Reset counters

     empty-ivar set-ivar! register-on-ivar ivar-avail? 
     ivar-apply-or-block ;(ivar-apply-or-block mark-pop-release!)
     
     ;; INTERNAL:
     threaderror
     make-shadowframe shadowframe-mut shadowframe-status 
     set-shadowframe-mut! set-shadowframe-status! 
     shadowframe-argval shadowframe-oper set-shadowframe-argval! set-shadowframe-oper!
     
     shadowstack-frames set-shadowstack-frames!
     shadowstack-tail  set-shadowstack-tail!
     shadowstack-head  set-shadowstack-head!
     this-stack
     )
  
 (import (rnrs (6))
	 (rnrs arithmetic fixnums (6))
         (only (scheme) fork-thread mutex-acquire mutex-release make-mutex make-thread-parameter get-thread-id
	       gensym list-head iota void random format printf fprintf define-record) ;; Chez scheme primitives
	 )

;; [2010.11.01] The default error isn't working on other threads for me:
(define (threaderror sym str . args)
 (fprintf (current-error-port) "\nERROR: ~a" (apply format str args))
 (flush-output-port (current-error-port))
 (par-status 'verbose)
 (exit))

;; DEBUGGING:
(define DEBUG #f)

(define-syntax ASSERT
  (lambda (x)
    (syntax-case x ()
      [(_ expr) #'(or expr (threaderror 'ASSERT (format "failed: ~s" #'expr)))])))


;; This is an unsafe version that doesn't handle escapes via continuations :
(define-syntax with-mutex
  (syntax-rules ()
    [(_ e0 e1 e2 ...)
     (let ([m e0])
       (mutex-acquire m)
       (let ([x (begin e1 e2 ...)])
         (mutex-release m)
         x))]))


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
	 [(_ x ...) (begin e ...)]))]))

;; ================================================================================
;; <-[ VERSION 5 ]->

;; Using a more restricted "pcall" syntax to try to minimize allocation.
;; Also, this includes Kent's modifications to get rid of allocation.

;; CHANGELOG:
;; 
;; [2010.10.31] Adding support for blocking on ivars. This is a new state for a shadowframe.
    
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
      (unless 
	(fx=? threads-registered desired)  ;; unlocked version, polling + monotonicity
	(wait-for-threads))))

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
  (define (par-status . args) 
    (with-mutex global-mut
      (let ((stackls (vector->list allstacks)))
	(fprintf (current-error-port)
		 "  [par] Par status:\n        par-finished ~s\n        allstacks: ~s\n        stacksizes: ~s\n\n"
		 par-finished (vector-length allstacks)
		 (map shadowstack-tail stackls))
	(when (memq 'verbose args)
	  (for-each (lambda (i stack) 
	              (printf "STACK ~s:\n" i)
	              (for-each (lambda (f) (printf "   ~a\n" f)) 
			(list-head (vector->list (shadowstack-frames stack)) 
				   (shadowstack-tail stack)))
		      )
	    (iota (length stackls)) stackls))
	)))

  ;; ----------------------------------------

  ;; Try to do work and mark it as done.
  (define (steal-work! stack frame)
    #;
    (define (bump)
      ;; How much can we do just holding the lock on the head-frame on this stack?
      (set-shadowstack-head! stack (fx+ 1 (shadowstack-head stack)))  ;; FIXME: need atomic ops
      (when DEBUG (print "BUMPED head on stack ~s to ~s because of status ~s \n" 
			 (shadowstack-id stack) (shadowstack-head stack) (shadowframe-status frame)))
      )

    (case (shadowframe-status frame) ;; Check BEFORE locking.
      [(available ivar-blocked)   
       (and (mutex-acquire (shadowframe-mut frame) #f) ;; Don't block on it           
            ;;(begin (mutex-acquire (shadowframe-mut frame)) #t) ;; NONBLOCKING VERSION APPEARS TO HAVE A PROBLEM!!?
	    ;; From here on out we've got the mutex:
	    (case (shadowframe-status frame) ;; Double check after waking up.
	      [(available) #t]
	      [(ivar-blocked)
	       (when DEBUG (print "  (TID ~s) ATTEMPT TO STEAL ivar-blocked FRAME, avail ~s \n" 
				  (get-thread-id) (ivar-avail? (shadowframe-argval frame)) ))
	       (ivar-avail? (shadowframe-argval frame))]  ;; TODO: if blocked on ivar, might want a different return value for steal-work!
	      [else (begin ;(bump) ;; FIXME: Head not used yet.
		      (mutex-release (shadowframe-mut frame)) ;; If someone beat us here, we fizzle
		      #f)]
		    )
	    (let ((oldstatus (shadowframe-status frame)))
	      (when DEBUG (print "  (TID ~s) STOLE work! ~s\n" (get-thread-id) frame))
	      ;;(print "STOLE work! ~s \n" frame)

	      (set-shadowframe-status! frame 'stolen) ;; Could have done this atomically with CAS
	      (mutex-release (shadowframe-mut frame)) ;; After we set status we can let go.
	      ;; Then let go to do the real work, note that this may do further pcall's:
	      (set-shadowframe-argval! frame 	                               
				       ((shadowframe-oper frame) 
				       ;; If stealing an ivar-blocked computation, we need an extra step:
					(if (eq? 'ivar-blocked oldstatus)
					    (begin (when DEBUG (print "Unpacking ivar for continuation: ~s" (shadowframe-argval frame)))
					      (ivar-val (shadowframe-argval frame)))
					    (shadowframe-argval frame))
				        ))
	      (when DEBUG (print "  (TID ~s) DONE with stolen work: ~s\n" (get-thread-id) frame))

	      ;; Now we *must* acquire the lock (even if we block) in order to set the status to done.
	      ;; [2010.10.31]  Really? Why?  Shouldn't we own it after it's stolen?
	      (mutex-acquire (shadowframe-mut frame)) ;; blocking...
	      (set-shadowframe-status! frame 'done) ;; Now the result is in place, and status is 'done
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
	    (if (fx=? i tl)
		#f ;; No work on this processor, try again. 
		(if (steal-work! stack (vector-ref frames i))
		    #t
		    (frmloop (fx+ 1 i))))))))

  ;; Fork a worker thread and return its stack:
  (define (make-worker)
    (define stack (new-stack))
    (fork-thread (lambda ()                
                   (this-stack stack) ;; Initialize stack. 
		   (let ([myid 
			  (with-mutex global-mut ;; Register our existence.
			    (set! threads-registered (+ 1 threads-registered))
			    threads-registered)])
		     ;; Steal work approximately forever:
		     (let steal-loop ()
		       (find-and-steal-once!)
		       (if par-finished
			   (with-mutex global-mut 
			      (set! threads-registered (- threads-registered 1))
			      (fprintf (current-error-port) "  [par] worker ~s terminating (TID ~s, ~s remain).\n"
				       myid (get-thread-id) threads-registered))
			   (steal-loop))))))
     stack)


  ;; --------------------------------------------------------------------------------	      
  ;; Helpers for stack/frame manipulation:

  ;; TODO: possibly make these syntax instead of functions:
  (define (push! stack oper val status)
    ;; No lock here because only the owning thread is allowed to push.
    (let ([frame (vector-ref (shadowstack-frames stack) (shadowstack-tail stack))])
      ;; Initialize the frame
      (set-shadowframe-oper!   frame oper)
      (set-shadowframe-argval! frame val)
      (set-shadowframe-status! frame  status)
      (set-shadowstack-tail! stack (fx+ (shadowstack-tail stack) 1)) ;; bump cursor
      ;; TODO: could check for stack-overflow here and possibly add another stack segment...
      frame))
  (define (pop! stack) 
     (when DEBUG (print "  (TID ~s) Popping frame #~a: ~a\n" (get-thread-id) (shadowstack-tail stack) 
			(vector-ref (shadowstack-frames stack) (shadowstack-tail stack))))
     (set-shadowstack-tail! stack (fx- (shadowstack-tail stack) 1)))

  ;; Capturing a common sequence of operations to avoid duplication:
  (define-inlined (mark-pop-release! stack frame)
    ;(set-shadowframe-status! frame 'stolen) ;; Just in case...
    ;(set-shadowframe-status! frame 'corrupt) ;; Just in case...
    ;; Another thread could glance at this frame after its been deactivated:
    (set-shadowframe-status! frame #f)
    (pop! stack) ;; Hold the lock on the frame while we destroy it.
    (mutex-release (shadowframe-mut frame)))

  ;; --------------------------------------------------------------------------------	      

  ;; Do a call in parallel with something else.
  (define-syntax pcall
    (syntax-rules ()
      [(_ op (f x) e2)
       (let ([stack (this-stack)]) ;; thread-local parameter
         (begin ;let ([op1 op] [f1 f] [x1 x]) ;; Optional Safety: Don't duplicate subexpressions:
	   (let ([frame (push! stack f x 'available)])  ;; Only each thread can push on its own stack.
	     (let ([val1 e2])  ;; Evaluate the second expression on this thread.
	       (let waitloop ()
                 (mutex-acquire (shadowframe-mut frame))
                 (case (shadowframe-status frame)
                   [(available)                   
		    (mark-pop-release! stack frame) ;; Pop before we even start the thunk from the frame.
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
                   [(done) (pop! stack) 
                         (mutex-release (shadowframe-mut frame))
                         (op (shadowframe-argval frame) val1)]
		   [else (threaderror 'waitloop "invalid status: ~s" (shadowframe-status frame))]
		   ))))))]))


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
	  (threaderror 'set-ivar! "error ivar should only be assigned once!  Already contains value ~s\n" old))
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

  ;; Like register-on-ivar but pushes a new stack frame that is blocked on the ivar.
  ;; If the new frame gets pushed this thread will go off and steal work.
  (define (ivar-apply-or-block fn ivar)
      ;; FIRST test: before blocking:
      (if (ivar-avail? ivar);(not (eq? v magic2)) ;; Potential mini-optimization: inline ivar-avail? here.
          (begin
	    (when DEBUG (print "  (TID ~s) Ivar available on first try ~s\n" (get-thread-id) ivar))
	    (fn (ivar-val ivar)) ; (fn v)
	    )
	  (let* ([stack (this-stack)] ;; thread-local parameter
		 [_ (when DEBUG (print "\n\n  (TID ~s) BLOCKING ON IVAR fn = ~s\n\n" (get-thread-id) fn))]
		 [frame (push! stack fn ivar 'ivar-blocked)]) ;; A new frame for the read-blocked computation.
	    (find-and-steal-once!) ;; Let some time pass before polling again.

	    ;; This is similar to the waitloop inside pcall:
	    (let waitloop ()
	      (define (release-and-steal!) ;; Used twice below.
		(mutex-release (shadowframe-mut frame))		 
		(find-and-steal-once!) ;; Meanwhile we should go try to make ourselves useful..		  
		(waitloop))

	      ;; We're the frame creator, when we get to this frame, we lock it off from all other comers.
	      ;; Thieves should do non-blocking probes.
	      (mutex-acquire (shadowframe-mut frame))
	      (let ([status (shadowframe-status frame)])
		(cond
		 [(eq? status 'ivar-blocked)
		  ;; If the status is still ivar-blocked and we hold the mutex it has not been stolen.
		  (when DEBUG (print "  (TID ~s) Polling ivar: ~s\n" (get-thread-id) ivar))
		  (if (ivar-avail? ivar) ;; SECOND test: can we unblock?
		      (begin
			(mark-pop-release! stack frame)
			(when DEBUG (print "  (TID ~s) Frame nuked.  Original owner executing continuation on ivar ~s\n" (get-thread-id) ivar))
			(fn (ivar-val ivar)))
		      ;; Otherwise steal and then come back to it:
		      (release-and-steal!))]

		 ;; IVar-blocked frames will be set to stolen by a thief too (AFTER the ivar is filled).
		 [(eq? status 'stolen) (release-and-steal!)]

		 [(eq? status 'done) 
		  (pop! stack)
		  (mutex-release (shadowframe-mut frame))
		  (when DEBUG (ASSERT (ivar-avail? ivar)))
		  ;; The ivar's (delimited) continuation is already executed, return value is here:
		  ; (shadowframe-argval frame) ;; But ivar-apply-or-block doesn't currently have a return value.
		  (void)
		  ]
		 
		 [else (threaderror 'waitloop "invalid status: ~s" (shadowframe-status frame))]
		 )))
	    (when DEBUG (print "  (TID ~s) IVAR unblocked, returning: ~s\n" (get-thread-id) ivar))
	    )))

;;================================================================================


) ;; End library.
