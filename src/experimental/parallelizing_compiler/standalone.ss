
;; SEPARATE STANDALONE VERSION FOR TESTING -- 
;; Note, this is duplicated code from threaded_utils.ss

;; [2010.10.31] VERSION (4) This should be trash...

(eval-when (compile eval load) 
  (optimize-level 2)
  (collect-trip-bytes (* 20 1048576)) ;; collects 47 times in ~3 sec
  )

;; Tweaking Work-stealing version to use multiple values and do only 2-way par.
(module ((parmv push! parmv-helper this-stack))

  (define test-depth 25) ;; Make a tree with 2^test-depth nodes.

  (define vector-build
    (lambda (n f)
      (let ([v (make-vector n)])
	(do ([i 0 (fx+ i 1)])
	    ((= i n) v)
	  (vector-set! v i (f i))
	  ))))
  
  ;; STATE:

  ;; Each thread's stack has a list of frames, from newest to oldest.
  ;; We use a lock-free approach for mutating/reading the frame list.
  ;; Therefore, a thief might steal an old inactive frame, but this poses no problem.
  ;; 
  ;; A thread's "stack" must be as efficient as possible, because
  ;; it essentially replaces the native scheme stack where par calls
  ;; are concerned.  (I wonder if continuations can serve any purpose here.)
  ;; Note, head is the "bottom" and tail is the "top".  We add to tail.
  (define-record shadowstack (id head tail frames))
  
  ;; Frames are locked individually.
  ;; status may be 'available or 'done
  (define-record shadowframe  (mut status thunkval))

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
      0            ;; Tail pointer.
      (vector-build 50 
	(lambda (_) (make-shadowframe (make-mutex) #f #f)))))

  ;; A per-thread parameter.
  (define this-stack (make-thread-parameter (new-stack)))
  
  ;; Mutated below:
  (define numprocessors #f)

  ;; ----------------------------------------

  ;; Try to do work and mark it as done.
  (define (do-work! frame)
    (and (eq? 'available (shadowframe-status frame))
	 (with-mutex (shadowframe-mut frame)
	   ;; If someone beat us here, we fizzle
	   (and (eq? 'available (shadowframe-status frame))
		(begin 
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
    (printf "\n  Initializing PAR system for ~s threads.\n" num-cpus)
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
    (set-shadowframe-thunkval! frame  thunk)
    (set-shadowframe-status!   frame  'available)
    (set-shadowstack-tail! stack (fx+ (shadowstack-tail stack) 1))
    ;; TODO! Check if we need to realloc the stack!
    frame)
  (define (pop! stack)
    (set-shadowstack-tail! stack (fx- (shadowstack-tail stack) 1)))

  (define (parmv-helper stack frame val1)
    (with-mutex (shadowframe-mut frame)
      (case (shadowframe-status frame)
	[(available)
	 (pop! stack) ;; Pop before we even start the thunk.
	 (values val1 ((shadowframe-thunkval frame)))]
	[else (pop! stack)
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
	   ))]))


  (define (test)
    (init-par (string->number (or (getenv "NUMTHREADS") "2")))

    (printf "Run using parallel add-tree via multiple-value based parmv:\n")
    (let ()
      (define (tree n)
	(if (zero? n) 1
	    (call-with-values (lambda () (parmv (tree (sub1 n)) (tree (sub1 n)))) +)))
      (par-reset!)
      (printf "\n~s\n\n" (time (tree test-depth)))
      (par-status)))

) 
