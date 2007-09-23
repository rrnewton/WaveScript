;; Now doing a hack to get rid of thunk allocation.

(eval-when (compile eval load) 
  (optimize-level 2)
  (collect-trip-bytes (* 20 1048576)) ;; collects 47 times in ~3 sec
  )

(module ()

(define-syntax ASSERT
  (lambda (x)
    (syntax-case x ()
      [(_ expr) #'(or expr (error 'ASSERT "failed: ~s" (IFCHEZ #'expr (format-syntax #'expr))))]
      ;; This form is (ASSERT integer? x) returning the value of x.
      [(_ fun val) #'(let ([v val])
		       (if (fun v) v			   
			   (error 'ASSERT "failed: ~s\n Value which did not satisfy above predicate: ~s" 
				  (IFCHEZ #'fun (format-syntax #'fun))
				  v)))]
      )))

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
  ;; status may be 'available, 'stolen, or 'done
  (define-record shadowframe  (mut status oper argval))
#;
  (begin (define make-shadowframe vector)
         (define (shadowframe-mut v)      (vector-ref v 0))
         (define (shadowframe-status v)   (vector-ref v 1))
         (define (shadowframe-oper v)     (vector-ref v 2)) 
         (define (shadowframe-argval v)   (vector-ref v 3))
         (define (set-shadowframe-mut! v x)      (vector-set! v 0 x))
         (define (set-shadowframe-status! v x)   (vector-set! v 1 x))
         (define (set-shadowframe-oper! v x)     (vector-set! v 2 x))
         (define (set-shadowframe-argval! v x)   (vector-set! v 3 x))
         )

  ;; There's also a global list of threads:
  (define allstacks #()) ;; This is effectively immutable.
  (define par-finished #f)
  ;; And a mutex for global state:
  (define global-mut (make-mutex))
  (define threads-registered 1)

  ;; A new stack has no frames, but has a (hopefully) unique ID:
  (define (new-stack) 
    (make-shadowstack (random 10000) 
      0            ;; Head pointer.
      0            ;; Tail pointer.
      (vector-build 50 
        (lambda (_) (make-shadowframe (make-mutex) #f #f #f)))))

  ;; A per-thread parameter.
  (define this-stack (make-thread-parameter (new-stack)))
  
  ;; Mutated below:
  (define numprocessors #f)

  ;; We spin until everybody is awake.
  (define (wait-for-everybody)
    (let wait-for-threads ()
      ;(printf "  ~s ~s\n" threads-registered numprocessors)
      (unless 
	;(= threads-registered numprocessors)
  	(with-mutex global-mut (= threads-registered numprocessors))
	  (wait-for-threads))))

    ;; DEBUGGING:
  ;;  Pick a print:
     (define (print . args) (with-mutex global-mut (apply printf args) (flush-output-port)))
  ;   (define (print . args) (apply printf args))
  ;   (define (print . args) (void)) ;; fizzle


  ;; ----------------------------------------

  (define (init-par num-cpus)
    (printf "\n  Initializing PAR system for ~s threads.\n" num-cpus)
    (with-mutex global-mut   
      (ASSERT (eq? threads-registered 1))
      (set! numprocessors num-cpus)
      (set! allstacks (make-vector num-cpus))
      (vector-set! allstacks 0 (this-stack))
      ;; We fork N-1 threads (the original one counts)
      (do ([i 1 (fx+ i 1)]) ([= i num-cpus] (void))
        (vector-set! allstacks i (make-worker))))
    (wait-for-everybody)
    (printf "Everyone's awake!\n"))

  (define (shutdown-par) (set! par-finished #t))
  (define (par-status) 
    (printf "Par status:\n  par-finished ~s\n  allstacks: ~s\n  stacksizes: ~s\n\n"
            par-finished (vector-length allstacks)
            (map shadowstack-tail (vector->list allstacks))))

  ;; ----------------------------------------

  ;; Try to do work and mark it as done.
  (define (steal-work! frame)
    (and (eq? 'available (shadowframe-status frame))
	 ;(mutex-acquire (shadowframe-mut frame) #f) ;; Don't block on it
	 (mutex-acquire (shadowframe-mut frame)) ;; NONBLOCKING VER HAS A PROBLEM
	 ;; From here on out we've got the mutex:
	 (if (eq? 'available (shadowframe-status frame)) ;; If someone beat us here, we fizzle
	     #t 
	     (begin (print "    fizzled....\n")
		    (mutex-release (shadowframe-mut frame)) 
		    #f))
	 (begin 
	   ;;(printf "STOLE work! ~s\n" frame)
	   (set-shadowframe-status! frame 'stolen)
;	   (print "STOLEN, releasing...\n")
	   (mutex-release (shadowframe-mut frame)) ;; Then let go to do the real work.
;	   (print "Now computing...\n")
	   (set-shadowframe-argval! frame 
	      ((shadowframe-oper frame) (shadowframe-argval frame)))
	   ;; Now we *must* acquire it in order to set the status to done.
;	   (print "Grabbing again to mark done.\n")
	   (mutex-acquire (shadowframe-mut frame)) ;; blocking...
;	   (print "  grabbed\n")
	   (set-shadowframe-status! frame 'done)	   
	   (mutex-release (shadowframe-mut frame)) ;; Then let go to do the real work.
;	   (print "  let go\n")
	   #t)))

  (define (make-worker)
    (define stack (new-stack))
    (fork-thread (lambda ()                
                   (this-stack stack) ;; Initialize stack. 
		   (with-mutex global-mut ;; Register our existence.
		     (set! threads-registered (add1 threads-registered)))
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
                                 (if (steal-work! (vector-ref frames i))
                                     (forever)
                                     (frmloop (fx+ 1 i)))))))))))
    stack)

  (define-syntax pcall
    (syntax-rules ()
      [(_ op (f x) e2)
       (let ([stack (this-stack)])
         (define (push! oper val)
           (let ([frame (vector-ref (shadowstack-frames stack) (shadowstack-tail stack))])
             ;; Initialize the frame
             (set-shadowframe-oper!   frame oper)
             (set-shadowframe-argval! frame val)
             (set-shadowframe-status! frame  'available)
             (set-shadowstack-tail! stack (fx+ (shadowstack-tail stack) 1))
             frame))
         (define (pop!) (set-shadowstack-tail! stack (fx- (shadowstack-tail stack) 1)))

         (let ([frame (push! f x)])
           (let ([val1 e2])
	     ;; We're the parent, when we get to this frame, we lock it off from all other comers.
	     ;; Thieves do non-blocking probes.
	     (let ([result 
		    (let spinwait ()
		      (mutex-acquire (shadowframe-mut frame))
		      (case (shadowframe-status frame)
			[(available)
			 ;(print "nobody stole it\n")
			 (pop!) ;; Pop before we even start the thunk.
			 (op val1 
			     ((shadowframe-oper frame) (shadowframe-argval frame))
			     )]
			;; Oops, they may be waiting to get back in here and set the result, let's get out quick.
			[(stolen) 
			 ;; For now we just spin until they're done:
			 (mutex-release (shadowframe-mut frame))			 
			 ;(print ".")
			 (spinwait)]
			;; It was stolen and is now completed:
			[else (pop!) (op val1 (shadowframe-argval frame))]))])
	       ;(print "  OUTTA THERE\n")
	       (mutex-release (shadowframe-mut frame))
	       result)
             )))]))



#;
  (define-syntax parmv
    (syntax-rules () 
      []))


;;================================================================================

  (init-par (string->number (or (getenv "NUMTHREADS") "2")))
  (printf "Run using parallel add-tree via pcall mechanism:\n")
  (let ()
    (define (tree n)
      (if (zero? n) 1
          (pcall + (tree (sub1 n)) (tree (sub1 n)))))
    (printf "\n~s\n\n" (time (tree test-depth)))
    (par-status))



) 




