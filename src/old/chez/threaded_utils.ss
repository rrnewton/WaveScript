
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

     (pcall find-and-steal-once! push! pop! mark-pop-release!)
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
     threaderror
     )
  
  ;(import chez_constants)

(define (format-syntax-nicely x) x)

;; [2010.11.01] The default error isn't working on other threads for me:
(define (threaderror sym str . args)
 (fprintf (current-error-port) "\nERROR: ~a" (apply format str args))
 (flush-output-port (current-error-port))
 (par-status 'verbose)
 (exit))


(define-syntax ASSERT
  (lambda (x)
    (syntax-case x ()
      [(_ expr) #'(or expr (threaderror 'ASSERT (format "failed: ~s" (format-syntax-nicely #'expr))))])))

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
	 [(_ x ...) (begin e ...)]))]))

;; ================================================================================
;; <-[ VERSION 5 ]->

;; Using a more restricted "pcall" syntax to try to minimize allocation.
;; Also, this includes Kent's modifications to get rid of allocation.

;; CHANGELOG:
;; 
;; [2010.10.31] Adding support for blocking on ivars. This is a new state for a shadowframe.

;; [2010.11.01] FACTORED THIS OUT INTO par5.sls
(begin
  ;;================================================================================

)  ;; End version 5


;; ================================================================================
;; <-[ VERSION 6 ]->

;; TODO: Use a proper cactus-stack.


;; ================================================================================
;; <-[ VERSION 7 ]->

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
