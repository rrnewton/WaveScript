

(library (tbb)

  (export make-spin-mutex spin-mutex-acquire spin-mutex-try-acquire spin-mutex-release free-spin-mutex
	  make-guarded-spin-mutex
          test-tbb)
  (import (scheme))

(define ___
  (let ((tbbpath (getenv "TBB")))
    (unless tbbpath (error 'tbb "TBB environment variable not set"))
    ;; [2010.11.02] Don't need to load it explicitly if we link it with the tbb_wrapper.so
#;
    (load-shared-object (string-append tbbpath "/lib/intel64/"
				       (case (machine-type)
					 ;; Note on linux the normal .so is a text file containing the text "INPUT (libtbb.so.2)"
					 [(a6le ta6le i3le ti3le i3fb ti3fb) "libtbb.so.2"]
					 [(ppcosx tppcosx i3osx ti3osx a6osx ta6osx) "libtbb.dylib"]
					 [else (error 'load-tbb "unhandled machine type! ~a" (machine-type))])))

    (load-shared-object "./tbb_wrapper.so")
    (fprintf (current-error-port) "  [TBB] TBB Loaded.\n")))

(define make-spin-mutex        (foreign-procedure "make_spin_mutex" () uptr))
(define spin-mutex-acquire     (foreign-procedure "acquire_spin_mutex" (uptr) void))
(define spin-mutex-release     (foreign-procedure "release_spin_mutex" (uptr) void))
(define spin-mutex-try-acquire (foreign-procedure "try_acquire_spin_mutex" (uptr) boolean))

;; Must be freed explicitly, not doing a wrapper/guardian thing right now:
;(define free-spin-mutex foreign-free) ;; Unsafe, foreign-free is only supposed to free foreign-alloc'd 
(define free-spin-mutex        (foreign-procedure "free_spin_mutex" (uptr) void))

;; Using the trick from the Chez Scheme Users Guide, Section 13
;; Silly interface... change this later:
(define make-guarded-spin-mutex
  (let ([malloc-guardian (make-guardian)])
    (lambda (guardee accessor)
      (let loop ()
        (let ([x (malloc-guardian)])
          (when x
	    ;(printf "Guardian FREEING mut ~s\n" (accessor x))
            ;(free-spin-mutex (accessor x))
            (loop))))
      ; then allocate and register the new storage
      (let ([x (make-spin-mutex)])
        (malloc-guardian guardee)
        x))))

; (define make_queuing_mutex        (foreign-procedure "make_queuing_mutex" () uptr))
; (define acquire_queuing_mutex     (foreign-procedure "acquire_queuing_mutex" (uptr) void))
; (define release_queuing_mutex     (foreign-procedure "release_queuing_mutex" (uptr) void))
; (define try_acquire_queuing_mutex (foreign-procedure "try_acquire_queuing_mutex" (uptr) boolean))

;; ================================================================================

(define (test-tbb)
  
  (define _ (printf "Begin first test:\n"))
  (define m (make-spin-mutex))
  (define __ (printf "Made mutex: ~s\n" m))
  (define ___ (spin-mutex-acquire m))
  (define bool (spin-mutex-try-acquire m))
  (define ____ (begin (printf "Try_acquire initially: ~s\n" bool)
		      (if bool (spin-mutex-release m))
		      (printf "Locked!\n")
		      (printf "Try_acquire subsequently: ~s\n" (spin-mutex-try-acquire m))
		      (spin-mutex-release m)
		      (printf "Unocked!\n")))
  (define threads 4)
  (define m2 (make-spin-mutex))
  (for-each
      (lambda (id)
	(fork-thread (lambda ()
		       (let loop ((i 10))		       
			 (spin-mutex-acquire m2)
			 (if (> i 0)
			     (printf "Thread ~s got mutex!\n" id)
			     (printf " * Thread ~s finished!\n" id))
			 (spin-mutex-release m2)
			 (when (> i 0) (loop (sub1 i)))))))
    (iota threads))  

  (sleep (make-time 'time-duration 0 1))
  (free-spin-mutex m2)
  (printf "Done sleeping... exiting\n")
  )

) ;; End library

