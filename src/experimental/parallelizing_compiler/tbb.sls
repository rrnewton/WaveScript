

(library (tbb)

  (export make-spin-mutex acquire-spin-mutex try-acquire-spin-mutex release-spin-mutex
          test-tbb)
  (import (scheme))

(define ___
  (let ((tbbpath (getenv "TBB")))
    (unless tbbpath (error 'tbb "TBB environment variable not set"))
    ;; Note on linux the normal .so is a text file containing the text "INPUT (libtbb.so.2)"
    (load-shared-object (string-append tbbpath "/lib/intel64/"
				       (case (machine-type)
					 [(a6le ta6le i3le ti3le i3fb ti3fb) "libtbb.so.2"]
					 [(ppcosx tppcosx i3osx ti3osx a6osx ta6osx) "libtbb.dylib"]
					 [else (error 'load-tbb "unhandled machine type! ~a" (machine-type))])))

    (load-shared-object "./tbb_wrapper.so")))

(define make-spin-mutex        (foreign-procedure "make_spin_mutex" () uptr))
(define acquire-spin-mutex     (foreign-procedure "acquire_spin_mutex" (uptr) void))
(define release-spin-mutex     (foreign-procedure "release_spin_mutex" (uptr) void))
(define try-acquire-spin-mutex (foreign-procedure "try_acquire_spin_mutex" (uptr) boolean))

; (define make_queuing_mutex        (foreign-procedure "make_queuing_mutex" () uptr))
; (define acquire_queuing_mutex     (foreign-procedure "acquire_queuing_mutex" (uptr) void))
; (define release_queuing_mutex     (foreign-procedure "release_queuing_mutex" (uptr) void))
; (define try_acquire_queuing_mutex (foreign-procedure "try_acquire_queuing_mutex" (uptr) boolean))

;; ================================================================================

(define (test-tbb)
  
  (define _ (printf "Begin first test:\n"))
  (define m (make-spin-mutex))
  (define __ (printf "Made mutex: ~s\n" m))
  (define ___ (acquire-spin-mutex m))
  (define bool (try-acquire-spin-mutex m))
  (define ____ (begin (printf "Try_acquire initially: ~s\n" bool)
		      (if bool (release-spin-mutex m))
		      (printf "Locked!\n")
		      (printf "Try_acquire subsequently: ~s\n" (try-acquire-spin-mutex m))
		      (release-spin-mutex m)
		      (printf "Unocked!\n")))
  (define threads 4)
  (define m2 (make-spin-mutex))
  (for-each
      (lambda (id)
	(fork-thread (lambda ()
		       (let loop ((i 10))		       
			 (acquire-spin-mutex m2)
			 (if (> i 0)
			     (printf "Thread ~s got mutex!\n" id)
			     (printf " * Thread ~s finished!\n" id))
			 (release-spin-mutex m2)
			 (when (> i 0) (loop (sub1 i)))))))
    (iota threads))

  (sleep (make-time 'time-duration 0 1))
  (printf "Done sleeping... exiting\n"))

) ;; End library

