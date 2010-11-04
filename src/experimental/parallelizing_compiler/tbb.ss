



;; Note the normal .so is a text file containing the text "INPUT (libtbb.so.2)"
(load-shared-object (string-append (getenv "TBB") "/lib/intel64/libtbb.so.2"))
(printf "TBB loaded\n")

(load-shared-object "./tbb_wrapper.so")
(printf "Wrapper loaded\n")

(define make_spin_mutex        (foreign-procedure "make_spin_mutex" () uptr))
(define acquire_spin_mutex     (foreign-procedure "acquire_spin_mutex" (uptr) void))
(define release_spin_mutex     (foreign-procedure "release_spin_mutex" (uptr) void))
(define try_acquire_spin_mutex (foreign-procedure "try_acquire_spin_mutex" (uptr) boolean))

; (define make_queuing_mutex        (foreign-procedure "make_queuing_mutex" () uptr))
; (define acquire_queuing_mutex     (foreign-procedure "acquire_queuing_mutex" (uptr) void))
; (define release_queuing_mutex     (foreign-procedure "release_queuing_mutex" (uptr) void))
; (define try_acquire_queuing_mutex (foreign-procedure "try_acquire_queuing_mutex" (uptr) boolean))

;; ================================================================================

(begin
  (printf "Begin first test:\n")
  (define m (make_spin_mutex))
  (printf "Made mutex: ~s\n" m)
  (acquire_spin_mutex m)
  (define bool (try_acquire_spin_mutex m))
  (printf "Try_acquire initially: ~s\n" bool)
  (if bool (release_spin_mutex m))
  (printf "Locked!\n")
  (printf "Try_acquire subsequently: ~s\n" (try_acquire_spin_mutex m))
  (release_spin_mutex m)
  (printf "Unocked!\n"))


(define threads 4)
(define m2 (make_spin_mutex))
(for-each
    (lambda (id)
      (fork-thread (lambda ()
		     (let loop ((i 10))		       
		       (acquire_spin_mutex m2)
		       (if (> i 0)
			   (printf "Thread ~s got mutex!\n" id)
			   (printf " * Thread ~s finished!\n" id))
		       (release_spin_mutex m2)
		       (when (> i 0) (loop (sub1 i)))))))
    (iota threads))

(sleep (make-time 'time-duration 0 1))
(printf "Done sleeping... exiting\n")
(exit)




