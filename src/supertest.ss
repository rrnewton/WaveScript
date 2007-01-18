#! /bin/sh
#|
exec mzscheme -qr "$0" ${1+"$@"}
|#

;; This really puts the system through the paces.
;; It's a PLT-ONLY script.  (Chez's system call doesn't return the error code.)

(require (lib "process.ss") (lib "date.ss"))

(define date 
  (let ((d (seconds->date (current-seconds))))
    (format "~a-~a-~a_~a:~a:~a" 
	    (date-year d) (date-month d) (date-day d)
	    (date-hour d) (date-minute d) (date-second d))))
(define logfile (format "supertest_~a.log" date))
(define log (open-output-file logfile))

; ----------------------------------------
(define failed #f)
(define (code->msg! m) (if (zero? m) "passed" 
			  (begin (set! failed #t) "-FAILED-")))
(define file->string
  (lambda (filename)
    (let ([p (open-input-file filename)])
      (let loop ([c (read-char p)]
                 [acc '()])
        (if (eof-object? c)
            (begin (close-input-port p)
                   (list->string (reverse acc)))
            (loop (read-char p) (cons c acc)))))))
(define string->file
  (lambda (str fn)
    (let ([p (open-output-file fn 'replace)])
      (display str p)
      (close-output-port p))))

(define (mail to subj msg)
  (string->file msg "temp.msg")
  (system (format "mail ~a -s '~a' < temp.msg" to subj))
  (delete-file "temp.msg")
  (printf "Mail Sent.\n"))
(define (ASSERT b) 
  (unless b 
    (mail "ryan.newton@alum.mit.edu" 
	  "Failure of supertest.ss" "Unexpected failed ASSERT in the test system.")
    (exit 1)))
; ----------------------------------------

;(ASSERT (system "source ../install_environment_vars"))

(define svn-revision
  (begin 
    (ASSERT (eqv? 0 (system/exit-code "which svn > /dev/null")))
    (ASSERT (eqv? 0 (system/exit-code "svn info | grep Revision | sed s/Revision:// > svn_rev.txt")))
    (read (open-input-file "svn_rev.txt"))))

;; This begins the tests:

(begin (printf "============================================================\n")
       (define cleaned (system/exit-code "make clean"))
       (fprintf log "Build directory cleaned:                 ~a\n" (code->msg! cleaned)))

;; install environment vars...

(begin (newline)
       (printf "============================================================\n")
       (define loaded (system/exit-code "./regiment_script.ss"))
       (fprintf log "chez: System loads from source:          ~a\n" (code->msg! loaded)))

(begin (newline)
       (printf "First: from source\n")
       (printf "============================================================\n")
       (define frmsrc (system/exit-code "./regiment_script.ss test"))
       (fprintf log "chez: Unit tests, loaded from source:    ~a\n" (code->msg! frmsrc)))

(begin (newline)
       (printf "Second: building Chez shared object\n")
       (printf "============================================================\n")
       (define buildso (system/exit-code "make chez"))
       (fprintf log "chez: Build .so file:                    ~a\n" (code->msg! buildso))

       (ASSERT (system "./regiment_script.ss 2> temp.out"))
       (define loadedso (system/exit-code "grep 'compiled .so' temp.out"))       
       (fprintf log "chez: System loads from .so file:        ~a\n" (code->msg! loadedso))
       (delete-file "temp.out")

;; Disabling this temporarily, problem with simalpha-generate-modules (and lang_wavescript):
;; FIXME:

;       (define runso (system/exit-code "./regiment_script.ss test"))
;       (fprintf log "chez: Unit tests, loaded from .so file:  ~a\n" (code->msg! runso))
       )

(begin (newline)
       (printf "Third: building bytecode in PLT\n")
       (printf "============================================================\n")
       (define pltbc (system/exit-code "make pltbc"))
       (fprintf log "plt: Building bytecode in PLT:           ~a\n" (code->msg! pltbc)))

;; TODO: Run tests from PLT:

;; TODO: Run testall_demos 

;; TODO: Checkout and run WaveScope engine.

(close-output-port log)

(mail "ws@nms.csail.mit.edu" ;"rrnewton@gmail.com" 
      (if failed 
	  (format "WaveScript rev ~a FAILED nightly regression tests" svn-revision)
	  (format "WaveScript rev ~a passed nightly regression tests" svn-revision))
      (file->string logfile))

;(printf "CLEANED: ~a  FRMSRC:~a\n" clean frmsrc)

;(exit 0)

#|
	@echo Testing different build methods.
	@echo
	@echo ======================================================================
	$(MAKE) clean 
	@echo
	@echo First from source:
	@echo ======================================================================
	./regiment_script.ss test
	echo 'ntohue' | regiment i --exit-error
	echo HMM $?
	@echo
	@echo Second from compiled shared-object:
	@echo ======================================================================
|#