#! /bin/sh
#|
exec mzscheme -qr "$0" ${1+"$@"}
|#

;; This really puts the system through the paces.
;; It's a PLT-ONLY script.  (Chez's system call doesn't return the error code.)

(require (lib "process.ss") (lib "date.ss"))

; ----------------------------------------
(define start-time (current-inexact-milliseconds))
(define failed #f)
(define (code->msg! m) (if (zero? m) "passed" 
			  (begin (set! failed #t) "-FAILED-")))
(define (file->string filename)
    (let ([p (open-input-file filename)])
      (let loop ([c (read-char p)]
                 [acc '()])
        (if (eof-object? c)
            (begin (close-input-port p)
                   (list->string (reverse acc)))
            (loop (read-char p) (cons c acc))))))
(define (string->file str fn)
    (let ([p (open-output-file fn 'replace)])
      (display str p)
      (close-output-port p)))

(define (mail to subj msg)
  (string->file msg "temp.msg")
  (system (format "mail ~a -s '~a' < temp.msg" to subj))
  (delete-file "temp.msg")
  (printf "Mail Sent.\n"))
(define-syntax ASSERT
  (lambda (x)
    (syntax-case x ()
      [(_ expr) 
       #'(or expr 
	     (begin (mail "ryan.newton@alum.mit.edu" 
			  "Failure of supertest.ss"
			  (format "ASSERT failed: ~s\n\nCurrent Directory: ~s\n" 
				  #'expr (current-directory)))
		    (exit 1)))])))
; ----------------------------------------
;;; Main Script:

;; Catch any errors encountered below and send an email:
(current-exception-handler
 (lambda (exn)
   (define msg
     (format "ERROR during script execution:\n   ~a\n\nException: ~s\n" 
	     (exn-message exn) exn))
   (mail "ryan.newton@alum.mit.edu" "Failure of supertest.ss" msg)
   (exit 1)))

(define date 
  (let ((d (seconds->date (current-seconds))))
    (format "~a-~a-~a_~a:~a:~a" 
	    (date-year d) (date-month d) (date-day d)
	    (date-hour d) (date-minute d) (date-second d))))
(define logfile (format "supertest_~a.log" date))
(define log (open-output-file logfile))

;(ASSERT (system "source ../install_environment_vars"))

(define svn-revision
  (begin 
    (ASSERT (eqv? 0 (system/exit-code "which svn > /dev/null")))
    (ASSERT (eqv? 0 (system/exit-code "svn info | grep Revision | sed s/Revision:// > svn_rev.txt")))
    (read (open-input-file "svn_rev.txt"))))

;; Here we begin running tests:

(begin (printf "============================================================\n")
       (define cleaned (system/exit-code "make clean"))
       (fprintf log "Build directory cleaned:                      ~a\n" (code->msg! cleaned)))

(begin (define runpetite (system/exit-code "echo | ../depends/petite"))
       (fprintf log "petite: Repository's Petite Chez runs:        ~a\n" (code->msg! runpetite))
       )

(begin (define testpetite
	 (system/exit-code 
	  "echo \"(define-top-level-value 'REGIMENT-BATCH-MODE #t) (test-units)\" | ../depends/petite main_chez.ss"))
       (fprintf log "petite: Load & run unit tests:                ~a\n" (code->msg! testpetite))
       )

(begin (define fullchez (system/exit-code "which chez > /dev/null"))
       (fprintf log "chez: Full Chez Scheme on the test system:    ~a\n" (code->msg! fullchez))
       )

(begin (newline)
       (printf "============================================================\n")
       (define loaded (system/exit-code "./regiment_script.ss"))
       (fprintf log "chez: WScript loads from source (via script): ~a\n" (code->msg! loaded)))

(begin (newline)
       (printf "============================================================\n")
       (define compilerworks (system/exit-code "echo '(compile 3)' | ./regiment_script.ss i --exit-error"))
       (fprintf log "chez: WScript has access to the compiler:     ~a\n" (code->msg! compilerworks)))

(begin (newline)
       (printf "First: from source\n")
       (printf "============================================================\n")
       (define frmsrc (system/exit-code "./regiment_script.ss test"))
       (fprintf log "chez: Unit tests, loaded from source:         ~a\n" (code->msg! frmsrc)))

(begin (newline)
       (printf "Second: building Chez shared object\n")
       (printf "============================================================\n")
       (define buildso (system/exit-code "make chez"))
       (fprintf log "chez: Build .so file:                         ~a\n" (code->msg! buildso))

       (ASSERT (system "./regiment_script.ss 2> temp.out"))
       (define loadedso (system/exit-code "grep 'compiled .so' temp.out"))       
       (fprintf log "chez: System loads from .so file:             ~a\n" (code->msg! loadedso))
       (delete-file "temp.out")

;; Disabling this temporarily, problem with simalpha-generate-modules (and lang_wavescript):
;; FIXME:

;       (define runso (system/exit-code "./regiment_script.ss test"))
;       (fprintf log "chez: Unit tests, loaded from .so file:       ~a\n" (code->msg! runso))
       )

(begin (newline)
       (printf "Third: building bytecode in PLT\n")
       (printf "============================================================\n")
       (define pltbc (system/exit-code "make pltbc"))
       (fprintf log "plt: Building bytecode in PLT:                ~a\n" (code->msg! pltbc)))

;; TODO: Run tests from PLT:

;; TODO: Run testall_demos 

;; TODO: Checkout and run WaveScope engine.

(fprintf log "\nTotal time spent testing: ~a minutes\n" 
	 (/ (round (* 10 
		      (/ (- (current-inexact-milliseconds) start-time) 1000. 60.)))
	    10))
(close-output-port log)

(mail ;"ws@nms.csail.mit.edu" 
      "rrnewton@gmail.com" 
      (if failed 
	  (format "WaveScript rev ~a FAILED nightly regression tests" svn-revision)
	  (format "WaveScript rev ~a passed nightly regression tests" svn-revision))
      (file->string logfile))

