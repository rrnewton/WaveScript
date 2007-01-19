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
  (define tmpfile (format "/tmp/temp~a.msg" (current-milliseconds)))
  (string->file msg tmpfile)
  (system (format "mail ~a -s '~a' < ~a" to subj tmpfile))
  (delete-file tmpfile)
  (printf "Mail Sent, to:~a subj: ~a\n" to subj)
  (printf "Body:\n~a\n" msg)
  )
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
(define log (open-output-file logfile 'replace))
(define scriptoutput (open-output-file "SUPERTEST_FULL_OUTPUT.log" 'replace))
(current-output-port scriptoutput)
(current-error-port scriptoutput)

;(ASSERT (system "source ../install_environment_vars"))

(define svn-revision
  (begin 
    (ASSERT (eqv? 0 (system/exit-code "which svn > /dev/null")))
    (ASSERT (eqv? 0 (system/exit-code "svn info | grep Revision | sed s/Revision:// > svn_rev.txt")))
    (read (open-input-file "svn_rev.txt"))))

;; Here we begin running tests:

(begin (printf "============================================================\n")
       (define cleaned (system/exit-code "make clean"))
       (fprintf log "Build directory cleaned:                      ~a\n" (code->msg! cleaned))
       (flush-output log))

(begin (define runpetite (system/exit-code "echo | ../depends/petite"))
       (fprintf log "petite: Repository's Petite Chez runs:        ~a\n" (code->msg! runpetite))
       (flush-output log))

(begin (define testpetite
	 (system/exit-code 
	  "echo \"(define-top-level-value 'REGIMENT-BATCH-MODE #t) (test-units)\" | ../depends/petite main_chez.ss &> 0_PETITE_UNIT_TESTS.log"))
       (fprintf log "petite: Load & run unit tests:                ~a\n" (code->msg! testpetite))
       (flush-output log))

(begin (define fullchez (system/exit-code "which chez > /dev/null"))
       (fprintf log "chez: Full Chez Scheme on the test system:    ~a\n" (code->msg! fullchez))
       (flush-output log)
       )

(begin (newline)
       (printf "============================================================\n")
       (define loaded (system/exit-code "./regiment_script.ss &> 1_SCRIPT_LOAD.log"))
       (fprintf log "chez: WScript loads from source (via script): ~a\n" (code->msg! loaded))
       (flush-output log))

(begin (newline)
       (printf "============================================================\n")
       (define compilerworks (system/exit-code "echo '(compile 3)' | ./regiment_script.ss i --exit-error"))
       (fprintf log "chez: WScript has access to the compiler:     ~a\n" (code->msg! compilerworks))
       (flush-output log))

(begin (newline)
       (printf "First: from source\n")
       (printf "============================================================\n")
       (define frmsrc (system/exit-code "./regiment_script.ss test &> 2_UNIT_TESTS.log"))
       (fprintf log "chez: Unit tests, loaded from source:         ~a\n" (code->msg! frmsrc))
       (flush-output log))

(begin (newline)
       (printf "Second: building Chez shared object\n")
       (printf "============================================================\n")
       (define buildso (system/exit-code "make chez &> 3_BUILD_SO.log"))
       (fprintf log "chez: Build .so file:                         ~a\n" (code->msg! buildso))
       (flush-output log)

       (ASSERT (system "./regiment_script.ss 2> temp.out"))
       (define loadedso (system/exit-code "grep 'compiled .so' temp.out"))       
       (fprintf log "chez: System loads from .so file:             ~a\n" (code->msg! loadedso))
       (flush-output log)
       (delete-file "temp.out")
;; Disabling this temporarily, problem with simalpha-generate-modules (and lang_wavescript):
;; FIXME:

;       (define runso (system/exit-code "./regiment_script.ss test"))
;       (fprintf log "chez: Unit tests, loaded from .so file:       ~a\n" (code->msg! runso))
       )

;; Now clean again:
(ASSERT (system "make clean"))

(begin (newline)
       (printf "Third: building Wscript bytecode in PLT\n")
       (printf "============================================================\n")
       (define wsparse (system/exit-code "make wsparse &> 4_BUILD_WSPARSE.log"))
       (fprintf log "plt: Building wsparse executable:             ~a\n" (code->msg! wsparse))
       (flush-output log)
       )

(begin (define pltbc (system/exit-code "make pltbc &> 5_BUILD_PLT_BYTECODE.log"))
       (fprintf log "plt: Building WScript as bytecode in PLT:     ~a\n" (code->msg! pltbc))
       (flush-output log))

;; THIS DOESN'T WORK YET: Doesn't return the proper error code.
#;
(begin (newline)
       (printf "Fourth: Running tests in PLT\n")
       (printf "============================================================\n")
       (define plttests (system/exit-code "echo '(test-units)' | mzscheme -f main_plt.ss &> 6_PLT_UNIT_TESTS.log"))
       (fprintf log "plt: Running tests in PLT:                    ~a\n" (code->msg! plttests))
       (flush-output log))

(begin (newline)
       (printf "Fifth: Running WaveScript Demos\n")
       (printf "============================================================\n")
       (current-directory "../demos/wavescope")
       (define wsdemos (system/exit-code "./testall_demos.ss &> 7_WS_DEMOS.log"))
       (current-directory "../../src")
       (fprintf log "\nchez: Running WaveScript Demos:             ~a\n" (code->msg! wsdemos))
       (flush-output log))

;; TODO: Run tests from PLT:

;; TODO: Run testall_demos 

;; TODO: Checkout and run WaveScope engine.

(fprintf log "\nTotal time spent testing: ~a minutes\n" 
	 (/ (round (* 10 
		      (/ (- (current-inexact-milliseconds) start-time) 1000. 60.)))
	    10))
(close-output-port log)

(mail "ws@nms.csail.mit.edu" 
      ;"rrnewton@gmail.com" 
      (if failed 
	  (format "WaveScript rev ~a FAILED nightly regression tests" svn-revision)
	  (format "WaveScript rev ~a passed nightly regression tests" svn-revision))
      (file->string logfile))

