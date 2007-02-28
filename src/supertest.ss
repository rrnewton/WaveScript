#! /bin/sh
#|
exec mzscheme -qr "$0" ${1+"$@"}
|#

;; This really puts the system through the paces.
;; It's a PLT-ONLY script.  (Chez's system call doesn't return the error code.)

;; TODO: Add a timeout!  In case the test gets stuck.

(require (lib "process.ss") (lib "date.ss"))


;; Let's clean up some:
(if (file-exists? "/tmp/wsparse_server_pipe") (delete-file "/tmp/wsparse_server_pipe"))
(if (file-exists? "/tmp/wsparse_server_response") (delete-file "/tmp/wsparse_server_response"))

;; Should we killall the wsparse_server processes also?
 
; ----------------------------------------

(define ryan-email "rrnewton@gmail.com")
;(define ryan-email "ryan.newton@alum.mit.edu")
(define start-time (current-inexact-milliseconds))
(define last-test-timer start-time)
(define failed #f)

(define (reset-timer!) (set! last-test-timer (current-inexact-milliseconds)))
(define (milli->minute t) (/ (round (* 10 (/ t 1000. 60.))) 10))
(define (code->msg! m) 
  (let ([val (if (zero? m) 
		 (format "passed (~a min)" 
			 (milli->minute (- (current-inexact-milliseconds) last-test-timer)))
		 (begin (set! failed #t) 
			(format "-FAILED- (code ~a)" m)))])
    (reset-timer!)
    val))
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
(define (fpf . args)
  (apply fprintf log args)
  (flush-output log))
(define-syntax ASSERT
  (lambda (x)
    (syntax-case x ()
      [(_ expr) 
       #'(or expr 
	     (begin (mail ryan-email
			  "Failure of supertest.ss"
			  (format "ASSERT failed: ~s\n\nCurrent Directory: ~s\n" 
				  #'expr (current-directory)))
		    (exit 1)))])))
; ----------------------------------------
;;; Main Script:

;; This should be run from this directory, but to make sure...
;(define test-directory (current-directory))
(define test-root (format "~a/WS_test_copy" (getenv "HOME")))
(define test-directory (format "~a/src" test-root))
(current-directory test-directory)

(ASSERT (putenv "REGIMENTD" test-root))

;; We use debugmode for all the tests below:
(ASSERT (putenv "REGDEBUGMODE" "ON"))

;(ASSERT (putenv "PATH" (format "~a/bin:~a" test-root (getenv "PATH"))))
;(ASSERT (putenv "PATH" (format "~a/depends:~a" test-root (getenv "PATH"))))

(ASSERT (system "echo Environment established: REGIMENTD:$REGIMENTD"))

;; Catch any errors encountered below and send an email:
(current-exception-handler
 (lambda (exn)
   (define msg
     (format "ERROR during script execution:\n   ~a\n\nException: ~s\n" 
	     (exn-message exn) exn))
   (mail ryan-email "Failure of supertest.ss" msg)
   (exit 1)))

(define date 
  (let ((d (seconds->date (current-seconds))))
    (format "~a-~a-~a_~a:~a:~a" 
	    (date-year d) (date-month d) (date-day d)
	    (date-hour d) (date-minute d) (date-second d))))
;(define logfile (format "~a/supertest_~a.log" (path->string (current-directory)) date))
(define logfile (format "~a/supertest_~a.log" test-directory date))
(define log (open-output-file logfile 'replace))
(define scriptoutput (open-output-file "SUPERTEST_SCRIPT_OUTPUT.log" 'replace))
(define orig-console (current-output-port))
(current-output-port scriptoutput)
(current-error-port scriptoutput)

(fprintf orig-console "Opened logfile: ~s\n" logfile)
;(flush-output log)
;(close-output-port log)
;(set! log (open-output-file logfile 'append))

(define svn-revision
  (begin 
    (ASSERT (eqv? 0 (system/exit-code "which svn > /dev/null")))
    (ASSERT (eqv? 0 (system/exit-code "svn info | grep Revision | sed s/Revision:// > svn_rev.txt")))
    (read (open-input-file "svn_rev.txt"))))







;; Here we begin running tests:

(fpf "WaveScript (rev ~a):\n" svn-revision)
(fpf "========================================\n")

(begin (reset-timer!)
       (define cleaned (system/exit-code "make clean"))
       (fpf "Build directory cleaned:                      ~a\n" (code->msg! cleaned)))

(begin (define runpetite (system/exit-code (format "echo | ~a/depends/petite" test-root)))
       (fpf "petite: Repository's Petite Chez runs:        ~a\n" (code->msg! runpetite)))

;; Now that we're sure petite runs let's get the machine type:
(define machine-type 
  (begin 
    (ASSERT (eqv? 0 (system/exit-code "echo '(machine-type)' | petite -q > machine_type.txt")))
    (read (open-input-file "machine_type.txt"))))

(begin (define testpetite
	 (system/exit-code 
	  "echo \"(define-top-level-value 'REGIMENT-BATCH-MODE #t) (test-units)\" | ../depends/petite main_chez.ss &> 0_PETITE_UNIT_TESTS.log"))
       (fpf "petite: Load & run unit tests:                ~a\n" (code->msg! testpetite)))

(begin (define fullchez (system/exit-code "which chez > /dev/null"))
       (fpf "chez: Full Chez Scheme on the test system:    ~a\n" (code->msg! fullchez)))

(begin (define loaded (system/exit-code "./regiment_script.ss &> 1_SCRIPT_LOAD.log"))
       (fpf "chez: WScript loads from source (via script): ~a\n" (code->msg! loaded)))

(begin (define compilerworks (system/exit-code "echo '(compile 3)' | ./regiment_script.ss i --exit-error"))
       (fpf "chez: WScript has access to the compiler:     ~a\n" (code->msg! compilerworks)))

(begin (newline)
       (printf "First: from source\n")
       (printf "============================================================\n")
       (define frmsrc (system/exit-code "./regiment_script.ss test &> 2_UNIT_TESTS.log"))
       (fpf "chez: Unit tests, loaded from source:         ~a\n" (code->msg! frmsrc)))

(begin (newline)
       (printf "Second: building Chez shared object\n")
       (printf "============================================================\n")
       (ASSERT (putenv "REGDEBUGMODE" "OFF"))
       (define buildso (system/exit-code "make chez &> 3_BUILD_SO.log"))
       (fpf "chez: Build .so file:                         ~a\n" (code->msg! buildso))

       (ASSERT (system "./regiment_script.ss 2> 4_LOAD_FROM_SO.log"))
       (define loadedso (system/exit-code "grep 'compiled .so' 4_LOAD_FROM_SO.log"))
       (fpf "chez: System loads from .so file:             ~a\n" (code->msg! loadedso))
       (ASSERT (putenv "REGDEBUGMODE" "ON"))

       ;; Now copy that .so file to our stored binaries directory.
       (when (directory-exists? "/var/www/regiment_binaries")
	 (fprintf orig-console "Copying prebuilt binary to website.\n")
	 (let* ([webfile (format "/var/www/regiment_binaries/~a_~a_main_chez.so" svn-revision machine-type)]
		[localfile (format "build/~a/main_chez.so" machine-type)])
	   (if (file-exists? webfile) (delete-file webfile))
	   (copy-file localfile webfile)
	   (ASSERT (system (format "chgrp www-data ~a" webfile)))
	   (ASSERT (system (format "chmod g+r ~a" webfile)))
	   ))

       ;; Disabling this temporarily, problem with simalpha-generate-modules (and lang_wavescript):
       ;; FIXME:

;       (define runso (system/exit-code "./regiment_script.ss test"))
;       (fpf "chez: Unit tests, loaded from .so file:       ~a\n" (code->msg! runso))
       )

(begin (define c-build (system/exit-code "make c &> 5_BUILD_C_EXTENSIONS.log"))
       (fpf "chez: Build C extensions:                     ~a\n" (code->msg! c-build)))

;; Now clean again:
(ASSERT (system "make clean"))

(begin (newline) (fpf "\n")
       (printf "Third: building Wscript bytecode in PLT\n")
       (printf "============================================================\n")
       (define wsparse (system/exit-code "make wsparse &> 6_BUILD_WSPARSE.log"))
       (fpf "plt: Building wsparse executable:             ~a\n" (code->msg! wsparse))

       ;; Now copy that executable file to our stored binaries directory.
       (when (directory-exists? "/var/www/regiment_binaries")
	 (fprintf orig-console "Copying prebuilt wsparse to website.\n")
	 (let* ([webfile (format "/var/www/regiment_binaries/~a_~a_wsparse" svn-revision machine-type)])
	   (if (file-exists? webfile) (delete-file webfile))
	   (copy-file "bin/wsparse" webfile)
	   (ASSERT (system (format "chgrp www-data ~a" webfile)))
	   (ASSERT (system (format "chmod g+r ~a" webfile)))
	   ))
       )

(begin (define pltbc (system/exit-code "make pltbc &> 7_BUILD_PLT_BYTECODE.log"))
       (fpf "plt: Building WScript as bytecode in PLT:     ~a\n" (code->msg! pltbc)))


;; [2007.02.28] This has been broken for a while, and the error code isn't working right.
#;
(begin (newline)
       (printf "Fourth: Running tests in PLT\n")
       (printf "============================================================\n")
        (define plttests (system/exit-code 
 			 (format "echo '(test-units)' | mzscheme -f ~a/main_plt.ss &> 8_PLT_UNIT_TESTS.log"
				 test-directory)))
        (fpf "plt: Running tests in PLT:                    ~a\n" (code->msg! plttests)))

(begin (newline)
       (printf "Fifth: Running WaveScript Demos\n")
       (printf "============================================================\n")
       (current-directory (format "~a/demos/wavescope" test-directory))
       (define getdata (system/exit-code "./download_sample_marmot_data"))
       (fpf "\nws: Downloading sample marmot data:           ~a\n" (code->msg! getdata))
       (define wsdemos (system/exit-code (format "./testall_demos.ss &> ~a/9_WS_DEMOS.log" test-directory)))
       (current-directory test-directory)
       (fpf "\nws: Running WaveScript Demos:                 ~a\n" (code->msg! wsdemos)))

(begin (current-directory (format "~a/lib/" test-root))
       (define stdlib (system/exit-code (format "echo exit | ws stdlib.ws &> ~a/10_stdlib.log" test-directory)))
       (fpf "ws: Loading stdlib.ws:                        ~a\n" (code->msg! stdlib))
       (define matrix (system/exit-code (format "echo exit | ws matrix.ws &> ~a/11_matrix.log" test-directory)))
       (fpf "ws: Loading matrix.ws:                        ~a\n" (code->msg! matrix))
       (current-directory test-directory))

(begin (current-directory (format "~a/apps/pipeline-web" test-root))
       (define pipeline-web (system/exit-code (format "make test &> ~a/11_pipeline-web.log" test-directory)))
       (fpf "ws: Running pipeline-web app:                 ~a\n" (code->msg! pipeline-web))
       (current-directory test-directory))

;;================================================================================
;; WAVESCOPE ENGINE:

(define engine-dir (format "~a/WS_test_engine" (getenv "HOME")))
(ASSERT (system (format "rm -rf ~a" engine-dir)))
(ASSERT (system 
	 (format 
	  "svn co svn+ssh://newton@nms.csail.mit.edu/export/home2/svn/WaveScope/trunk/code/v1 ~a" 
	  engine-dir)))

(ASSERT (putenv "WAVESCOPED" engine-dir))
(ASSERT (system "echo WaveScope ENV var set: $WAVESCOPED"))
(current-directory engine-dir)

(define engine-svn-revision
  (begin 
    (ASSERT (eqv? 0 (system/exit-code "svn info | grep Revision | sed s/Revision:// > svn_rev.txt")))
    (read (open-input-file "svn_rev.txt"))))

(fpf "\nWaveScope Engine (rev ~a):\n" engine-svn-revision)
(fpf "========================================\n")

(begin (define engine-cleaned (system/exit-code "make clean"))
       (fpf "Engine directory cleaned:                     ~a\n" (code->msg! engine-cleaned)))


(begin (current-directory engine-dir)
       (define engine-make (system/exit-code (format "make all &> ~a/11_ENGINE_MAKE_ALL.log" test-directory)))
       (fpf "Engine 'make all':                            ~a\n" (code->msg! engine-make)))

;; TODO: This doesn't return ERROR code:
(begin (current-directory engine-dir)
       (define testSignal (system/exit-code (format "./testSignal-SMSegList &> ~a/12_testSignal.log" test-directory)))
       ;(fpf "Engine: testSignal-SMSegList                  ~a\n" (code->msg! testSignal))
       (code->msg! testSignal)
       (fpf "Engine: testSignal-SMSegList                  ~a\n"
	    (if (zero? testSignal) "?maybe passed?" "-FAILED-"))
       )

;; TODO: This probably doesn't return ERROR code:
(begin (current-directory engine-dir)
       (define pipeMemory (system/exit-code (format "./PipeMemory-SMSegList --at_once --push_batch 10 &> ~a/13_PipeMemory.log" 
						    test-directory)))
       (code->msg! pipeMemory)
       (fpf "Engine: PipeMemory-SMSegList                  ~a\n" 
	    (if (zero? pipeMemory) "?maybe passed?" "-FAILED-"))
       )

;;================================================================================
;; Now test WSC:

(begin ;; This runs faster if we load Regiment pre-compiled:
       ;(current-directory test-directory) (ASSERT (system "make chez"))
       (fpf "\n")
       (current-directory (format "~a/demos/wavescope" test-directory))
       (define wsc-demos (system/exit-code (format "./testall_wsc &> ~a/14_WSC_DEMOS.log" test-directory)))
       (current-directory test-directory)
       (fpf "wsc: Running WaveScript Demos with WSC:       ~a\n" (code->msg! wsc-demos)))

;;================================================================================

(fpf "\nTotal time spent testing: ~a minutes\n" 
     (milli->minute (- (current-inexact-milliseconds) start-time)))

;(fpf "\n\nWaveScript Rev: ~a\n" svn-revision)
;(fpf "WaveScope Engine Rev: ~a\n" engine-svn-revision)

(fpf "\nMachine:\n   ")
(system (format "uname -a &> temp.log"))
(fpf (file->string "temp.log"))

(fpf "G++ version:\n   ")
(system (format "g++ --version | head -1 &> temp.log"))
(fpf (file->string "temp.log"))

(fpf "mzscheme version:\n   ")
(system (format "mzscheme --version &> temp.log"))
(fpf (file->string "temp.log"))

(fpf "full Chez Scheme version:  ")
(system (format "chez --version &> temp.log"))
(fpf (file->string "temp.log"))

(fpf "repository's Petite Chez Scheme version:  ")
(system (format "~a/depends/petite --version &> temp.log" test-root))
(fpf (file->string "temp.log"))

(close-output-port log)
(define thesubj 
  (if failed 
      (format "[Regression] WaveScript/Scope rev ~a/~a FAILED nightly tests" svn-revision engine-svn-revision)
      (format "[Regression] WaveScript/Scope rev ~a/~a passed nightly tests" svn-revision engine-svn-revision)))
(define themsg  (file->string logfile))

(mail ryan-email thesubj themsg)
;(if failed (mail "ws@nms.csail.mit.edu" thesubj themsg))



;; As icing on the cake let's post this on the web too:
;; This should run on faith:
(when (directory-exists? "/var/www/regression")
  (printf "Going to try publishing to website.\n")
  (let* (;[d (seconds->date (current-seconds))]
	 [webfile (format ;"/var/www/regression/rev~a_eng~a_~a-~a-~a:~a:~a_~a"
		          "/var/www/regression/rev~a_eng~a_~a"
			  svn-revision engine-svn-revision
			  ;(date-year d) (date-month d) (date-day d)
			  ;(date-hour d) (date-minute d)
			  (if failed "FAILED" "passed"))])
    (if (file-exists? webfile) (delete-file webfile))
    (fprintf orig-console "Copying log to website. ~a\n" webfile)
    (copy-file logfile webfile)
    (ASSERT (system (format "chgrp www-data ~a" webfile)))
    (ASSERT (system (format "chmod g+r ~a" webfile)))
    ))
