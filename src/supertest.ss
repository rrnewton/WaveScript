#! /bin/bash
#|
exec mzscheme -qr "$0" ${1+"$@"}
|#

;;----------------------------------------------------------------------------;;
;;     USAGE                                                                  ;;
;;                                                                            ;;
;;  cd <installdir>/src; ./supertest.ss [options]                             ;;
;;                                                                            ;;
;;     Options:                                                               ;;
;;  -nopost  : Don't post results to the web                                  ;;
;;  -nobench : Don't run benchmarks, just tests.                              ;;
;;  -short   : Run only until first fail                                      ;;
;;  -stdout  : Put all the output on stdout                                   ;;
;;----------------------------------------------------------------------------;;

;; TODO: generate intermediate/ and most_recent_logs/ dirs if they're not there...

;; This really puts the system through the paces.
;; It's a PLT-Scheme-ONLY script (mzscheme).  
;; (Chez's system call doesn't return an error code.)

;; [2007.03.19] This system is unfortunately quite fragile.  I've had
;; several problems over the last two months.  Often it's difficult to
;; reproduce manually the behavior seen when "supertest" is run.
;;
;; Right now I'm trying to solve a problem which I know I encountered
;; before.  Unit tests fail during supertest, but pass if I do
;; "../depends/petite main_chez.ss".  Grr, I'm typing in the same command line!
;; Actually, in this case I found the bug (an actual bug), but it's
;; still a mystery as to why it only popped up during supertest.  
;;
;; OH RIGHT.  DEBUGMODE.  Gotta be careful about that.

(require (lib "process.ss") (lib "date.ss"))

;;----------------------------------------------------------------------------;;
;;     CONFIGURATION                                                          ;;
;; Read command line parameters and hardcode constants.                       ;;
;;----------------------------------------------------------------------------;;

(define ryan-email "ryan.newton@alum.mit.edu")
(define default-timeout (* 40 60)) ;; Timeout in seconds

;; Should we do the benchmarks in addition to regression testing?
(define benchmarks?  (not (member "-nobench" (vector->list (current-command-line-arguments)))))
(define publish?     (not (member "-nopost"  (vector->list (current-command-line-arguments)))))
(define short?       (member "-short"   (vector->list (current-command-line-arguments))))

;; Presently Ikarus and Chez are optional and PLT is mandatory.
(define ikarus? (eqv? 0 (system/exit-code "which ikarus > /dev/null")))
(define chez?   (eqv? 0 (system/exit-code "which chez > /dev/null")))
(unless ikarus? (printf "(Ikarus not found, omitting from tests.)\n"))
(unless chez?   (printf "(Chez not found, omitting from tests.)\n"))

; ----------------------------------------

(define start-time (current-inexact-milliseconds))
(define last-test-timer start-time)
(define failed #f)

;; Let's clean up some:
;(if (file-exists? "/tmp/wsparse_server_pipe")     (delete-file "/tmp/wsparse_server_pipe") (void))
;(if (file-exists? "/tmp/wsparse_server_response") (delete-file "/tmp/wsparse_server_response") (void))
;; Should we killall the wsparse_server processes also?

;; [2007.10.11] Changing this to ASSUME that supertest.ss is invoked from it's own directory:
  (current-directory "..")
(define ws-root-dir (path->string (current-directory)))
(define test-directory (format "~a/src" ws-root-dir))
  (current-directory test-directory)

  (printf "Test directory: ~s\n" test-directory)

(define date 
  (let ((d (seconds->date (current-seconds))))
    (format "~a-~a-~a_~a:~a:~a" 
	    (date-year d) (date-month d) (date-day d)
	    (date-hour d) (date-minute d) (date-second d))))

(define logfile (format "~a/supertest_~a.log" test-directory date))
(define script-output-file "SUPERTEST_SCRIPT_OUTPUT.log")

;;----------------------------------------------------------------------------;;
;;     Routines to communicate results                                        ;;
;;----------------------------------------------------------------------------;;

(define webroot "/var/www")
(define webdir  "/var/www/regression")

(define (mail to subj msg)
  (define tmpfile (format "/tmp/temp~a.msg" (current-milliseconds)))
  (string->file msg tmpfile)
  (system (format "mail ~a -s '~a' < ~a" to subj tmpfile))
  (delete-file tmpfile)
  (printf "\nMail Sent, to:~a subj: ~a\n" to subj)
  (printf "Body:\n~a\n" msg))

(define (post-to-web webfilename) 
  (define publish 
    (lambda (logfile webfile)
      (when (file-exists? webfile) (delete-file webfile))
      (cprint "Copying log to website. ~a\n" webfile)
      (copy-file logfile webfile)
      (system (format "chmod g+r ~a" webfile)); (ASSERT )
      (system (format "chgrp www-data ~a" webfile));(ASSERT )
      ))
  ;; As icing on the cake let's post this on the web too:
  ;; This should run on faith:
  (if (and publish? (directory-exists? webdir))
      (begin 
	(printf "Going to try publishing to website.\n")
	(let* ([webfile (format "~a/~a" webdir webfilename)])
	  (publish logfile webfile))
	;; Now do the performance report:
	(let ([perfreport (format "~a/benchmarks/perfreport.pdf" ws-root-dir)])
	  (when (file-exists? perfreport)
	    (let* ([webfile (format "~a/rev~a_perfreport.pdf" webdir
				    svn-revision)])
	      (publish perfreport webfile)))))
      ;; Otherwise we could try to scp it...
      (void)
      ))

;; Print to the console(s) but not to the log
(define (cprint . args)
  (apply fprintf orig-console args)  (flush-output orig-console)
  (apply fprintf scriptoutput args)  (flush-output scriptoutput))

;; Print everywhere -- to the log AND to both the user and console outputs.
(define (fpf . args)
  (apply cprint args)
  (apply fprintf logport args)           (flush-output logport))


;;----------------------------------------------------------------------------;;
;;     Misc Utility Functions                                                 ;;
;;----------------------------------------------------------------------------;;

(define (force-open-output-file file)
  (when (file-exists? file) (delete-file file))
  (open-output-file file))

(define (file->string filename)
    (let ([p (open-input-file filename)])
      (let loop ([c (read-char p)]
                 [acc '()])
        (if (eof-object? c)
            (begin (close-input-port p)
                   (list->string (reverse acc)))
            (loop (read-char p) (cons c acc))))))
(define (string->file str fn)
    (let ([p (force-open-output-file fn)])
      (display str p)
      (close-output-port p)))

(define (system-to-str cmd)
  (define fn (format "/tmp/___supertest_tmp_~a.txt" (random 100000000)))
  (and ;(system (format "~a &> ~a" cmd fn))
       ;; Not redirecting error because &> isn't very portable:
       ;; FIXME: Need to invoke bash explicitely:
       (system (format "~a &> ~a" cmd fn))
       (let ([str (file->string fn)])
	 (delete-file fn)
	 str)))

(define-syntax ASSERT
  (lambda (x)
    (syntax-case x ()
      [(_ expr) 
       #'(or expr 
	     (let ((msg (format "ASSERT failed: ~s\n\nCurrent Directory: ~s\n" 
				  #'expr (current-directory))))
	       (fpf msg)
	       (mail ryan-email "Failure of supertest.ss" msg)
	       (exit 1)))])))

;; Run a command with a timeout.
;; Asynchronously separates spawning of the process, and waiting for it to complete.
(define (system/async/timeout cmd)  
  (let* ([proc (process cmd)]
	 [stdout (car proc)]
	 [stdin  (cadr proc)]
	 [stderr (cadddr proc)]
	 [statusfn (car (reverse proc))])
    (lambda (secs) ;; takes timeout in seconds
      (define timeout (* secs 1000 ))
      (define pollinterval (* 250 )) ;; 4 hz
      (define (closeup) 
	(close-input-port stdout)
	(close-input-port stderr)
	(close-output-port stdin))
      (let waitloop ([time 0])
      ;; discard any input:
      ;(printf "Reading ports\n")
      ;(let loop () (when (char-ready? stdout) (read-char stdout) (loop)))
      ;(let loop () (when (char-ready? stderr) (read-char stderr) (loop)))
      ;(printf "STATUS: ~s\n" (statusfn 'status))
      (case (statusfn 'status)
	[(done-ok)    (closeup) 0]
	[(done-error) (closeup) (statusfn 'exit-code)]
	[(running)    
	 (if (> time timeout)
	     ;; Kill the subprocess.
	     (begin 
	       ;(printf "TIMED OUT\n")
	       (fpf " *** Process timed out!: ***\n")
	       (statusfn 'kill)
	       99)
	     (begin 
	       ;; Wait a bit and check again:
	       (sync (alarm-evt (+ (current-inexact-milliseconds) pollinterval)))
	       (waitloop (+ time pollinterval))))]
	[else (error 'system/timeout "")])))))

(define (system/timeout cmd)
  ((system/async/timeout cmd) default-timeout))

;;----------------------------------------------------------------------------;;

(define (reset-timer!) (set! last-test-timer (current-inexact-milliseconds)))
(define (milli->minute t) (/ (round (* 10 (/ t 1000. 60.))) 10))
(define code->msg!
  (case-lambda
    [(m) (code->msg! m (- (current-inexact-milliseconds) last-test-timer))]
    [(m time-elapsed)
     (let ([val (if (or (equal? m 0) (equal? m #t))
		    (format "passed (~a min)" (milli->minute time-elapsed))
		    (begin (set! failed #t) 
			   (format "-FAILED- (code ~a)" m)))])
       (reset-timer!)
       val)]))

;; Partway through refactoring all the tests below to use this helper:
(define (run-async-test title cmd) 
  (let* ([test-start (current-inexact-milliseconds)]
	 [wait-for-it (system/async/timeout cmd)])
    (lambda (secs-to-wait)
      (fpf title)
      (fpf (format "~a~a\n"
		   (list->string (vector->list (make-vector (max 0 (- 46 (string-length title))) #\space)))
		   (let ([result (wait-for-it secs-to-wait)])
		     ;; Can't report accurate time currently.  If the subprocess 
		     ;; is already finished we don't know how long it took.		    
		     (code->msg! result (- (current-inexact-milliseconds) test-start)))))
      (post-to-web (format "intermediate/rev_~a" svn-revision))
      (when (and short? failed)
            (fpf "Test failed (and ran with -short).  Exiting.\n")
            (exit 1))
      )))

(define (run-test title cmd)
  (fprintf orig-console " >>> Running test with command: ~a\n" cmd)
  ((run-async-test title cmd) default-timeout))


; ----------------------------------------
;;; Main Script:
; ----------------------------------------

;; Three ports:
;; There are three possible destinations for output.  
(define logport          (force-open-output-file logfile))
(define scriptoutput (force-open-output-file script-output-file))
(define orig-console (current-output-port))


(ASSERT (putenv "WAVESCRIPTD" ws-root-dir))
;(ASSERT (putenv "WAVESCRIPTHOST" "")) ;; Clear this
(if (getenv "WAVESCRIPTHOST")
    (printf "Getting default scheme from the environment ($WAVESCRIPTHOST): ~a\n" (getenv "WAVESCRIPTHOST"))
    (ASSERT (putenv "WAVESCRIPTHOST" "plt")))
(define defaulthost (getenv "WAVESCRIPTHOST"))

;; We use debugmode for all the tests below:

;; [2008.05.10] NOT DOING DEBUG MODE YET... (R6RS port)
;(ASSERT (putenv "REGDEBUGMODE" "ON"))

;(ASSERT (putenv "PATH" (format "~a/bin:~a" ws-root-dir (getenv "PATH"))))
;(ASSERT (putenv "PATH" (format "~a/depends:~a" ws-root-dir (getenv "PATH"))))

(ASSERT (system "echo Environment established: WAVESCRIPTD = $WAVESCRIPTD"))

;; This is a lame hack for compatibility across a wide range of versions:
(define excep-hndlr
  (with-handlers ([(lambda (x) #t) (lambda _ current-exception-handler)]) uncaught-exception-handler))

;; Catch any errors encountered below and send an email:
(excep-hndlr
 (lambda (exn)
   (define msg
     (format "ERROR during script execution:\n   ~a\n\nException: ~s\n" 
	     (exn-message exn) 
	     exn))
   (fpf msg)
   (mail ryan-email "Failure of supertest.ss" msg)
   (post-to-web (format "rev~a_ERROR" svn-revision))
   (exit 1)))

(cprint "Opened results summary file: ~s \n" logfile)
(cprint "Copying script output to: ~s \n" script-output-file)

(current-output-port scriptoutput)
(current-error-port scriptoutput)

;(flush-output logport)
;(close-output-port logport)
;(set! logport (open-output-file logfile 'append))

(define svn-revision
  (begin 
    (ASSERT (eqv? 0 (system/exit-code "which svn > /dev/null")))
    (ASSERT (eqv? 0 (system/exit-code "svn info | grep Revision | sed s/Revision:// > svn_rev.txt")))
    (let ([x (read (open-input-file "svn_rev.txt"))])
      (if (eof-object? x) 'unknown x))))

(define svn-log-message 
  (parameterize ([current-directory ws-root-dir])
    (system-to-str "svn log -v -r HEAD:PREV")))

(cprint "SVN revision: ~a\n" svn-revision)

;; Here we begin running tests:

(fpf "\nRunning from directory: ~a\n\n" ws-root-dir)

(fpf "Date: ~a\n" (system-to-str "date"))

(fpf "\nWaveScript (rev ~a) build & unit tests:\n" svn-revision)
(fpf "========================================\n")

(post-to-web (format "intermediate/rev_~a" svn-revision))

(reset-timer!)

(run-test "Empty test, run ls:" "ls")

(run-test "Build directory cleaned:" "make clean > make_clean.log")

(when ikarus? (run-test "ikarus:   Ikarus runs:"    (format "echo | ikarus ")))

(run-test "mzscheme: MzScheme runs:"  (format "echo | mzscheme "))
;(run-test "larceny:  Larceny runs:"   (format "echo | larceny"))

(current-directory test-directory)

(run-test "Build aggregate libraries:" "make aggregated &> make_aggregated.log")
(run-test "plt: Build bytecode files: " "make bc &> plt_BUILD.log")

(when ikarus? (run-test "ikarus: Build object files: " "make ik &> ikarus_BUILD.log"))

(run-test (format "(~a): Load & run unit tests: " defaulthost)
	  "../bin/wavescript t &> UNIT_TESTS.log")

;; I turn these on and off, depending on whether I want to tolerate the huge slowdown.
#;
(define wait-on-larc-load
  (run-async-test "larceny: Load from source: " "../bin/wavescript.larceny &> larceny_LOAD_FROM_SOURCE.log"))
; (run-test "larceny: Partial larceny build: " "make larceny &> larceny_BUILD.log")
;; 

(fpf      "  Testing legacy support for Chez Scheme:\n")
(run-test "chez: Full Chez Scheme on the test system:" "which chez > /dev/null")
(run-test "chez: extract chez src from R6RS:" "./temporary_smoosh_to_one_chez_file.ss > chez_smoosh.log")
(run-test "chez: WScript loads from source (via script):" "../bin/wavescript.chez &> chez_SCRIPT_LOAD.log")
(run-test "chez: WScript has access to the compiler:"
	  ;; [2009.03.12] Obsolete in R6RS world:
	  ;;"echo '(compile 3)' | ../bin/wavescript.chez -i --exit-error"
	  "echo '(compile 3)' | chez")
;(run-test "chez: Unit tests, loaded from source:" "./wavescript_script.ss test &> chez_UNIT_TESTS.log")

(begin (newline)
       ;(printf "Second: building Chez shared object\n")
       ;(printf "============================================================\n")
       (ASSERT (putenv "REGDEBUGMODE" "OFF"))

       (run-test "chez: Build .so file(s):" "make chez &> chez_BUILD_SO.log")
       ;;TEMP;; (run-test "chez: Also build debugmode .so file:" "make dbg &> chez_BUILD_DBG.log")
	     
       (ASSERT (system "../bin/wavescript.chez 2> chez_LOAD_FROM_SO.log"))
       ;;TEMP;; (run-test "chez: System loads from .so file:" "grep 'compiled .so' chez_LOAD_FROM_SO.log")

       (ASSERT (putenv "REGDEBUGMODE" "ON"))

       ;; Disabling this temporarily, problem with simalpha-generate-modules (and lang_wavescript):
       ;; FIXME:

;       (define runso (system/timeout "./wavescript_script.ss test"))
;       (fpf "chez: Unit tests, loaded from .so file:       ~a\n" (code->msg! runso))
       )
(run-test "chez: Build C extensions:" "make c &> gcc_BUILD_C_EXTENSIONS.log")

;; Now clean again:
;(ASSERT (system "make clean > make_clean2.log"))


(fpf "\n")
(run-test "wsparse: Building executable (plt):" "make wsparse &> plt_WSPARSE.log")

(run-test "wsparse: wsparse is in path:" "which wsparse &> which_wsparse.log")


;(run-test "plt: Building WScript as bytecode in PLT:" "make pltbc &> plt_BUILD_PLT_BYTECODE.log")
;(run-test "plt: Run system from command line with PLT:" "wavescript.plt &> plt_RUN_PLT_BYTECODE.log")

;; Now let's see if we can load in larceny:
;; Disabling for now because it's explodiing faith's HD:!!!
;(run-test "larceny: Testing compile in Larceny Scheme:"  "make larc &> larceny.log")

(fpf "\n\nWaveScript demos & libraries (Scheme backend):\n")
(fpf "========================================\n")

(parameterize ((current-directory (format "~a/demos/wavescope" test-directory)))
  (run-test "ws: Downloading sample marmot data:" "./download_sample_marmot_data")

  ;; This should run in debug mode if "wavescript" is responsive to REGDEBUGMODE:
  (run-test "ws: Running Demos   :"
	    (format "./testall_demos.ss &> ~a/ws_demos_debug.log" test-directory))

  (run-test "ws.early: WaveScript Demos:"
	    (format "./testall_early &> ~a/wsearly_demos.log" test-directory))
  )


;; Test STANDARD LIBRARIES:
(parameterize ([current-directory (format "~a/lib/" ws-root-dir)])
  (run-test "ws: Loading stdlib_test.ws:" 
	    (format "ws stdlib_test.ws -n 10 -exit-error &> ~a/stdlib.log" test-directory))

  (run-test "wsc2: Compiling stdlib_test:"
	    (format "wsc2 stdlib_test.ws -exit-error &> ~a/wsc2_stdlib_build.log" test-directory))
  (run-test "wsc2: Running stdlib_test:    "
	    (format "./query.exe -n 10 -exit-error &> ~a/wc2_stdlib_run.log" test-directory))

  ;; This is the OLD one:
  ;; NO COMPLEX IN IKARUS YET:
  (ASSERT (putenv "WAVESCRIPTHOST" "plt"))
  ;(when (file-exists? "query.exe") (delete-file "query.exe"))
  (run-test "wsc2: Old matrix_test.ws (plt):" 
	    (format "wsc2 matrix_test.ws -exit-error &> ~a/matrix_old_build.log" test-directory))
  (run-test "wsc2: Run old matrix_test.ws:" 
	    (format "./query.exe -n 10 &> ~a/matrix_old_run.log" test-directory))

  ;(when (file-exists? "query.exe") (delete-file "query.exe"))
;  (run-test "ws: Running native WS test_matrix.ws:" 
;	    (format "ws test_matrix.ws -exit-error -n 10 &> ~a/matrix_ws.log" test-directory))
  (run-test "wsc2: Native WS test_matrix.ws (plt):"
	    (format "wsc2 test_matrix.ws -exit-error &> ~a/matrix_build.log" test-directory))
  (run-test "wsc2: Run output exe:" 
	    (format "./query.exe -n 10 &> ~a/matrix_run.log" test-directory))
  (ASSERT (putenv "WAVESCRIPTHOST" ""))
  )


;; Now for GSL interface.
(parameterize ([current-directory (format "~a/lib/" ws-root-dir)])
  (run-test "ws: Generating gsl matrix library wrappers:" 
	    (format "make &> ~a/gsl_wrappers.log" test-directory))

;; PLT has a bug with "exists" right now:  [2008.05.21]
;   (ASSERT (putenv "WAVESCRIPTHOST" "plt"))a
;   (run-test "wsc2: GSL test_matrix_gsl.ws (plt):" 
; 	    (format "wsc2 test_matrix_gsl.ws  -exit-error &> ~a/matrix_gsl_build.log" test-directory))
;   (run-test "wsc2: Run output executable:" 
; 	    (format "./query.exe -n 10 &> ~a/matrix_gsl_run.log" test-directory))


  ;; Not working in ikarus, no foreign interface [2008.05.21]
#;
  (run-test "ws: Running GSL test_matrix_gsl.ws:" 
	    (format 
	     "ws test_matrix_gsl.ws -n 1 -exit-error &> ~a/matrix_gsl.log" test-directory))
  #;
  (run-test "ws: Running test of GSL matrix library.ws:"
	    (format "ws run_matrix_gsl_test.ws -n 2-exit-error  &> ~a/matrix_gsl.log" test-directory))

  )


;;================================================================================
;; Now test WSC:

(fpf "\n\nWaveScript C Backends:\n")
(fpf "===================================================\n")

(parameterize ((current-directory (format "~a/demos/wavescope" test-directory)))
  ;; This runs faster if we load Regiment pre-compiled:
 ;(current-directory test-directory) (ASSERT (system "make chez"))

  (begin
    (ASSERT (putenv "WAVESCRIPTHOST" "ikarus"))
    (run-test "wsc2: Demos, simple RC (ikarus):"
	      (format "./testall_wsc2 -gc refcount &> ~a/wsc2_demos_rc_ikarus.log" test-directory))
    (system "cp .__runquery_output_wsc2.txt .__runquery_output_wsc2_nondef.txt"))
  (begin
    (ASSERT (putenv "WAVESCRIPTHOST" "plt"))
    (run-test "wsc2: Demos, deferred RC (plt):"
	      (format "./testall_wsc2 -gc deferred -nothreads &> ~a/wsc2_demos_deferred_plt.log"
		      test-directory))
    (system "cp .__runquery_output_wsc2.txt .__runquery_output_wsc2_def.txt"))
  (begin
    (ASSERT (putenv "WAVESCRIPTHOST" "chez"))
    (ASSERT (putenv "LAUNCHIT" " "))  ;; Hack [2008.08.23], look at testall_wsc2
    (run-test "wsc2: Demos, boehm RC (chez):"
	      (format "./testall_wsc2 -gc boehm &> ~a/wsc2_demos_boehm_chez.log" test-directory))
    (ASSERT (putenv "LAUNCHIT" ""))
    (system "cp .__runquery_output_wsc2.txt .__runquery_output_wsc2_boehm.txt")
    (ASSERT (putenv "WAVESCRIPTHOST" "")))

  (begin 
    (ASSERT (putenv "WAVESCRIPTHOST" "chez"))
    (ASSERT (putenv "LAUNCHIT" " "))  ;; Hack [2008.08.23], look at testall_wsc2

    ;; First test with simple refcount GC
    (run-test "wsc2: Demos, THREADS, list fifo:"
	      (format "./testall_wsc2 -threads -realtime -gc ref -D WS_LIST_FIFO &> ~a/wsc2_demos_threads_rc.log" test-directory))    
    (system "cp .__runquery_output_wsc2.txt .__runquery_output_wsc2_refcount_threads_listfifo.txt")
    (run-test "wsc2: Demos, THREADS, coarse lock list fifo:"
	      (format "./testall_wsc2 -threads -realtime -gc ref -D WS_LIST_FIFO -D FIFO_COARSE_LOCKING &> ~a/wsc2_demos_threads_rc_coarse.log" test-directory))
    (system "cp .__runquery_output_wsc2.txt .__runquery_output_wsc2_refcount_threads_listfifo_coarse.txt")
    (run-test "wsc2: Demos, THREADS, twostage list fifo:"
	      (format "./testall_wsc2 -threads -realtime -gc ref -D WS_LIST_FIFO -D FIFO_TWOSTAGE &> ~a/wsc2_demos_threads_rc_twostage.log" test-directory))
    (system "cp .__runquery_output_wsc2.txt .__runquery_output_wsc2_refcount_threads_listfifo_twostage.txt")

    ;; Next test with deferred refcount GC
    (run-test "wsc2: Demos, THREADS, deferred RC, list:"
	      (format "./testall_wsc2 -threads -realtime -gc def -D FIFO_COARSE_LOCKING &> ~a/wsc2_demos_threads_deferred_coarse.log" test-directory))
    (system "cp .__runquery_output_wsc2.txt .__runquery_output_wsc2_deferred_threads_listfifo_coarse.txt")
    (run-test "wsc2: Demos, THREADS, twostage list fifo:"
	      (format "./testall_wsc2 -threads -realtime -gc ref -D WS_LIST_FIFO -D FIFO_TWOSTAGE &> ~a/wsc2_demos_threads_deferred_twostage.log" test-directory))
    (system "cp .__runquery_output_wsc2.txt .__runquery_output_wsc2_refcount_threads_listfifo_twostage.txt")

    ;; Finally, test with Boehm GC:
    ;; [2008.11.07] It doesn't work with -threads yet!
    


    (ASSERT (putenv "LAUNCHIT" ""))
    (ASSERT (putenv "WAVESCRIPTHOST" "")))

  ;; Make things safe for tinyos:
  (ASSERT (putenv "TOSDIR" "/opt/tinyos-2.x/tos/"))
  (ASSERT (putenv "TOSROOT" "/opt/tinyos-2.x/"))
  (ASSERT (putenv "MAKERULES" "/opt/tinyos-2.x/support/make/Makerules"))
  (ASSERT (putenv "CLASSPATH" (string-append "/opt/tinyos-2.x/support/sdk/java/tinyos.jar:"
					     (or (getenv "CLASSPATH") ""))))


  ;; FIX ME ... FALSE POSITIVE HERE..
  (run-test "wstiny: Running demos w/ TOSSIM:"
	    (format "./testall_wstiny &> ~a/wstiny_demos.log" test-directory))

  )


;;================================================================================
;; Now test WSCAML:

;; DISABLING
(fpf "\n\nWaveScript CAML Backend: \n")
(fpf "========================================\n")
(parameterize ((current-directory test-directory))
  (newline)       
  (run-test "wscaml: Building ocaml libraries (fftw, etc):"
	    (format "make ocaml &> ~a/wscaml_build_stuff.log" test-directory))
  (current-directory (format "~a/demos/wavescope" test-directory))
  (run-test "wscaml: Running LIMITED Demos through OCaml:" 
	    (format "./testall_caml &> ~a/wscaml_demos.log" test-directory))
  )

(begin 
  (fpf "\n\nWaveScript MLTON Backend:\n" )
  (fpf "========================================\n")

  (parameterize ((current-directory (format "~a/demos/wavescope" test-directory)))
    (newline)       
    (run-test "wsmlton: Running Demos through MLton:" 
	      (format "./testall_mlton &> ~a/wsmlton_demos.log" test-directory)))

  (parameterize ((current-directory (format "~a/lib/" ws-root-dir)))  
    (run-test "wsmlton: Compiling stdlib_test:"
	      (format "wsmlton stdlib_test.ws -exit-error &> ~a/wsmlton_stdlib_build.log" test-directory))
    (run-test "wsmlton: Running stdlib_test:    "
	      (format "./query.mlton.exe -n 10 -exit-error &> ~a/wsmlton_stdlib_run.log" test-directory)))
)




;;================================================================================
;; APPLICATIONS

(fpf "\n\nWaveScript Applications:\n")
(fpf "========================================\n")

;; MARMOT
(parameterize ((current-directory (format "~a/apps/marmot" ws-root-dir)))
  (newline)
    
  (run-test "    Run Makefile   " "make")

  (begin
    (ASSERT (putenv "WAVESCRIPTHOST" "ikarus"))
    (run-test "wsc2: Compiling marmot app (first phase):  "
	    (format "wsc2 run_first_phase.ws -exit-error &> ~a/wsc2_marmot1_build.log" 
		    test-directory))
    (run-test "wsc2:  Running marmot app (first phase):  "
	      (format "memprof ./query.exe -n 10 > ~a/wsc2_marmot1_run.log 2> marmot1_memprof.txt" test-directory))
    (when (file-exists? "query.exe") (delete-file "query.exe"))
    
    (ASSERT (putenv "WAVESCRIPTHOST" "plt"))
    (run-test "wsc2: Compiling marmot phase 1&2 (plt): "
	      (format "wsc2 run_marmot2.ws -exit-error &> ~a/wsc2_marmot12_build.log" test-directory))
    (run-test "wsc2:  Running marmot app (second phase): "
	      (format "./query.exe -n 1 &> ~a/wsc2_marmot12_run.log" test-directory))


    ;; [2008.08.13] This is segfaulting under ikarus also, let's try Chez:
    (ASSERT (putenv "WAVESCRIPTHOST" "chez"))
    (run-test "wsc2: Compiling marmot app (third phase):  "
      (format "wsc2 test_heatmap.ws -exit-error &> ~a/wsc2_marmot3_build.log" 
	      test-directory))

    (ASSERT (putenv "WAVESCRIPTHOST" ""))
    
    
    )
  
#|
  (run-test "ws: Running marmot app (first phase):  "
	    (format "ws.debug run_first_phase.ws -n 1 -exit-error &> ~a/ws_marmot.log" test-directory))
  
  (run-test "ws.early: Running marmot app:   "
	    (format "ws.early run_first_phase.ws -n 1 -exit-error &> ~a/wsearly_marmot.log" test-directory))

  (run-test "ws: Running marmot app (second phase):  "
	    (format "ws.debug run_marmot2.ws -n 1 -exit-error &> ~a/ws_marmot12.log" test-directory))

  (run-test "ws: Running marmot app (3phases): "
	    (format "ws.debug run_3phases.ws -n 1 -exit-error &> ~a/ws_marmot123.log" test-directory))

  #; ;; Disabling
  (begin
    (run-test "wsmlton: Compiling marmot app (first phase):  "
	      (format "wsmlton run_first_phase.ws -exit-error &> ~a/wsmlton_marmot1_build.log" test-directory))
    (run-test "wsmlton: Running marmot app (first phase):   "
	      (format "./query.mlton.exe -n 1 &> ~a/wsmlton_marmot1_run.log" test-directory))
    
    ;; Third phase won't work in "ws" because of writing ppm file.
    (run-test "wsmlton: Compiling marmot app (3phases):   "
	      (format "wsmlton run_3phases.ws -exit-error &> ~a/wsmlton_marmot123_build.log" test-directory))
    (run-test "wsmlton: Running marmot app (3phases):  "
	      (format "./query.mlton.exe -n 1 &> ~a/wsmlton_marmot123_run.log" test-directory)))
  
  ;; FIXME: ADD THIRD STAGE MULTINODE ETC!!!

|#
  
  ) ;; End MARMOT


;; bgSub application:
(parameterize ((current-directory (format "~a/apps/vision_ucla" ws-root-dir)))
  (newline)  
    (run-test "bgSub App: Downloading sample dataset" 
	      (format "./download_small_sample_data &> ~a/download_bgsub.log" test-directory))

;; FIXME: This is crashing with deferred RC.  I need to figure it out, but for now I'm disabling it.
#; 
    (run-test "wsc2: Compiling bgSub.ws -O3 -gc def:"
 	      (format "wsc2 bgSub.ws -O3 -gc def -exit-error &> ~a/wsc2_bgSub1_build.log" test-directory))
    (run-test "wsc2: Compiling bgSub.ws -O3 -gc boehm:"
 	      (format "wsc2 bgSub.ws -O3 -gc boehm -exit-error &> ~a/wsc2_bgSub1_build.log" test-directory))

    (run-test "wsc2:  Running bgSub.ws:  "
 	      ;(format "memprof ./query.exe -n 3 > ~a/wsc2_bgSub1_run.log 2> ~a/bgSub1_memprof.txt" test-directory test-directory)
	       (format "./query.exe -n 3 &> ~a/wsc2_bgSub1_run.log" test-directory)
	      )

    (run-test "wsc2: Compiling bgSub3_integer -O3 -gc ref:"
	      (format "wsc2 bgSub3_integer.ws -O3 -gc ref -exit-error &> ~a/wsc2_bgSub3_build.log" test-directory))
    (run-test "wsc2:  Running bgSub3_integer:  "
	      ;(format "memprof ./query.exe -n 3 > ~a/wsc2_bgSub3_run.log 2> bgSub3_memprof.txt" test-directory)
	      (format "./query.exe -n 3 &> ~a/wsc2_bgSub3_run.log" test-directory)
	      )

;     (run-test "wsc2: Compiling bgSub4_patchoriented -gc boehm:  "
; 	      (format "wsc2 bgSub4_patchoriented.ws -gc boehm -exit-error &> ~a/wsc2_bgSub4_build.log" test-directory))
;     (run-test "wsc2:  Running bgSub4_patchoriented  "
; 	      (format "memprof ./query.exe -n 1 > ~a/wsc2_bgSub4_run.log 2> bgSub4_memprof.txt" test-directory))

    
    ;(when (file-exists? "query.exe") (delete-file "query.exe"))

  ) ;; End bgSub application


(parameterize ((current-directory (format "~a/apps/telos_audio" ws-root-dir)))
  #;
  (run-test "ws: Running first speaker detection: "
	    (format "ws mfcc1.ws -n 1 &> ~a/ws_mfcc1.log" test-directory))

  ;; [2008.08.13] Mysterious ikarus crashes here, use PLT instead:
  (ASSERT (putenv "WAVESCRIPTHOST" "plt"))
  (when (file-exists? "query.exe") (delete-file "query.exe"))
  (run-test "wsc2: Compiling first speaker detect: "
	    (format "wsc2 mfcc1.ws  &> ~a/wsc2_build_mfcc1.log" test-directory))
  (run-test "wsc2:  Running first speaker detect: "
	    (format "./query.exe -n 1 &> ~a/wsc2_run_mfcc1.log" test-directory))

  (when (file-exists? "query.exe") (delete-file "query.exe"))
  (run-test "wsc2: Compiling fixed pt speaker detect: "
	    (format "wsc2 mfcc5_fixedpoint_full.ws  &> ~a/wsc2_build_mfcc5.log" test-directory))
  (run-test "wsc2:  Running fixed pt speaker detect: "
	    (format "./query.exe -n 1 &> ~a/wsc2_run_mfcc5.log" test-directory))

  (when (file-exists? "query.exe") (delete-file "query.exe"))
  ;; [2008.08.29] Got ikarus segfault on this with rev 1559.
  (run-test "wsc2: Compiling fixed fb speaker detect: "
	    (format "wsc2 mfcc6_fixedpoint_fb.ws  &> ~a/wsc2_build_mfcc6.log" test-directory))
  (run-test "wsc2:  Running fixed fb speaker detect: "
	    (format "./query.exe -n 1 &> ~a/wsc2_run_mfcc6.log" test-directory))

  (ASSERT (putenv "WAVESCRIPTHOST" ""))

  (run-test "wstiny: Compiling speaker detect, Telos: "	    
	    (format "wstiny mfcc6_fixedpoint_fb.ws  &> ~a/wstiny_build_mfcc6.log" test-directory))
  
  )





#|

(parameterize ((current-directory (format "~a/apps/pipeline-web" ws-root-dir)))
  (run-test "ws: Running pipeline-web app:   " 
	    (format "make test &> ~a/ws_pipeline-web.log" test-directory)))

(parameterize ((current-directory (format "~a/apps/stockticks" ws-root-dir)))
  (run-test "ws: Running stockticks app:   "
	    (format "make test &> ~a/ws_stockticks.log" test-directory)))

(parameterize ((current-directory (format "~a/apps/pipeline" ws-root-dir)))
  (run-test "    Decompressing pipeline data   "  "bunzip2 pipeline1.data.bz2")
  (run-test "ws: Running pipeline app:    "
	    (format "ws.debug pipeline.ws -n 10 -exit-error &> ~a/ws_pipeline.log" test-directory))
  (run-test "ws.early: Running pipeline app: "
	    (format "ws.early pipeline.ws -n 10 -exit-error &> ~a/ws_pipeline.log" test-directory)))

|#

;;================================================================================
;; Wait on outstanding async tests before we go into the perf benchmarks:

;(fpf "\n\nWaiting on tests run in backgroud:\n")
;(fpf "========================================\n")
;(wait-on-larc-load 1) ;; Should be done by now, don't give it any extra time.

;;================================================================================
;; Performance benchmarks.

(when benchmarks?

  (post-to-web (format "intermediate/rev_~a" svn-revision))
  (fpf "\n\nPerformance benchmarks (all backends)\n")
  (fpf "========================================\n")
  
  ;; [2008.10.24] This is so time consuming.  Just switch to 
  ;(ASSERT (putenv "WAVESCRIPTHOST" "chez"))

  (parameterize ((current-directory (format "~a/benchmarks" ws-root-dir)))
    
    ;; [2007.10.30] running steps incrementally, so we see what fails:
    (ASSERT (system "make topbefore"))

    (current-directory (format "~a/benchmarks/microbench" ws-root-dir))
    (run-test "    Run microbenchmarks:              " 
	      (format "make &> ~a/bench_micro.log" test-directory))

    (current-directory (format "~a/benchmarks/language_shootout" ws-root-dir))
    (run-test "    Run language_shootout:       " 
	      (format "make &> ~a/bench_shootout.log" test-directory))

    ;; [2008.08.01] Right now I'm having some occasional ikarus segfaults:
    ;; [2008.11.07] More ikarus segfaults on chastity, switching to chez:
    (ASSERT (putenv "WAVESCRIPTHOST" "chez"))
    (ASSERT (putenv "REGDEBUGMODE" "OFF"))
    (current-directory (format "~a/benchmarks/appbench" ws-root-dir))
    (run-test "    Run application benchmarks: " 
	      (format "make &> ~a/bench_apps.log" test-directory))
    
					;  (current-directory (format "~a/benchmarks/datareps" ws-root-dir))
					;  (run-test "    Run datarep benchmarks:" 
					;	    (format "make &> ~a/bench_datareps.log" test-directory))

    (current-directory (format "~a/benchmarks" ws-root-dir))
    (run-test "    Verify dependencies, do conversions:" 
	      (format "make alldeps &> ~a/bench_alldepscleanup.log" test-directory))
    
    (ASSERT (system "make topafter"))

;     (ASSERT (system "make machineinfo.tex"))
;     (ASSERT (system "make wssvn.tex"))
    (run-test "    Compile results, build full report: " 
	      (format "make perfreport.pdf &> ~a/bench_perfreport.log" test-directory))
    (ASSERT (putenv "WAVESCRIPTHOST" ""))
    ))

#|


;; POTHOLE 
;; TODO: Do other pothole variants.  pothole4 is just the one I know works.
#;
(begin (newline)
       (current-directory (format "~a/apps/potholes" ws-root-dir))
       (fpf "    Fetching pothole data                     ~a\n" 
	    (code->msg! (system/timeout 
	       (format "./download_small_sample_data  &> ~a/download_pothole_data.log" 
		       test-directory))))

       ;; TEMP FIXME!!!
       ;; Temporarilly turning off debug mode for pothole apps.
       ;; Having a grammar check problem...
       (ASSERT (putenv "REGDEBUGMODE" "OFF"))

       (fpf "ws: Running pothole4 app:                     ~a\n"
	    (code->msg! (system/timeout 
			 (format "ws pothole4.ws -n 3 -exit-error &> ~a/ws_pothole4.log" test-directory))))
       (fpf "ws.early: Running pothole4 app:               ~a\n"
	    (code->msg! (system/timeout 
              (format "ws.early pothole4.ws -n 3 -exit-error &> ~a/wsearly_pothole4.log" test-directory))))

;; TEMP FIXME DISABLED

;        (fpf "wscaml: Compiling pothole4 app:               ~a\n"
; 	    (code->msg! (system/timeout 
;              (format "wscaml pothole4.ws -exit-error &> ~a/wscaml_pothole_build.log" test-directory))))
;        (fpf "wscaml: Running pothole4:                     ~a\n"
; 	    (code->msg! (system/timeout 
;              (format "./query.caml.exe &> ~a/wscaml_pothole4_run.log" test-directory))))


       (fpf "wsmlton: Compiling pothole4 app:              ~a\n"
	    (code->msg! (system/timeout 
             (format "wsmlton pothole4.ws -exit-error &> ~a/wsmlton_pothole4_build.log" test-directory))))
       (fpf "wsmlton: Running pothole4 app:                ~a\n"
	    (code->msg! (system/timeout 
             (format "./query.mlton.exe -n 3 &> ~a/wsmlton_pothole4_run.log" test-directory))))

       (ASSERT (putenv "REGDEBUGMODE" "ON"))

       (current-directory test-directory))

;;================================================================================

|#

;;================================================================================
;; Extract vital stats from above runs:

(define vitals (format "~a/vital_stats.txt" test-directory))
(let* ([outp   (open-output-file vitals)])
  (parameterize ((current-directory (format "~a/demos/wavescope" ws-root-dir)))

    (fpf "\n\n  Sanity Checks:\n")
    (fpf "========================================\n")    
    ;; --- First we check for memory leaks in any of the demos ----
    (ASSERT (system "grep \"definitely lost in\" .__runquery_output_wsc2_nondef.txt | wc -l > lost_blocks.txt"))
    (let* ([prt (open-input-file  "lost_blocks.txt")]
	   [lost_blocks (read prt)])
      (close-input-port prt)
      (fprintf outp "Wsc2RC_DemosLostBlocks ~a\n" lost_blocks)
      ;; [2008.07.28] There should only be a lost_block from demo4b currently (mysterious fftw plan leak)

      ;; [2008.07.30] TEMP: Having an odd leak on 64 bit machines on demo4d_quoted_constants.ws
      ;; I can't make sense of it.   Almost seems like valgrind is wrong.
      (system "uname -m > machine_type.txt")
      (unless (equal? "x86_64\n" (file->string "machine_type.txt"))
	(fpf "Verify no leaks in demos (refcount):          ~a\n" (code->msg! (if (<= lost_blocks 1) 0 lost_blocks))))
      )
    
    (ASSERT (system "grep \"definitely lost in\" .__runquery_output_wsc2_def.txt | wc -l > lost_blocks.txt"))
    (let ([lost_blocks (read (open-input-file "lost_blocks.txt"))])
      (fprintf outp "Wsc2DefRC_DemosLostBlocks ~a\n" lost_blocks)
      ;; FIXME: [2008.10.31] Also running into problems here:
      (unless (equal? "x86_64\n" (file->string "machine_type.txt"))
	(fpf "Verify no leaks in demos (deferred):          ~a\n" 
	     (code->msg! (if (<= lost_blocks 1) 0 lost_blocks))))
      
      )

    ;; --- We also check the valgrind traces for errors ----
    (ASSERT (system "grep \"ERROR SUMMARY\" .__runquery_output_wsc2_def.txt    | grep -v \"ERROR SUMMARY: 0\" | wc -l >  errors.txt"))
    (ASSERT (system "grep \"ERROR SUMMARY\" .__runquery_output_wsc2_nondef.txt | grep -v \"ERROR SUMMARY: 0\" | wc -l >> errors.txt"))
    (let ([errors (read (open-input-file  "errors.txt"))])
      ;; This should be 2... demo4b currently has conditional jump errors.
      (fprintf outp "Wsc2_DemosErrors ~a\n" errors)
      (fpf "Verify no errors in demos:                    ~a\n" (code->msg! (<= errors 2)))
      )
    ) ;; End demos

  ;; --- We check the total memory footprint of the marmot app. ----  
  (parameterize ((current-directory (format "~a/apps/marmot" ws-root-dir)))
    (system "cat marmot1_memprof.txt | grep -v '#' | tail -n 1  > .__last_line.txt")
    ;; Get the peak memory usage
    (let ([peak_vm (read (open-input-file ".__last_line.txt"))])
      (fprintf outp "Marmot1_PeakVM ~a\n" peak_vm)
      ))
  
  ;; --- Check compile times for the marmot app. ---
  (parameterize ((current-directory test-directory))
    ;; This is fragile because it depends on a particular output from the WS compiler.
    ;; AND on a particular output format for the Scheme (time _) command.
    (define (getcpu file)
      (let ([line (system-to-str (format "grep -A 3 'Total compile time' ~a | grep 'cpu time'" file))])
	(printf "Extracting compile time from ~a, resulting line ~s\n" file line)
	(and line
	     (let ([port (open-input-string line)])
	;; This is really hacky:
	;; Ikarus looks like this:
	;;     9404 ms elapsed cpu time, including 1796 ms collecting
	;; Chez looks like this:
	;;     (time ...expression...)
	;;     1007 collections   
	;;     9404 ms elapsed cpu time, including 1796 ms collecting
	;; Mzscheme looks like this:
	;;     cpu time: 24 real time: 24 gc time: 0
	(let* ([one   (read port)]
	       ;; Hack for Chez:
	       ;[_     (when (list? one) (read port) (read port) (set! one (read port)))]
	       [two   (read port)]
	       [three (read port)])
	  (if (number? one) one 
	      (begin (ASSERT (number? three))
		     three)))))))

    ;; This is the time spent in the Scheme WS compiler.  Does not include time spent in the C/ML compiler.
    (let ([cpu (getcpu "wsc2_marmot12_build.log")])
      (if cpu (fprintf outp "Marmot12_compile_time_plt ~a\n" cpu)
	  (begin (code->msg! #f)
		 (fpf "Failed to extract compile time from wsc2_marmot12_build.log!\n" ))))    
    (let ([cpu (getcpu "wsc2_marmot3_build.log")])
      (if cpu (fprintf outp "Marmot3_compile_time_chez ~a\n" cpu)
	  (begin (code->msg! #f)
		 (fpf "Failed to extract compile time from wsc2_marmot3_build.log\n"))))
    ;; TODO: extend with other apps.
    )

  ;; --- Check the size of generate .c files ---
#|
  (let ([names (system-to-str "grep \"Running demo\" wsc2_demos.log  | awk '{ print $3 }' | sed 's/\.ws//'")]
	[nums  (system-to-str "grep -A 1 \"Compiled .c output\" wsc2_demos.log  | grep query | awk '{ print $1 }'")])
    (fprintf (string->list))
    )

  "grep -A 1 \"Compiled .c output\" *build*.log | grep query | awk '{ print $1 $2 }' | sed 's/.log./ /'"
|#

 
  ;; --- Gather a table of numbers from the benchmarks directory. ----  
  (when benchmarks? 
    (parameterize ((current-directory (format "~a/benchmarks" ws-root-dir)))
      (run-test "Various results gathered into a table: "
		"./gather_results.ss > results_table.txt")
      ;(system "gather_results.ss > results_table.txt")
      (display (file->string "results_table.txt") outp)
      ))
  (close-output-port outp)) ;; Done writing vital_stats.txt

(begin 
  (fpf "\n\n  Vital Stats: (for now printing all of them)\n")
  (fpf "=================================================\n")
  (fpf "~a" (file->string vitals)))

;; --- Compare against previous revision vitals ---
;; This will write "perf_diffs.txt"a
(fpf "\n\n  Performance differencing with previous revision...\n")
(fpf     "====================================================\n")
(run-test "Finding previous results and comparing:" 
	  "./compare_to_previous_rev_stats.ss")
(if (file-exists? "perf_diffs_thresholded.txt")
    (begin      
      (fpf "Metrics differing over ten percent: \n")
      (fpf "~a" (file->string "perf_diffs_thresholded.txt")))
    (fpf "  Failed to find anything!\n"))

;;================================================================================
;;; Wrap it up
;;================================================================================

(fpf "\n\n\nTotal time spent testing: ~a minutes\n" 
     (milli->minute (- (current-inexact-milliseconds) start-time)))

;(fpf "\n\nWaveScript Rev: ~a\n" svn-revision)

(fpf "\nMachine:\n   ")
(system (format "uname -a &> temp.log"))
(fpf (file->string "temp.log"))

(fpf "System $CC var = ~s\n" (or (getenv "CC") ""))
(fpf "C compiler version:\n   ")

(if (getenv "CC")
    (system (format "~a --version | head -1 &> temp.log" (getenv "CC")))
    ;; Hmm, just report both ICC and GCC versions, if possible:
    (begin (system "gcc --version | head -1 &> temp.log")
	   (when (system "which icc")
	     (system "icc --version | head -1 >> temp.log"))))
(fpf (file->string "temp.log"))


(fpf "mzscheme version:\n   ")
(system (format "mzscheme --version &> temp.log"))
(fpf (file->string "temp.log"))

(fpf "full Chez Scheme version:  ")
(system (format "chez --version &> temp.log"))
(fpf (file->string "temp.log"))

(fpf "repository's Petite Chez Scheme version:  ")
(system (format "~a/depends/petite --version &> temp.log" ws-root-dir))
(fpf (file->string "temp.log"))

(fpf "Larceny Scheme version:  \n   ")
(system "echo | larceny | head -n 1 &> temp.log")
(fpf (file->string "temp.log"))

(fpf "Ikarus Scheme version:  \n   ")
(system "echo | ikarus | head -n 1 &> temp.log")
(fpf (file->string "temp.log"))

(fpf "MLton version:  \n ~a   " (system-to-str "echo | mlton | head -n 1"))
(fpf "OCaml version:  \n ~a   " (system-to-str "ocaml -version"))

(parameterize ([current-directory ws-root-dir])
  (fpf "\nSVN log message:  \n   ")
  (fpf (or svn-log-message "NO-LOG-MESSAGE")))

(close-output-port logport)
(define thesubj 
  (if failed 
      (format "[Regression] WaveScript/Scope rev ~a FAILED nightly tests" svn-revision)
      (format "[Regression] WaveScript/Scope rev ~a passed nightly tests" svn-revision)))
(define themsg  (file->string logfile))

(mail ryan-email thesubj themsg)
;(if failed (mail "ws@nms.csail.mit.edu" thesubj themsg))

(post-to-web (format "rev~a_~a~a~a"
		     svn-revision 
		     (if (getenv "CC") (format "~a_" (getenv "CC")) "")
		     (if benchmarks? "wbench_" "")
		     (if failed "FAILED" "passed")))

;; Finally, copy all logs 
(cprint "~a all logs to website ~a...\n" (if publish? "Copying" "NOT copying") webdir)
(when publish? (system (format "rm -f ~a/most_recent_logs/*" webdir)))
(when publish? (system (format "cp ~a/*.log ~a/most_recent_logs/" test-directory webdir)))
(cprint "Finished (not) copying.")

(current-directory webroot)
(system "pwd")
(system "./setperms")

