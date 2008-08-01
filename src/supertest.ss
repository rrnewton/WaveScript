#! /bin/bash
#|
exec mzscheme -qr "$0" ${1+"$@"}
|#

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

;; [2007.11.03] FIXME!!! BADLY NEED A TIMEOUT ON ALL SUBPROCESSES!

(require (lib "process.ss") (lib "date.ss"))

(define publish? (not (member "-nopost" (vector->list (current-command-line-arguments)))))

;; Should we do the benchmarks in addition to regression testing?
(define benchmarks? (not (member "-nobench" (vector->list (current-command-line-arguments)))))

;; Let's clean up some:
(if (file-exists? "/tmp/wsparse_server_pipe")     (delete-file "/tmp/wsparse_server_pipe") (void))
(if (file-exists? "/tmp/wsparse_server_response") (delete-file "/tmp/wsparse_server_response") (void))

;; Should we killall the wsparse_server processes also?
 
; ----------------------------------------

(define ryan-email "ryan.newton@alum.mit.edu")
(define start-time (current-inexact-milliseconds))
(define last-test-timer start-time)
(define failed #f)

;; This should be run from this directory, but to make sure...
;(define test-directory (current-directory))
;(define test-root (format "~a/WS_test_copy" (getenv "HOME")))
;
;; [2007.10.11] Changing this to ASSUME that supertest.ss is invoked from it's own directory:
(current-directory "..")
(define test-root (path->string (current-directory)))
(define test-directory (format "~a/src" test-root))
(current-directory test-directory)

(printf "Test directory: ~s\n" test-directory)

(define date 
  (let ((d (seconds->date (current-seconds))))
    (format "~a-~a-~a_~a:~a:~a" 
	    (date-year d) (date-month d) (date-day d)
	    (date-hour d) (date-minute d) (date-second d))))
;(define logfile (format "~a/supertest_~a.log" (path->string (current-directory)) date))
(define logfile (format "~a/supertest_~a.log" test-directory date))
(define log (open-output-file logfile #:exists 'replace))
(define scriptoutput (open-output-file "SUPERTEST_SCRIPT_OUTPUT.log" #:exists 'replace))
(define orig-console (current-output-port))

(define (reset-timer!) (set! last-test-timer (current-inexact-milliseconds)))
(define (milli->minute t) (/ (round (* 10 (/ t 1000. 60.))) 10))
(define (code->msg! m) 
  (let ([val (if (or (equal? m 0) (equal? m #t))
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
    (let ([p (open-output-file fn #:exists 'replace)])
      (display str p)
      (close-output-port p)))
(define (system-to-str cmd)
  (define fn (format "/tmp/___supertest_tmp_~a.txt" (random 100000000)))
  (and (system (format "~a &> ~a" cmd fn))
       (file->string fn)))

(define (mail to subj msg)
  (define tmpfile (format "/tmp/temp~a.msg" (current-milliseconds)))
  (string->file msg tmpfile)
  (system (format "mail ~a -s '~a' < ~a" to subj tmpfile))
  (delete-file tmpfile)
  (printf "Mail Sent, to:~a subj: ~a\n" to subj)
  (printf "Body:\n~a\n" msg)
  )
;; Print to the log and to the screen:
(define (fpf . args)
  (apply fprintf log args)
  (apply printf args)
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

;; Run a command with a timeout.
(define (system/timeout cmd)
  (let* ([proc (process cmd)]
	 [stdout (car proc)]
	 [stdin  (cadr proc)]
	 [stderr (cadddr proc)]
	 [statusfn (car (reverse proc))])
    ;; Let something run for 30 min
    (define timeout (* 30 60 1000 ))
    (define pollinterval (* 250 ))
    (define (closeup) 
      (close-input-port stdout)
      (close-input-port stderr)
      (close-output-port stdin))
    (define start-time (current-inexact-milliseconds))
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
	       (fprintf orig-console "   Process timed out!\n")
	       (statusfn 'kill)
	       99)
	     (begin 
	       ;; Wait a bit and check again:
	       (sync (alarm-evt (+ (current-inexact-milliseconds) pollinterval)))
	       (waitloop (+ time pollinterval))))]
	[else (error 'system/timeout "")]))))

(define engine-dir (format "~a/WS_test_engine_~a" (getenv "HOME") (random 10000)))
(define engine-svn-revision 'unknown)
(define (setup-engine-dir!)
  (fprintf orig-console "  Checking out engine from svn into directory\n  ~a\n" engine-dir)
  (fpf "  Checking out engine from svn into directory\n   ~a\n" engine-dir)
  (ASSERT (system (format "rm -rf ~a" engine-dir)))
  (ASSERT (system 
	   (format 
	    "svn co -r 1495 svn+ssh://newton@nms.csail.mit.edu/export/home2/svn/WaveScope/trunk/code/v1 ~a" 
	    ;; [2007.10.29] Switching back to HEAD revision:
					;"svn co svn+ssh://newton@nms.csail.mit.edu/export/home2/svn/WaveScope/trunk/code/v1 ~a" 
	    engine-dir)))

  (ASSERT (putenv "WAVESCOPED" engine-dir))
  (ASSERT (system "echo WaveScope ENV var set: $WAVESCOPED"))
  (parameterize ([current-directory engine-dir])
    (set! engine-svn-revision
	  (begin 
	    (ASSERT (eqv? 0 (system/exit-code "svn info | grep Revision | sed s/Revision:// > svn_rev.txt")))
	    (read (open-input-file "svn_rev.txt"))))))

(define webroot "/var/www")
(define webdir  "/var/www/regression")

(define (post-to-web webfilename) 
  (define publish 
    (lambda (logfile webfile)
      (when (file-exists? webfile) (delete-file webfile))
      (fprintf orig-console "Copying log to website. ~a\n" webfile)
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
	(let ([perfreport (format "~a/benchmarks/perfreport.pdf" test-root)])
	  (when (file-exists? perfreport)
	    (let* ([webfile (format "~a/rev~a_eng~a_perfreport.pdf" webdir
				    svn-revision engine-svn-revision)])
	      (publish perfreport webfile)))))
      ;; Otherwise we could try to scp it...
      (void)
      ))

;; Partway through refactoring all the tests below to use this helper:
(define (run-test title cmd) 
  (fpf title)
  (fpf (format "~a~a\n"
	(list->string (vector->list (make-vector (max 0 (- 46 (string-length title))) #\space)))
	(code->msg! (system/timeout cmd))))
  (post-to-web (format "intermediate/rev_~a" svn-revision)))




; ----------------------------------------
;;; Main Script:
; ----------------------------------------


(ASSERT (putenv "REGIMENTD" test-root))

;; We use debugmode for all the tests below:

;; [2008.05.10] NOT DOING DEBUG MODE YET... (R6RS port)
;(ASSERT (putenv "REGDEBUGMODE" "ON"))

;(ASSERT (putenv "PATH" (format "~a/bin:~a" test-root (getenv "PATH"))))
;(ASSERT (putenv "PATH" (format "~a/depends:~a" test-root (getenv "PATH"))))

(ASSERT (system "echo Environment established: REGIMENTD:$REGIMENTD"))

;; Catch any errors encountered below and send an email:
(uncaught-exception-handler
 (lambda (exn)
   (define msg
     (format "ERROR during script execution:\n   ~a\n\nException: ~s\n" 
	     (exn-message exn) 
	     exn))
   (display msg orig-console);(fprintf orig-console msg)
   (mail ryan-email "Failure of supertest.ss" msg)
   ;; Might as well try this too:
   (fpf msg)
   (post-to-web (format "rev~a_ERROR" svn-revision))
   (exit 1)))

(current-output-port scriptoutput)
(current-error-port scriptoutput)

(fprintf orig-console "Opened logfile: ~s \n" logfile)
;(flush-output log)
;(close-output-port log)
;(set! log (open-output-file logfile 'append))

(define svn-revision
  (begin 
    (ASSERT (eqv? 0 (system/exit-code "which svn > /dev/null")))
    (ASSERT (eqv? 0 (system/exit-code "svn info | grep Revision | sed s/Revision:// > svn_rev.txt")))
    (read (open-input-file "svn_rev.txt"))))

(fprintf orig-console "SVN revision: ~a\n" svn-revision)

;; Here we begin running tests:

(fpf "\nRunning from directory: ~a\n\n" test-root)

(fpf "\nWaveScript (rev ~a) build & unit tests:\n" svn-revision)
(fpf "========================================\n")

(post-to-web (format "intermediate/rev_~a" svn-revision))

(reset-timer!)
(run-test "Build directory cleaned:" "make clean > make_clean.log")

(run-test "ikarus:   Ikarus runs:"    (format "echo | ikarus "))
(run-test "mzscheme: MzScheme runs:"  (format "echo | mzscheme "))
;(run-test "larceny:  Larceny runs:"   (format "echo | larceny"))

(current-directory test-directory)

(run-test "Build aggregate libraries:" "make aggregated &> make_aggregated.log")
(run-test "ikarus: Build object files: " "make ik &> ikarus_BUILD.log")
(run-test "ikarus: Load & run unit tests: "
	  "../bin/regiment.ikarus t &> ikarus_UNIT_TESTS.log")
(run-test "plt: Build bytecode files: " "make bc &> plt_BUILD.log")

;; I turn these on and off, depending on whether I want to tolerate the huge slowdown.
;(run-test "larceny: Load from source: " "../bin/regiment.larceny &> larceny_LOAD_FROM_SOURCE.log")
; (run-test "larceny: Partial larceny build: " "make larceny &> larceny_BUILD.log")
;; 


#| ;; Comment chez

(run-test "chez: Full Chez Scheme on the test system:" "which chez > /dev/null")
(run-test "chez: WScript loads from source (via script):" "./regiment_script.ss &> chez_SCRIPT_LOAD.log")
(run-test "chez: WScript has access to the compiler:"
	  "echo '(compile 3)' | ./regiment_script.ss i --exit-error")
(run-test "chez: Unit tests, loaded from source:" "./regiment_script.ss test &> chez_UNIT_TESTS.log")

(begin (newline)
       (printf "Second: building Chez shared object\n")
       (printf "============================================================\n")
       (ASSERT (putenv "REGDEBUGMODE" "OFF"))

       (run-test "chez: Build .so file:" "make chez &> chez_BUILD_SO.log")
       (run-test "chez: Also build debugmode .so file:" "make dbg &> chez_BUILD_DBG.log")
	     
       (ASSERT (system "./regiment_script.ss 2> chez_LOAD_FROM_SO.log"))
       (run-test "chez: System loads from .so file:" "grep 'compiled .so' chez_LOAD_FROM_SO.log")


       (ASSERT (putenv "REGDEBUGMODE" "ON"))


       ;; Disabling this temporarily, problem with simalpha-generate-modules (and lang_wavescript):
       ;; FIXME:

;       (define runso (system/timeout "./regiment_script.ss test"))
;       (fpf "chez: Unit tests, loaded from .so file:       ~a\n" (code->msg! runso))
       )

(run-test "chez: Build C extensions:" "make c &> gcc_BUILD_C_EXTENSIONS.log")

;; Now clean again:
;(ASSERT (system "make clean > make_clean2.log"))

|# ;; End comment chez

(fpf "\n")
(run-test "wsparse: Building executable (plt):" "make wsparse &> plt_WSPARSE.log")

(run-test "wsparse: wsparse is in path:" "which wsparse &> which_wsparse.log")


;(run-test "plt: Building WScript as bytecode in PLT:" "make pltbc &> plt_BUILD_PLT_BYTECODE.log")
;(run-test "plt: Run system from command line with PLT:" "regiment.plt &> plt_RUN_PLT_BYTECODE.log")

;; Now let's see if we can load in larceny:
;; Disabling for now because it's explodiing faith's HD:!!!
;(run-test "larceny: Testing compile in Larceny Scheme:"  "make larc &> larceny.log")

(fpf "\n\nWaveScript demos & libraries (Scheme backend):\n")
(fpf "========================================\n")

(parameterize ((current-directory (format "~a/demos/wavescope" test-directory)))
  (run-test "ws: Downloading sample marmot data:" "./download_sample_marmot_data")

  (ASSERT (putenv "REGIMENTHOST" "ikarus"))
  (run-test "ws: Running WaveScript Demos (ikarus):"
	    (format "./testall_demos.ss &> ~a/ws_demos.log" test-directory))
  (run-test "ws.early: WaveScript Demos (ikarus):"
	    (format "./testall_early &> ~a/wsearly_demos.log" test-directory))

  ;; Do the demos in PLT also.
  (ASSERT (putenv "REGIMENTHOST" "plt"))  
  (run-test "ws: Running WaveScript Demos (plt):"
	    (format "./testall_demos.ss &> ~a/ws_demos.log" test-directory))
  (ASSERT (putenv "REGIMENTHOST" ""))

;   (putenv "REGIMENTHOST" "plt")
;   (run-test  "plt: Running WaveScript Demos:"
; 	     (format "./testall_demos.ss &> ~a/plt_demos.log" test-directory))
;   (putenv "REGIMENTHOST" "")


  )



;; Test STANDARD LIBRARIES:
(parameterize ([current-directory (format "~a/lib/" test-root)])
  (run-test "ws: Loading stdlib_test.ws:" (format "ws stdlib_test.ws -n 10 -exit-error &> ~a/stdlib.log" test-directory))

  ;; This is the OLD one:
  ;; NO COMPLEX IN IKARUS YET:
  (ASSERT (putenv "REGIMENTHOST" "plt"))
  ;(when (file-exists? "query.exe") (delete-file "query.exe"))
  (run-test "wsc2: Old matrix_test.ws (plt):" 
	    (format "wsc2 matrix_test.ws -exit-error &> ~a/matrix_old_build.log" test-directory))
  (run-test "wsc2: Run old matrix_test.ws:" 
	    (format "./query.exe -n 10 &> ~a/matrix_old_run.log" test-directory))

  ;(when (file-exists? "query.exe") (delete-file "query.exe"))
#;  
  (run-test "ws: Running native WS test_matrix.ws:" 
	    (format "ws test_matrix.ws -exit-error -n 10 &> ~a/matrix_ws.log" test-directory))
  (run-test "wsc2: Native WS test_matrix.ws (plt):"
	    (format "wsc2 test_matrix.ws -exit-error &> ~a/matrix_build.log" test-directory))
  (run-test "wsc2: Run output exe:" 
	    (format "./query.exe -n 10 &> ~a/matrix_run.log" test-directory))
  (ASSERT (putenv "REGIMENTHOST" ""))
  )


;; Now for GSL interface.
(parameterize ([current-directory (format "~a/lib/" test-root)])
  (run-test "ws: Generating gsl matrix library wrappers:" 
	    (format "make &> ~a/gsl_wrappers.log" test-directory))

;; PLT has a bug with "exists" right now:  [2008.05.21]
;   (ASSERT (putenv "REGIMENTHOST" "plt"))a
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
;; WAVESCOPE ENGINE:

#; ;; Disabling
(begin 
(setup-engine-dir!) 

(fpf "\n\nWaveScope Engine (rev ~a):\n" engine-svn-revision)
(fpf "========================================\n")

(post-to-web (format "intermediate/rev_~a" svn-revision))
(parameterize ([current-directory engine-dir])
  
  (fpf "Engine: directory cleaned:                     ~a\n" (code->msg! (system/timeout "make clean")))  
  (fpf "Engine: 'make all':                            ~a\n" 
       (code->msg! (system/timeout (format "make all &> ~a/enigne_MAKE_ALL.log" test-directory))))  

  ;; TODO: This doesn't return ERROR code:
  (let ([testSignal (system/timeout (format "./testSignal-SMSegList &> ~a/engine_testSignal.log" test-directory))])
    (code->msg! testSignal)
    (fpf "Engine: testSignal-SMSegList                  ~a\n"
	 (if (zero? testSignal) "?maybe passed?" "-FAILED-")))
  (post-to-web (format "intermediate/rev_~a" svn-revision))
  
  ;; TODO: This probably doesn't return ERROR code:
  (let ([pipeMemory (system/timeout (format "./PipeMemory-SMSegList --at_once --push_batch 10 &> ~a/engine_PipeMemory.log" 
					    test-directory))])    
    (code->msg! pipeMemory)
    (fpf "Engine: PipeMemory-SMSegList                  ~a\n" 
	 (if (zero? pipeMemory) "?maybe passed?" "-FAILED-")))
  (post-to-web (format "intermediate/rev_~a" svn-revision))
)

) ;; End engine




;;================================================================================
;; Now test WSC:

(fpf "\n\nWaveScript C/C++ Backends (C++ uses XStream):\n")
(fpf "===================================================\n")

(parameterize ((current-directory (format "~a/demos/wavescope" test-directory)))
  ;; This runs faster if we load Regiment pre-compiled:
 ;(current-directory test-directory) (ASSERT (system "make chez"))
#; ;; Disabling
  (run-test "wsc: Running WaveScript Demos with WSC:"
	    (format "./testall_wsc &> ~a/wsc_demos.log" test-directory))

  (ASSERT (putenv "REGIMENTHOST" "ikarus"))
  (run-test "wsc2: Demos, simple RC (ikarus):"
	    (format "./testall_wsc2 -gc refcount &> ~a/wsc2_demos.log" test-directory))
  (system "cp .__runquery_output_wsc2.txt .__runquery_output_wsc2_nondef.txt")
  (ASSERT (putenv "REGIMENTHOST" "plt"))
  (run-test "wsc2: Demos, deferred RC (plt):"
	    (format "./testall_wsc2 -gc deferred -nothreads &> ~a/wsc2_plt_demos.log" test-directory))
  (system "cp .__runquery_output_wsc2.txt .__runquery_output_wsc2_def.txt")
  (ASSERT (putenv "REGIMENTHOST" ""))

  (run-test "wstiny: Running demos w/ TOSSIM:"
	    (format "./testall_wstiny &> ~a/wstiny_demos.log" test-directory))

  )

; (parameterize ((current-directory (format "~a/lib/" test-root)))
;   (run-test "wsc: Compiling stdlib_test:"
; 	    (format "wsc stdlib_test.ws -exit-error &> ~a/wsc_stdlib_build.log" test-directory))
;   #;
;   (run-test "wsc: Running stdlib_test:"
; 	    (format "./query.exe -exit-error &> ~a/wsc_stdlib_run.log" test-directory)))




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

;; TEMP DISABLE:
#;
  (parameterize ((current-directory (format "~a/lib/" test-root)))  
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
(parameterize ((current-directory (format "~a/apps/marmot" test-root)))
  (newline)
    
  (run-test "    Run Makefile   " "make")

  (begin
    (ASSERT (putenv "REGIMENTHOST" "ikarus"))
    (run-test "wsc2: Compiling marmot app (first phase):  "
	    (format "wsc2 run_first_phase.ws -exit-error &> ~a/wsc2_marmot1_build.log" 
		    test-directory))
    (run-test "wsc2:  Running marmot app (first phase):  "
	      (format "memprof ./query.exe -n 10 > ~a/wsc2_marmot1_run.log 2> marmot1_memprof.txt" test-directory))
    (when (file-exists? "query.exe") (delete-file "query.exe"))
    
    (ASSERT (putenv "REGIMENTHOST" "plt"))
    (run-test "wsc2: Compiling marmot phase 1&2 (plt): "
	      (format "wsc2 run_marmot2.ws -exit-error &> ~a/wsc2_marmot12_build.log" test-directory))
    (run-test "wsc2:  Running marmot app (second phase): "
	      (format "./query.exe -n 1 &> ~a/wsc2_marmot12_run.log" test-directory))
    (ASSERT (putenv "REGIMENTHOST" ""))

    (run-test "wsc2: Compiling marmot app (third phase):  "
      (format "wsc2 test_heatmap.ws -exit-error &> ~a/wsc2_marmot1_build.log" 
	      test-directory))
    
    
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

  #; ;; Disabling
  (begin 
    (run-test "wsc: Compiling marmot app (first phase):  "
	      (format "wsc run_first_phase.ws -exit-error &> ~a/wsc_marmot1_build.log" 
		      test-directory))
    (run-test "wsc: Running marmot app (first phase):  "
	      (format "./query.exe -n 1 &> ~a/wsc_marmot1_run.log" test-directory))
    (run-test "wsc: Compiling marmot app (second phase):  "
	      (format "wsc run_marmot2.ws -exit-error &> ~a/wsc_marmot12_build.log" test-directory))
    ;; [2007.10.12] Need -n for the C++ engine!!! This query will not die when the file ends.
    (run-test "wsc: Running marmot app (second phase): "
	      (format "./query.exe -n 1 &> ~a/wsc_marmot12_run.log" test-directory)))
|#
  
  ) ;; End MARMOT



(parameterize ((current-directory (format "~a/apps/telos_audio" test-root)))
  #;
  (run-test "ws: Running first speaker detection: "
	    (format "ws mfcc1.ws -n 1 &> ~a/ws_mfcc1.log" test-directory))

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
  (run-test "wsc2: Compiling fixed fb speaker detect: "
	    (format "wsc2 mfcc6_fixedpoint_fb.ws  &> ~a/wsc2_build_mfcc6.log" test-directory))
  (run-test "wsc2:  Running fixed fb speaker detect: "
	    (format "./query.exe -n 1 &> ~a/wsc2_run_mfcc6.log" test-directory))

  (run-test "wstiny: Compiling speaker detect, Telos: "
	    (format "wstiny mfcc6_fixedpoint_fb.ws  &> ~a/wstiny_build_mfcc6.log" test-directory))
  
  )





#|

(parameterize ((current-directory (format "~a/apps/pipeline-web" test-root)))
  (run-test "ws: Running pipeline-web app:   " 
	    (format "make test &> ~a/ws_pipeline-web.log" test-directory)))

(parameterize ((current-directory (format "~a/apps/stockticks" test-root)))
  (run-test "ws: Running stockticks app:   "
	    (format "make test &> ~a/ws_stockticks.log" test-directory)))

(parameterize ((current-directory (format "~a/apps/pipeline" test-root)))
  (run-test "    Decompressing pipeline data   "  "bunzip2 pipeline1.data.bz2")
  (run-test "ws: Running pipeline app:    "
	    (format "ws.debug pipeline.ws -n 10 -exit-error &> ~a/ws_pipeline.log" test-directory))
  (run-test "ws.early: Running pipeline app: "
	    (format "ws.early pipeline.ws -n 10 -exit-error &> ~a/ws_pipeline.log" test-directory)))

|#


;;================================================================================
;; Performance benchmarks.

(when benchmarks?

  (post-to-web (format "intermediate/rev_~a" svn-revision))
  (fpf "\n\nPerformance benchmarks (all backends)\n")
  (fpf "========================================\n")

  (parameterize ((current-directory (format "~a/benchmarks" test-root)))
    
    ;; [2007.10.30] running stepts incrementally, so we see what fails:
    (run-test "    Setup Engines:             " 
	      (format "make engine &> ~a/bench_setup.log" test-directory))
    (ASSERT (system "make topbefore"))

    (current-directory (format "~a/benchmarks/microbench" test-root))
    (run-test "    Run microbenchmarks:              " 
	      (format "make &> ~a/bench_micro.log" test-directory))

    (current-directory (format "~a/benchmarks/language_shootout" test-root))
    (run-test "    Run language_shootout:       " 
	      (format "make &> ~a/bench_shootout.log" test-directory))

    (current-directory (format "~a/benchmarks/appbench" test-root))
    (run-test "    Run application benchmarks: " 
	      (format "make &> ~a/bench_apps.log" test-directory))
    
					;  (current-directory (format "~a/benchmarks/datareps" test-root))
					;  (run-test "    Run datarep benchmarks:" 
					;	    (format "make &> ~a/bench_datareps.log" test-directory))

    (current-directory (format "~a/benchmarks" test-root))
#;
    (run-test "    Verify dependencies, do conversions:" 
	      (format "make alldeps &> ~a/bench_alldepscleanup.log" test-directory))
    
    (ASSERT (system "make topafter"))
    (ASSERT (system "make machineinfo.tex"))
    (ASSERT (system "make wssvn.tex"))
    (ASSERT (system "make enginesvn.tex"))
    (run-test "    Compile results, build full report: " 
	      (format "make perfreport.pdf &> ~a/bench_perfreport.log" test-directory))
    ))

#|


;; POTHOLE 
;; TODO: Do other pothole variants.  pothole4 is just the one I know works.
#;
(begin (newline)
       (current-directory (format "~a/apps/potholes" test-root))
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
  (parameterize ((current-directory (format "~a/demos/wavescope" test-root)))

    (fpf "\n\n  Sanity Checks:\n")
    (fpf "========================================\n")    
    ;; --- First we check for memory leaks in any of the demos ----
    (ASSERT (system "grep \"definitely lost in\" .__runquery_output_wsc2_nondef.txt | wc -l > lost_blocks.txt"))
    (let ([lost_blocks (read (open-input-string (file->string "lost_blocks.txt")))])
      (fprintf outp "Wsc2RC_DemosLostBlocks ~a\n" lost_blocks)
      ;; [2008.07.28] There should only be a lost_block from demo4b currently (mysterious fftw plan leak)

      ;; [2008.07.30] TEMP: Having an odd leak on 64 bit machines on demo4d_quoted_constants.ws
      ;; I can't make sense of it.   Almost seems like valgrind is wrong.
      (system "uname -m > machine_type.txt")
      (unless (equal? "x86_64" (file->string "machine_type.txt"))
	(fpf "Verify no leaks in demos (refcount):          ~a\n" (code->msg! (<= lost_blocks 1))))
      )
    (ASSERT (system "grep \"definitely lost in\" .__runquery_output_wsc2_def.txt | wc -l > lost_blocks.txt"))
    (let ([lost_blocks (read (open-input-string (file->string "lost_blocks.txt")))])
      (fprintf outp "Wsc2DefRC_DemosLostBlocks ~a\n" lost_blocks)
      (fpf "Verify no leaks in demos (deferred):          ~a\n" (code->msg! (<= lost_blocks 1)))
      )    

    ;; --- We also check the valgrind traces for errors ----
    (ASSERT (system "grep \"ERROR SUMMARY\" .__runquery_output_wsc2_def.txt    | grep -v \"ERROR SUMMARY: 0\" | wc -l >  errors.txt"))
    (ASSERT (system "grep \"ERROR SUMMARY\" .__runquery_output_wsc2_nondef.txt | grep -v \"ERROR SUMMARY: 0\" | wc -l >> errors.txt"))
    (let ([errors (read (open-input-string (file->string "errors.txt")))])
      ;; This should be 2... demo4b currently has conditional jump errors.
      (fprintf outp "Wsc2_DemosErrors ~a\n" errors)
      (fpf "Verify no errors in demos:                    ~a\n" (code->msg! (<= errors 2)))
      )
    ) ;; End demos

  ;; --- We check the total memory footprint of the marmot app. ----  
  (parameterize ((current-directory (format "~a/apps/marmot" test-root)))
    (system "cat marmot1_memprof.txt | grep -v '#' | tail -n 1  > .__last_line.txt")
    ;; Get the peak memory usage
    (let ([peak_vm (read (open-input-file ".__last_line.txt"))])
      (fprintf outp "Marmot1_PeakVM ~a\n" peak_vm)
      )
    )
  
  ;; --- Gather a table of numbers from the benchmarks directory. ----  
  (when benchmarks? 
    (parameterize ((current-directory (format "~a/benchmarks" test-root)))
      (code->msg! (system "gather_results.ss > results_table.txt"))
      (display (file->string "results_table.txt") outp)
      ))        
  (close-output-port outp)) ;; Done writing vital_stats.txt
(begin 
  (fpf "\n\n  Vital Stats:\n")
  (fpf "========================================\n")
  (fpf "~a" (file->string vitals)))

;;================================================================================
;;; Wrap it up
;;================================================================================

(fpf "\n\n\nTotal time spent testing: ~a minutes\n" 
     (milli->minute (- (current-inexact-milliseconds) start-time)))

;(fpf "\n\nWaveScript Rev: ~a\n" svn-revision)
;(fpf "WaveScope Engine Rev: ~a\n" engine-svn-revision)

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
(system (format "~a/depends/petite --version &> temp.log" test-root))
(fpf (file->string "temp.log"))

(fpf "Larceny Scheme version:  \n   ")
(system "echo | larceny | head -n 1 &> temp.log")
(fpf (file->string "temp.log"))

(fpf "Ikarus Scheme version:  \n   ")
(system "echo | ikarus | head -n 1 &> temp.log")
(fpf (file->string "temp.log"))

(fpf "MLton version:  \n   ")
(fpf (system-to-str "echo | mlton | head -n 1"))

(fpf "OCaml version:  \n   ")
(fpf (system-to-str "ocaml -version"))

(close-output-port log)
(define thesubj 
  (if failed 
      (format "[Regression] WaveScript/Scope rev ~a/~a FAILED nightly tests" svn-revision engine-svn-revision)
      (format "[Regression] WaveScript/Scope rev ~a/~a passed nightly tests" svn-revision engine-svn-revision)))
(define themsg  (file->string logfile))

(mail ryan-email thesubj themsg)
;(if failed (mail "ws@nms.csail.mit.edu" thesubj themsg))

(post-to-web (format "rev~a_eng~a_~a~a~a"
		     svn-revision engine-svn-revision
		     (if (getenv "CC") (format "~a_" (getenv "CC")) "")
		     (if benchmarks? "wbench_" "")
		     (if failed "FAILED" "passed")))

;; Finally, copy all logs 
(fprintf orig-console "~a all logs to website...\n" (if publish? "Copying" "NOT copying"))
(when publish? (system (format "rm -f ~a/most_recent_logs/*" webdir)))
(when publish? (system (format "cp ~a/*.log ~a/most_recent_logs/" webdir test-directory)))
(fprintf orig-console "Finished (not) copying.")

(current-directory webroot)
(system "./setperms")

