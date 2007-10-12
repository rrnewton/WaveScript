#! /bin/bash
#|
exec mzscheme -qr "$0" ${1+"$@"}
|#

;; This really puts the system through the paces.
;; It's a PLT-ONLY script.  (Chez's system call doesn't return the error code.)

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


;; TODO: Add a timeout!  In case the test gets stuck.

(require (lib "process.ss") (lib "date.ss"))


;; Let's clean up some:
(if (file-exists? "/tmp/wsparse_server_pipe")     (delete-file "/tmp/wsparse_server_pipe"))
(if (file-exists? "/tmp/wsparse_server_response") (delete-file "/tmp/wsparse_server_response"))

;; Should we killall the wsparse_server processes also?
 
; ----------------------------------------

;(define ryan-email "rrnewton@gmail.com")
(define ryan-email "ryan.newton@alum.mit.edu")
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
;(define test-root (format "~a/WS_test_copy" (getenv "HOME")))
;
;; [2007.10.11] Changing this to ASSUME that supertest.ss is invoked from it's own directory:
(current-directory "..")
(define test-root (path->string (current-directory)))
(define test-directory (format "~a/src" test-root))
(current-directory test-directory)

(ASSERT (putenv "REGIMENTD" test-root))

;; We use debugmode for all the tests below:
(ASSERT (putenv "REGDEBUGMODE" "ON"))

;(ASSERT (putenv "PATH" (format "~a/bin:~a" test-root (getenv "PATH"))))
;(ASSERT (putenv "PATH" (format "~a/depends:~a" test-root (getenv "PATH"))))

(ASSERT (system "echo Environment established: REGIMENTD:$REGIMENTD"))

;; Catch any errors encountered below and send an email:
(uncaught-exception-handler
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

(fpf "\nWaveScript (rev ~a) build & unit tests:\n" svn-revision)
(fpf "========================================\n")

(begin (reset-timer!)
       (fpf "Build directory cleaned:                      ~a\n" 
	    (code->msg! (system/exit-code "make clean > make_clean.log"))))

(begin (define runpetite (system/exit-code (format "echo | ~a/depends/petite" test-root)))
       (fpf "petite: Repository's Petite Chez runs:        ~a\n" (code->msg! runpetite)))

;; Now that we're sure petite runs let's get the machine type:
(define machine-type 
  (begin 
    (ASSERT (eqv? 0 (system/exit-code "echo '(machine-type)' | petite -q > machine_type.txt")))
    (read (open-input-file "machine_type.txt"))))

(begin (current-directory test-directory)
       (define testpetite
	 (system/exit-code 
	  "echo \"(define-top-level-value 'REGIMENT-BATCH-MODE #t) (test-units)\" | ../depends/petite main_chez.ss &> petite_UNIT_TESTS.log"))
       (fpf "petite: Load & run unit tests:                ~a\n" (code->msg! testpetite)))

(begin (define fullchez (system/exit-code "which chez > /dev/null"))
       (fpf "chez: Full Chez Scheme on the test system:    ~a\n" (code->msg! fullchez)))

(begin (define loaded (system/exit-code "./regiment_script.ss &> chez_SCRIPT_LOAD.log"))
       (fpf "chez: WScript loads from source (via script): ~a\n" (code->msg! loaded)))

(begin (define compilerworks (system/exit-code "echo '(compile 3)' | ./regiment_script.ss i --exit-error"))
       (fpf "chez: WScript has access to the compiler:     ~a\n" (code->msg! compilerworks)))

(begin (newline)
       (printf "First: from source\n")
       (printf "============================================================\n")
       (define frmsrc (system/exit-code "./regiment_script.ss test &> chez_UNIT_TESTS.log"))
       (fpf "chez: Unit tests, loaded from source:         ~a\n" (code->msg! frmsrc)))

(begin (newline)
       (printf "Second: building Chez shared object\n")
       (printf "============================================================\n")
       (ASSERT (putenv "REGDEBUGMODE" "OFF"))

       (fpf "chez: Build .so file:                         ~a\n" (code->msg! (system/exit-code "make chez &> chez_BUILD_SO.log")))
       (fpf "chez: Also build debugmode .so file:          ~a\n" (code->msg! (system/exit-code "make dbg &> chez_BUILD_DBG.log")))
	     
       (ASSERT (system "./regiment_script.ss 2> chez_LOAD_FROM_SO.log"))
       (define loadedso (system/exit-code "grep 'compiled .so' chez_LOAD_FROM_SO.log"))
       (fpf "chez: System loads from .so file:             ~a\n" (code->msg! loadedso))

#|
       (fpf "chez: Build .boot file:                       ~a\n"
	    (code->msg! (system/exit-code "make boot &> chez_BUILD_BOOT.log")))
       (ASSERT (system "./bin/regiment 2> chez_LOAD_FROM_BOOT.log"))
       (fpf "chez: System loads from .boot file:           ~a\n" 
	    (code->msg! (system/exit-code "grep 'compiled .so' chez_LOAD_FROM_BOOT.log")))


|#
       (ASSERT (putenv "REGDEBUGMODE" "ON"))


       ;; Now copy that .so file to our stored binaries directory.
       ;; But we only do this on faith, so first test if the dir is there:

;; [2007.03.13] Might this out because we have a seperate, more thorough script that does it:
#;
       (when (directory-exists? "/var/www/regiment_binaries")
	 (fprintf orig-console "Copying prebuilt binary to website.\n")
	 (let* ([webfile (format "/var/www/regiment_binaries/~a/~a_~a_main_chez.so" 
				 machine-type svn-revision machine-type)]
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

(begin (define c-build (system/exit-code "make c &> gcc_BUILD_C_EXTENSIONS.log"))
       (fpf "chez: Build C extensions:                     ~a\n" (code->msg! c-build)))

;; Now clean again:
(ASSERT (system "make clean > make_clean2.log"))

(begin (newline) (fpf "\n")
       (printf "Third: building bytecode in PLT\n")
       (printf "============================================================\n")
       (define wsparse (system/exit-code "make wsparse &> plt_BUILD_WSPARSE.log"))
       (fpf "plt: Building wsparse executable:             ~a\n" (code->msg! wsparse))


;; [2007.03.13] Might take this out because we have a seperate, more thorough script that does it:
#;
       ;; Now copy that executable file to our stored binaries directory.
       (when (directory-exists? "/var/www/regiment_binaries")
	 (fprintf orig-console "Copying prebuilt wsparse to website.\n")
	 (let* ([webfile (format "/var/www/regiment_binaries/~a/~a_~a_wsparse" 
				 machine-type svn-revision machine-type)])
	   (if (file-exists? webfile) (delete-file webfile))
	   (copy-file "bin/wsparse" webfile)
	   (ASSERT (system (format "chgrp www-data ~a" webfile)))
	   (ASSERT (system (format "chmod g+r ~a" webfile)))
	   ))
       )

(begin (define pltbc (system/exit-code "make pltbc &> plt_BUILD_PLT_BYTECODE.log"))
       (fpf "plt: Building WScript as bytecode in PLT:     ~a\n" (code->msg! pltbc)))

(begin (define pltrun (system/exit-code "regiment.plt &> plt_RUN_PLT_BYTECODE.log"))
       (fpf "plt: Run system from command line with PLT:   ~a\n" (code->msg! pltrun)))

;; [2007.02.28] This has been broken for a while, and the error code isn't working right.
(begin (newline)
       (printf "Fourth: Running tests in PLT\n")
       (printf "============================================================\n")
       (define plttests (system/exit-code 
			 ;(format "echo '(test-units)' | mzscheme -f ~a/main_plt.ss &> 8_PLT_UNIT_TESTS.log" test-directory)
			 "regiment.plt test &> plt_UNIT_TESTS.log"
			  ))
       (fpf "plt: Running tests in PLT:                    ~a\n" (code->msg! plttests)))


(fpf "\n\nWaveScript demos & libraries (Scheme backend):\n")
(fpf "========================================\n")

(begin (newline)
       (printf "Fifth: Running WaveScript Demos\n")
       (printf "============================================================\n")
       (current-directory (format "~a/demos/wavescope" test-directory))
       (define getdata (system/exit-code "./download_sample_marmot_data"))
       (fpf "ws: Downloading sample marmot data:           ~a\n" (code->msg! getdata))

       (fpf "ws: Running WaveScript Demos:                 ~a\n" 
	    (code->msg! (system/exit-code (format "./testall_demos.ss &> ~a/ws_demos.log" test-directory))))
       (fpf "ws.early: Running Demos (no static elab):     ~a\n" 
	    (code->msg! (system/exit-code (format "./testall_early &> ~a/wsearly_demos.log" test-directory))))
       (current-directory test-directory)
       )


(begin (current-directory (format "~a/lib/" test-root))
       (define stdlib (system/exit-code (format "echo 10 | ws stdlib_test.ws -exit-error &> ~a/stdlib.log" test-directory)))
       (fpf "ws: Loading stdlib_test.ws:                   ~a\n" (code->msg! stdlib))

       ;; This is the OLD one:
       (fpf "ws: Loading old matrix_test.ws:               ~a\n" 
	    (code->msg! (system/exit-code (format "echo 10 | ws matrix_test.ws -exit-error &> ~a/matrix_old.log" test-directory))))

       (fpf "ws: Running native WS test_matrix.ws:         ~a\n" 
	    (code->msg! (system/exit-code (format "echo 10 | ws test_matrix.ws -exit-error &> ~a/matrix_ws.log" test-directory))))
       

       (current-directory test-directory))

;; Now for GSL interface.
(begin (current-directory (format "~a/lib/" test-root))
       (fpf "ws: Generating gsl matrix library wrappers:   ~a\n" 
	    (code->msg! (system/exit-code (format "make &> ~a/gsl_wrappers.log" test-directory))))       

       (fpf "ws: Running GSL test_matrix_gsl.ws:           ~a\n" 
	    (code->msg! (system/exit-code (format 
               "echo 10 | ws test_matrix_gsl.ws -exit-error &> ~a/matrix_gsl.log" test-directory))))
#;
       (fpf "ws: Running test of GSL matrix library.ws:    ~a\n"
	    (code->msg! (system/exit-code 
             (format "echo 10 | ws run_matrix_gsl_test.ws -exit-error  &> ~a/matrix_gsl.log" test-directory))))
       (current-directory test-directory))

(begin (current-directory (format "~a/demos/wavescope" test-directory))
       (putenv "REGIMENTHOST" "plt")
       (define pltdemos (system/exit-code 
			 (format "./testall_demos.ss &> ~a/plt_demos.log" test-directory)))
       (putenv "REGIMENTHOST" "")
       (fpf "plt: Running demos in PLT:                    ~a\n" (code->msg! pltdemos)))

;;================================================================================
;; WAVESCOPE ENGINE:

(begin 

(define engine-dir (format "~a/WS_test_engine_~a" (getenv "HOME") (random 10000)))
(ASSERT (system (format "rm -rf ~a" engine-dir)))
(ASSERT (system 
	 (format 
	  ;; TEMPORARILLY FIXING AT 1495
	  "svn co -r 1495 svn+ssh://newton@nms.csail.mit.edu/export/home2/svn/WaveScope/trunk/code/v1 ~a" 
	  engine-dir)))

(ASSERT (putenv "WAVESCOPED" engine-dir))
(ASSERT (system "echo WaveScope ENV var set: $WAVESCOPED"))
(current-directory engine-dir)

(define engine-svn-revision
  (begin 
    (ASSERT (eqv? 0 (system/exit-code "svn info | grep Revision | sed s/Revision:// > svn_rev.txt")))
    (read (open-input-file "svn_rev.txt"))))

(fpf "\n\nWaveScope Engine (rev ~a):\n" engine-svn-revision)
(fpf "========================================\n")

(begin (define engine-cleaned (system/exit-code "make clean"))
       (fpf "Engine: directory cleaned:                     ~a\n" (code->msg! engine-cleaned)))


(begin (current-directory engine-dir)
       (define engine-make (system/exit-code (format "make all &> ~a/enigne_MAKE_ALL.log" test-directory)))
       (fpf "Engine: 'make all':                            ~a\n" (code->msg! engine-make)))

;; TODO: This doesn't return ERROR code:
(begin (current-directory engine-dir)
       (define testSignal (system/exit-code (format "./testSignal-SMSegList &> ~a/engine_testSignal.log" test-directory)))
       ;(fpf "Engine: testSignal-SMSegList                  ~a\n" (code->msg! testSignal))
       (code->msg! testSignal)
       (fpf "Engine: testSignal-SMSegList                  ~a\n"
	    (if (zero? testSignal) "?maybe passed?" "-FAILED-"))
       )

;; TODO: This probably doesn't return ERROR code:
(begin (current-directory engine-dir)
       (define pipeMemory (system/exit-code (format "./PipeMemory-SMSegList --at_once --push_batch 10 &> ~a/engine_PipeMemory.log" 
						    test-directory)))
       (code->msg! pipeMemory)
       (fpf "Engine: PipeMemory-SMSegList                  ~a\n" 
	    (if (zero? pipeMemory) "?maybe passed?" "-FAILED-"))
       )

) ;; End engine

;;================================================================================
;; Now test WSC:

(fpf "\n\nWaveScript C++ Backend (uses engine):\n")
(fpf "========================================\n")

(begin ;; This runs faster if we load Regiment pre-compiled:
       ;(current-directory test-directory) (ASSERT (system "make chez"))
       (current-directory (format "~a/demos/wavescope" test-directory))
       (define wsc-demos (system/exit-code (format "./testall_wsc &> ~a/wsc_demos.log" test-directory)))
       (current-directory test-directory)
       (fpf "wsc: Running WaveScript Demos with WSC:       ~a\n" (code->msg! wsc-demos)))

#;
(begin 
       (current-directory (format "~a/lib/" test-root))
       (fpf "wsc: Compiling stdlib_test:                   ~a\n"
	    (code->msg! (system/exit-code 
               (format "wsc stdlib_test.ws -exit-error &> ~a/wsc_stdlib_build.log" test-directory))))
#;
       (fpf "wsc: Running stdlib_test:                     ~a\n"
	    (code->msg! (system/exit-code 
	      (format "./query.exe -exit-error &> ~a/wsc_stdlib_run.log" test-directory))))
       (current-directory test-directory))


#|
;;================================================================================
;; Now test WSCAML:

(fpf "\n\nWaveScript CAML Backend: \n")
(fpf "========================================\n")

(begin (newline)
       (current-directory test-directory)
       (fpf "wscaml: Building ocaml libraries (fftw, etc): ~a\n" 
	    (code->msg! (system/exit-code (format "make ocaml &> ~a/wscaml_build_stuff.log" test-directory))))
       (current-directory (format "~a/demos/wavescope" test-directory))
       (fpf "wscaml: Running Demos through OCaml:          ~a\n" 
	    (code->msg! (system/exit-code (format "./testall_caml &> ~a/wscaml_demos.log" test-directory))))
       (current-directory test-directory))

|#

(fpf "\n\nWaveScript MLTON Backend:\n" )
(fpf "========================================\n")

(begin (newline)
       (current-directory test-directory)
       (current-directory (format "~a/demos/wavescope" test-directory))
       (fpf "wsmlton: Running Demos through MLton:         ~a\n" 
	    (code->msg! (system/exit-code (format "./testall_mlton &> ~a/wsmlton_demos.log" test-directory))))
       (current-directory test-directory))
(begin 
       (current-directory (format "~a/lib/" test-root))
       (fpf "wsmlton: Compiling stdlib_test:               ~a\n"
	    (code->msg! (system/exit-code 
               (format "wsmlton stdlib_test.ws -exit-error &> ~a/wsmlton_stdlib_build.log" test-directory))))
       (fpf "wsmlton: Running stdlib_test:                 ~a\n"
	    (code->msg! (system/exit-code 
	      (format "./query.mlton.exe -n 10 -exit-error &> ~a/wsmlton_stdlib_run.log" test-directory))))
       (current-directory test-directory))



;;================================================================================
;; APPLICATIONS

(fpf "\n\nWaveScript Applications:\n")
(fpf "========================================\n")


(begin (current-directory (format "~a/apps/pipeline-web" test-root))
       (define pipeline-web (system/exit-code (format "make test &> ~a/ws_pipeline-web.log" test-directory)))
       (fpf "ws: Running pipeline-web app:                 ~a\n" (code->msg! pipeline-web))
       (current-directory test-directory))

(begin (current-directory (format "~a/apps/stockticks" test-root))
       (fpf "ws: Running stockticks app:                   ~a\n"
	    (code->msg! (system/exit-code (format "make test &> ~a/ws_stockticks.log" test-directory))))
       (current-directory test-directory))

(begin (current-directory (format "~a/apps/pipeline" test-root))
       (fpf "    Decompressing pipeline data               ~a\n" 
	    (code->msg! (system/exit-code "bunzip2 pipeline1.data.bz2")))
       (fpf "ws: Running pipeline app:                     ~a\n"
	    (code->msg! (system/exit-code 
              (format "echo 10 | ws.debug pipeline.ws -exit-error &> ~a/ws_pipeline.log" test-directory))))
       (fpf "ws.early: Running pipeline app:               ~a\n"
	    (code->msg! (system/exit-code 
              (format "echo 10 | ws.early pipeline.ws -exit-error &> ~a/ws_pipeline.log" test-directory))))
       (current-directory test-directory))

;; MARMOT
(begin (newline)
       (current-directory (format "~a/apps/marmot" test-root))
       (fpf "    Run Makefile                              ~a\n" 
	    (code->msg! (system/exit-code "make")))
       (fpf "ws: Running marmot app (first phase):         ~a\n"
	    (code->msg! (system/exit-code 
            (format "echo 1 | ws.debug run_first_phase.ws -exit-error &> ~a/ws_marmot.log" test-directory))))
       (fpf "ws.early: Running marmot app:                 ~a\n"
	    (code->msg! (system/exit-code 
            (format "echo 1 | ws.early run_first_phase.ws -exit-error &> ~a/wsearly_marmot.log" test-directory))))

       (fpf "ws: Running marmot app (second phase):        ~a\n"
	    (code->msg! (system/exit-code 
            (format "echo 1 | ws.debug run_marmot2.ws -exit-error &> ~a/ws_marmot12.log" test-directory))))

       (fpf "ws: Running marmot app (3phases):             ~a\n"
	    (code->msg! (system/exit-code 
            (format "echo 1 | ws.debug run_3phases.ws -exit-error &> ~a/ws_marmot123.log" test-directory))))

       (fpf "wsmlton: Compiling marmot app (first phase):  ~a\n"
	    (code->msg! (system/exit-code (format "wsmlton run_first_phase.ws -exit-error &> ~a/wsmlton_marmot1_build.log" test-directory))))
       (fpf "wsmlton: Running marmot app (first phase):    ~a\n"
	    (code->msg! (system/exit-code (format "./query.mlton.exe -n 1 &> ~a/wsmlton_marmot1_run.log" test-directory))))
       
       ;; Third phase won't work in "ws" because of writing ppm file.
       (fpf "wsmlton: Compiling marmot app (3phases):      ~a\n"
	    (code->msg! (system/exit-code (format "wsmlton run_3phases.ws -exit-error &> ~a/wsmlton_marmot123_build.log" test-directory))))
       (fpf "wsmlton: Running marmot app (3phases):        ~a\n"
	    (code->msg! (system/exit-code (format "./query.mlton.exe -n 1 &> ~a/wsmlton_marmot123_run.log" test-directory))))
       

;; FIXME: ADD THIRD STAGE ETC!!!


       (fpf "wsc: Compiling marmot app (first phase):      ~a\n"
	    (code->msg! (system/exit-code (format "wsc run_first_phase.ws -exit-error &> ~a/wsc_marmot1_build.log" test-directory))))
       (fpf "wsc: Running marmot app (first phase):        ~a\n"
	    (code->msg! (system/exit-code (format "./query.exe &> ~a/wsc_marmot1_run.log" test-directory))))
       (fpf "wsc: Compiling marmot app (second phase):     ~a\n"
	    (code->msg! (system/exit-code (format "wsc run_marmot2.ws -exit-error &> ~a/wsc_marmot12_build.log" test-directory))))
       (fpf "wsc: Running marmot app (second phase):       ~a\n"
	    (code->msg! (system/exit-code (format "./query.exe &> ~a/wsc_marmot12_run.log" test-directory))))

       (current-directory test-directory)
       )






;; POTHOLE 
;; TODO: Do other pothole variants.  pothole4 is just the one I know works.
#;
(begin (newline)
       (current-directory (format "~a/apps/potholes" test-root))
       (fpf "    Fetching pothole data                     ~a\n" 
	    (code->msg! (system/exit-code 
	       (format "./download_small_sample_data  &> ~a/download_pothole_data.log" 
		       test-directory))))

       ;; TEMP FIXME!!!
       ;; Temporarilly turning off debug mode for pothole apps.
       ;; Having a grammar check problem...
       (ASSERT (putenv "REGDEBUGMODE" "OFF"))

       (fpf "ws: Running pothole4 app:                     ~a\n"
	    (code->msg! (system/exit-code 
			 (format "echo 3 | ws pothole4.ws -exit-error &> ~a/ws_pothole4.log" test-directory))))
       (fpf "ws.early: Running pothole4 app:               ~a\n"
	    (code->msg! (system/exit-code 
              (format "echo 3 | ws.early pothole4.ws -exit-error &> ~a/wsearly_pothole4.log" test-directory))))

;; TEMP FIXME DISABLED
#|
       (fpf "wscaml: Compiling pothole4 app:               ~a\n"
	    (code->msg! (system/exit-code 
             (format "wscaml pothole4.ws -exit-error &> ~a/wscaml_pothole_build.log" test-directory))))
       (fpf "wscaml: Running pothole4:                     ~a\n"
	    (code->msg! (system/exit-code 
             (format "./query.caml.exe &> ~a/wscaml_pothole4_run.log" test-directory))))
|#

       (fpf "wsmlton: Compiling pothole4 app:              ~a\n"
	    (code->msg! (system/exit-code 
             (format "wsmlton pothole4.ws -exit-error &> ~a/wsmlton_pothole4_build.log" test-directory))))
       (fpf "wsmlton: Running pothole4 app:                ~a\n"
	    (code->msg! (system/exit-code 
             (format "./query.mlton.exe -n 3 &> ~a/wsmlton_pothole4_run.log" test-directory))))

       (ASSERT (putenv "REGDEBUGMODE" "ON"))

       (current-directory test-directory))

;;================================================================================

(fpf "\n\n\nTotal time spent testing: ~a minutes\n" 
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
