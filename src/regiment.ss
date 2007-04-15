;#! /usr/bin/scheme --script
;#!/usr/bin/chez --script

;;;; Regiment.ss
;;;; This file is a script that drives the regiment compiler/simulator.
;;;; It loads the Regiment system from source or from a compiled .so file.

(define start-dir (current-directory))

;; Capture these because they are overwritten.
(define orig-scheme-start (scheme-start))
(define orig-scheme-script (scheme-script))

(define regiment-origin "unknown") ;; This tracks how the system was loaded.

(define stderr
  (let ((buffer-size 1))
    (let ((p (make-output-port 2 (make-string buffer-size))))
      (set-port-output-size! p (- buffer-size 1))
      p)))

;; If we are loading the current script from source, then we need to
;; somehow load the regiment system.  If, however, this script is
;; compiled, we take that as an indication that the system will be
;; loaded by other means.
(eval-when (eval) ; but not load/compile!
  (parameterize ([current-directory (string-append (getenv "REGIMENTD") "/src/")])
    (define UNSAFEOPTMODE (equal? (getenv "REGOPTLVL")  "3"))
    (define DEBUGINVOCATION (equal? (getenv "REGDEBUGMODE")  "ON"))

    (define sofile
      (cond
       [UNSAFEOPTMODE (format "./build/~a/main_chez_OPT.so" (machine-type))]
       [DEBUGINVOCATION (format "./build/~a/main_chez_DBG.so" (machine-type))]
       [else (format "./build/~a/main_chez.so" (machine-type))]))

    (define (yucky-hack)
      ;; [2007.01.29] I REALLY SHOULDN'T HAVE TO DO THIS.
      ;; (But currently I can't get the system to work when loaded from .so)
      ;; SHOULD ONLY DO THIS WHEN WE'RE LOADING FROM .SO:
      (eval '(import scheme))
      (load (string-append (getenv "REGIMENTD") "/src/chez/regmodule.ss"))
      (eval '(import reg:module)))

    (if (top-level-bound? 'run-ws-compiler)
	;; [2007.04.15] If it's already been loaded from a boot file...
	(yucky-hack)
	;(void)
	#;
	(begin
	  
	  )
	(if (file-exists? sofile)
	    
	    ;; If the compiled version is there, use that:
	    (begin 
	      (set! regiment-origin "compiled .so")
	      (load sofile)
	      (yucky-hack)
	      )
	    (begin (fprintf stderr "Loading Regiment from source...\n")
		   (set! regiment-origin "source")
		   (load "./main_chez.ss")))
	)))

; =======================================================================

(define (print-help)
  (printf "Regiment system, version ~s (rev ~s) (loaded from ~a)\n" 
	  (regiment-version) 
	  svn-revision ;(svn-revision)
	  regiment-origin)
  (printf "Usage: regiment command [options] ~n")
  (printf "~n")
  (printf "Commands: ~n")
  (printf "  help          prints this message~n")
  (printf "  compile  (c)  compile Regiment source (.rs) to token machines~n")
  (printf "  simulate (s)  simulate a token machine or simulator file~n")
  (printf "  interact (i)  start up Scheme REPL with Regiment loaded~n")
  (printf "  test     (t)  run all regiment tests~n")
  (printf "  log      (l)  simulator trace manipulation mode~n")
  (printf "  wsint    (wsint)  WaveScript evaluator mode~n")
  (printf "  wscomp   (wscomp) WaveScript compiler mode~n")
  (printf "  wscaml   (wscaml) WaveScript compiler Caml backend~n")
  (printf "~n")
  (printf "General Options:  ~n")
  (printf "  -v   verbose compilation/simulation, includes warnings~n")
  (printf "  -q   suppress banners and other nonessential output~n")
  (printf "~n")
  (printf "Compiler Options: ~n")
  (printf "  -d2  use new compiler: deglobalize2 ~n")
  (printf "  -lt  type check only, print typed program to stdout           ~n")
  (printf "  -ltt type check only, print *only* top level types to stdout  ~n")
  (printf "  -l0  stop compilation just before deglobalize          (.sexp)~n")
  (printf "  -l1  compile barely to tokens (just after deglobalize) (.tm0)~n")
  (printf "  -l2  compile to just tokens (maximally lowered)        (.tm)~n")
  (printf "  -l4  output generated simulator-alpha code             (.sim.alpha)~n")
  (printf "  -l5  output generated NesC/Tossim code                 (.sim.nesc) ~n")
  (printf "  -debug       print extra info, inspect errors ~n")
  (printf "~n")
  (printf "Simulator Options: ~n")
  (printf "  -timeout <n>  timeout after n clock ticks\n")
  (printf "  -plot         when simulation finishes, gnuplot output\n")
  (printf "  -repl         when simulation finishes, run interactive REPL\n")
  (printf "~n")
  (printf "Interactive Options: ~n")
  (printf "  --script  <file>    run a scheme file as a script~n")
  (printf "  -exit-error        exit process w/ nonzero error code on a scheme error~n")
  (printf "~n")
  (printf "Log-manipulation Options: ~n")
  (printf "  -print    <file>    print any log-file in human readable format~n")
  (printf "  -examine  <file>    describe the chunking format of an existing logfile~n")
  (printf "  -reencode <f1> <f2> reencode a logfile in a compressed but fast-loading way~n")
  (printf "  -vw <worldfile>     (not really a log) if gui is loaded, view saved world~n")
  (printf "~n")
  (printf "WSINT options: ~n")
  (printf "  -dump <file>  don't go into stream browser, dump output stream to file~n")
  (printf "WSCOMP options: ~n")
  (printf "  -c0           only run the WaveScript compiler, stop at C++~n")
  )

(define (print-types-and-exit prog . opts)
  (define verbose? (memq 'verbose opts))
  (printf ";; Regiment program with infered types: \n")
  ;; Run just the verify regiment pass, it will associate types:
  (parameterize ([print-vector-length #f])
  (match (verify-regiment `(lang '(program ,prog)))
    [(,lang '(program ,p ,t))
     (match p
       [(letrec ([,id* ,t* ,rhs*] ...) ,bod)
	(for-each (lambda (id t rhs)
		    (if verbose?
			(begin (pretty-print `(define ,id : ,t ,rhs))(newline))
			(printf "~a : ~a\n" (pad-width 30 id) t)))
	  id* t* rhs*)
	(if verbose? (pretty-print bod))]
       [,p (pretty-print p)])
     ;(printf "\n;; Regiment program return type: ~a\n" t)
     (printf "  : ~a\n" t)
     (exit 0)]
    [,other (error 'print-types-and-exit "bad output from verify-regiment: ~s" other)])))

(define main 
  (lambda args    
    (define makesimcode #f)
    (define outfile #f)
    (define plot #f)
    (define simrepl #f)
;    (disp "Main called w ARGS: " args)
    (when (null? args) (print-help) (regiment-exit 0))
    
;    (printf "regimentc: compile regiment programs!~n")
    (let ([opts '()] ;; This is a bit sketchy.  The same flags are sent to run-compiler and run-simulator-alpha.
	  )
      ;; Loop goes through the arguments, processing them accordingly:
      ;; Anything not matched by this is presumed to be a file name.
      (letrec ([loop 
	      (lambda (args)
		(match args
		    [() '()]

		    [(-v ,rest ...) 
		     (set! opts (cons 'verbose opts))
		     (regiment-verbose #t)
		     (loop rest)
		     ]
		    [(.h ,rest ...) (print-help) (regiment-exit 0)]

		    [(-plot ,rest ...) (set! plot #t) (loop rest)]
		    [(-repl ,rest ...) (set! simrepl #t) (loop rest)]

		    [(-d2 ,rest ...) (set! opts (cons 'deglobalize2 opts)) (loop rest)]

		    [(-lt ,rest ...) (set! opts (cons 'type-only-verbose opts)) (loop rest)]
		    [(-ltt ,rest ...) (set! opts (cons 'type-only opts)) (loop rest)]

		    [(-l0 ,rest ...) (set! opts (cons 'almost-tokens opts))   (loop rest)]
		    [(-l1 ,rest ...) (set! opts (cons 'barely-tokens opts))   (loop rest)]
		    [(-l2 ,rest ...) (set! opts (cons 'full-tokens opts))  (loop rest)]

		    [(-l4 ,rest ...) 
		     (set! makesimcode #t)
		     (set! opts (cons 'to-simcode opts)) (loop rest)]

		    [(-l5 ,rest ...)
		     ;; [2006.11.11] Not handled right now:
		     (set! opts (cons 'to-nesc opts))
		     (pass-list 
		      (snoc emit-nesc (snoc flatten-tokmac
			     (remq flatten-tokmac (remq emit-nesc (pass-list))))))
		     (loop rest)]

		    [(-exit-error ,rest ...)
		     (printf "SETTING BATCH MODE\n")
		     (define-top-level-value 'REGIMENT-BATCH-MODE #t)
		     (loop rest)]
		    
;		    [(--script ,rest ...) (set! opts (cons 'script opts))  (loop rest)]
		    [(-debug ,rest ...)		     
		     (define-top-level-value 'REGIMENT-BATCH-MODE #f)
		     (regiment-emit-debug #t)
		     (loop rest)]

		    [(-quiet ,rest ...)
		     (regiment-quiet #t)
		     (loop rest)]

		    [(-dump ,file ,rest ...)
		     (set! outfile file)
		     (loop rest)]

		    [(-c0 ,rest ...) (set! opts (cons 'stop-at-c++ opts)) (loop rest)]

		    [(-timeout ,n ,rest ...)
		     (let ((n (read (open-input-string (format "~a" n)))))
		     (set! opts (cons 'timeout (cons n opts))))]

		    ;; otherwise a file to compile that we add to the list
		    [(,fn ,rest ...)
		     ;(regiment-compile-file fn)
		     (cons (symbol->string fn) (loop rest))]

		    [,_ (error "Bad command line arguments to regimentc: ~a~n" args)]
		    ))])

        ;; I keep disjoint options for the modes so I use the same option-processor for all modes (loop)
	(let ([symargs (map string->symbol args)])
	  ;; [2007.01.29] Killing this:
	  ;(unless (null? (cdr symargs)) (printf "Processing options: ~s\n" (cdr symargs)))
	  (let ([mode (car symargs)] [filenames (loop (cdr symargs))])
	;; AFTER, those options are processed we switch on the mode flag.
	(case mode
	  ;; Unit Test mode:
	  [(t test)
	   (define-top-level-value 'REGIMENT-BATCH-MODE #t)
	   (test-units)
	   ;(test-everything)
	   ]

	  ;; Compile mode:
	  [(c compile)
	   ;(define-top-level-value 'REGIMENT-BATCH-MODE #t)
	   (if (null? filenames)
	       (begin
		 (printf "No input file.  Type top-level Regiment expression.~n")
		 (printf "> ")(flush-output-port)
		 (let* ([expr (read)])
		   (printf "~n Using default output file: out.tm...~n")		
		   (apply run-compiler expr "out.tm" opts)))
	       (begin 
		 (if (> (length filenames) 1)
		     (error 'regiment_command_line_compiler 
			    "can't handle more than one filename at a time currently: ~s" filenames))
		 ;; Otherwise we're good to go.
		 (cond 
		  [(memq 'type-only-verbose opts)
		   (print-types-and-exit 
		    (apply regiment-compile-file (car filenames) 'to-typed opts)
		    'verbose)]
		  [(memq 'type-only opts)
		   (print-types-and-exit 
		    (apply regiment-compile-file (car filenames) 'to-typed opts))]
		  [else 
		   (apply regiment-compile-file (car filenames) 'write-file opts)])
		 ))]

	  ;; Simulation mode (also may invoke compiler):
	  [(s simulate)

	   (parameterize ([simulation-logger-level 1]
			  );; Only get send/receive, groundtruth, and newworld msgs.
	   (let ((fn (if (null? filenames)
			 "out.sim"
			 (car filenames))))
	     
	     (define-top-level-value 'go-sim
	       (lambda ()
		 (printf "Running simulation from file: ~a\n" fn)
		 (let ((result
			(apply load-regiment fn opts)
#;
			;; Be careful to watch for parameterization:	     
			(mvlet (([prog params] (read-regiment-source-file fn)))
			  (with-evaled-params params
					      (lambda () 
						(apply run-simulator-alpha prog 
						       'srand (current-time)
						       opts))))))
		   ;; Print simalpha stats:
		   (print-stats)
		   (if plot (gnuplot result))
		   (if simrepl (new-cafe))
		   result)))

	     (IF_GRAPHICS 
	      ;; This starts swl then evals the expression.
	      (bounce-to-swl '(go-sim))	      
	      (begin (printf "WOOT\n")
		     (go-sim)
		     ))
	     ))]

	  ;; Interactive mode.  A Scheme REPL.
	  ;; [2006.02.21] This is a better way of exposing the normal scheme startup behavior:
	  [(i interact)
	   (printf "Exposing Regiment through interactive read-eval-print loop:\n")
	   ;; [2006.02.28] We want to reset global bindings before loading any more source:
	   ;(eval '(import scheme))
	   ;; Can't trust new code to not mutate primitive names:
	   (optimize-level 1)

	   (cond
	    [(null? filenames) (call-with-values new-cafe (lambda ls (apply exit ls)))]
	    ;; --script must be the first argument after "regiment i"
	    ;;
	    ;; This won't occur, chez grabs the --script parameter
	    ;; directly.  Code should go in the scheme-script parameter.
	    ;;
	    ;; Note, if we're doing it this way we pass the raw
	    ;; arguments, not those processed by "loop" above.
	    [(equal? (cadr args) "--script")
	     ;(printf "Using Regiment to invoke script: ~a\n" args)
	     ;(error 'regiment.ss "this shouldn't happen.")

	     ;; --script implies --exit-error:
	     (loop '(-exit-error))
	     (apply orig-scheme-script (cddr args))]
	    [else 
	     ;(inspect (list->vector args))
	     (apply orig-scheme-start (cdr args))])]

	  ;; Printing SExp log files.
	  [(l log)
	   (match (map string->symbol (cdr args))
	     [() (if (file-exists? "__temp.log") (reg:printlog "__temp.log")
		     (if (file-exists? "__temp.log.gz") (reg:printlog "__temp.log.gz")
			 (error 'regiment:log:print "no log file supplied or found")))]
	     [(-print ,file) 
	      (let loop ([s (reg:read-log (symbol->string file) 'stream)])
		(unless (stream-empty? s) 
		  (printf "~a\n" (stream-car s))
		  (loop (stream-cdr s))))]
	     [(-print ,_ ...) (error 'regiment:log:print "only can print exactly one logfile at a time: ~a" args)]
	     [(-reencode ,in ,out)
	      ;; Do not replace output file if it's there:
	      (let ((out (open-output-file (symbol->string out) '(compressed)))
		    (block-size 1000)  ;; 1000 lines of log chunked at a time.
		    (count 0))
		(progress-dots 
		 (lambda ()
		   (let loop ((in (reg:read-log (symbol->string in) 'stream))
			      (n block-size) (acc '()))
		     (cond
		      [(stream-empty? in) 
		       (fasl-write (list->vector (reverse! acc)) out)
		       (close-output-port out)]
		      [(fxzero? n)
		       (fasl-write (list->vector (reverse! acc)) out)
		       (loop in block-size '())]
		      [else 
		       (set! count (add1 count))
		       (loop (stream-cdr in) (fx- n 1) (cons (stream-car in) acc))]))
		   )))]
	     [(-reencode ,_ ...)
	      (error 'regiment:log:reencode 
		     "bad arguments for log reencoding, expects input and output file: ~a" args)]
	     [(-examine ,file)
	      (newline)
	      (let* ([file (symbol->string file)]
		     [in (lambda ()
			   (if (equal? (extract-file-extension file) "gz")
			       (open-input-file file 'compressed)
			       (open-input-file file)))]
		     [first (read (in))])
		;; First classify contents.
		(cond
		 [(vector? first)
		  (printf "Log file batched into vectors, first vector is size: ~a\n" (vector-length first))]
		 [(list? first)
		  (printf "Log file contains raw, unbatched log entries.\n")]
		 [else (printf "WARNING: Log file contains the following unrecognized object: ~a" first)])
		;; Next classify fasl/plaintext.
		(let* ([in (in)]
		       [c1 (read-char in)]
		       [c2 (read-char in)])
		  ;; TODO: THIS DOESN'T WORK!!! COULD HAVE COMMENTS AT THE BEGINNING OF THE FILE.
		  (if (equal? "#@" (list->string (list c1 c2)))
		      (printf "First expression in file is FASL encoded.  (Binary fast-loading format.)\n")
		      (printf "First characters in file appears to be non-FASL plaintext (might be wrong) ~a.\n"
			      (if (equal? (extract-file-extension file) "gz")
				  " (except for being gzipped)" ""))
		      )))]
	     [(-examine ,_ ...)
	      (error 'regiment:log:examine "-examine expects exactly one file name argument: ~a" args)]

	     [(-vw ,worldfile)
	      (let ([file (symbol->string worldfile)])
		(IF_GRAPHICS 			     
		 (begin 
		   (unless (file-exists? file)
		     (error 'regiment:log:view-world "this worldfile doesn't exist: ~s" file)
		     (exit -1))
		   ;; Read only the first entry, set up as global world.
		   (simalpha-current-simworld (read (open-input-file file)))
		   (animate-world! (simalpha-current-simworld))
		   (printf "Animated world from world file ~s.\n" file)
		   ;(clean-simalpha-counters!)
		   ;(inspect (simalpha-current-simworld))
		   (bounce-to-swl '(begin 
				     ;(clean-simworld! (simalpha-current-simworld))
				     (init-graphics)
				     (simalpha-draw-world (simalpha-current-simworld))
					;(printf "HMM: ~s\n" (simalpha-current-simworld))
				     ;; Run a stupidly simple query just to initialize things. 
					;(run-simulator-alpha (run-compiler '3) 'use-stale-world)
				     )
				  )
		   )
		 (begin (printf "view-world (-vw) mode doesn't really make sense without GUI loaded!")
			(exit -1)))
		)]

	     [,other (warning 'regiment:log "unsupported options or missing arguments: ~a" other)
		     (exit -1)]
	     )]

	  ;; Interpreting (preparsed) wavescript code.
	  [(wsint)
	   (let ()
	   (define prog (match filenames
			  ;; If there's no file given read from stdin
			  [() (console-input-port)]
			  [(,fn ,rest ...) 
			   ;; If it's a ws file we need to parse the file:
			   (if (equal? "ws" (extract-file-extension fn))
			       (or (read-wavescript-source-file fn)
				   (error 'wsint "couldn't parse file: ~s" fn))
			       ;; Otherwise let's assume 
			       (open-input-file fn))]

			  [,else (error 'regiment:wsint "should take one file name as input, given: ~a" else)]))
	   (let ([return (wsint prog)])
	     (import streams)
	     ;(import imperative_streams)
	     (if (stream? return)
		 (if outfile
		     (begin
		       (printf "Dumping output to file: ~s\n" outfile)
		       (stream-dump return outfile))
		     ;; Otherwise, browse it interactively:
		     (parameterize ([print-vector-length #t])
		       (browse-stream return)))		 
		 (printf "\nWS query returned a non-stream value:\n  ~s\n" return))))]
	  
	  [(wscomp)
	   ;(define-top-level-value 'REGIMENT-BATCH-MODE #t)
	   (let ()
	     (define port (match filenames
			  ;; If there's no file given read from stdout
			  [() (console-input-port)]
			  [(,fn ,rest ...) 
			   (if (equal? "ws" (extract-file-extension fn))
			       (or (read-wavescript-source-file fn)
				   (error 'wsint "couldn't parse file: ~s" fn))
			       ;; Otherwise let's assume 
			       (open-input-file fn))]
			  ;[,else (error 'regiment:wscomp "should take one file name as input, given: ~a" else)]
			  ))
	     (apply wscomp port opts)
	   )]

	  [(wscaml)
	   ;(define-top-level-value 'REGIMENT-BATCH-MODE #t)
	   (let ()
	     (define exp (match filenames
			  ;; If there's no file given read from stdout
			  [() (console-input-port)]
			  [(,fn ,rest ...) 
			   (if (equal? "ws" (extract-file-extension fn))
			       (or (read-wavescript-source-file fn)
				   (error 'wsint "couldn't parse file: ~s" fn))
			       ;; Otherwise let's assume 
			       (open-input-file fn))]
			  ;[,else (error 'regiment:wscomp "should take one file name as input, given: ~a" else)]
			  ))
	     (apply wscaml exp opts)
	   )]

	  
	  )))))))
  
(define (reg:printlog file)
  (let ((stream (reg:read-log file 'stream)))
    (let loop ((s (reg:read-log file 'stream)))
      (unless (null? s)
	(display (log-line->human-readable 0 (stream-car s) ()))
	(loop (stream-cdr s))))))

(IF_GRAPHICS
 ;; This starts swl and evaluates the expression afterwards.
 (define (bounce-to-swl exp)
   (printf "Going into SWL system.\n")
   (with-output-to-file "/tmp/swl_tmp_loading.ss"
     (lambda () (pretty-print exp)) 'replace)
   (printf "Temporary file written to contain post-swl-load instructions.\n")
   (orig-scheme-start "/tmp/swl_tmp_loading.ss")
   ))

(define (regiment-exit code)
  ;; In case we're building a heap, we set this before we exit.
  ;(disp "SETTING HEAP: " regiment-origin (top-level-value 'regiment-origin))
  (set! regiment-origin "saved heap")
  ;(disp "HEAP SET: " regiment-origin (top-level-value 'regiment-origin))
  (exit code))

; =======================================================================
(suppress-greeting #t)
;; If we're running from heap, we need to set the scheme-start:
;; Set the invocation directory to whatever the current directory is:
(scheme-start (lambda args (set! start-dir (cd)) 
		      ;(random-seed (current-time))
		      ;; [2007.04.11] This is unsatisfactory:
		      (random-seed (* (real-time) (cpu-time)))
		      ;; We also need to make sure that we reset the REGIMENTD parameter:
		      ;; It might not be the same on the system loading the heap as the one compiling it!
		      (REGIMENTD (default-regimentd))
		      (apply main args)))

;; If we're running from the heap and we're running in script mode,
;; that's because I'm using "regiment i" as the interpreter for a
;; script.
(scheme-script (lambda (fn . args)
		 (set! start-dir (current-directory))
		 ;(printf "SCHEME SCRIPT\n")
		 (unless (and (>= (length args) 1)
			      (member (car args) '("i" "interact")))
		   (error 'regiment 
			  "bad script-mode arguments to regiment, --script should only be used with 'interact' mode: ~a\n"
			  args))
		 ;(inspect args)
		 ;(inspect (command-line-arguments))
		 ;(apply main `(,(cadr args) "--script" ,@(cddr args)))
		 (command-line (cons fn (cdr args)))
		 (command-line-arguments (cdr args))
		 (load fn) ;; We don't call main, we already know it's interact mode.		 
		 ))

;; [2007.04.11] This is unsatisfactory:
(random-seed (* (real-time) (cpu-time)))
;(random-seed (current-time))

;; If we're running from source, we invoke main right here:
;; Shave off the first argument, it just carries the working directory:
;(apply main (cdr (command-line-arguments)))
(if (null? (command-line-arguments))
    (error 'regiment.ss "script must take at least one argument.  First argument should be working directory."))
(apply main (cdr (command-line-arguments)))
