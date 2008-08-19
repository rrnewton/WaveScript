;;;; main_chez.ss
;;;; Loads the regiment compiler in Chez Scheme.

;;;; NOTE: This file uses (include ...) rather than (load ...) 
;;;; This basically inlines all the code in question into this file at compile time.
;;;; Thus, making a compiled copy of this file makes a compiled copy of the whole system.
;;;; HOWEVER: I broke this rule for things that depend on whether or not SWL is loaded.
;;;; TODO FIXME: I can also make this happen at compile time, via macros.

  
; =======================================================================

;; Wipe *all* previous bindings before coming RELOADING the system.
;; [2006.02.28] Without this we get killed by the fact that we redefine "module".
;; This is *only* relevant if repeatedly loading regiment into a single REPL.
(when (top-level-bound? 'REGIMENTD) 
  (printf "WIPING previous bindings before reloading Regiment system.\n")
  (eval '(import scheme)))


;;; Compile-time configuration.
;;;
;;; This runs extra-early, when the file is compiled.         <br>
;;; It sets up the location of the Regiment tree.             <br>
;;;
;;; The Regiment compiler expects case-sensitive treatment of symbols:
;;; (But hopefully it should work either way, as long as its consistent.
(eval-when (compile load eval)

  (define open-string-input-port open-input-string)
  (define (open-string-output-port)
    (let ([prt (open-output-string)])
      (values prt 
	      (lambda () (get-output-string prt)))))
	   
  ;; We load this at compile time to figure out some of our
  ;; compilation options:
  (include "config.ss")

  (case-sensitive #t)
	   
	   ;; This is a bit weird, ultimately the global param
	   ;; REGIMENTD is the thing to look at, but we have some
	   ;; issues with the order of evaluation/loading/binding
	   ;; here, so first we bind this:
	   (define-syntax default-regimentd
	     (syntax-rules ()
	       [(_) (if (getenv "REGIMENTD") (getenv "REGIMENTD") (current-directory))]))

	   (define default-source-directories
	     (#%list 
;				(string-append (default-regimentd) "/src/chez")
;				(string-append (default-regimentd) "/src/generic")
	      (string-append (default-regimentd) "/src")
				;"."  ;; Alas this causes some problems currently...
	      ))
	   (source-directories default-source-directories)
	   
	   ;(optimize-level 0) ;0/1/2/3)

	   ;; This makes our modules work properly in newer versions of Chez:
	   ;; Otherwise we get unorder definitions, which breaks certain Regiment code.
	   ;(if (top-level-bound? 'internal-defines-as-letrec*)
	   ;    (internal-defines-as-letrec* #t))

           ;(define current_interpreter 'chezscheme)           
	   (define which-scheme 'chezscheme)           

	   (define primeval #%eval)
	   #;
	   (define eval 
	     (lambda (x env)
	       (import scheme)
	       (eval x)))
	   
	   (define-syntax eval 
	     (lambda (x)
	       (syntax-case x ()
		 [id (identifier? #'id) #'#%eval]
		 [(_ x)   #'(#%eval x)]
		 [(_ x e) #'(#%eval x)])))

	   (define reg:set_opt_lvl!
	     (lambda ()
	       (case (REGOPTLVL)
	       [(0)
		;; Disable source optimization altogether.
		(optimize-level 0)
		(run-cp0 (lambda (cp0 x) x))]
	       [(2) (optimize-level 2)]
	       [(3)
		(printf "Configuring compiler for full optimize mode. (UNSAFE)\n")
		;; This configuration is for running extended simulation-experiments only:
		;; REMEMBER to also disable IFDEBUG in constants.ss
		(define-top-level-value 'simulator-batch-mode #t)
		(optimize-level 3)
		(compile-compressed #f)
		(generate-inspector-information #f)
		;; Messing with this didn't seem to help performance.
		#;
		(run-cp0
		 (lambda (cp0 x)
		   (parameterize ([cp0-effort-limit 50000]  ;; per-call inline effort, 200 default
				  [cp0-score-limit  500]   ;; per-call expansion allowance, 20 default
				  [cp0-outer-unroll-limit 3]) ;; inline recursive?
		     (let ((x (cp0 x)))
		       (parameterize (
				      (cp0-effort-limit 0)
				      )
			 (cp0 x))))))]
	       [else (error 'regiment-compiler "bad setting for REGOPTLVL: <~s>" (REGOPTLVL))])))

	   (reg:set_opt_lvl!) ;; According to $REGOPTLVL
	   
	   )




;; Trying to set the svn rev when the code is *compiled*:
;; Set to #f if we can't get it.
(define-syntax bind-svn-revision
  (lambda (x)
    (syntax-case x ()
      [(_)
       (let ()
	 (define (system-to-str str)
	   (let* ([pr (process str)]
		  [in (car pr)]
		  [out (cadr pr)]
		  [id  (caddr pr)])
	     (let ((p (open-output-string)))
	       (let loop ((c (read-char in)))
		 (if (eof-object? c)	  
		     (begin 
		       (close-input-port in)
		       (close-output-port out)
		       (get-output-string p))
		     (begin (display c p)
			    (loop (read-char in))))))))
	 (and (not (eq? (machine-type) 'i3nt))
		  (zero? (system "which svn &> /dev/null"))
		  (parameterize ([current-directory (string-append (default-regimentd) "/src")])
		    ;(printf"<<<<<<<<<<<READING SVN REV>>>>>>>>>>>>\n")
		    (let ([rev (read (open-input-string (system-to-str "svn info | grep Revision | sed s/Revision://")))])
		      (if (eof-object? rev)
			  (set! rev (read (open-input-string (system-to-str 
		           "svn info https://svn.csail.mit.edu/wavescript/branches/wavescope | grep Revision | sed s/Revision://")))))
		      (with-syntax ([revis (datum->syntax-object #'_ rev)])
			#'(define-top-level-value 'svn-revision (let ([x 'revis]) (if (number? x) x #f))))
		      )
		    )))])))

(bind-svn-revision)

;======================================================================
;;; Setup stuff.

;(include "./config.ss") ;; Need to do this for windows... haven't got a good system yet.
(include "config.ss")

;; Set some Chez scheme parameters.
(print-graph #t )
(print-gensym #f)
;(print-level 8)
(print-level #f) ;20)
(print-length #f) ;80)
(print-vector-length #t)
;(pretty-maximum-lines 700)
(pretty-maximum-lines #f)
(pretty-line-length 150)
(pretty-standard-indent 0)

;; Storing and then modifying the default break handler.
;; This is just a little hack to let us break "on" a value and then continue.
;; ...<removed>...
;; [2006.08.28] Nixing that hack.  It's much better to just have our own inspect function:
(define (inspect/continue x) (inspect x) x)

;; Debugging, find out where a spurious inspect is!!
(define (debug-inspect x) (warning 'inspect "inspector called! ~s" x) (call/cc #%inspect))


;; Because I run from command line, it helps to enter the inspector after an error.
;; First let's backup the existing error-handler:
(unless (top-level-bound? 'default-error-handler)
  (define-top-level-value 'default-error-handler (error-handler)))

(define inspector-error-handler
 (lambda (who msg . args)
   (call/cc (lambda (k) 	     
	      (parameterize ([error-handler default-error-handler]
			     ;[current-directory (string-append (REGIMENTD) "/src/generic")]
			     )
		(fprintf (console-output-port)
			 "~%Error~a: ~a.~%"
			 (if who (format " in ~s" who) "")
			 (parameterize ([print-level 3] [print-length 6])
			   (apply format msg args)))

		(when (and (top-level-bound? 'REGIMENT-BATCH-MODE)
			   (top-level-value 'REGIMENT-BATCH-MODE))
		  (exit 1)) ;; Should only do this if we're running as a script.

		(fprintf (console-output-port)
			 "Entering debugger to inspect current continuation, type 'q' to exit.\n")
		;; [2008.03.30] Also reset the source-directories so Chez can find the code:
		(source-directories default-source-directories)
		;; Set the current directory so that the inspector can find line numbers:
		(inspect k))))))

(error-handler inspector-error-handler)

;; A Chez hack to see all the source locations in a continuation.
(define (continuation->sourcelocs k)
    (let loop ([depth 0] [ob (inspect/object k)])
      (when (> (ob 'depth) 1)
	(call-with-values (lambda () (ob 'source-path))
	  (lambda args
	    (when (= (length args) 3) ;; Success
	      (apply printf "~a: File: ~a, line ~a char ~a\n" depth args)
	      )))
	(loop (add1 depth) (ob 'link)))))
(define k->files continuation->sourcelocs)


;; This forces the output to standard error in spite the Scheme
;; parameters console-output-port and current-output-port.
(define stderr  
  (let ((buffer-size 1))
    (let ((p (make-output-port 2 (make-string buffer-size))))
      (set-port-output-size! p (- buffer-size 1))
      p)))

;======================================================================
;;; Print a banner message.


;(inspect (eval '(command-line-arguments)))

;(inspect (command-line))
;(inspect (command-line-arguments))

(unless #f ;(inspect (member "-quiet" (command-line-arguments)))
  (fprintf stderr "Regiment: Loading ~a compiler in chezscheme~a...\n"
	 (let ([ws #f]  [reg #f])
	   (IFWAVESCOPE (set! ws #t) (set! reg #t))
	   (cond 
	    [(and ws reg) "ws+reg"]
	    [ws   "ws"]
	    [reg "reg"]))
	 (if (top-level-bound? 'regiment-origin)
	     (format " (from ~a)" regiment-origin)    
	     " (LOADED VIA UNKNOWN METHOD!?)"
	     )))


;======================================================================
;; [2005.11.16] This is a nasty dependency, but I had to write the "sleep" function in C:
;; This tries to dynamically load the shared object the first time the function is called:
(define (sleep t)
  (IF_GRAPHICS
   (thread-sleep t) ;; This uses the SWL thread library.  (Not real OS threads.)
   (begin
    ;(printf "Dynamically loading usleep from shared library...\n")(flush-output-port (current-output-port))
     (parameterize ((current-directory (string-append (REGIMENTD) "/src/")))
       (if (not (file-exists? (format "build/~a/usleep_libc.so" (machine-type))))
	   ;; This even resorts to calling make to build the sleep object!!
	   ;; This is kinda silly, and will cause a bunch of text to dump to stdout/err.
	   (system "(cd C; make usleep_libc)"))
       (if (file-exists? (format "build/~a/usleep_libc.so" (machine-type)))
					;(parameterize ((current-directory (format "chez/usleep/~a" (machine-type))))
	   (load (format "build/~a/usleep_libc.so" (machine-type)))
	   ;; If that build failed and we still don't have it we have to throw an error:
	   (define-top-level-value 'sleep (lambda args (error 'sleep "function not loaded from C shared object file.")))))
     (sleep t))))

;; [2007.03.13] Haven't been using this recently, disabling it due to Windows incompatibility:
;; Only make a system call once to sync us with the outside world.
;(define current-time (seconds-since-1970))


;======================================================================
;;; Begin loading files.  

;; Source directories need to include wavescript/src/ 
(include "chez_aggregated.ss")

;======================================================================

;; [2008.02.18] On the way out lets set this to something sane:
;(source-directories '("."))
(source-directories (cons "." (source-directories)))
