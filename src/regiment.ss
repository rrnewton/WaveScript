;#! /usr/bin/scheme --script
;#!/usr/bin/chez --script

;;;; Regiment.ss
;;;; This file is a script that drives the regiment compiler/simulator.
;;;; It loads the Regiment system from source or from a compiled .so file.

;;;; A lot of this setup and config stuff is unpleasantly hackish.
;;;; Some of it is designed to work around quirkiness with using Chez
;;;; from source, compiled source, or saved heap.

(define start-dir (current-directory))

;; Capture these because they are overwritten.
(define orig-scheme-start (scheme-start))
(define orig-scheme-script (scheme-script))

(unless (top-level-bound? 'regiment-origin)
  (define-top-level-value 'regiment-origin "unknown")) ;; This tracks how the system was loaded.
;; This should be one of:
;;  "unknown"
;;  "source"
;;  "compiled .so"
;;  "compiled .boot"
;;  "saved heap"


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

    ;; HACK HACK HACK: DEPENDS ON THIS SPECIFIC SYMBOL NAME:
    (if (top-level-bound? 'default-unit-tester)
	;; [2007.04.15] If it's already been loaded from a boot file...
	(begin 
	  (yucky-hack)
	  ;; This is kind of a hack too:
	  ;(set! regiment-origin "compiled .boot")
	  )
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
