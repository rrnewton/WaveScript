;#! /usr/bin/scheme --script
;#!/usr/bin/chez --script

;;;; LEGACY VERSION:
;;;; Chez doesn't support R6Rs yet.  This is part of a hack to keep using Chez.

;;;; Regiment.ss
;;;; This file is a script that drives the wavescript compiler/simulator.
;;;; It loads the Regiment system from source or from a compiled .so file.

;;;; A lot of this setup and config stuff is unpleasantly hackish.
;;;; Some of it is designed to work around quirkiness with using Chez
;;;; from source, compiled source, or saved heap.

(define start-dir (current-directory))

;; Capture these because they are overwritten.
(define orig-scheme-start (scheme-start))
(define orig-scheme-script (scheme-script))

(unless (top-level-bound? 'wavescript-origin)
  (define-top-level-value 'wavescript-origin "unknown")) ;; This tracks how the system was loaded.
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
;; somehow load the wavescript system.  If, however, this script is
;; compiled, we take that as an indication that the system will be
;; loaded by other means.
(eval-when (eval) ; but not load/compile!
  (parameterize ([current-directory (string-append (getenv "WAVESCRIPTD") "/src/")])
    (define UNSAFEOPTMODE (equal? (getenv "REGOPTLVL")  "3"))
    (define DEBUGINVOCATION (equal? (getenv "REGDEBUGMODE")  "ON"))

    (define sofile
      (cond
       [UNSAFEOPTMODE (format "./build/~a/legacy_main_chez_OPT.so" (machine-type))]
       [DEBUGINVOCATION (format "./build/~a/legacy_main_chez_DBG.so" (machine-type))]
       [else (format "./build/~a/legacy_main_chez.so" (machine-type))]))

    ;; HACK HACK HACK: DEPENDS ON THIS SPECIFIC SYMBOL NAME:
    (if (top-level-bound? 'default-unit-tester)
	;; [2007.04.15] If it's already been loaded from a boot file...
	(begin (void) 
	  ;(yucky-hack)
	  ;; This is kind of a hack too:
	  ;(set! wavescript-origin "compiled .boot")
	  )
	(if (file-exists? sofile)
	    
	    ;; If the compiled version is there, use that:
	    (begin 
	      (set! wavescript-origin "compiled .so")
	      (load sofile)
	      ;(yucky-hack)
	      )
	    (begin (fprintf stderr "Loading Regiment from source...\n")
		   (set! wavescript-origin "source")
		   (load "./legacy_main_chez.ss")))
	)))


; =======================================================================


; =======================================================================
(suppress-greeting #t)
;; If we're running from heap, we need to set the scheme-start:
;; Set the invocation directory to whatever the current directory is:
(scheme-start (lambda args (set! start-dir (cd)) 
		      ;(random-seed (current-time))
		      ;; [2007.04.11] This is unsatisfactory:
		      (random-seed (* (real-time) (cpu-time)))
		      ;; We also need to make sure that we reset the WAVESCRIPTD parameter:
		      ;; It might not be the same on the system loading the heap as the one compiling it!
		      (WAVESCRIPTD (default-wavescriptd))

		      ;; Let's be nice and restore the command-line-arguments:
		      ;(inspect (vector args (command-line-arguments)))
		      ;(command-line-arguments (cdr args))

		      (apply main args)))

;; If we're running from the heap and we're running in script mode,
;; that's because I'm using "wavescript i" as the interpreter for a
;; script.
(scheme-script (lambda (fn . args)
		 (set! start-dir (current-directory))
		 ;(printf "SCHEME SCRIPT\n")
		 (unless (and (>= (length args) 1)
			      (member (car args) '("i" "interact")))
		   (error 'wavescript 
			  "bad script-mode arguments to wavescript, --script should only be used with 'interact' mode: ~a\n"
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
    (error 'wavescript.ss "script must take at least one argument.  First argument should be working directory."))
(apply main (cdr (command-line-arguments)))
