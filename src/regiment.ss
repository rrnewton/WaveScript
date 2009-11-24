#!r6rs

;;#! /usr/bin/env scheme-script

;; This is an R6RS "top level program" that invokes the WS compiler.

;; First the import section.  Load everything:
(import (except (rnrs (6)) error) 	
	(main_r6rs)
	(main)
	;; Some shorthands for the interactive REPL:
	(ws shortcuts)
;	(only (scheme) new-cafe)
	)

;(new-cafe)
;(repl)

;; Next, we do some configuration stuff.

;; This should be one of:
;;  "unknown"
;;  "source"
;;  "compiled .so"
;;  "compiled .boot"
;;  "saved heap"
(unless (top-level-bound? 'regiment-origin)
  (define-top-level-value 'regiment-origin "unknown")) ;; This tracks how the system was loaded.

;; [2009.03.12] Temp: for r6rs chez:
#;
(IFCHEZ (eval-when (eval load compile) 
	  (printf "SETTING OPTIMIZE LEVEL TO 2!\n")
	  (optimize-level 2)) (void))

;;================================================================================
;(ws "/home/newton/demos/demo1c_timer.ws")
;;================================================================================

;(printf "COMMAND LINE ~s\n" (command-line))

(define invoke-dir 
  (if (< (length (command-line)) 2)
      (begin 
	(warning 'regiment.ss "script should take at least one argument.  (First argument should be working directory.)")
	(printf "\n **** DEFAULTING TO CURRENT DIRECTORY: ~s\n\n" (current-directory))
	(current-directory))
      (cadr (command-line))))

;; By convention the first argument to this script is the directory
;; from which it was originally invoked.  Switch to this directory.


;(unless (file-exists? invoke-dir) (error 'regiment.ss "First argument should be working directory.  Dir does not exist: " invoke-dir))
(current-directory invoke-dir)

;; Now invoke the main program based on the command line arguments.

;(if (null? (command-line)) (error 'regiment.ss "script must take at least one argument.  First argument should be working directory."))

#;
;; Trying to set the svn rev when the code is *compiled*:
;; Set to #f if we can't get it.
;; (This is duplicated in legacy_main_chez.ss)
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

;(bind-svn-revision)

(apply main (if (< (length (command-line)) 2) '()
		(cddr (command-line))))
