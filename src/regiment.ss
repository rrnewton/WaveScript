#!r6rs

;;#! /usr/bin/env scheme-script

;; This is an R6RS "top level program" that invokes the WS compiler.

;; First the import section.  Load everything:
(import (rnrs) 	
	(main_r6rs)
	(main)
	;; Some shorthands for the interactive REPL:
	(ws shortcuts)
	)

;; Next, we do some configuration stuff.

;; This should be one of:
;;  "unknown"
;;  "source"
;;  "compiled .so"
;;  "compiled .boot"
;;  "saved heap"
(unless (top-level-bound? 'regiment-origin)
  (define-top-level-value 'regiment-origin "unknown")) ;; This tracks how the system was loaded.

;;================================================================================
;(ws "/home/newton/demos/demo1c_timer.ws")
;;================================================================================

(if (< (length (command-line)) 2)
    (error 'regiment.ss "script must take at least one argument.  First argument should be working directory."))

;; By convention the first argument to this script is the directory
;; from which it was originally invoked.  Switch to this directory.

(define invoke-dir (cadr (command-line)))
;(unless (file-exists? invoke-dir) (error 'regiment.ss "First argument should be working directory.  Dir does not exist: " invoke-dir))
(current-directory invoke-dir)

;; Now invoke the main program based on the command line arguments.

;(if (null? (command-line)) (error 'regiment.ss "script must take at least one argument.  First argument should be working directory."))

;(printf "COMMAND LINE ~s\n" (command-line))
(apply main (cddr (command-line)))
