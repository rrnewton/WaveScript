#! /bin/sh
#| 
exec chez --script "$0" `pwd` ${1+"$@"};
|#

;;;; .title Analyze Dead-simple fire monitoring vs. groundtruth.

;;;; Usage: ./<script-name> <outputname> <gradient-mode> <retry-delay> <max-retries>
;;;;   <br><br>

;;;; This script uses the deadsimple_alarm.rs query to read *all* the
;;;; temperature readings in the network (over a threshold).  The
;;;; script then takes

;----------------------------------------
;; Load the compiler.
(load (string-append (getenv "REGIMENTD") "/src/compiler_chez.ss"))

;; Run the simulation.
(define results
  (load-regiment (++ REGIMENTD "/demos/firelightning/deadsimple_alarm.rs")
		 '[simulation-logger "./deadsimple.log.gz"]
		 ;;'[sim-timeout 500000]
		 ))

