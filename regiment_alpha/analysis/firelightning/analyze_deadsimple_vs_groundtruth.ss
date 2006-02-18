#! /bin/sh
#| 
exec scheme --script "$0" `pwd` ${1+"$@"};
|#

;;;; .title Analyze Dead-simple fire monitoring vs. groundtruth.

;;;; Usage: ./<script-name> <outputname> <gradient-mode> <retry-delay> <max-retries>
;;;;   <br><br>

;;;; This script uses the deadsimple_alarm.rs query to read *all* the
;;;; temperature readings in the network (over a threshold).  The
;;;; script then takes


