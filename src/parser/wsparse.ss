#! /bin/bash
#|
exec mzscheme -qu "$0" -- ${1+"$@"}
|#
;;exec mzscheme -mqt "$0" ${1+"$@"}

;; Just a little PLT script to parse the input file and write result to stdout.

(module wsparse mzscheme 

;(provide main)
(require "regiment_parser.ss")
(require (lib "pretty.ss"))

(define allargs (vector->list (current-command-line-arguments)))

(print-graph #t)
(print-vector-length #f)

(when (member "--nopos" allargs) (source-position-tracking #f))

;; Don't print-graph in the output:
(when (member "--nograph" allargs) (print-graph #f))

(define pretty? (not (member "--nopretty" allargs)))

(define (main filename)
  (if pretty?      
      (pretty-print (ws-parse-file filename))
      (begin (write (ws-parse-file filename))
	     (newline)
	     ))
  (flush-output (current-output-port)))

;; Here's our script invocation:

;; When run in --persist mode we run in a loop.
(if (member "--persist" allargs)
    (let loop ()
      (define filename (read))
      (unless (equal? filename 'exit)
	(main filename) 
	(loop)))
    ;; Otherwise the (single) file to parse is the first argument.
    (if (null? allargs)
	(error 'wsparse "No filename provided...")
	(main (car allargs))))
(exit 0)

)

;(require wsparse)
