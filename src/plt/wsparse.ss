#! /bin/bash
#|
exec mzscheme -mqt "$0" ${1+"$@"}
|#

;; Just a little PLT script to parse the input file and write result to stdout.

(module wsparse mzscheme 

(require "regiment_parser.ss")
(require (lib "pretty.ss"))

(define allargs (vector->list (current-command-line-arguments)))

(print-graph #t)
(when (member "--nopos" allargs) (source-position-tracking #f))

(define pretty? (not (member "--nopretty" allargs)))

(define (main filename)
  (if pretty?      
      (pretty-print (ws-parse-file filename))
      (write (ws-parse-file filename))))


;; When run in --persist mode we run in a loop.
(if (member "--persist" allargs)
    (let loop ()
      (define filename (read))
      (unless (equal? filename 'exit)
	(main filename) 
	(loop)))
    ;; Otherwise the (single) file to parse is the first argument.
    (main (car allargs)))
(exit 0)

)

;(require wsparse)
