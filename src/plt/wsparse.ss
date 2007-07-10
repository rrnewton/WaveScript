#! /bin/bash
#|
exec mzscheme -mqt "$0" ${1+"$@"}
|#

;; Just a little PLT script to parse the input file and write result to stdout.

(module wsparse mzscheme 

(require "regiment_parser.ss")
(require (lib "pretty.ss"))

#;
(if (not (= 1 (vector-length (current-command-line-arguments))))
    (error 'wsparse "Takes only one argument: name of file to parse. Given: ~s" 
	   (current-command-line-arguments)))

;(define filename (format "~a" (read)))
(define filename (vector-ref (current-command-line-arguments) 0))

;; Now just write it to stdout:
;(display (ws-postprocess (reg-parse-file filename)))(newline)
;(pretty-print (ws-postprocess (reg-parse-file filename)))

(print-graph #t)
(when (member "--nopos" (vector->list (current-command-line-arguments)))
  (source-position-tracking #f))

(if (member "--nopretty" (vector->list (current-command-line-arguments)))
    (write (ws-parse-file filename))
    (pretty-print (ws-parse-file filename)))

(exit 0)
)

;(require wsparse)
