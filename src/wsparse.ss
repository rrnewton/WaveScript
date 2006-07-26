
;; Just a little PLT script to parse the input file and write result to stdout.

(module wsparse mzscheme 

(require "reg_grammar.ss")
(require (lib "pretty.ss"))

(if (not (= 1 (vector-length (current-command-line-arguments))))
    (error 'wsparse "Takes only one argument: name of file to parse. Given: ~s" 
	   (current-command-line-arguments)))

;(define filename (format "~a" (read)))
(define filename (vector-ref (current-command-line-arguments) 0))

;; Now just write it to stdout:
;(display (ws-postprocess (reg-parse-file filename)))(newline)
(pretty-print (ws-postprocess (reg-parse-file filename)))
)
