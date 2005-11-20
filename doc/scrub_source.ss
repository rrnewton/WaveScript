#!/usr/bin/env petite --script

;; [2005.11.19] This scrubs my scheme source file of characters and
;; constructs that LAML SchemeDoc can't handle.

;; [2005.11.19] WHOA!  Just had a problem with semicolon followed by
;; double quote for some reason!  That's probably a bug, should report it.


;; This script must only be run from the current directory.
(load "../src/chez/match.ss")

(define in (open-input-file (car (command-line-arguments))))
(define out (open-output-file (cadr (command-line-arguments)) 'replace))

(define-syntax apply-ordered
  (lambda (x)
    (syntax-case x ()
      [(appord f x ...)
       (with-syntax (((tmp ...) (map 
				    (lambda (_) (datum->syntax-object #'appord (gensym "tmp")))
				  (syntax-object->datum #'(x ...)))))
	 #'(let* ((tmp x) ...) (f tmp ...)))])))

;; Read the *in* and write the *out*:
(letrec ([subst 
	  (lambda (c)
	    (case c
;	      [(#\[) #\(]
;	      [(#\]) #\)]
	      ;; Can't handle the commas within a match construct:
;	      [(#\,) #\?]
	      ;; Hell, I'm just removing quasiquote entirely:
;	      [(#\`) #\']
	      [else c]))]
	 [loop 
	  (lambda (a b)
	    (match (list a b)
	      [(#!eof #!eof) (void)]
	      [(,_ #!eof)    (write-char (subst a) out)]
	      ;; S-exp comment: 
	      [(#\# #\;) 
	       ;; Read and discard the expression
	       (read in) 
	       (apply-ordered loop (read-char in) (read-char in))]
	      ;; Block comment:
	      [(#\# #\|)
	       (apply-ordered deadloop (read-char in) (read-char in))]

	      ;; Syntax quotation
	      [(#\# #\')
	       (write-char #\' out)
	       (apply-ordered loop (read-char in) (read-char in))]

	      ;; Raw Primitive references:
	      [(#\# #\%)
	       (apply-ordered loop (read-char in) (read-char in))]

	      ;; Remove unquotes before lists
	      [(#\, ,open) (guard (memq open '(#\( #\[)))
	       (loop b (read-char in))]
	      ;; Otherwise replace unquote by ?
	      [(#\, ,_) 
	       (write-char #\? out)
	       (loop b (read-char in))]

	      ;; Semi then Double-quote
	      [(#\; #\")
	       (write-char #\; out)
	       (write-char #\space out) ;; Insert an extra space
	       (loop b (read-char in))]
	      
	      ;; Anything else is fit to print:
	      [(,_ ,__)
	       (write-char (subst a) out)
	       (loop b (read-char in))]))]

	 [deadloop 
	  (lambda (a b)
	    (match (list a b)
	      ;; End block comment:
	      [(#\| #\#) 
	       (apply-ordered loop (read-char in) (read-char in))]
	      [(,_ ,__)
	       (deadloop b (read-char in))]))]
	 )
  (apply-ordered loop (read-char in) (read-char in)))

(close-input-port in)
(close-output-port out)
