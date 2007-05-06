
; #!/usr/bin/env petite --script

;; Just load the matcher from source:
(load (string-append (getenv "REGIMENTD") "/src/chez/match.ss"))(import iu-match)

; [2006.01.13] Remember to manually strip the ::= character
; sequence.  That causes a problem for some reason!!


; #! /bin/sh
; #| 
; $REGIMENTD/depends/petite --script "$0" `pwd` ${1+"$@"}; 
; |#

;; [2005.11.26] This script invocation wasn't working for me on faith.csail:
;; #!/usr/bin/env petite --script

;; [2005.11.19] This scrubs my scheme source file of characters and
;; constructs that LAML SchemeDoc can't handle.

;; [2005.11.19] WHOA!  Just had a problem with semicolon followed by
;; double quote for some reason!  That's probably a bug, should report it.


(define in (open-input-file (car (command-line-arguments))))
(define out (open-output-file (cadr (command-line-arguments)) 'replace))


(define read-line
  (case-lambda 
    [() (read-line (current-output-port))]
    [(p) (let loop ((c (read-char p)) (acc '()))
	   (if (or (eof-object? c) (char=? #\newline c))
	       (list->string (reverse! acc))
	       (loop (read-char p) (cons c acc))))]))

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
	       (display "NOTE!SYNTAX_QUOTATION_REMOVED" out)
	       ;; Read and discard the expression
	       (read in) 
	       ;(write-char #\' out)
	       (apply-ordered loop (read-char in) (read-char in))]

	      ;; Raw Primitive references:
	      [(#\# #\%)
	       (apply-ordered loop (read-char in) (read-char in))]

	      ;; Remove unquotes-at's:  [2006.01.13]
	      [(#\, #\@)
	       (apply-ordered loop (read-char in) (read-char in))]

	      ;; Remove unquote's before lists:
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
	       ;; Then we go ahead and cut out the rest of the line.  Shouldn't hurt a comment:
	       (display (read-line in) out)
	       (newline out)
	       (apply-ordered loop (read-char in) (read-char in))]
	      ;; Other line-comments: 
	      [(#\; ,_)
	       (write-char a out)
	       (write-char b out)
	       (display (read-line in) out)	       
	       (newline out)
	       (apply-ordered loop (read-char in) (read-char in))]
	      
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
