#! /usr/bin/env scheme-script

;; This script scrapes the export lists from all the files that go into the main library.
;; It aggregates them to generate main_r6rs.sls

(import (rnrs)
	;(ikarus)
	)

;; Default: build the main aggregate library:
(define importsfile "main_r6rs_import_list.sexp")
(define outputlib '(main_r6rs)) ;; "./main_r6rs.sls"

;; Override the defaults with the command-line arguments.
(when (> (length (command-line)) 1)
    (set! importsfile (cadr (command-line)))
    (set! outputlib (read (open-string-input-port (caddr (command-line))))))

(define import-list
  (let ([p (open-input-file importsfile)])
    (let loop ([x (read p)])
      (if (eof-object? x) '()
	  (cons x (loop (read p)))))))

(define (libname->file input)
  (cond
   [(memq (car input) '(prefix except only))
    (libname->file (cadr input))]
   [else (string-append 
	  "."
	  (apply string-append 
		 (map (lambda (s) (string-append "/" (symbol->string s))) input))
	  )]))

(define (symappend . syms) (string->symbol (apply string-append (map symbol->string syms))))

(define (read-exports libname)
  (define file (libname->file libname))
  ;; HACK: prioritize .ikarus.sls... the plain .sls may be larceny's, and larceny has problems.
  (define lib (read (if (file-exists? (string-append file ".ikarus.sls"))
			(open-input-file (string-append file ".ikarus.sls"))
			(open-input-file (string-append file ".sls")))))
  (define exports (caddr lib))
  (unless (and (list? exports) (>= (length exports) 1) (eq? (car exports) 'export))
    (error 'read-exports "Couldn't extract from file: ~a.sls" file))
  ;; Now parse the rename constructs:
  (set! exports 
	(apply append 	       
	       (map (lambda (entry)
		      (if (and (pair? entry) (eq? (car entry) 'rename))
			  (map cadr (cdr entry))
			  (list entry)))
		 (cdr exports))))
  ;; Now, we look at what kind of import it was and adjust the export list.
  (let loop ([libname libname])
    (cond
     [(eq? (car libname) 'prefix)
      (let ([pref (caddr libname)])
	(map (lambda (s) (symappend pref s)) (loop (cadr libname))))]
     [(eq? (car libname) 'except)
      (let ([leaveout (cddr libname)]
	    [base (loop (cadr libname))])
	(for-each (lambda (s) (set! base (remq s base))) leaveout)
	base)]
     [(eq? (car libname) 'only)
      (cddr libname)]
     [else exports]))   
  )

(define (remove-duplicates ls)
  (let ([hsh (make-eq-hashtable (length ls))])
    (for-each (lambda (sym) (hashtable-set! hsh sym #t)) ls)
    (vector->list (hashtable-keys hsh))))

(define all-exports 
  (remove-duplicates (apply append (map read-exports import-list))))

(define main-lib
  `(library ,outputlib
     (export ,@all-exports)
     (import ,@import-list)))

(define out (open-file-output-port (string-append (libname->file outputlib) ".sls")
				   (file-options no-fail) 'block (native-transcoder)))
(display "#!r6rs\n\n" out)
(write main-lib out) (newline out)
(close-output-port out)

(display "File written: ")(display (libname->file outputlib))(newline)(flush-output-port (current-output-port))

