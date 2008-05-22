;#! /usr/bin/env scheme-script

;; This script scrapes the export lists from all the files that go into the main library.
;; It aggregates them to generate main_r6rs.sls

(import (rnrs)
	;(ikarus)
	)

;; Default: build the main aggregate library:
(define importsfile "main_r6rs_import_list.sexp")
(define outputlib '(main_r6rs)) ;; "./main_r6rs.sls"

(define ignored0 (begin (display "command line arguments: ") (write (command-line)) (newline)))

;; Override the defaults with the command-line arguments.
(when (> (length (command-line)) 1)
   (set! importsfile (cadr (reverse (command-line)))) ;; agnostic to missing first arg
   (set! outputlib (read (open-string-input-port (car (reverse (command-line)))))))

(define ignored1 (begin (display "imports/output: ") (write (list importsfile outputlib)) (newline)))

;; HACK: The non-binding appendix of R6RS recommends that
;; Scheme scripts begin with "#! " or "#!/", but neither of
;; those is a legal input to the read or get-datum procedures
;; in portable R6RS code.  This is yet another example of
;; poor design in the R6RS documents.
;;
;; To work around this, files that may be Scheme scripts
;; can be opened by open-possible-scheme-script instead
;; of open-input-file.  If the file begins with "#! " or #!/",
;; then its first line is consumed before the port is returned.

(define (open-possible-scheme-script file)
 (let* ((line1 (call-with-port (open-input-file file) get-line))
        (p (open-input-file file)))
   (if (and (>= (string-length line1) 3)
            (or (string=? (substring line1 0 3) "#! ")
                (string=? (substring line1 0 3) "#!/")))
       (get-line p))
   p))


(define import-list
 (let ([p (open-possible-scheme-script importsfile)])
   (let loop ([x (read p)])
     (if (eof-object? x) '()
	  (cons x (loop (read p)))))))

(define (libname->file input)
 (cond
  [(memq (car input) '(prefix except only))
   (libname->file (cadr input))]
  [else
   (if (not (for-all symbol? input))
       (begin (display "Bad input to libname->file: ")
              (write input)
              (newline)))
   (assert (for-all symbol? input))
   (string-append 
    "."
    (apply string-append 
           (map (lambda (s) (string-append "/" (symbol->string s))) input)))]))

(define (symappend . syms)
 (assert (for-all symbol? syms))
 (string->symbol (apply string-append (map symbol->string syms))))

(define (read-exports libname)
 ;; HACK: prioritize .ikarus.sls... the plain .sls may be larceny's, and larceny has problems.
 (define (read-carefully file)
   (display "Reading ")
   (display file)
   (newline)
   (let ((file (if (file-exists? (string-append file ".ikarus.sls"))
                   (string-append file ".ikarus.sls")
                   (string-append file ".sls"))))
     (read (open-possible-scheme-script file))))
 (define file (libname->file libname))
 (define lib (read-carefully file))
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
				   ;(file-options no-fail) ;; problem with larceny
				   (file-options ) 
				   'block (native-transcoder)))
(display "#!r6rs\n\n" out)
(write main-lib out) (newline out)
(close-output-port out)

(display "File written: ")(display (libname->file outputlib))(newline)(flush-output-port (current-output-port))
