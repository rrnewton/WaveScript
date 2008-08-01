#! /bin/bash
#|
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require (lib "process.ss"))

(define (file->string filename)
    (let ([p (open-input-file filename)])
      (let loop ([c (read-char p)]
                 [acc '()])
        (if (eof-object? c)
            (begin (close-input-port p)
                   (list->string (reverse acc)))
            (loop (read-char p) (cons c acc))))))
(define (system-to-str cmd)
  (define fn (format "/tmp/___gather_script_tmp_~a.txt" (random 100000000)))
  (and (system (format "~a &> ~a" cmd fn))
       (file->string fn)))
(define (string->tokens str)
  (define prt (open-input-string str))
  (let loop ([x (read prt)])
    (if (eof-object? x) '()
	(cons x (loop (read prt))))))
(define (pad-width n str)
  (if (< (string-length str) n)
      (string-append str (make-string (- n (string-length str)) #\space))
      str))

(define (gather dir file)
  (when (file-exists? (format "~a/~a" dir file))
    (let* ([str (system-to-str (format "cat ~a/~a | grep -v '^[ ]*#'" dir file))]
	   [prt (open-input-string str)]
	   [header (cdr (string->tokens (read-line prt)))]
	   [results 
	    (let loop ()
	      (let ([line (read-line prt)])
		(if (eof-object? line)
		    '()
		    (cons (string->tokens line) (loop))	  
		    )))])
      (for-each
	  (lambda (line)
	    (let ([name (car line)])
	      (for-each (lambda (variant num)
			  (printf "~a ~a\n" (pad-width 50 (format "~a_~a_~a" dir name variant)) num))
		header (cdr line))))
	results))))


(gather "appbench" "RESULTS.txt")
(gather "microbench" "RESULTS_misc.txt")
(gather "microbench" "RESULTS_datapass.txt")
