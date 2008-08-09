#! /bin/bash
#|
exec mzscheme -qr "$0" ${1+"$@"}
|#

;; [2008.08.08] This script compares the vital_stats.txt table against
;; that of the previous revision on the same machine.

(require (lib "process.ss")
	 (lib "pretty.ss"))
;(require (lib "69.ss" "srfi")) ;; hash tables

(define (file->string filename)
    (let ([p (open-input-file filename)])
      (let loop ([c (read-char p)]
                 [acc '()])
        (if (eof-object? c)
            (begin (close-input-port p)
                   (list->string (reverse acc)))
            (loop (read-char p) (cons c acc))))))
(define (system-to-str cmd)
  (define fn (format "/tmp/___supertest_tmp_~a.txt" (random 100000000)))
  (and (system (format "~a &> ~a" cmd fn))
       (file->string fn)))
(define (string->list str)
  (let ([port (open-input-string str)])	       
    (let loop2 ([tok (read port)])
      (if (eof-object? tok) '()
	  (cons tok (loop2 (read port)))))))
(define (file->linelists fn)
  (define prt (open-input-file fn))
  (let loop ([line (read-line prt)])
    (if (eof-object? line) '()	     
	(cons (string->list line)
	      (loop (read-line prt))))))
(define (pad-width n str)
  (if (string? str)
      (if (< (string-length str) n)
	  (string-append str (make-string (- n (string-length str)) #\space))
	  str)
      (pad-width n (format "~a" str))))

(define svn-revision
  (begin 
    (system "svn info | grep Revision | sed s/Revision:// > svn_rev.txt")
    (let ([x (read (open-input-file "svn_rev.txt"))])
      (unless (number? x) (error 'svn-revision "Should have got a number, got this: ~a" x))
      x)))

;; ================================================================================

;; Read in our vital_stats.txt table:

(define mystats (file->linelists "vital_stats.txt"))

;(define hash (make-hash (length mystats)))
(define hash (make-hasheq))
(for-each (lambda (stat) 
	    ;(printf "Setting ~a ~a ~a\n" (car stat) (cadr stat) (symbol? (car stat)))
	    (hash-set! hash (car stat) (cadr stat))) mystats)

;; We could get the previous vital_stats.txt off the web, but we want the one off this local machine.
(printf "Searching for previous revision...\n")

;; We check up to 20 revisions in the past.
(define startdir (current-directory))
(current-directory (getenv "HOME"))
(define oldstats
  (call/cc 
   (lambda (jumpout)   
     (let loop ([i 1])
       (unless (> i 20)
	 ;; Here we make a nasty ASSUMPTION about where the previous revision working copy would be located.
	 (let ([files (string->list (or (system-to-str (format "ls -d ~a_WS_test_copy*" (- svn-revision i))) ""))])
	   ;; Here we've also ASSUMED that the filenames make valid symbols.
	   (for-each (lambda (sym) 
		       (let ([fn (format "~a/~a/src/vital_stats.txt" (getenv "HOME") sym)])
			 (when (file-exists? fn)
			   (printf "Found stats from most recent previous results: ~a\n" fn)			   
			   (jumpout (file->linelists fn))
			   )))
	     files))
	 (loop (add1 i))
	 ))
     #f)))

;; Now compare:
(define diffs
  (sort 
	(filter (lambda (x) x)
	  (map (lambda (stat)
		 (define sym (car stat))
		 (define old (cadr stat))
		 (define new (hash-ref hash (car stat) #f))
					;(printf "~a : ~a -> ~a\n" sym old new)      
		 (and new 
		      (if (zero? old)
			  (list +inf.0 old new sym)
			  (list (exact->inexact (/ new old)) old new sym))))
	    oldstats))
	(lambda (a b) (< (car a) (car b)))))

(define (print-entry entry)
  (printf "~a ~a ~a   ~a\n"
		    (pad-width 20 (car entry))
		    (pad-width 7 (cadr entry))
		    (pad-width 7 (caddr entry))
		    (cadddr entry)))



(when (file-exists? "perf_diffs.txt")             (delete-file "perf_diffs.txt"))
(when (file-exists? "perf_diffs_thresholded.txt") (delete-file "perf_diffs_thresholded.txt"))

(current-directory startdir)

(with-output-to-file "perf_diffs.txt"  
  (lambda ()
    (for-each print-entry diffs)))

;; Report perf diffs over a threshold (e.g. five percent)
;(define threshold-diff 0.05)
(define threshold-diff 0.1)

(with-output-to-file "perf_diffs_thresholded.txt"  
  (lambda ()
    (for-each print-entry 
      (filter (lambda (entry) (or (< (car entry) (- 1.0 threshold-diff)) 
				  (> (car entry) (+ 1.0 threshold-diff))))
	diffs))))

(printf "Results printed to perf_diffs.txt and perf_diffs_thresholded.txt \n")

;(pretty-print diffs)
;(pretty-print mystats)
;(pretty-print oldstats)
;(pretty-print hash)
