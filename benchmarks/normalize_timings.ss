#! /bin/bash
#|
exec regiment i --script "$0" ${1+"$@"}
|# 


(define args (cddr (ASSERT (member "--script" (command-line)))))
;(define-values (plotfile datafile) (match args [(,x ,y) (values x y)]))
(define datafile (car args))
(define newfile  (cadr args))

(define data-strings (file->lines datafile))

(define data 
  (filter (compose not null?)
    (map string->slist (filter (lambda (s) (not (memq #\# (string->list s)))) data-strings))))

(unless (apply = (map length data))
  (error 'dump_plot_script.ss "Expected all rows in datafile ~a to have same number of fields.\nInstead found this distribution: ~a" 
	 datafile
	 (map length data)))

(define backends  
  (match data  ;; Get the header:
    [((Benchmark ,backends ...) . ,_)  backends]
    [,else (error 'dump_plot_script.ss "data file didn't have a proper header line: ~s" (car data))]))

(define c2-index (list-find-position 'c2 backends))
(if c2-index
    (printf "Found c2 backend at position: ~a\n" c2-index)
    (printf "Didn't find c2 backend, not normalizing to it.\n"))

(define benchmark-runs (cdr data))

(define out (force-open-output-file newfile))

(define (print-sep ls) (for-each (lambda (x) (display x out)) (insert-between " " ls)))

(display "Benchmark " out)
(print-sep backends)
(newline out)

(for-each 
    (lambda (run)
      (define name (car run))
      (define nums (cdr run))      
      (define c2-time (if c2-index (list-ref nums c2-index)))
      ;(define mx (apply max nums))      
      (fprintf out "~a " name)
      (print-sep
       (if c2-index 
	   (map (lambda (n) (inexact (/ c2-time n))) nums)
	   nums))
      (newline out)
      ;(printf "Got max for ~a: ~a and c2: ~a\n" name mx c2-time)
      )
    benchmark-runs)

(close-output-port out)
(printf "Finished writing ~a\n" newfile)