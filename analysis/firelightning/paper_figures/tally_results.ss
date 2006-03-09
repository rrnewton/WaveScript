#! /bin/sh
#| 
exec regiment i --script "$0" ${1+"$@"};
|#


(define NOISELEVELS 6)

(define out1s 
  (map (lambda (n) (open-output-file (format "data/averaged_noise~a.dat" n) 'replace))
    (iota 1 NOISELEVELS)))
;  (open-output-file "data/averaged.dat" 'replace))
;; Also just dumping them all in one file for a scatterplot

(define out2s 
  (map (lambda (n) (open-output-file (format "data/aggregated_noise~a.dat" n) 'replace))
    (iota 1 NOISELEVELS)))

(define DIVZERO -20000) ;; Signals div zero in the output for avgs/stdevs

(define avgdresults '())

(define (avg ls) (if (null? ls) DIVZERO
		     (floor (/ (apply + ls) (length ls)))))

(define (stddev ls)
  (let* ([sum (exact->inexact (apply + ls))]
	 [len (length ls)]
	 [av (if (zero? len) DIVZERO (/ sum len))]
	 [var (if (zero? len) DIVZERO
		  (/ (foldl (lambda (n acc) (+ (^ (- n av) 2) acc)) 0 ls) 
					;(sub1 len)
		     len
		     ))])
    ;(printf "sum ~a len ~a av ~a var ~a\n" sum len av var)    
    (if (= var DIVZERO) 0 
	(sqrt var))
    ))

(for-each (lambda (out2)
	    (fprintf out2 "# Columns: thresh, noise, falsepos, lag, no-pre-detection?\n"))
  out2s)

(for i = 1 to 20
 (for j = 1 to NOISELEVELS
      (let* ([fix (lambda (n) (if (integer? n) (inexact->exact n) n))]
	     [lst (filter (lambda (x) (= (length x) 4));(not (null? x)))
		   (file->linelists (format "data/results_thresh~a_noise~a.dat" i (fix (/ j 1.0))) #\#))])
	(if (not (andmap (lambda (ls) (= (length ls) 4)) lst))
	    (error 'tally_results "Not all entries were 4 numbers long: ~a\n" lst))
	;(printf "LST: ~a\n" lst)
	;; Dump into aggregate file: 

	;; FIXME: This isn't right:
	(for-each (match-lambda ((,fp ,falsenegs ,lag ,det))
		    (fprintf (list-ref out2s (sub1 j))  "~a ~a ~a ~a ~a\n" i (/ j 1.0) fp lag det))
	  lst)

	;; Accumulate averaged results:
	(set! avgdresults (cons (list i j 
				      (apply + (map car lst)) ;(avg (map car lst))  ;; total falsepositives
				      (apply + (map cadr lst)) ;; total falsenegs
				      (avg    (filter positive? (map caddr lst)))  ;; average lag
				      (stddev (filter positive? (map caddr lst)))) ;; stddev lag
				avgdresults))))
 ;(newline out2) (newline out2)	
 )

(printf "AVGS:\n")
(pretty-print avgdresults)


;; Collect the average results for each noise level.
(for j = 1 to NOISELEVELS
     (let ((lst (filter (match-lambda ((,t ,n ,fpos ,fneg ,av ,stddev)) (= n j)) avgdresults)))
       (unless (null? lst)
	 ;(gnuplot (map (match-lambda ((,t ,n ,av)) `(,t ,av)) lst))
	 (fprintf (list-ref out1s (sub1 j)) "# Columns (thresh, falsepos, falseneg, lag, lagstdev) all with noise = ~a\n" j)
	 (for-each (match-lambda ((,t ,n ,fpos ,fneg ,av ,stddev))
		     (fprintf (list-ref out1s (sub1 j)) "~a ~a ~a ~a ~a \n" 
			      (pad-width 3 t) 
			      (pad-width 2 fpos) 
			      (pad-width 2 fneg)
			      (pad-width 6 av)
			      (pad-width 4 stddev)
			      ))
	   lst)
	 ;(fprintf out1 "\n\n")
	 )))


