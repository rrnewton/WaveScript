#! /bin/sh
#| 
exec regiment i --script "$0" ${1+"$@"};
|#

(define out1s 
  (map (lambda (n) (open-output-file (format "data/averaged_noise~a.dat" n) 'replace))
    (iota 1 6)))
;  (open-output-file "data/averaged.dat" 'replace))
;; Also just dumping them all in one file for a scatterplot

(define out2s 
  (map (lambda (n) (open-output-file (format "data/aggregated_noise~a.dat" n) 'replace))
    (iota 1 6)))

(define avgdresults '())

(define (avg ls) (if (null? ls) +nan.0
		     (floor (/ (apply + ls) (length ls)))))

(define (stddev ls)
  (let* ([sum (exact->inexact (apply + ls))]
	 [len (length ls)]
	 [av (/ sum len)]
	 [var (/ (foldl (lambda (n acc) (+ (^ (- n av) 2) acc)) 0 ls) 
		 ;(sub1 len)
		 len
		 )])
    ;(printf "sum ~a len ~a av ~a var ~a\n" sum len av var)    
    (sqrt var)
    ))

(for-each (lambda (out2)
	    (fprintf out2 "# Columns: thresh, noise, falsepos, lag, no-pre-detection?\n"))
  out2s)

(for i = 1 to 9
 (for j = 1 to 6
      (let ([lst (filter (lambda (x) (= (length x) 3));(not (null? x)))
		   (file->linelists (format "data/results_thresh~a_noise~a.dat" i j) #\#))])
	(if (not (andmap (lambda (ls) (= (length ls) 3)) lst))
	    (error 'tally_results "Not all entries were 3 numbers long: ~a\n" lst))
	;(printf "LST: ~a\n" lst)
	;; Dump into aggregate file:	
	(for-each (match-lambda ((,fp ,lag ,det))
		    (fprintf (list-ref out2s (sub1 j))  "~a ~a ~a ~a ~a\n" i j fp lag det))
	  lst)
	(set! avgdresults (cons (list i j (avg (map car lst)) 
				      (avg    (map cadr lst))
				      (stddev (map cadr lst)))
				avgdresults))))
 ;(newline out2) (newline out2)	
 )

(printf "AVGS:\n")
(pretty-print avgdresults)


(for j = 1 to 6
     (let ((lst (filter (match-lambda ((,t ,n ,fp ,av ,stddev)) (= n j)) avgdresults)))
       (unless (null? lst)
	 ;(gnuplot (map (match-lambda ((,t ,n ,av)) `(,t ,av)) lst))
	 (fprintf (list-ref out1s (sub1 j)) "# Columns (thresh, falsepos, lag, lagstdev) all with noise = ~a\n" j)
	 (for-each (match-lambda ((,t ,n ,fp ,av ,stddev))
		     (fprintf (list-ref out1s (sub1 j)) "~a ~a ~a ~a \n" 
			      (pad-width 3 t) 
			      (pad-width 2 fp) 
			      (pad-width 6 av)
			      (pad-width 4 stddev)
			      ))
	   lst)
	 ;(fprintf out1 "\n\n")
	 )))



