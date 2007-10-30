#! /bin/sh
#|
exec regiment i "$0" ${1+"$@"};
|#

(define (dump-lines file lines)  
  (let ([p (open-output-file file 'replace)])
    (parameterize ([print-level #f]
		   [print-length #f]
		   [pretty-maximum-lines #f])
      (for-each (lambda (ln) 
		  (for-each (lambda (elm) (display elm p) (display " " p)) ln)
		  (newline p))
	lines))
    (close-output-port p)))

(system "cat array_array.out | grep TimeElapsed: | sed 's/~/-/g' | awk '{ print $NF }'  > .tmp.out")
(define aa_results (file->slist ".tmp.out"))
;(gnuplot aa_results 'boxes '(title "array of arrays, 1000x1000, 100000x3, 3x100000, ws/wsc/wsmlton"))

(dump-lines "arrarr.result"
	    (cons '("Implementation 1000x1000  100000x3  3x100000")
		  (map cons '(Scheme C++ MLton) (group 3 aa_results))))


#|
(define (arraytuplecombo outer inner comment1 comment2)
  (system (format "cat ~a_~a.out | grep TimeElapsed: | sed 's/,//g' | sed 's/~a/-/g' | awk '{ print $5\" \"$7\" \"$8\" \"$NF }'  > .tmp.out"
		  outer inner #\~))
  (let ([results (group 2 (group 4 (file->slist ".tmp.out")))])
    (match results
      [([(,xd* ,yd* ,reps* ,time*) ...] ...)
       (ASSERT (all-equal? xd*))
       (ASSERT (all-equal? yd*))
       (ASSERT (all-equal? reps*))
       (gnuplot (map car time*)
		'boxes `(title ,(format "~a ~a x ~a ws/wsc/wsmlton ~a reps" comment1 (caar xd*) (caar yd*) (caar reps*))))
       (gnuplot (map cadr time*)
		'boxes `(title ,(format "~a ~a x ~a ws/wsc/wsmlton ~a reps" comment2 (cadar xd*) (cadar yd*) (cadar reps*))))])
    ))

(arraytuplecombo 'array 'tuple "square array of tuples"   "fine grain array of tuples")
(arraytuplecombo 'tuple 'array "square tuple of arrais" "course grain tuple of arrays")
|#