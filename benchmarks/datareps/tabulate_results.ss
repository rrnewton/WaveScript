#! /bin/sh
#|
exec regiment i --script "$0" ${1+"$@"};
|#

;; This script gathers data from the results/ directory and writes it to .results files in the current directory.

(define startdir (current-directory))

(define (dump-lines file lines)  
  (when (file-exists? file) (delete-file file))
  (let ([p (open-output-file file)])
    (parameterize ([print-level #f]
		   [print-length #f]
		   [pretty-maximum-lines #f])
      (for-each (lambda (ln) 
		  (for-each (lambda (elm) (display elm p) (display " " p)) ln)
		  (newline p))
	lines))
    (close-output-port p)))

(define (grab-results name)
  (system "cat array_array.out | grep TimeElapsed: | sed 's/~/-/g' | awk '{ print $NF }'  > .tmp.out")
  ;; Assumes lines of the form "RUNNING WITH X"
  (system "grep RUNNING array_array.out | awk '{ print $3 }'  > .tmp2.out")
  (let ([aa_results (file->slist ".tmp.out")]
	[backends (file->slist ".tmp2.out")])
    (ASSERT (= (* 3 (length backends)) (length aa_results)))
    (dump-lines (format "~a/~a_arrarr.result" startdir name)
		(cons '("Implementation 1000x1000  100000x3  3x100000")
		      (map cons backends (group 3 aa_results))))))

(printf "First Array/Array...\n")
(begin (printf "  Grabbing alloc results...\n")
       (current-directory "./results/alloc")
       (grab-results "alloc")
       (current-directory startdir))

(begin (printf "  Grabbing fold results...\n")
       (current-directory "./results/fold")
       (grab-results "fold")
       (current-directory startdir))

(define (arraytuplecombo name outer inner comment1 comment2)
  (parameterize ([current-directory (format "~a/results/~a" startdir name)])
    (system (format "cat ~a_~a.out | grep TimeElapsed: | sed 's/,//g' | sed 's/~a/-/g' | awk '{ print $5\" \"$7\" \"$8\" \"$NF }'  > .tmp.out"
		    outer inner #\~))
    (system "grep RUNNING array_array.out | awk '{ print $3 }'  > .tmp2.out")
    (let ([results (group 2 (group 4 (file->slist ".tmp.out")))]
	  [backends (file->slist ".tmp2.out")])
      (match results
	[([(,xd* ,yd* ,reps* ,time*) ...] ...)
	 (ASSERT (all-equal? xd*))
	 (ASSERT (all-equal? yd*))
	 (ASSERT (all-equal? reps*))

	 (dump-lines (format "~a/~a_~a~a.result" startdir name outer inner)
		     (cons (cons "Implementation "
				 (map (lambda (dims)
					(format "~a_x_~a_x_~areps" (car dims) (cadr dims) (caddr dims)))
				   (map list (car xd*) (car yd*) (car reps*))))
			   (map cons backends time*)
			   ))
	 #;#;       
	 (gnuplot (map car time*)
		  'boxes `(title ,(format "~a ~a x ~a ws/wsc/wsmlton ~a reps" comment1 (caar xd*) (caar yd*) (caar reps*))))
	 (gnuplot (map cadr time*)
		  'boxes `(title ,(format "~a ~a x ~a ws/wsc/wsmlton ~a reps" comment2 (cadar xd*) (cadar yd*) (cadar reps*))))])
      )))

(printf "Next Array/Tuple (alloc)...\n")
;(current-directory (format "~a/results/alloc" startdir))
(arraytuplecombo 'alloc 'array 'tuple "square array of tuples"   "fine grain array of tuples")
(printf "And Tuple/Array  (alloc)...\n")
(arraytuplecombo 'alloc 'tuple 'array "square tuple of arrais" "course grain tuple of arrays")


(printf "And Array/Tuple (fold)...\n")
;(current-directory (format "~a/results/alloc" startdir))
(arraytuplecombo 'fold 'array 'tuple "square array of tuples"   "fine grain array of tuples")
(printf "Finally Tuple/Array (fold)...\n")
(arraytuplecombo 'fold 'tuple 'array "square tuple of arrais" "course grain tuple of arrays")


(exit 0)

