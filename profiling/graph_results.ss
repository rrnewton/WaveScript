#! /bin/sh
#|
exec regiment i "$0" ${1+"$@"};
|#


;(system "cat array_array.out | grep TimeElapsed: | awk '{ print $NF }'  > .tmp.out")
;(define aa_results (file->slist ".tmp.out"))
;(gnuplot aa_results 'boxes '(title "array of arrays, 1000x1000, 100000x3, 3x100000, ws/wsc/wsmlton"))

(define (arraytuplecombo outer inner comment1 comment2)
  (system (format "cat ~a_~a.out | grep TimeElapsed: | sed 's/,//g' | awk '{ print $5\" \"$7\" \"$NF }'  > .tmp.out"
		  outer inner))
  (let ([results (group 2 (group 3 (file->slist ".tmp.out")))])
    (match results
      [([(,xd* ,yd* ,time*) ...] ...)
       (ASSERT (all-equal? xd*))
       (ASSERT (all-equal? yd*))
       (inspect time*)
       (gnuplot (map car time*)
		'boxes `(title ,(format "~a ~a x ~a ws/wsc/wsmlton" comment1 (caar xd*) (caar yd*))))
       (gnuplot (map cadr time*)
		'boxes `(title ,(format "~a ~a x ~a ws/wsc/wsmlton" comment2 (cadar xd*) (cadar yd*))))])
    ))

(arraytuplecombo 'array 'tuple "square array of tuples"   "fine grain array of tuples")
(arraytuplecombo 'tuple 'array "square tuple of arrais" "course grain tuple of arrays")


#|

(system "cat array_tuple.out | grep TimeElapsed: | sed 's/,//g' | awk '{ print $5\" \"$7\" \"$NF }'  > .tmp.out")
(define at_results (group 2 (group 3 (file->slist ".tmp.out"))))

(match at_results
  [([(,xd* ,yd* ,time*) ...] ...)
   (ASSERT (all-equal? xd*))
   (ASSERT (all-equal? yd*))
   (gnuplot (map car time*)
	    'boxes `(title ,(format "square tuple of arrays ~a x ~a ws/wsc/wsmlton" (caar xd*) (caar yd*))))
   (gnuplot (map cadr time*)
	    'boxes `(title ,(format "course grain tuple of arrays ~a x ~a ws/wsc/wsmlton" (cadar xd*) (cadar yd*))))])

#|
(define atxd (map caar at_results))  (ASSERT (apply = atxd)) (set! atxd (car atxd))
(define atyd (map cadar at_results)) (ASSERT (apply = atyd)) (set! atyd (car atyd))
(gnuplot (map caddar at_results)
	 'boxes `(title ,(format "square array of tuples ~a x ~a ws/wsc/wsmlton" atxd atyd)))
(define atxd (map caadr at_results))  (ASSERT (apply = atxd)) (set! atxd (car atxd))
(define atyd (map cadadr at_results)) (ASSERT (apply = atyd)) (set! atyd (car atyd))
(gnuplot (map caddar at_results)
	 'boxes `(title ,(format "fine grain array of tuples ~a x ~a ws/wsc/wsmlton" atxd atyd)))
|#

(system "cat tuple_array.out | grep TimeElapsed: | sed 's/,//g' | awk '{ print $5\" \"$7\" \"$NF }'  > .tmp.out")
(define ta_results (group 2 (group 3 (file->slist ".tmp.out"))))

(match ta_results
  [([(,xd* ,yd* ,time*) ...] ...)
   (ASSERT (all-equal? xd*))
   (ASSERT (all-equal? yd*))
   (gnuplot (map car time*)
	    'boxes `(title ,(format "square tuple of arrays ~a x ~a ws/wsc/wsmlton" (caar xd*) (caar yd*))))
   (gnuplot (map cadr time*)
	    'boxes `(title ,(format "course grain tuple of arrays ~a x ~a ws/wsc/wsmlton" (cadar xd*) (cadar yd*))))])
|#



#|
;(inspect ta_results)
(define taxd (map caar ta_results))  (ASSERT (apply = taxd)) (set! taxd (car taxd))
(define tayd (map cadar ta_results)) (ASSERT (apply = tayd)) (set! tayd (car tayd))
;(inspect (vector taxd tayd))
(gnuplot (map caddar ta_results)
 	 'boxes `(title ,(format "square tuple of arrays ~a x ~a ws/wsc/wsmlton" taxd tayd)))

(define taxd (map caadr ta_results))  (ASSERT (apply = taxd)) (set! taxd (car taxd))
(define tayd (map cadadr ta_results)) (ASSERT (apply = tayd)) (set! tayd (car tayd))

(inspect (map caddar ta_results))

(gnuplot (map caddar ta_results)
 	 'boxes `(title ,(format "course grain tuple of arrays ~a x ~a ws/wsc/wsmlton" taxd  tayd)))

|#