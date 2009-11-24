

(define dat (file->slist "~/data/FINAL_histogram.dat"))

(define lines (match dat
		[() ()]
		[(,a ,b . ,[rest]) (cons (list a b) rest)]))

(define result
  (let loop ([x (reverse lines)] [sum 0])
    (match x
      [() ()]
      [((,rep ,cnt) . ,rest)
       (let ([newsum (+ sum cnt)])
	 (cons (list rep newsum)
	       (loop rest newsum)))])))

(for-each
    (match-lambda ((,rep ,sum))
      (printf "~s ~s\n" rep sum)
      )
  (reverse result))

;(new-cafe)
