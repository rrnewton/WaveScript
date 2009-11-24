

(define files 
  '("/home/newton/data/slave10.txt.dstlog"
    "/home/newton/data/slave14.txt.dstlog",
    ))

(define ports (map open-input-file files))

(define (readln p) 
  (let* ([a (read p)]
	 [b (read p)]
	 [c (read p)])
    (list a b c)))

(define (loop)
  
  )
    