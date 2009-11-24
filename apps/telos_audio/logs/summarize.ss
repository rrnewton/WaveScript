#! /bin/bash
#|
exec regiment i -v 0 --script "$0"
|#



(let ()

  (define minutes-run 4)

  ;; How many messages are needed for one stream element.
  ;; Should extract from the compile_* log
  ;(define number_messages '(10 10 10 2 2 1))
  ;(define number_messages '(23 23 58 1 1 1))

  ;; This is with size 28 messages:
  ;(define number_messages '[15 15 37 5 5 3])
  ;;(define number_messages '[15 15 37 4 4 2])
  
  ;; This is with size 106:
  (define number_messages '[4 4 10 2 2 1])

  
  ;; What percentage of the time do 
  (define (count-sliding-window ls win)
    ;;(pretty-print ls)
    ;(newline)(display ls)(newline)
    (let loop ([good 0] [total 0] [ls ls] [len (length ls)])      
      (if (< len win) 
	  (begin 
	    ;(printf "  good and total ~a ~a\n" good total)
	    (/ good total))
	  (loop (if (zero? (apply + (list-head ls win)))
		    (add1 good) good)
		;(add1 total)
		;; Must take into account the gaps too:
		(+ 1 total (car ls))
		(cdr ls)
		(sub1 len)))))

  (define (percent x)
    (/ (inexact (round (* x 100 100))) 100))

  (define (diffs ls)
    (if (null? (cdr ls)) '()
	(cons (- (cadr ls) (car ls))
	      (diffs (cdr ls)))))

  (define alldeltas (make-vector 7 'uninit))

  (printf "Cut processedInput surviveNetwork adjusted\n")
  (for i = 1 to 6
       (match (file->slist (format "cut_~s.ss" i))
	 [([,id* ,msgno* ,dropped* ,parent*] ...)
	  ;; Expected is just based on the length of time.  
	  ;; We ran it for 2 minutes at 40hz:
	  (define expected (* minutes-run 60 40))

	  (define nondropped (- (apply max msgno*) (apply min msgno*)))
	  ;(define dropped (- (apply max dropped*) (apply min dropped*)))
	  ;(define dropped2 (- expected nondropped))

	  (define deltas (map sub1 (diffs msgno*)))
	  (define _ (vector-set! alldeltas i deltas))

	  (define network_loss
	    ;; Note: having some problems with what appears to be message duplication.
	    ;; This results in consecutive messages that have the same ID.
	    (apply + (filter (lambda (delta) (and (< delta 200) (> delta 0))) deltas)))

	  (define adjusted_good_rate
	    
	    (let ([empirical_set (case i
				   [(3) (vector-ref alldeltas 1)] ;; HACK -- need to adjust data rate for real
				   [else deltas])])
	      (count-sliding-window (filter (lambda (d) (>= d 0)) empirical_set)
				    ;;(map (lambda (d) (if (< d 0) 0 d)) deltas) ;; Benefit of the doubt on these apparent duplicates
				    (list-ref number_messages (sub1 i)))))

	  ;; FIXME: This does not subtract out those duplicate packets.
	  ;(define nondropped (length msgno*))
	  
	  ;(fprintf (current-error-port) "diffs ~a\n\n" (filter (lambda (x) #t #|(not (zero? x))|#) deltas)) (flush-output-port)
	  
	  ;; Throw out any too large:
	  (printf "~s   ~s           ~s        ~s\n" i 		 
		  (percent (/ nondropped expected))
		  ;(percent (- 1 (/ network_loss (+ network_loss network_total))))
		  (percent (- 1 (/ network_loss (+ network_loss nondropped))))
		  (percent adjusted_good_rate) 
		  )]
	 ))
)
