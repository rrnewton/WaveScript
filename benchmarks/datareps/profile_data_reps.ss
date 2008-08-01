#! /bin/sh
#|
exec regiment.plt i --script "$0" ${1+"$@"} -exit-error;
# USING PLT FOR NOW BECAUSE WE CANT GET TIMING INFO IN IKARUS
|#


;exec regiment i "$0" ${1+"$@"} -exit-error;

;(optimize-level 2)


;exec regiment i "$0" `pwd` ${1+"$@"};
;exec regiment i --script $*

;;;; This file represents an experiment in automatically profiling the
;;;; efficiency of different data representations on a given backend
;;;; and hardware platform.

;; This controls how much work we do:
;; Should do averaging... but that's the same as just running longer.
(define testscale (* 300 1000 1000)) ;; Took 10 min on faith.
;(define testscale (* 150 1000 1000))
;(define testscale (* 100 1000 1000)) ;; one hundred million cells
;(define testscale (* 50 1000 1000))
;(define testscale (* 1 1000 1000))


;================================================================================
;;; Generate WS code that can be passed to the compiler.

;; ================================================================================
;;; First the methods that build/access data structures.

(define (build-array n fn) `(Array:build ,n ,fn))
(define (access-array n) 9)
(define (fold-array len) 'Array:fold)

(define (build-tuple n fn) `(tuple ,@(list-build n (lambda (i) `(app ,fn ,i)))))
(define (access-tuple exp ind len)  `(tupref ,ind ,len ,exp))
(define (fold-tuple len) ;; Doesn't work for empty-tuple
  `(lambda (op init tup)
     ,(let loop ([n 0] [acc 'init])
	(if (= n len) acc
	    (loop (add1 n) `(app op ,acc (tupref ,n ,len tup)))))))
     
#;
     (op init
	 ,(match (iota len)
	    [(,a) `(tupref ,a ,len tup)]
	    [(,a . ,[b]) `(op (tupref ,a ,len tup) ,b)]))



;(define (build-list n))
;(define (access-list var ind))

;; This table contains [name ma-size builder accessor fold]
(define data-methods
  `([array ,(greatest-fixnum) ,build-array ,access-array ,fold-array]
    ;; Don't make tuples bigger than 500!
    [tuple 500 ,build-tuple ,access-tuple ,fold-tuple]
    ))


;; ================================================================================
;;; Next the code generation to build data structure tests.

;; This generates the basic boilerplate:
(define (execonce-boilerplate code)
  `(iterate (annotations) ;; [2007.11.02] Now with annotations.
	    (letrec ([first Bool '#t])
	      (lambda (,(unique-name 'x) vq) 
		(#() 'b)
		(if first
		    (begin ,code (set! first '#f) 
			   (emit vq (tuple))
			   ;; Uses error to exit immediately, not the best form:
			   ;; This also assumes depth first traversal, that's not good:
			   (wserror '"Exiting benchmark.")
			   vq)
		    vq)
		))
	    (timer '3.0)))

(define (timeit exp)
  `(let ([st1 (clock)])     
     (begin
       ,exp
       (let ([ellapsed1 (g- (clock) st1)])
	 (begin
	   (print (string-append (string-append "TimeElapsed: " (show ellapsed1)) "\n"))
	   ;; Do an extra little loop here, perhaps, to fix th MLton backends exiting early problem.
	   ;; Or need to be able to do a flush on the output port....
	   (for (i 0 80) (print " "))(print "\n"))
	 ))))




;(define implementation (make-parameter 'unknown))

;; This computes how many reps it should do to alloc a given number of cells.
(define (alloc-test t1 t2 xd yd)
  (define reps (max 1 (inexact->exact (floor (/ testscale xd yd)))))
  `(begin (print ,(format " Stressing-Allocation, ~a of ~a, ~a x ~a, ~a reps, " 
			  t1 t2 xd yd reps))
	  ,(let-match ([(,_ ,mx1 ,build1 ,access1 ,fold1) (assq t1 data-methods)]
		       [(,_ ,mx2 ,build2 ,access2 ,fold2) (assq t2 data-methods)])
	     (when (> xd mx1) (error 'alloc-test "~a exceeds max size ~a for type ~a" xd mx1 t1))
	     (when (> yd mx2) (error 'alloc-test "~a exceeds max size ~a for type ~a" yd mx2 t2))
	     `(begin 
		,(timeit `(for (i 1 ,(inexact->exact (floor reps)))
			      ,(build1 (min xd mx1) 
				       `(lambda (_) ,(build2 (min yd mx2) '(lambda (i) i))))))
		))))

(define (fold-test-withgrain t1 t2 xd yd)
  (define reps (max 1 (inexact->exact (floor (/ testscale xd yd)))))
  `(begin (print ,(format " Fold-with-grain ~a of ~a, ~a x ~a, ~a reps, "
			  t1 t2 xd yd reps))
	  ,(let-match ([(,_ ,mx1 ,build1 ,access1 ,fold1) (assq t1 data-methods)]
		      [(,_ ,mx2 ,build2 ,access2 ,fold2) (assq t2 data-methods)])
	    (when (> xd mx1) (error 'fold-test-withgrain "~a exceeds max size ~a for type ~a" xd mx1 t1))
	    (when (> yd mx2) (error 'fold-test-withgrain "~a exceeds max size ~a for type ~a" yd mx2 t2))
	    `(begin 
	       ,(timeit `(let ([data ,(build1 (min xd mx1) 
					      `(lambda (_) ,(build2 (min yd mx2) '(lambda (i) '1))))]
			       [result (Mutable:ref (assert-type Int64 (gint 0)))])
			   (begin
			     (for (i 1 ,reps)
			       (set! result 
				     (intToInt64
				      (app ,(fold1 xd)
					   (lambda (acc innerdat)
					     (_+_ acc (app ,(fold2 yd) _+_ 0 innerdat)))
					   0 data))))
			     (print (string-append (string-append "sum: " (show result)) " "))
			     )))
	       ))))

;; ================================================================================
;;; Running programs under different backends:

(define current-output-file (make-parameter #f))
(define tmp-file ".tmp_file.txt")

(define (run-w/scheme prog)
;  [implementation 'ws]
  (parameterize ()
    (printf "\nRUNNING WITH SCHEME\n")
    (printf "================================================================================\n")
    ;; The program exits via a wserror call, so we set this up:
    (parameterize ([wserror-handler
		    (lambda (str) (printf "wserror: ~a\n" str))])
      (let ([strm (wsint prog '())])
	(pretty-print (stream-car strm))
	))))

(define (run-w/mlton prog)
;  [implementation 'wsmlton]
  (parameterize ()
    (printf "\nRUNNING WITH MLTON\n")
    (printf "================================================================================\n")
    (wsmlton prog '())
    (printf "Compiling with mlton... ")
    (flush-output-port (current-output-port))
    (printf "finished (~a).\n" (system "wsmlton-secondphase query.sml &> /dev/null"))
					;(printf "finished (~a).\n" (system "wsmlton-secondphase query.sml"))
    (flush-output-port (current-output-port))
    (system (format "./query.mlton.exe &> tmp_file.txt"))
    (display (file->string "tmp_file.txt")))
  )

#;
(define (run-w/cpp prog)
;  [implementation 'wsc]
  (parameterize ()    
    ;; [2007.11.06] Getting segfaults with the new scheduler.
    ;(printf "\nRUNNING WITH C++/XSTREAM COREFIT_DF\n")
    ;(putenv "WAVESCOPED" (string-append (ASSERT (getenv "REGIMENTD")) "/benchmarks/engine/newest"))
    ;(wscomp prog '() '(scheduler corefit-scheduler-df))
    (printf "\nRUNNING WITH C++/XSTREAM 1495 DF\n")
    (putenv "WAVESCOPED" (string-append (ASSERT (getenv "REGIMENTD")) "/benchmarks/engine/1495"))
    (wscomp prog '(scheduler depth-first))

    
    (printf "================================================================================\n")

    (printf "Compiling with g++... ") (flush-output-port (current-output-port))
					;(printf "finished (~a).\n" (system "wsc-g++ query -O3 &> /dev/null"))
    (printf "finished (~a).\n" (system "wsc-g++ query "))
    (flush-output-port (current-output-port))
    (system (format "./query.exe -j 1 --at_once > ~a" tmp-file))
    (display (file->string tmp-file)))
  )

(define (run-all prog t1 t2)
  ;(define prog (execonce-boilerplate (full-2d-test-suite t1 t2)))
  (define filename (format "results/~a_~a.out" t1 t2))
  (define file1 (begin (when (file-exists? filename) (delete-file filename))
		       (open-output-file filename)))
  ;(define file1 (current-output-port))  
  (printf "\n\n *** Running all tests, data type: ~a of ~as *** \n\n" t1 t2)

  (fprintf file1 "\n\n *** Running all tests, data type: ~a of ~as *** \n\n" t1 t2)
  (system "date > tmp_file.txt")
  (system "uname -a >> tmp_file.txt")
  (display (file->string "tmp_file.txt") file1)

  ;(inspect prog)
  (with-output-to-port file1
    (lambda ()
      (parameterize ([current-output-file filename]
		     ;;[current-output-port file1]
		     [ws-print-output-port file1])
	(run-w/scheme prog) (flush-output-port (current-output-port))
	;;(run-w/cpp prog)    (flush-output-port)
	(run-w/mlton prog) (flush-output-port (current-output-port))
	)))
  (close-output-port file1)
  )

;; ================================================================================
;;; The main Script:

(printf "Running script to test data representations.\n")

(print-graph #f)
(regiment-verbosity 0)

(begin ;; TEST ALLOCATION:
  (printf "\n<<<< FIRST TESTING ALLOCATION >>>>\n\n")
  (run-all (execonce-boilerplate `(begin ,(alloc-test 'array 'array 1000 1000)
					   ,(alloc-test 'array 'array 100000 3)
					   ,(alloc-test 'array 'array 3 100000)
					   ))           'array 'array)

  ;; Don't do really big tuples
  ;(run-all (execonce-boilerplate `(begin ,(bigbig 'array 'tuple) ,(bigsmall 'array 'tuple))) 'array 'tuple)
  (run-all (execonce-boilerplate `(begin ,(alloc-test 'array 'tuple 500 500)
					 ,(alloc-test 'array 'tuple 83333 3))) 
	                                              'array 'tuple)
  ;; ACK, all of a sudden this takes 10 MINUTES to compile on MLTON.
  ;; Something's wrong with emit-mlton in particular.
  #;
  (run-all (execonce-boilerplate `(begin ,(alloc-test 'tuple 'array 500 500) 
					 ,(alloc-test 'tuple 'array 3 83333)))
                                        	      'tuple 'array)
  (run-all (execonce-boilerplate `(begin ,(alloc-test 'tuple 'array 70 70)
					 ,(alloc-test 'tuple 'array 3 83333)))
	                                              'tuple 'array)
  (system "mv results/*.out results/alloc/")
) ;; End alloc test


(begin ;; TEST SEQUENTIAL READ ACCESS (FOLD):
  (printf "\n<<<< NOW TESTING SEQUENTIAL ACCESS >>>>\n\n")
  (run-all (execonce-boilerplate `(begin ,(fold-test-withgrain 'array 'array 1000 1000)
					 ,(fold-test-withgrain 'array 'array 100000 3)
					 ,(fold-test-withgrain 'array 'array 3 100000)
					 ))                    'array 'array)

  (run-all (execonce-boilerplate `(begin ;,(fold-test-withgrain 'tuple 'array 500 500)
				          ,(fold-test-withgrain 'tuple 'array 70 70)
					 ,(fold-test-withgrain 'tuple 'array 3 83333)
					 ))                    'tuple 'array)
  (run-all (execonce-boilerplate `(begin ,(fold-test-withgrain 'array 'tuple 70 70)
					 ,(fold-test-withgrain 'array 'tuple 83333 3)
					 ))                    'array 'tuple)
  (system "mv results/*.out results/fold/"))
			               


;(run-all 'array 'array) ;; Nested arrays:
;(run-all 'array 'tuple)
;(run-all 'tuple 'array)

(exit)
