#! /bin/sh
#|
exec regiment.opt i "$0" ${1+"$@"};
|#

;exec regiment i "$0" `pwd` ${1+"$@"};
;exec regiment i --script $*

;;;; This file represents an experiment in automatically profiling the
;;;; efficiency of different data representations on a given backend
;;;; and hardware platform.


;================================================================================
;;; Generate WS code that can be passed to the compiler.

;; ================================================================================
;;; First the methods that build/access data structures.

(define (build-array n fn) `(Array:build ,n ,fn))

(define (access-array n) 9)

(define (build-tuple n fn) `(tuple ,@(list-build n (lambda (i) `(app ,fn ,i)))))
(define (access-tuple exp ind len)
    #;
    (let* ([tmp (unique-name 'tmp)]
	   [pat (list-build len (lambda (i) (if (= i ind) tmp '_)))])
      `(let ([,pat 'unknown ,exp]) tmp))
    `(tupref ,ind ,len ,exp)
    )

;(define (build-list n))
;(define (access-list var ind))

;; This table contains [name ma-size builder accessor]
(define data-methods
  `([array ,(most-positive-fixnum) ,build-array ,access-array]
    ;; Don't make tuples bigger than 500!
    [tuple 500 ,build-tuple ,access-tuple]
    ))


;; ================================================================================
;;; Next the code generation to build data structure tests.

;; This generates the basic boilerplate:
(define (execonce-boilerplate code)
  `(iterate (letrec ([first Bool '#t])
	      (lambda (,(unique-name 'x) vq) 
		;(#() (VQueue #()))
		(#() 'b)
		(if first
		    (begin ,code (set! first '#f) 
			   (emit vq (tuple))
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
	 (print (string-append (string-append "TimeElapsed: " (show ellapsed1)) "\n"))))))

(define (build-2d-test reps xd yd t1 t2)
  (let-match ([(,_ ,mx1 ,build1 ,access1) (assq t1 data-methods)]
	      [(,_ ,mx2 ,build2 ,access2) (assq t2 data-methods)])
    (when (> xd mx1) (error 'build-2d-test "~a exceeds max size ~a for type ~a" xd mx1 t1))
    (when (> yd mx2) (error 'build-2d-test "~a exceeds max size ~a for type ~a" yd mx2 t2))
    `(begin 
       ,(timeit `(for (i 1 ,(inexact->exact (floor reps)))
		     ,(build1 (min xd mx1) 
			      `(lambda (_) ,(build2 (min yd mx2) '(lambda (i) i))))))
       )))

(define testscale (* 150 1000 1000))
;(define testscale (* 100 1000 1000)) ;; one hundred million cells
;(define testscale (* 50 1000 1000))
;(define testscale (* 1 1000 1000))

#|
;; Doing ~100 million cells in each test:
(define (bigbig t1 t2) 
  `(begin (print "Stressing Allocation, big x big ")
	  ,(build-2d-test (* testscale 100) 1000 1000 t1 t2)))
(define (bigsmall t1 t2)
  `(begin (print "Stressing Allocation, verybig x small ")
	  ,(build-2d-test (* testscale 333) 100000 3 t1 t2)))
(define (smallbig t1 t2)   
  `(begin (print "Stressing Allocation, small x verybig ")
	 ,(build-2d-test (* testscale 333) 3 100000 t1 t2)))
(define (full-2d-test-suite t1 t2)  
  `(begin
     ;; First stress allocate:
     ,(bigbig   t1 t2)
     ,(bigsmall t1 t2)
     ,(smallbig t1 t2)
     ))
|#

(define implementation (make-parameter 'unknown))

;; This computes how many reps it should do to alloc a given number of cells.
(define (alloc-test t1 t2 xd yd)
  (define reps (max 1 (inexact->exact (floor (/ testscale xd yd)))))
  `(begin (print ,(format "~a : Stressing Allocation, ~a of ~a, ~a x ~a, ~a reps, " 
			  (implementation) t1 t2 xd yd reps))
	  ,(build-2d-test reps xd yd t1 t2)))

(define (full-2d-test-suite t1 t2)  
  `(begin
     ;; First stress allocate:
     ,(alloc-test t1 t2 1000 1000)
     ,(alloc-test t1 t2 100000 3)
     ,(alloc-test t1 t2 3 100000)))

;; ================================================================================
;;; Running programs under different backends:

(define current-output-file (make-parameter #f))
(define tmp-file ".tmp_file.txt")

(define (run-w/scheme prog)
  [implementation 'ws]
  (parameterize ()
    (printf "\nRUNNING WITH SCHEME\n")
    (printf "================================================================================\n")
    ;; The program exits via a wserror call, so we set this up:
    (parameterize ([wserror-handler
		    (lambda (str) (printf "wserror: ~a\n" str))])
      (pretty-print (stream-car (wsint prog))))))

(define (run-w/mlton prog)
  [implementation 'wsmlton]
  (parameterize ()
    (printf "\nRUNNING WITH MLTON\n")
    (printf "================================================================================\n")
    (wsmlton prog)
    (printf "Compiling with mlton... ")
    (flush-output-port (current-output-port))
    (printf "finished (~a).\n" (system "wsmlton-secondphase query.sml &> /dev/null"))
					;(printf "finished (~a).\n" (system "wsmlton-secondphase query.sml"))
    (flush-output-port (current-output-port))
    (system (format "./query.mlton.exe &> tmp_file.txt"))
    (display (file->string "tmp_file.txt")))
  )

(define (run-w/cpp prog)
  [implementation 'wsc]
  (parameterize ()
    (printf "\nRUNNING WITH C++/XSTREAM\n")
    (printf "================================================================================\n")
    (wscomp prog)

    (printf "Compiling with g++... ") (flush-output-port (current-output-port))
					;(printf "finished (~a).\n" (system "wsc-g++ query -O3 &> /dev/null"))
    (printf "finished (~a).\n" (system "wsc-g++ query "))
    (flush-output-port (current-output-port))
    (system (format "./query.exe -j 1 --at_once > ~a" tmp-file))
    (display (file->string tmp-file)))
  )

(define (run-all prog t1 t2)
  ;(define prog (execonce-boilerplate (full-2d-test-suite t1 t2)))
  (define filename (format "~a_~a.out" t1 t2))
  (define file1 (open-output-file filename 'replace))
  ;(define file1 (current-output-port))  
  (printf "\n\n *** Running all tests, data type: ~a of ~as *** \n\n" t1 t2)

  (fprintf file1 "\n\n *** Running all tests, data type: ~a of ~as *** \n\n" t1 t2)
  (system "date > tmp_file.txt")
  (system "uname -a >> tmp_file.txt")
  (display (file->string "tmp_file.txt") file1)

  ;(inspect prog)
  (parameterize ([current-output-file filename]
		 [current-output-port file1]
		 [ws-print-output-port file1])
    (run-w/scheme prog) (flush-output-port)
    (run-w/cpp prog)    (flush-output-port)
    (run-w/mlton prog) (flush-output-port)
    )  
    (close-output-port file1)
  )

;; ================================================================================
;;; The main Script:

(printf "Running script to test data representations.\n")

(print-graph #f)
(regiment-quiet #t)

;(define prog (execonce-boilerplate (full-2d-test-suite 'array 'array)))
;(inspect prog)(run-all prog)
(run-all (execonce-boilerplate (full-2d-test-suite 'array 'array)) 'array 'array)

;; Don't do really big tuples
;(run-all (execonce-boilerplate `(begin ,(bigbig 'array 'tuple) ,(bigsmall 'array 'tuple))) 'array 'tuple)
(run-all (execonce-boilerplate `(begin ,(alloc-test 'array 'tuple 500 500)
				       ,(alloc-test 'array 'tuple 83333 3))) 
	                                            'array 'tuple)

;; ACK, all of a sudden this takes 10 MINUTES to compile on MLTON.
#;
(run-all (execonce-boilerplate `(begin ,(alloc-test 'tuple 'array 500 500) 
				       ,(alloc-test 'tuple 'array 3 83333)))
	                                            'tuple 'array)

(run-all (execonce-boilerplate `(begin ,(alloc-test 'tuple 'array 70 70)
				       ,(alloc-test 'tuple 'array 3 83333)))
	                                            'tuple 'array)


;(run-all 'array 'array) ;; Nested arrays:
;(run-all 'array 'tuple)
;(run-all 'tuple 'array)


(exit)

#;
(define prog 
  (execonce-boilerplate 
   `(begin 
      (print '"woot\n")
      
      (let ([arr (Array #(Int Int)) (Array:build 10 
				     (lambda (i) (Int) (tuple 2 3)))])
	
	(emit vq arr)
	)
      
      ;(emit vq (tuple))
      )
   ))