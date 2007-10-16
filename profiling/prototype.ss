#! /bin/sh
#|
exec regiment i "$0" ${1+"$@"};
|#

;exec regiment i "$0" `pwd` ${1+"$@"};
;exec regiment i --script $*

;;;; This file represents an experiment in automatically profiling the
;;;; efficiency of different data representations on a given backend
;;;; and hardware platform.


;================================================================================
;;; Generate WS code that can be passed to the compiler.

(printf "RUNNING\n")
(print-graph #f)


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

(define data-methods
  `([array ,build-array ,access-array]
    [tuple ,build-tuple ,access-tuple]
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
  (let-match ([(,_ ,build1 ,access1) (assq t1 data-methods)]
	      [(,_ ,build2 ,access2) (assq t2 data-methods)])
    `(begin 
       ,(timeit `(for (i 1 ,(inexact->exact (floor reps)))
		     ,(build1 xd `(lambda (_) ,(build2 yd '(lambda (i) i))))))
       )))

(define (full-2d-test-suite t1 t2)
  (define scale 0.1) ;; 1 = hundred million units
  `(begin
     ;; First stress allocate:
     (print "Stressing Allocation, big x big ")
     ;; Doing 100 million cells in each test:
     ,(build-2d-test (* scale 100) 1000 1000 t1 t2)
     (print "Stressing Allocation, verybig x small ")
     ,(build-2d-test (* scale 333) 100000 3 t1 t2)
     (print "Stressing Allocation, small x verybig ")
     ,(build-2d-test (* scale 333) 3 100000 t1 t2)
     ))


;; ================================================================================
;;; Running programs under different backends:

(define current-output-file (make-parameter #f))

(define (run-w/scheme prog)
  (printf "\nRUNNING WITH SCHEME\n")
  (printf "================================================================================\n")
  ;; The program exits via a wserror call, so we set this up:
  (parameterize ([wserror-handler
		  (lambda (str) (printf "wserror: ~a\n" str))])
    (pretty-print (stream-car (wsint prog))))
  )

(define (run-w/mlton prog)
  (printf "\nRUNNING WITH MLTON\n")
  (printf "================================================================================\n")
  (wsmlton prog)
  (printf "Compiling with mlton... ")
  (flush-output-port (current-output-port))
  (printf "finished (~a).\n" (system "wsmlton-secondphase query.sml &> /dev/null"))
  (flush-output-port (current-output-port))
  (system (format "./query.mlton.exe &> tmp_file.txt"))
  (display (file->string "tmp_file.txt"))
  )

(define (run-w/cpp prog)
  (printf "\nRUNNING WITH C++/XSTREAM\n")
  (printf "================================================================================\n")
  (wscomp prog)

  (printf "Compiling with g++... ") (flush-output-port (current-output-port))
  (printf "finished (~a).\n" (system "wsc-g++ query -O3 &> /dev/null"))
  ;(printf "finished (~a).\n" (system "wsc-g++ query "))
  (flush-output-port (current-output-port))
  (system (format "./query.exe -j 1 --at_once > tmp_file.txt"))
  (display (file->string "tmp_file.txt"))
  )

(define (run-all t1 t2)
  (define prog (execonce-boilerplate (full-2d-test-suite t1 t2)))
  (define filename (format "~a_~a.out" t1 t2))
  ;(define file1 (open-output-file filename 'replace))
  (define file1 (current-output-port))  
  (printf "Running all tests...\n\n") 
  ;(inspect prog)
  (parameterize ([current-output-file filename]
		 [current-output-port file1]
		 [ws-print-output-port file1])
    (run-w/scheme prog)
    (run-w/cpp prog)
    (run-w/mlton prog))
  ;(close-output-port file1)
  )

;; ================================================================================
;;; The main Script:

;(regiment-quiet #t)


;(run-all 'array 'array) ;; Nested arrays:
(run-all 'array 'tuple)


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