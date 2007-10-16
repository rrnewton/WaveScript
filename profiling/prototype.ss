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
       ,(timeit `(for (i 1 ,reps) ,(build1 xd `(lambda (_) ,(build2 yd '(lambda (i) i))))))       
       )))

(define (full-2d-test-suite t1 t2)
  `(begin
     ;; First stress allocate:
     (print "Stressing Allocation, big x big ")
     ;; Doing 100 million cells in each test:
     ,(build-2d-test 100 1000 1000 t1 t2)
     (print "Stressing Allocation, verybig x small ")
     ,(build-2d-test 333 100000 3 t1 t2)
     (print "Stressing Allocation, small x verybig ")
     ,(build-2d-test 333 3 100000 t1 t2)
     ))


(define prog (execonce-boilerplate (full-2d-test-suite 'array 'array)))

;; ================================================================================
;;; Running programs under different backends:


(define (run-w/scheme prog)
  (printf "\nRUNNING WITH SCHEME\n")
  (printf "================================================================================\n")
  (pretty-print (stream-car (wsint prog))))

(define (run-w/mlton prog)
  (printf "\nRUNNING WITH MLTON\n")
  (printf "================================================================================\n")
  (wsmlton prog)
  (printf "Compiling with mlton... ")
  (flush-output-port (current-output-port))
  (format "finished (~a).\n"(shell "wsmlton-secondphase query.sml &> /dev/null"))
  (flush-output-port (current-output-port))
  (shell "./query.mlton.exe")
  )

(printf "Running all tests...\n\n")
(regiment-quiet #t)
(run-w/scheme prog)
(run-w/mlton prog)
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