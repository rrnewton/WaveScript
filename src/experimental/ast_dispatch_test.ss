#! /bin/sh
#|
exec regiment i --script "$0"
|#

;; The "datatype" variants: 

;; HACK: this must be the maximum used below:
(define numvariants #f)

(define (make-variants n) 
  (unique-name-counter 0)
  (list-build n (lambda _ (unique-name "a"))))

;(define variants (make-variants numvariants))
(define variants #f)

;(define test-depth 5)
(define test-depth 22)

(define foo #f)

(import rn-match)

(optimize-level 2)
(printf "# Optimize level: ~s\n" (optimize-level))

(define-syntax mytime
  (syntax-rules ()
    [(_ name e)
     (let* ([start (statistics)]
	    [result e]
	    [end (statistics)]
	    [diff (sstats-difference end start)])
       ;(printf "# name cpu real bytes gc-count gc-cpu gc-real gc-bytes\n")
       (printf "~a ~a ~a ~a ~a ~a ~a ~a\n" 
	       (pad-width 30 name)
	       (pad-width 6 (sstats-cpu diff))
	       (pad-width 6 (sstats-real diff)) 
	       (pad-width 10 (sstats-bytes diff))
	       (pad-width 4 (sstats-gc-count diff))
	       (pad-width 6 (sstats-gc-cpu diff))
	       (pad-width 6 (sstats-gc-real diff))
	       (pad-width 10 (sstats-gc-bytes diff)))
       (flush-output-port)
       result)]))


(define (random-variant)  (list-ref variants (random-variant-index)))
(define (random-variant-index) (random numvariants))

(define (print-header)
  (printf "# name variants                cpu    real   bytes    numgc  gccpu  gcreal gc-bytes\n"))

;; ============================================================
;; <-[ VERSION 1 ]->
;; List/match representation.

;(import iu-match)
(import rn-match)



(define list-bintree
  '(define (bintree n)    
     (if (zero? n)
	 `(,(random-variant) #f #f)
	 `(,(random-variant) ,(bintree (sub1 n)) ,(bintree (sub1 n))))))

;; We're running two different tests, rebuild the tree, or count the nodes in the tree:
(define list-reconstruct
  '(define initcode
     '(begin 
	(define baseval #f)
	(define-syntax combine (syntax-rules () [(_ variant l r) (list variant l r)])))))
(define list-sum
  '(define initcode
     '(begin 
	(define baseval 1)
	(define-syntax combine (syntax-rules () [(_ variant l r) (fx+ l r)])))))

(define list-match-dispatcher
  '(define (build-dispatcher x)
     `(match ,x
	[#f baseval] ;; Need to get rid of this..
	,@(map (lambda (variant)
		 `[(,variant ,',[left] ,',[right]) (combine ',variant left right)])
	    variants))))

(define list-cond-dispatcher
  '(define (build-dispatcher x)
    `(let loop ([x ,x])
       (if (not x) baseval ;; Need to get rid of this..
	   (let ([tag (car x)]) ;; This makes a big difference (shaves 1/4 off time)... CSE doesn't get it.
	     (cond
	      ,@(map (lambda (variant)
		       `[(eq? ',variant tag) ;  (vector-ref x 0)
			 (let ();([tail (cdr x)])
			   (combine ',variant
				    ;(loop (car  tail)) (loop (cadr tail))
				    (loop (cadr x)) (loop (caddr x))
				    ))])
		  variants)))))))

(define simple-run
  '(lambda (name)
    (set! foo (bintree test-depth))
    ;(printf "# ~a: Size in scheme objects: ~:d\n" name (count-nodes foo))
    (collect 4)
    (let* 
	([code `(lambda () ,initcode ,(build-dispatcher 'foo))]
	 ;[fun (mytime (format "# ~a-compile" name) (eval code))]
	 [fun (eval code)])
      ;(print-graph #f)(pp code)(inspect code)
      (mytime (format "~a ~a" name numvariants)
	      (fun)))))


;; ============================================================
;; <-[ VERSION 2 ]->   Vector/match representation.

(define vector-reconstruct
  '(define initcode
     '(begin 
	(define baseval #f)
	(define-syntax combine (syntax-rules () [(_ variant l r) (vector variant l r)])))))
(define vector-sum
  '(define initcode
     '(begin 
	(define baseval 1)
	(define-syntax combine (syntax-rules () [(_ variant l r) (fx+ l r)])))))

(define vector-bintree
  '(define (bintree n)    
     (if (zero? n)
	 `#(,(random-variant) #f #f)
	 `#(,(random-variant) ,(bintree (sub1 n)) ,(bintree (sub1 n))))))

(define vector-match-dispatcher
  '(define (build-dispatcher x)
     `(match ,x
	[#f baseval] ;; Need to get rid of this..
	,@(map (lambda (variant)
		 `[#(,variant ,',[left] ,',[right]) (combine ',variant left right)])
	    variants))))

(define vector-cond-dispatcher
  '(define (build-dispatcher x)
    `(let loop ([x ,x])
       (if (not x) baseval ;; Need to get rid of this..
	   (let ([tag (vector-ref x 0)]) ;; This makes a big difference (shaves 1/4 off time)... CSE doesn't get it.
	     (cond
	      ,@(map (lambda (variant)
		       `[(eq? ',variant tag) ;  (vector-ref x 0)
			 (combine ',variant
				  (loop (vector-ref x 1))
				  (loop (vector-ref x 2)))])
		  variants)))))))

;; ============================================================
;; <-[ VERSION 3 ]->   Record representation.


(define (record-init)
  `(begin
     ,@(map (lambda (variant)
	      `(define-record ,variant (left right)))
	 variants)))

(define record-reconstruct
  '(define initcode
     '(begin 
	(define baseval #f)
	(define-syntax combine (syntax-rules () [(_ variant l r) (variant l r)])))))
(define record-sum
  '(define initcode
     '(begin 
	(define baseval 1)
	(define-syntax combine (syntax-rules () [(_ variant l r) (fx+ l r)])))))


(define record-predicate-dispatcher
  '(begin     

     (define (bintree n)
       (eval 
	(let loop ([n n])
	  (define constructor (symbol-append 'make- (random-variant)))
	  (if (zero? n)
	      `(,constructor #f #f)
	      `(,constructor ,(loop (sub1 n)) ,(loop (sub1 n)))))))
     
     (define (build-dispatcher x)
       `(let loop ([x ,x])
	  (cond
	   [(not x) baseval] ;; Need to get rid of this..
	   ,@(map (lambda (variant)
		    `[(,(symbol-append variant '?) x) 
		      (combine ,(symbol-append 'make- variant)
		       (loop (,(symbol-append variant '-left) x))
		       (loop (,(symbol-append variant '-right) x)))])
	       variants))))
     ))
  
;; ============================================================
;; <-[ VERSION 4 ]->  Constant time dispatch vector, w/ vector syntax.


(define constant-dispatch
  '(begin 
     (define (bintree n)    
       (if (zero? n)
	   `#(,(random-variant-index) #f #f)
	   `#(,(random-variant-index) ,(bintree (sub1 n)) ,(bintree (sub1 n)))))
     (define (build-dispatcher x)
       `(let ()

	  (define (dispatch table x)
	    (if (not x) baseval ;; Need to get rid of this..
		((vector-ref table (vector-ref x 0))
		 (vector-ref x 1)
		 (vector-ref x 2))))
	  (define client
	    (letrec ([tab (vector 
			   ,@(mapi (lambda (ind variant)
				     `(lambda (left right) (vector ,ind (dispatch tab left) (dispatch tab right))))
				   variants)
			   )])
	      tab))
	  (lambda ()
	    (dispatch client ,x)))))
  )

(define run2
  '(lambda (name)
    (set! foo (bintree test-depth))
    ;(printf "# ~a: Size in scheme objects: ~:d\n" name (count-nodes foo))
    (collect 4)
    (let* 
	([code `(let () ,initcode ,(build-dispatcher 'foo))]
	 ;[fun (mytime (format "# ~a-compile" name) (eval code))]
	 [fun (eval code)])
      (mytime (format "~a ~a" name numvariants)
	      (fun)))))

;; ============================================================
;; <-[ VERSION 5 ]->  Logarithmic search 


(define log-dispatch
  '(begin 
    (define (bintree n)    
      (if (zero? n)
	  `#(,(random-variant-index) #f #f)
	  `#(,(random-variant-index) ,(bintree (sub1 n)) ,(bintree (sub1 n)))))
    (define (build-dispatcher x)
      `(let loop ([x ,x])
	 (if (not x) baseval ;; Need to get rid of this..
	   (let ([tag (vector-ref x 0)]) ;; This makes a big difference (shaves 1/4 off time)... CSE doesn't get it.	    
	     ,(let logloop ([numv numvariants] [offset 0])
		(if (= 1 numv)
		    `(combine ,offset
			      (loop (vector-ref x 1))
			      (loop (vector-ref x 2)))
		    (let ([half (quotient numv 2)])
		      `(if (fx< tag ,(+ offset half))
			   ,(logloop half offset)
			   ,(logloop (+ half (remainder numv 2)) (+ offset half))
			   ))))
	     ))))))

;; ============================================================

(define paramspace '(1 2 3 4 5 10 15 20 25 35 50 100 150 200))
;(define paramspace '(50))

(define consume
  (lambda ()
    (eprintf "\n# Running to consume trees \n")
    (printf "\n# Running to consume trees \n")
    (print-header)
    (for-each 
	(lambda (var)
	  (eprintf "Running with numvariants = ~a\n" var)
	  (set! numvariants var)
	  (set! variants (make-variants numvariants))
	  
 	  (eval '(import iu-match)) (eval `(let () ,list-sum ,list-bintree ,list-match-dispatcher (,simple-run "list-iu-match")))
 	  (eval '(import rn-match)) (eval `(let () ,list-sum ,list-bintree ,list-match-dispatcher (,simple-run "list-rn-match")))
 	  (eval `(let ()                           ,list-sum ,list-bintree ,list-cond-dispatcher  (,simple-run "list-cond")))
	  
 	  (eval '(import iu-match)) (eval `(let () ,vector-sum ,vector-bintree ,vector-match-dispatcher (,simple-run "vector-iu-match")))
 	  (eval '(import rn-match)) (eval `(let () ,vector-sum ,vector-bintree ,vector-match-dispatcher (,simple-run "vector-rn-match")))
 	  (eval `(let ()                           ,vector-sum ,vector-bintree ,vector-cond-dispatcher  (,simple-run "vector-cond")))

 	  (eval (record-init)) (eval `(let ()        ,record-sum ,record-predicate-dispatcher (,simple-run "record-predicates")))

 	  (eval `(let ()  ,vector-sum ,constant-dispatch  (,run2 "constant-dispatch")))

	  (eval `(let ()  ,vector-sum ,log-dispatch  (,simple-run "log-dispatch")))
	  )
      paramspace)))

(define rebuild
  (lambda ()
    (eprintf "\n# Running to consume and rebuild trees \n")
    (printf "\n# Running to consume and rebuild trees \n")
    (print-header)
    (for-each 
	(lambda (var)
	  (eprintf "Running with numvariants = ~a\n" var)
	  (set! numvariants var)
	  (set! variants (make-variants numvariants))
	  
	  (eval '(import iu-match)) (eval `(let () ,list-reconstruct ,list-bintree ,list-match-dispatcher (,simple-run "list-iu-match")))
	  (eval '(import rn-match)) (eval `(let () ,list-reconstruct ,list-bintree ,list-match-dispatcher (,simple-run "list-rn-match")))
	  (eval `(let ()                           ,list-reconstruct ,list-bintree ,list-cond-dispatcher  (,simple-run "list-cond")))
	  
	  (eval '(import iu-match)) (eval `(let () ,vector-reconstruct ,vector-bintree ,vector-match-dispatcher (,simple-run "vector-iu-match")))
	  (eval '(import rn-match)) (eval `(let () ,vector-reconstruct ,vector-bintree ,vector-match-dispatcher (,simple-run "vector-rn-match")))
	  (eval `(let ()                           ,vector-reconstruct ,vector-bintree ,vector-cond-dispatcher  (,simple-run "vector-cond")))

	  (eval (record-init)) (eval `(let ()        ,record-reconstruct ,record-predicate-dispatcher (,simple-run "record-predicates")))

	  (eval `(let ()  ,vector-reconstruct ,constant-dispatch  (,run2 "constant-dispatch")))
	  )
      paramspace)))


(define (runall)
 
  (define skew-factor 0.3)

  ;; First we run our tests for uniformly distributed variant-occurrences.
  (set! random-variant-index (lambda () (random numvariants)))
  (with-output-to-file  "consume_only.dat"  consume 'replace)
  (with-output-to-file  "rebuild.dat"  rebuild 'replace)

  ;; Next we test an exponentially skewed distribution of variants:
  (printf "\n  ## Testing Skewed Variant Distribution: ##\n")
  (set! random-variant-index
	(lambda () (let loop ([n 0])
		     (if (>= n numvariants)
			 (sub1 numvariants)
			 (if (<= (random 1.0) skew-factor) 
			     n (loop (fx+ 1 n)))))))

  (with-output-to-file  "skewed_consume_only.dat"  consume 'replace)
  (with-output-to-file  "skewed_rebuild.dat"  rebuild 'replace)

  (printf "\n  ## Testing REALLY Skewed Variant Distribution: ##\n")
  (set! skew-factor 0.51)

  (with-output-to-file  "moreskewed_consume_only.dat"  consume 'replace)
  (with-output-to-file  "moreskewed_rebuild.dat"  rebuild 'replace)
)

(system "rm -rf o2")
(system "mkdir o2")
(current-directory "o2")
(optimize-level 2)
(runall)

(current-directory "..")
(system "rm -rf o3")
(system "mkdir o3")
(current-directory "o3")
(optimize-level 3)
(runall)


#|

(define-datatype expr
  (a (left right))
  (b (left right)))

(let loop ([x x])
  (dispatch x
    [a (left right) (make-a (loop left) (loop right))]))

|#



;; <-[ VERSION 5 ]->



