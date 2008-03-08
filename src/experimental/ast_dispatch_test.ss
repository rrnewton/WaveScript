#! /bin/sh
#|
exec regiment i --script "$0"
|#

;; The "datatype" variants: 

(unique-name-counter 0)

(define numvariants 200)

(define variants (list-build numvariants (lambda _ (unique-name "a"))))

;(define test-depth 5)
(define test-depth 20)

(define foo #f)

(optimize-level 2)
(printf "Optimize level: ~s\n" (optimize-level))

;; ============================================================
;; <-[ VERSION 1 ]->
;; List/match representation.

;(import iu-match)
(import rn-match)

(let () 
  (define (bintree n)    
    (if (zero? n)
	`(,(list-get-random variants) #f #f)
	`(,(list-get-random variants) ,(bintree (sub1 n)) ,(bintree (sub1 n)))))

#;
  (define (build-dispatcher x)
    `(match ,x
	 [#f #f] ;; Need to get rid of this..
	 ,@(map (lambda (variant)
		 `[(,variant ,',[left] ,',[right]) (list ',variant left right)])
	    variants))
    )

  (define (build-dispatcher x)
    `(let loop ([x ,x])
       (if (not x) #f ;; Need to get rid of this..
	   (let ([tag (car x)]) ;; This makes a big difference (shaves 1/4 off time)... CSE doesn't get it.
	     (cond
	      ,@(map (lambda (variant)
		       `[(eq? ',variant tag) ;  (vector-ref x 0)
			 (let ();([tail (cdr x)])
			   (list ',variant
				 ;(loop (car  tail)) (loop (cadr tail))
				 (loop (cadr x)) (loop (caddr x))
				 ))])
		  variants))))))

  (collect 4)
  (printf "\nTesting List Representation:\n")
  (set! foo (bintree test-depth))
  (printf "Size: ~:d\n" (count-nodes foo))
  (printf "  Timing compile:\n")
  (collect 4)
  (let ([fun (time (eval `(lambda () ,(build-dispatcher 'foo))))])
    (printf "  Timing dispatch:\n")
    (time (fun)))
  ) ;; End list rep


;; ============================================================
;; <-[ VERSION 2 ]->   Vector/match representation.

(let () 
  (define (bintree n)    
    (if (zero? n)
	`#(,(list-get-random variants) #f #f)
	`#(,(list-get-random variants) ,(bintree (sub1 n)) ,(bintree (sub1 n)))))

#;
  (define (build-dispatcher x)
    `(match ,x
	 [#f #f] ;; Need to get rid of this..
	 ,@(map (lambda (variant)
		 `[#(,variant ,',[left] ,',[right]) (vector ',variant left right)])
	    variants))
    )

  (define (build-dispatcher x)
    `(let loop ([x ,x])
       (if (not x) #f ;; Need to get rid of this..
	   (let ([tag (vector-ref x 0)]) ;; This makes a big difference (shaves 1/4 off time)... CSE doesn't get it.
	     (cond
	      ,@(map (lambda (variant)
		       `[(eq? ',variant tag) ;  (vector-ref x 0)
			 (vector ',variant
				 (loop (vector-ref x 1))
				 (loop (vector-ref x 2)))])
		  variants))))))

  (printf "\nTesting Vector Representation:\n")
  (set! foo (bintree test-depth))
  (printf "Size: ~:d\n" (count-nodes foo))
  (printf "  Timing compile:\n")
;(print-graph #f)(inspect (build-dispatcher 'foo))
  (collect 4)
  (let ([fun (time (eval `(lambda () ,(build-dispatcher 'foo))))])
    (printf "  Timing dispatch:\n")
    (time (fun)))
  
  )


;; ============================================================
;; <-[ VERSION 3 ]->   Record representation.

(let () 
  (define (build-datatype)
    `(begin
       ,@(map (lambda (variant)
		`(define-record ,variant (left right)))
	   variants)))
  
  (define (bintree n)
    (define constructor (symbol-append 'make- (list-get-random variants)))
    (if (zero? n)
	`(,constructor #f #f)
	`(,constructor ,(bintree (sub1 n)) ,(bintree (sub1 n)))))

  (define (build-dispatcher x)
    `(let loop ([x ,x])
       (cond
	 [(not x) #f] ;; Need to get rid of this..
	 ,@(map (lambda (variant)
		 `[(,(symbol-append variant '?) x) 
		   (,(symbol-append 'make- variant)
		    (loop (,(symbol-append variant '-left) x))
		    (loop (,(symbol-append variant '-right) x)))])
	    variants))))

  (printf "\nTesting Record Representation:\n")
  ;; Install datatype defs:
  (eval (build-datatype))
  (set! foo (eval (bintree test-depth)))
  (printf "  Timing compile:\n")
  ;(inspect foo)
  ;(print-graph #f)
  ;(pp (build-dispatcher 'foo))
  ;(inspect (eval (build-dispatcher 'foo)))
  (collect 4)
  (let ([fun (time (eval `(lambda () ,(build-dispatcher 'foo))))])
    (printf "  Timing dispatch:\n")
    (time (fun)))
  
  )


;; ============================================================
;; <-[ VERSION 4 ]->  Constant time dispatch vector, w/ vector syntax.

(let () 
  (define (bintree n)    
    (if (zero? n)
	`#(,(random numvariants) #f #f)
	`#(,(random numvariants) ,(bintree (sub1 n)) ,(bintree (sub1 n)))))


  (define (build-dispatcher x)
    `(let ()

       (define (dispatch table x)
	 (if (not x) #f ;; Need to get rid of this..
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
	 (dispatch client ,x))))

  (printf "\nTesting Constant-Dispatch Vector Representation:\n")
  (set! foo (bintree test-depth))
  (printf "Size: ~:d \n" (count-nodes foo))
  (printf "  Timing compile:\n")
;(print-graph #f)(inspect (build-dispatcher 'foo))
  (collect 4)
  (let ([fun (time (eval (build-dispatcher 'foo)))])
    (printf "  Timing dispatch:\n")
    (time (fun)))  
  )


;; ============================================================


#|

(define-datatype expr
  (a (left right))
  (b (left right)))

(let loop ([x x])
  (dispatch x
    [a (left right) (make-a (loop left) (loop right))]))

|#



;; <-[ VERSION 5 ]->



