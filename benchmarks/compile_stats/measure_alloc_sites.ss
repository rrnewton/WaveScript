#! /bin/bash
#|
exec regiment i --script "$0"
|#

(printf "Running script to catalog allocation sites")(flush-output-port)
(printf "================================================================================\n")

(regiment-quiet #t)

(include "./all-files.ss")
;(define all-files `("demos/wavescope/demo3g_arrays.ws"))
;(define all-files `("apps/marmot/run_marmot2.ws"))


(define (pad name)
  (define namestr (format "~a" name))
  (define padincr 40)
  (define padsize (- padincr (remainder (string-length namestr) padincr)))
  (define padding (make-string (if (= padsize padincr) 0 padsize) #\space))
  (format "~a~a" name padding))

#;
(define-pass catalog-allocs
  (define static 0)
  (define dyn-path 0)
  (define dyn-size 0)
  [Expr 
   (lambda (xp fallthru)
     (match xp 
       [(Array:make ',n )]
       [(if )])
     )])

;; Maintains a record of three numbers:
;;  1: Number of array-mallocs on the "trunk" control path, with static size.
;;  2: Number on trunk, but without static size.
;;  3: Number off trunk (within a branch or loop).
(define (catalog-allocs prog)
  (match prog
    [(,lang '(program ,bod ,_ ...))
     (OnTrunk bod)]))

(define (add ls k)  
  (if (null? ls) '(0 0 0 0) (apply map + ls)))
(define (const? x)
  (match (peel-annotations x)
    [',n #t]
    [,else #f]))

(define containers #f)

(define (flow-to-emit-only? var exp)
  (core-generic-traverse 
   (lambda (xp fallthru)
     (match xp
       [,v (guard (eq? v var)) 
	   ;(printf "ACK, escaped: ~s\n" v)
	   #f] ;; A failure!, escaped!
       
       ;; If it feeds into a primitive, that should be ok, EXCEPT for cons.     
       [(cons ,v ,ls) (guard (eq? v var)) (ASSERT simple-expr? ls) #f]
       ;; Also array:set can capture the pointer:
       [(Array:set ,_ ,__ ,v) (guard (eq? v var)) #f]
       ;; (TODO: HASHTABLES)
       ;; Anything else that needs to be ruled out? 

       ;; Any other primitive... it's fine if we flow into it:
       [(,prim ,rand* ...) (guard (regiment-primitive? prim))
	;(ASSERT (curry andmap simple-expr?)  rand*) 
	#t]

;        [(Array:set ,v ,[ind] ,[x]) (guard (eq? v var)) (and ind x)] ;; This is ok.
;        [(Array:ref ,v ,[ind]) (guard (eq? v var)) ind] ;; This is ok.
;        [(Array:length )]
;        ;; Anything else?

       [(emit ,_ ,v) (guard (eq? v var)) 
	;(printf "YEP it was emitted: ~s\n" v)
	#t] ;; This is excusable.
       [,oth (fallthru oth)]))
   (lambda (ls k)
     (andmap id ls))
   exp))

(define OnTrunk 
  (core-generic-traverse
   (lambda (xp fallthru)
     (match xp
       [(let ([,v ,lhs ,[peel-annotations -> rhs]]) ,bod)
	(map + (OnTrunk bod)
	     (match rhs
	       [(,make ,n . ,_) (guard (eq-any? make 'Array:make 'Array:makeUNSAFE))
		(print-level 4)
		(if (const? n)
		   `(1 0 0 ,(if (flow-to-emit-only? v bod) 1 0))
		   '(0 1 0 0))]
	       [,else (OnTrunk rhs)]))]
       [(,make ,n . ,_) (guard (eq-any? make 'Array:make 'Array:makeUNSAFE))
	(error 'OnTrunk "Missed at binding site: ~s" `(,make ,n . ,_))
	#;
	(if (const? n)
	    '(1 0 0)
	    '(0 1 0))]
       ;; These forms drive us off trunk:
       [(if ,[test] ,[(OffTrunk 'if) -> left] ,[(OffTrunk 'if) -> right])
	(map + test left right)] 
       [(for (,i ,[st] ,[en]) ,[(OffTrunk 'for) -> bod])
	(map + st en bod)]
       [(while ,[tst] ,[(OffTrunk 'while) -> bod])
	(map + tst bod)]
       [,oth (fallthru oth)]))
   add))

(define (OffTrunk insideof)
  (core-generic-traverse
   (lambda (xp fall)
     (match xp
       [(,make ,n . ,_) (guard (eq-any? make 'Array:make 'Array:makeUNSAFE))
	(set! containers (cons insideof containers))
	'(0 0 1 0)]
       [,oth (fall oth)]
       ))
   add))



;; Main:

(printf "Taking statistics on array allocation sites...\n")

(for-each (lambda (file)
	    (define path (** (REGIMENTD) "/" file))
	    (current-directory (dirname path))
	    (printf "  Processing: ~s\n" path)
	    ;(inspect (read-wavescript-source-file path))
	    ;(browse-stream (wsint path '()))
	    (call/cc (lambda (jumpout)
		       (set! containers '())
		       (ws-compiler-hooks 
			`([propagate-copies
			   ,(lambda (x)
			      ;(inspect x)
			      (define catalog (catalog-allocs x))
			      ;(pp (blaze-path-to/assq x 'Array:make 'Array:makeUNSAFE))
			      (printf " ALLOC: ~a ~a ~a ~a\n" 
				      (pad (basename path)) 
				      (apply string-append (insert-between " " (map number->string catalog)))
				      (program-ast-size x) containers)
			      (jumpout #f))]))
		       ;(wscomp path '() 'wsc2)
		       (wsint path '())
		       ))
	    )
  all-files)

(exit 0)