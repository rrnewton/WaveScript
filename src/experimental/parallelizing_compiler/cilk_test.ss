

(eval-when (compile eval load) 
  ;(optimize-level 3)
  (optimize-level 2)
  ;(optimize-level 0)
  ;(collect-trip-bytes (* 20 1048576)) ;; collects 47 times in ~3 sec
  )

;(include "chez_threaded_utils.ss")
;(import threaded_utils)
;(import (par6))
(import (cilk))

;; ====================================================================================================
;; Example programs

(define prog1
  '(cilk 
    (a)
    (define b (spawn foo 3 4))
    (define c (empty-ivar))
    (spawn bar 5 6)
    (e)
    (define cresult (read-ivar c))
    (f cresult)
    (sync)
    (g cresult)
    (baz b d)
;    (sync)
    ))

(define prog2
  '(cilk   
    (define i1 (empty-ivar))
    
    (spawn (lambda () (set-ivar! i1 33)))
    
    (define r1 (read-ivar i1))
    (printf "yay ~s\n" r1)
    (sync)
    r1 
    ))

;; ====================================================================================================

;; A syntax for writing Cilk-like programs.
(define-syntax cilk
  (lambda (x)
    (define (gather-spawn-vars cmds)
      (syntax-case cmds (spawn define)
	[() '()]
        [((spawn f ..) rest ...) (gather-spawn-vars #'(rest ...))]
        [((define var _) rest ...)
	 ;;((define var (spawn f ...)) rest ...)
	 (cons #'var (gather-spawn-vars #'(rest ...)))]
        [(_ rest ...) (gather-spawn-vars #'(rest ...))]))

    (define magic-val #f)

    ;; We convert a subsequence of the cilk construct (until the next sync point)
    ;; We also get a list of all the subsequent fork/join blocks within the cilk construct:
    (define (convert-cmds cmds subsequent-blocks)
      (define (make-pcall rest right)
	#`(pcall ;(lambda (a b) (void)) ;; Nothing to do after join.
	         ;(lambda (a b) (if (eq? b '#,magic-val) a b))
		 ;;
		 (lambda (a b) a)
		 ;; The right hand side is the "spawn".  Given the current syntax the
		 ;; spawn cannot contain a read. (Without another (cilk...) block, that is.)

		 ((lambda (_) #,(convert-cmds rest subsequent-blocks)) 'ignored) ;; continuation
		 #,right))
      (syntax-case cmds (spawn sync define read-ivar)
        [() #'(void)]
        [((spawn f args ...) rest ...) 
	  (make-pcall #'(rest ...) #'(f args ...))]

        [((define var (spawn f args ...)) rest ...)
	  (make-pcall #'(rest ...) #'(set! var (f args ...)))]

	[((define var (read-ivar ivar)) rest ...)
	 #`(let ((kont (trace-lambda var (tmp)
	                 (set! var tmp)
			 #,(convert-cmds #'(rest ...) subsequent-blocks)
			 ;; FIXME: It is NOT VALID to jump to the next chunk before the sync is finished!!
			 ; #,(guarded-exec-chunks subsequent-blocks)
			 )))
	     (ivar-apply-or-block kont ivar) ; '#,magic-val
	     )]

	;; This allows variable bindings within a Cilk block.
	[((define var other) rest ...) 
	  #`(begin (set! var other) #,(convert-cmds #'(rest ...) subsequent-blocks))]

	[(sync) (error 'cilk "internal error")]
        [(other) #'other]
        [(other rest ...)
	 #`(begin other 
	 	  #,(convert-cmds #'(rest ...) subsequent-blocks))]))

    (define (guarded-exec-chunks names)
      (if (null? (cdr names))
	  #`(#,(car names))
	  #`(let ((#,(car names) (#,(car names))))
	      (if (eq? '#,magic-val #,(car names))
		  '#,magic-val
		  #,(guarded-exec-chunks (cdr names))))))
      
    ;; Returns a list of lists of commands, separated by the syncs (join points).
    (define (chop-at-syncs cmds)
      (syntax-case cmds (sync)
	[() '(())]
        [((sync) rest ...)
	 (cons '() (chop-at-syncs #'(rest ...)))]
        [(other rest ...) 
	 (let ((segs (chop-at-syncs #'(rest ...))))
	 (cons (cons #'other (car segs))
	       (cdr segs)))]))

    ;; Main body: transform a (cilk ...) construct:
    (syntax-case x (spawn sync define)
      [(cilk cmds ...)
       (set! magic-val (datum->syntax #'cilk (gensym "chunkfailed")))

       (let* ([vars (gather-spawn-vars #'(cmds ...))]
              [blocks (chop-at-syncs #'(cmds ...))]
	      [names (map (lambda (n) (datum->syntax #'cilk
				       (string->symbol (string-append "chunk" (number->string n)))
				       ))
		        (iota (length blocks))
			;(syntax->list #'(cmds ...))
			)]
	      [exprs (let loop ([ls blocks] [names names])
		       (if (null? ls) '()
			   (cons (convert-cmds (car ls) (cdr names))
				 (loop (cdr ls) (cdr names)))))])
        #`(let #,(map (lambda (v) (list v (syntax 'cilk-var-uninit))) vars)
	    #,@exprs

	   ;; This (as yet unused) version is for packaging up each
	   ;; fork/join block as a separate function (to assemble the
	   ;; continuation piecemeal).
	   #;
	    (letrec #,(map (lambda (v e) #`(#,v (lambda () #,e))) names exprs)
	       ;(and #,@(map (lambda (name) #`(not (eq? '#,magic-val (#,name)))) names))
	       #,(guarded-exec-chunks names)
	    )))
      ])))

;; ====================================================================================================

;(pretty-print (expand prog1))
(print-gensym #f)
;(pretty-print (expand prog1))
;(pretty-print (expand prog2))
;(pretty-print (expand '(cilk (spawn printf "   print from cilk test\n") 44)))
;(exit)

;; ====================================================================================================

(define-syntax ASSERT
  (lambda (x)
    (syntax-case x ()
      [(_ expr) #'(or expr (error 'ASSERT (format "failed: ~s" #'expr)))])))

(define (cilkfib n)
  (if (fx< n 2) 1
	(cilk    
	 (define left (spawn cilkfib (fx- n 1)))
	 (define right (cilkfib (fx- n 2)))
	 (sync)
	 (fx+ left right))))

(init-par 2)


(define counter 0)
(define (test msg result fn)
  (set! counter (add1 counter))
  (printf "================================================================================\n")
  (printf "Test #~s: ~a\n" counter msg) (flush-output-port)
  ;(ASSERT (eq? (fn) result))
  (let ((val (fn)))
    (if (equal? result val)
	(printf "Test #~s: PASSED!\n\n" counter)
	(error 'unit-test (format "Test #~s: FAILED, expected ~s received ~s!\n" counter result val))))
  )


(test "cilk simple test 1" 33 (lambda () (cilk 33)))
(test "cilk simple test 2" 44 (lambda () (cilk (spawn printf "   print from cilk test\n") 44)))

(test "cilk FIB 20" 10946 (lambda () (cilkfib 20)))

(test "Cilk Ivar1 on one thread" 33 
      (lambda () (cilk    
		  (define iv (empty-ivar))
		  (set-ivar! iv 33)
		  (define x  (read-ivar iv))
		  (sync)
		  x)))

(test "Cilk Ivar with blocking"  33
      (lambda () (cilk    
		  (define iv (empty-ivar))
		  (spawn (lambda () (let loop ((i 1000000)) (unless (= 0 i) (loop (sub1 i))))
				 (set-ivar! iv 33)))
		  (define x  (read-ivar iv))
		  (printf " *** Woo, read completed! ~s\n" x)
		  'yay
		  (sync)
		  x)))

;; [2010.11.01] This triggers my current duplicate-execute-read-continuation bug.
(test "Cilk Ivar with blocking 2"  132
      (lambda () (cilk    
		  (define iv (empty-ivar))
		  (spawn (lambda () (let loop ((i 1000000)) (unless (= 0 i) (loop (sub1 i))))
				 (set-ivar! iv 33)))
		  (define iv2 (empty-ivar))
		  (define x  (read-ivar iv))
		  (printf " *** Woo, read completed! ~s (TID ~s)\n" x (get-thread-id))
		  (set-ivar! iv2 99)
		  'yay
		  (define y (read-ivar iv2))
		  (sync)
		  (+ x y))))

;; Example from William Leiserson that deadlocks if ivar-blocked continuations are not stealable.
(define (willexample)
  (define i1 (empty-ivar))
  (define i2 (empty-ivar))
  (define (foo)
    (cilk (let loop ((i 1000000)) (unless (= 0 i) (loop (sub1 i)))) ;; expensive
          (printf "*** Foo (TID ~s): Done with work now writing I1\n" (get-thread-id))
          (set-ivar! i1 33)))
  (define (snafoo)
    (cilk (define x1 (read-ivar i1))
          (printf "*** Snafoo (TID ~s): Done read I1, now writing I2\n" (get-thread-id))
	  (set-ivar! i2 (+ x1 44))))
  (define (quux)
    (cilk (spawn snafoo)
	  (define y1 (read-ivar i2))
          (printf "*** Quux (TID ~s): done reading I2\n" (get-thread-id))
	  ))
  (define (baz)
    (cilk (spawn foo)
	  (spawn quux)
          (printf "*** Bar (TID ~s): after spawns\n" (get-thread-id))
	  (define x2 (read-ivar i1))
          (printf "*** Bar (TID ~s): done reading I1\n" (get-thread-id))
	  (define y2 (read-ivar i2))
          (printf "*** Bar (TID ~s): done reading I2\n" (get-thread-id))
	  (par-status 'verbose)
	  (sync)
          (printf "*** Bar (TID ~s): Return final value... \n" (get-thread-id))
	  (* x2 y2)))
  (baz)
  )

(test "William Leiserson's example" 2541 (lambda () (willexample)))



#;
(pretty-print
 (let cilkfib ((n 10))
   (if (fx< n 2) 1
       (let ([l #f]
	     [r #f])
	 (pcall
	  (lambda (_ x) (set! l x))
	  ((lambda (_) (set! r (cilkfib (#2%fx- n 2)))) #f)
	  (cilkfib (#2%fx- n 1)))
	 (#2%fx+ l r)))))

#;
;; This could be better, let letrec do the let-n-set if it must.
(pretty-print
 (let cilkfib ((n 20))
   (if (< n 2) 1
       (let ([r #f])
	 (letrec ((l 
		   (pcall
		    (lambda (_ x) x)
		    ((lambda (_) (set! r (cilkfib (#2%fx- n 2)))) #f)
		    (cilkfib (#2%fx- n 1)))))
	   (#2%fx+ l r))
	 ))))
#;
(pretty-print
 (let cilkfib ((n 10))
   (if (< n 2) 1
       (pcall
	fx+ 
	((lambda (_) (cilkfib (#2%fx- n 2))) #f)
	(cilkfib (#2%fx- n 1))))))


(exit)
