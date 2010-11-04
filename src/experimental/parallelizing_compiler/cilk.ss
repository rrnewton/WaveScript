

(eval-when (compile eval load) 
  ;(optimize-level 3)
  (optimize-level 2)
  (collect-trip-bytes (* 20 1048576)) ;; collects 47 times in ~3 sec
  )

(include "chez_threaded_utils.ss")
(import threaded_utils)


;; ====================================================================================================
;; Example programs

(define prog1
  '(cilk 
    (a)
    (define b (spawn foo 3 4))
    (define cresult (c))
    (spawn bar 5 6)
    (e)
    (f)
    (sync)
    (g)
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

    ;; We convert a subsequence of the cilk construct (until the next sync point)
    ;; We also get a list of all the subsequent fork/join blocks within the cilk construct:
    (define (convert-cmds cmds subsequent-blocks)
      (define (make-pcall rest right)
	#`(pcall (lambda (a b) (void)) ;; Nothing to do after join.
		 ((lambda (_) #,(convert-cmds rest subsequent-blocks)) 'ignored) ;; continuation
		 #,right))
      (syntax-case cmds (spawn sync define)
        [() #'(void)]
        [((spawn f args ...) rest ...) 
	  (make-pcall #'(rest ...) #'(f args ...))]

        [((define var (spawn f args ...)) rest ...)
	  (make-pcall #'(rest ...) #'(set! var (f args ...)))]

	;; This allows variable bindings within a Cilk block.
	[((define var other) rest ...) 
	  #`(begin (set! var other) #,(convert-cmds #'(rest ...) subsequent-blocks))]
	[(sync) (error 'cilk "internal error")]
        [(other) #'other]
        [(other rest ...)
	 #`(begin other 
	 	  #,(convert-cmds #'(rest ...) subsequent-blocks))]))
      
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
	
    (syntax-case x (spawn sync define)
      [(cilk cmds ...)
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
	    (let #,(map (lambda (v e) #`(#,v (lambda () #,e))) names exprs)
	       #,@(map list names)
	    )))
      ])))

;; ====================================================================================================

;(pretty-print (expand prog1))
(print-gensym #f)
;(pretty-print (expand prog1))
;(pretty-print (expand prog2))

;; ====================================================================================================

(define (cilkfib n)
  (if (fx< n 2) 1
	(cilk    
	 (define left (spawn cilkfib (fx- n 1)))
	 (define right (cilkfib (fx- n 2)))
	 (sync)
	 (fx+ left right))))

(init-par 2)

(printf "cilk test 1: ~s\n" (cilk 33))
(printf "cilk test 2: ~s\n" (cilk (spawn printf "   printf test\n") 44))

(printf "FIB 20 = ~s \n" (cilkfib 20)) ;; 10946

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
