
(library (cilk)
 (export 
     cilk
     )
  
 (import (rnrs (6))
         (par6)
;         (only (scheme) fork-thread mutex-acquire mutex-release make-mutex make-thread-parameter get-thread-id
;	       gensym list-head iota void random format printf fprintf define-record) ;; Chez scheme primitives
	 )

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

) ;; End library