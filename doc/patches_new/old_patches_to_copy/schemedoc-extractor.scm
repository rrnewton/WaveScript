

;; Given the source file, scheme-input-file, make another file scheme-output-file,
;; in which the lexical comments are turned into syntactic comments in terms of comment forms.
;; The first parameter of a comment form is the number of semicolons in the lexical comment.
;; The second parameter is the comment text itself. Several consequtive lines of (equally leveled
;; comments) are organized in one comment form.
(define (lexical-to-syntactical-comments! scheme-input-file scheme-output-file)
 (if (file-exists? scheme-output-file) (delete-file scheme-output-file))
 (let* ((ip (open-input-file scheme-input-file))
        (op (open-output-file scheme-output-file)))
     (cond ((eq? scheme-documentation-commenting-style 'multi-semicolon)
               (multi-lexical-to-syntactical-comments-given-ports ip op))
           ((eq? scheme-documentation-commenting-style 'documentation-mark)
               (mark-lexical-to-syntactical-comments-given-ports ip op))
           (else (laml-error "lexical-to-syntactical-comments!: Unknown scheme documentation commenting style:" 
                             scheme-documentation-commenting-style)))
     (close-input-port ip)
     (close-output-port op)
     ;; RRN: Injecting a post-processing phase here.  This assumes my utilities are loaded.
;     (slist->file (file->slist scheme-output-file) scheme-output-file 'write)
     (slist->file
      (map even-more-nasty-fixes
	(match (file->slist scheme-output-file)
	[() ()]
	[((module ,name ,parent (require ,r ...) (provide ,p ...) (chezimports ,i ...)	,e* ...) . ,[rest])
	 (guard (symbol? name) (symbol? parent))
	 (printf "RRN: demodulizing source file: regiment module found ~a\n" name)	
	 (append e* rest)]
	[((,module ,name ,e* ...) . ,[rest])
	 (guard (memq module '(module chez:module)))	 
	 (printf "RRN: demodulizing source file: module found ~a\n" (if (symbol? name) name ""))
	 ;; Take away the module declaration.
	 ;; [2006.08.04] !!! Dammit, just ignoring everything that looks like it might be part of the module spec:
	 (append (filter (lambda (x)
			   (or (eq? module 'chez:module)
			       (not (pair? x))
			       (not (memq (car x) '(require provide chezimports chezprovide)))
			       (begin (printf "Ignoring suspected module spec component: ~a\n" (car x)) #f)
			       ))
		   e*)
		 rest)]
	[(,fst . ,[rest]) (cons fst rest)]))
      scheme-output-file 'write)
;     (inspect scheme-output-file)
     ))

;; RRN: This is terrible, but it barfs if it gets a comment as the last thing in a list!!
(define (even-more-nasty-fixes x)
  (match x
    ;; Comments in the end of lists cause problems!
    [(,[other] ... (comment!!! ,stuff ...))
     (printf "Fixing comment at end of list: ~a\n" stuff)
     `(,other ... (comment!!! ,stuff ...) 'RRN:MORE-LAME-LAML-FIXES!)
     ;`(,other ... )
     ]
    ;; Ok I have too many problems with ellipses and unquote, just killing matches.
    [(match ,x ...)
     (printf "Removing Match. ~a\n" (car x))
     ''RRN:LAME-HACK-DISCARDING-MATCH]

    [(,[x*] ...) x*]
    [,x x]
  ))

;; The name of the file where the function lexical-to-syntactical-comments! places it's output
(define temp-output-file "schemesource.tmp")
