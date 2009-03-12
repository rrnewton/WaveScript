#! /bin/bash
#|
exec chez --script $0 $*
|#

;; Currently this is chez-specific.

;; A simple make style utility for incremental builds with Chez.  This
;; not supposed to be general or correct, rather, it's a temporary fix
;; for this particular project.
;;
;; Ryan Newton [2009.02.17]

(import (rnrs))

;; One argument function invoked on a source file:
(define (invoke-compiler f) 
  (define root (remove-file-extension f))
  (optimize-level 2) ;; [2009.03.12] TEMP
  (generate-inspector-information #f)
  ;(printf "  Compiling: ~a opt-level ~a \n" f (optimize-level))
  (printf "[optlvl ~a]  " (optimize-level))
  (compile-file f (string-append root ".so"))
  )

;; The file to compile.
(define source 
  (if (null? (command-line-arguments))
      (begin (display "Enter filename: ") 
	     (let ((x (read)))
	       (if (symbol? x)
		   (symbol->string x)
		   x)
	       ))
      (car (command-line-arguments))
      ))

(define (id x) x)
(define remove-file-extension
  (lambda (filename)
    (let loop ([ls (reverse (string->list filename))])
      (cond
        [(null? ls) filename] ;no extension to remove
        [(eq? (car ls) #\.)
         (list->string (reverse (cdr ls)))]
        [else (loop (cdr ls))]))))

(define (resolve-name los)
    (case (car los)
      [(rnrs scheme) #f]
      [(except for) (resolve-name (cadr los))]
      [else 
       (string-append
	"."
	(apply string-append 
	       (map (lambda (s) (string-append "/" (symbol->string s))) los))
	".sls")]
      ))

(define (extract-deps sexp)
  (define (cleanup modname)
    (case (car modname)
      [(except prefix only) (cadr modname)]
      [else modname]
      ))
  (case (car sexp)
    [(import)  (map cleanup (cdr sexp))]
    [(library)
     ;(printf "library\n")
     (map cleanup (cdr (cadddr sexp)))
     ]))

(define (id x) x)
(define intersection
  (case-lambda
    [(set1 set2)
     (let loop ([set1 set1])
       (cond
         [(null? set1) '()]
         [(member (car set1) set2) (cons (car set1) (loop (cdr set1)))]
         [else (loop (cdr set1))]))]
    [all 
     (let loop ([set1 (car all)] [sets (cdr all)])
       (if (null? sets)
           set1
           (loop (intersection set1 (car sets)) (cdr sets))))]))
(define difference
  (lambda (set1 set2)
    (let loop ([set1 set1]
	       [set2 set2])
    (cond
     ((null? set1) '())
     ((member (car set1) set2) (loop (cdr set1) set2))
     (else (cons (car set1)  (loop (cdr set1) set2)))))
    ))
(define remove-file-extension
  (lambda (filename)
    (let loop ([ls (reverse (string->list filename))])
      (cond
        [(null? ls) filename] ;no extension to remove
        [(eq? (car ls) #\.)
         (list->string (reverse (cdr ls)))]
        [else (loop (cdr ls))]))))

;; [2005.11.17] This one is similar 
;; It looks like the chez primitive doesn't provide a handle on stderror.
(define (system-to-str str)
  (let* ([ls (process str)]
	 [in (car ls)] [out (cadr ls)] [id (caddr ls)])
    (let-values (((p extract) (open-string-output-port)))
    (let loop ((c (read-char in)))
      (if (eof-object? c)	  
	  (begin 
	    (close-input-port in)
	    (close-output-port out)
	    (extract))
	  (begin (display c p)
		 (loop (read-char in))))))))

(define (is-changed? file)
  (define (modtime file)
    (read (open-input-string 
	   (system-to-str (string-append "stat " file " -c \"%Y\"")))))
  (define obj  (string-append (remove-file-extension file) ".so"))
  (or (not (file-exists? obj))
      (> (modtime file) (modtime obj))))


;; Returns a new checked, changed
(define (compile-loop ls checked changed)
  (if (null? ls) 
      (values checked changed)
      (if (member (car ls) checked)
	  (compile-loop (cdr ls) checked changed)
	  (let ([thisfile (car ls)])
	    (printf "  Checking: ~a\n" thisfile)
	    (let* ([file (open-input-file thisfile)]
		   [deps (extract-deps (read file))])
	      (close-port file)
	      ;; Compile our dependencies:
	      (let ([depfiles (filter id (map resolve-name deps))])
		(let-values ([(chk chng) (compile-loop depfiles checked changed)])
		  (let ([chk2 (cons thisfile chk)]
			[depschanged (intersection depfiles chng)])
		    (if (and (not (is-changed? thisfile)) 
			     (null? depschanged))
			(compile-loop (cdr ls) chk2 chng)
			(begin (invoke-compiler thisfile)
			       (compile-loop (cdr ls) chk2 (cons thisfile chng))))
		    ))))))))

(display "Processing dependencies for: ")
(display source)(newline)
(compile-loop (list source) '() '())
(display "Done.\n")
