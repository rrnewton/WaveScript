#! /bin/bash
#|
exec chez79 --script $0 $*
|#

;; A simple make style utility for R6RS implementations that do 
;; not provide recursive dependency-aware compiles.
;;
;; Ryan Newton [2009.02.17]

(import (rnrs))

;; The name of the scheme executable.
(define scheme "chez")

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

;; One argument function invoked on a source file:
;(define invoke-compiler compile-file)
(define (invoke-compiler f) 
  (define root (remove-file-extension f))
  ;(printf "Compiling: ~a\n" f)
 (compile-file f (string-append root ".so"))
  )

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

;; Returns true if something is recompiled.
(trace-define (compile-loop ls alreadyhit)
  (if (null? ls) '()
      (let* ([thisfile (car ls)]
	     [file (open-input-file thisfile)]
	     [deps (extract-deps (read file))])
	(close-port file)
;	(printf "   DEPS ~a : ~a \n" thisfile (filter id (map resolve-name deps)))
	
	;; Compile our dependencies:
	(let* ([depfiles (filter id (map resolve-name deps))]
	       [ups  (compile-loop depfiles alreadyhit)] ; (difference depfiles alreadyhit)
	       [depschanged (intersection depfiles ups)]
	       ;[__ (printf "DEPS CHANGED: ~a\n" depschanged)]
	       [withthis
		(if (null? depschanged)  ups 
		    (begin (invoke-compiler thisfile) 
			   (cons thisfile ups)))]
	       )

	  (compile-loop (cdr ls) withthis)
	  ))))

(display "Compiling source file and dependencies: ")
(display source)(newline)
(compile-loop (list source) '())
(display "Done.\n")
