
;  #! /Users/newton/bin/Darwin-Power_Macintosh/petite --script

;(eval-when (eval load compile)

 (load (string-append (getenv "HOME") "/scheme/chez/full_chez.ss"))
 (load "compiler_chez.ss")

(define (print-version) 
  (printf "Regiment compiler, version ~s~n" regiment-version))
	   
(define main 
  (lambda fns
    (printf "regimentc: compile regiment programs!~n")
    (letrec ([loop 
	      (lambda (args)
		(match args
		    [() '()]

		    [(,opt ,rest ...) (guard (equal? opt "-v"))
		     (print-version) (exit 0)]

		    ;; otherwise a file to compile that we add to the list
		    [(,fn ,rest ...)
		     ;(regiment-compile-file fn)
		     (cons fn (loop rest))]
		    [,_ (error "Bad command line arguments to regimentc: ~a~n" args)]	  
		    ))])
      (let ([actual-files (loop fns)])
	(for-each (lambda (fn)
	(run-compiler 
	

(suppress-greeting #t)
(scheme-start main)

;)

 
