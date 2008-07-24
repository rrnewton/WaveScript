
;; Here I will implement a very rudimentary interactive inspector.
;; Chez is the only implementation I know of that exposes a nice
;; inspector.  Larceny has one, but just for error continuations?

;; DOESN'T WORK FOR CYCLIC STRUCTURES!

(define (generic-inspect x)
;   (printf "\nInteractive inspector (This is not implemented yet): \n")
;   (pretty-print x)(newline)
;   (printf "\nPress enter to continue.\n")
;   ;(get-line (console-input-port))
;   (read-char)

  ;; Very rudimentary inspector:
  (let loop ([x x])
    (define (dispatch x)
      (let ([line (get-line (current-input-port))])
	(cond
	 [(eof-object? line) (newline)] ;; Exit
	 [(equal? line "p") 
	  (newline) (pretty-print x) (newline)
	  (loop x)]
	 [(equal? line "w") 
	  (newline) (write x) (newline)(newline)
	  (loop x)]
	 [(equal? line "") (loop x)]
	 [else (printf "Invalid command or argument: ~s.  Type ? for options.\n" line)]
	 )))
    (define (prompt)
      (display " : ")(flush-output-port (current-output-port)))
    (let ([str1 (with-output-to-string (lambda () (write x)))]
	  ;[str2 (with-output-to-string (lambda () (pretty-print x)))]
	  )
      (cond
       [(< (string-length str1) 70)
	(display str1)
	(display (list->string (make-list (- 70 (string-length str1)) #\space)))
	(prompt) (dispatch x)]
       [else 
	(display (substring str1 0 66))(display " ...")
	(prompt) (dispatch x)
	;(error 'inspect "this function is unfinished")
	])
      )

    #;
    (let-values ([(p extract) (open-string-output-port)])
      (pretty-print x p)
      (let ([str (extract)])
	(if )
	)
      )
    
    )
  )
