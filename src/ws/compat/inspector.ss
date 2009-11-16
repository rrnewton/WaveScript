
;; Here I will implement a very rudimentary interactive inspector.
;; Chez is the only implementation I know of that exposes a nice
;; inspector.  Larceny has one, but just for error continuations?

;; DOESN'T WORK FOR CYCLIC STRUCTURES!

(trace-define (generic-inspect x)
;   (printf "\nInteractive inspector (This is not implemented yet): \n")
;   (pretty-print x)(newline)
;   (printf "\nPress enter to continue.\n")
;   ;(get-line (console-input-port))
;   (read-char)

  ;; Very rudimentary inspector:
  (call/cc 
   (lambda (cont)
    (let loop ([x x])
     (define (dispatch x)
      (let ([line (get-line (current-input-port))])
	(cond
	 [(eof-object? line) (newline)] ;; Exit
	 [(member line '("p" "print"))
	  (newline) (pretty-print x) (newline)
	  (loop x)]
	 [(member line '("w" "write"))
	  (newline) (write x) (newline)(newline)
	  (loop x)]

	 [(member line '("c" "cont" "continuation"))
	  (loop cont)]

	 [(member line '("n" "native"))
	  (printf "\nSwitching to ~a's native inspector...\n" which-scheme)
	  (native-inspect x)]

	 [(member line '("q" "quit"))
	  (printf "\nExiting and returning last value.\n")
	  x]
 	 
	 [(equal? line "?")
	  (printf "\nWS Homegrown Scheme object inspector, commands:\n\n")
	  (printf "  print(p) ............ pretty-print object\n")
	  (printf "  write(w) ............ write object\n")
	  (printf "  cont(c)  ............ continuation of 'inspect' call\n")
	  (printf "  native(n) ........... drop to native inspector (Chez)\n")
	  (printf "  quit(q) ............. exit and continue execution\n")
	  
; Chez Scheme options:    
;  
;    up(u) ............... return to [nth] previous level
;    top(t) .............. return to initial object
;    forward(f) .......... move to [nth] next expression
;    back(b) ............. move to [nth] previous expression
;    => .................. send object to procedure
;    file ................ switch to named source file
;    list ................ list the current source file [line [count]]
;    files ............... show open files
;    mark(m) ............. mark location [with symbolic mark]
;    goto(g) ............. go to marked location [mark]
;    new-cafe(n) ......... enter a new cafe
;    quit(q) ............. exit inspector
;    help(h) ............. help
	  (newline)(loop x)]
	 
	 [(equal? line "") (loop x)]
	 [else (printf "Invalid command or argument: ~s.  Type ? for options.\n" line) (loop x)]
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
    
    ))))
