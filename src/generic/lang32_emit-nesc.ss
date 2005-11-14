

(define emit-nesc-language
  (lambda (lst)     
    (match lst
      [#(,mstr ,cstr ,hstr)
       (parameterize ((current-directory "~/cur/haskell"))
       (printf "~nDumping token machine into directory: ~s~n" (current-directory))
       (let ([modF    (open-output-file (string-append emit-nesc-modname "M.nc") 'replace)]
	     [confF   (open-output-file (string-append emit-nesc-modname ".nc") 'replace)]
	     [headerF (open-output-file (string-append emit-nesc-modname ".h") 'replace)])
	 (display mstr modF)
	 (display cstr confF)
	 (display hstr headerF)	
	 (close-output-port modF)
	 (close-output-port confF)
	 (close-output-port headerF))
       (printf "~nBinding top-level function (run_tm) to compile&assemble this token machine.~n")
       (define-top-level-value 'run_tm
	 (lambda ()
	   (printf "Running token machine in directory: ~s~n~n from str: ~n~s~n~n" 
		   (current-directory) 
		   (substring mstr 0 (min 200 (string-length mstr))))

	   ;; Use the NesC compiler:
	   (system "make pc")))
       ;; For now also go ahead and run it
       (run_tm))]
      [,other (error 'emit-nesc-language "Bad program: ~s" other)])))
