


(define haskellize-tokmac-language
  (lambda (str) 
    
    (printf "~nDumping token machine into directory: ~s~n" (current-directory))
    (let ([out (open-output-file "test_tokmac_comp.tm" 'replace)])
      (display str out)
      (close-output-port out))
    (printf "~nBinding top-level function (run_tm) to compile&assemble this token machine.~n")
    (define-top-level-value 'run_tm
      (lambda ()
	(parameterize ([current-directory "haskell"])
	  (printf "Running token machine in directory: ~s~n~n from str: ~n~s~n~n" 
		  (current-directory) str)

	  (if (not (file-exists? "assembler"))
	      (error 'run_tm "Missing assembler.")
	      (let ([file (or (and (file-exists? "test_tokmac_comp.tm") "test_tokmac_comp.tm")
			      (and (file-exists? "../test_tokmac_comp.tm") "../test_tokmac_comp.tm")
			      (error 'run_tm "Missing test_tokmac_comp.tm") )])	    		
		;; First use the assembler:
		(system (string-append "./assembler " file))
		;; Then use the NesC compiler:
		;(system "make pc")
		 )))))
    (run_tm)))


