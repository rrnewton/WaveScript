#!r6rs  

;; This is the script that imports the main top-level module.

(import (rnrs) (rnrs eval)
        (main_r6rs)
	(main)
	;(scheme base)
	)

(display "Hrm\n" (current-output-port))
;(display (format "yay ~a\n" do-times))
;(printf "yay ~a\n" do-times)

(reg:define-struct (foo a b c))
(printf "Struct and methods: ~s ~s ~s\n\n" (make-foo 1 2 3) foo-a set-foo-a!)

;(reg:define-struct (bench-stats bytes tuples cpu-time))

(printf "Matched: ~a\n" (match 3 [3 99] [,else 1010]))

;(time-accum (printf "time1\n"))
;(time-accum (printf "time2\n"))
;(time-accum-report)

;(printf "Cheesy lamda ~a\n" (map (\\ x 3) '(1 2 3 )))

(printf "match? ~a\n" (match? '(1 2 3 ) (,a ,b 3)))

(printf "output to str ~s\n" (with-output-to-string (lambda () (printf "string"))))

;(printf "helpers tester ~a\n" testhelpers)
;(testhelpers)(newline)(newline)
(printf "Here I pull some testers: ~a\n" 
	(list test-streams testhelpers test-regiment_helpers
	      test-type_environments
	      imp:test-imperative_streams
	      test-rewrite
	      ;test-wavescript_emit-c
	      ))
;(test-streams)(newline)(newline)

;(repl)
(printf "SYSTEM to string: ");(flush-output-port (current-output-port))
(printf "~s\n" (system-to-str "ls"));(flush-output-port (current-output-port))

(printf "\nDisplay constrained: ")
(display-constrained "test  " '(abcdefghijklmnop 10) 
			      " again  " '(abcdefghijklmnop 16) 
			      '(abcdefghijklmnop 17))
(newline)
(printf "\n\nWith output to string display constrained:\n   ")
(flush-output-port (current-output-port))
(pretty-print
(with-output-to-string 
       (lambda ()
	 (display-constrained "test  " '(abcdefghijklmnop 10) 
			      " again  " '(abcdefghijklmnop 16) 
			      '(abcdefghijklmnop 17)))))

(display "A list: ")(write '(1 2 3 4)) (newline)
(display "Types compat: ")(display (types-compat? '(Stream Node) '(Stream 'a) ))(newline)
;(printf  "Types compat, eval: ")(flush-output-port (current-output-port))

(display "Annot Expr: ")(annotate-expression '(lambda (v) (Int) (_+_ v v)) (empty-tenv) '())(newline)
(display "Annot Expr, eval: ")
(display  (eval 
	   '(annotate-expression '(lambda (v) (Int) (_+_ v v)) (empty-tenv) '())
	   ;'(annotate-expression ''3 (empty-tenv) '())
	   (environment '(r6rs) '(main_r6rs)))) (newline)
;(display  (eval '(types-compat? '(Stream Node) '(Stream 'a) ) (environment '(r6rs) '(main_r6rs)))) (newline)
;(display  (eval 'types-compat? (environment '(r6rs) '(main_r6rs)))) (newline)
;(display  (eval '3 (environment '(r6rs) '(main_r6rs) ))) (newline)
(printf  "Types compat, eval2: ~s\n" (reg:top-level-eval '(types-compat? '(Stream Node) '(Stream 'a) )))

;(newline)
;(eval '(read-eval-print-loop) (environment '(scheme base) '(main_r6rs) '(main)))
;(pretty-print (read))



(printf "All testers ~a\n" (reg:all-unit-tests))
(printf "All test names: ~a\n" (reg:all-tester-names))
(test-units)
;(test-ws)
(printf " testing complete\n")

(printf "Pull a symbol from the sim module ~s\n" wssim:print)
(printf "Pull a symbol from the lang_wavescript ~s\n" wavescript-language)
(printf "Pull a symbol from the interpret-meta ~s\n" Eval)
;(printf "Pull a symbol from the interpret-meta ~s\n" footest)


(define-top-level-value 'foobar (lambda (x) x))
(printf "top level val defined ~a\n" (top-level-value 'foobar))

(printf "Match against a string? ~s\n" (match "foo" ["bar" 33] ["foo" 44] [,else 55]))

;; Call wavescript!
(printf "Main procedure ~s\n" main)
;(main 'wsint "-v" 2 "demos/wavescope/demo1c_timer.ws")
(main 'wsmlton "-v" 2 "demos/wavescope/demo1c_timer.ws")
;(ws "demos/wavescope/demo1c_timer.ws")





#;
(define (ikarus-compile-match-test x)
  (match x [(,first ,[rest] ...)  999]))

(flush-output-port (current-output-port)) 
#;
(dump-heap "larc.heap" (lambda args (import (err5rs load)) (load "newloads.ss") 
						     ;(repl)
						     (exit)))

