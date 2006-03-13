#cs ;; Case Sensitivity
(module pass21_cleanup-token-machine mzscheme

  (require (lib "include.ss"))
  (require (lib "list.ss"))
  (require "plt_constants.ss")
  (require "iu-match.ss"
	   "prim_defs.ss")
  (require (all-except "helpers.ss" filter test-this these-tests))
  (require (all-except "regiment_helpers.ss" filter test-this these-tests))
  (require (all-except "grammar_checker.ss" test-this these-tests))
  (require (all-except "tml_generic_traverse.ss" test-this these-tests))
  (require (lib "trace.ss"))

  (include (build-path  "generic" "pass21_cleanup-token-machine.ss"))
  
;  (provide deglobalize
;;	   test-this these-tests test12 tests12)
  (provide (all-defined))
  )
   
;   (cleanup-token-machine '(tokens))
   
;(test21)  


;(.require "regiment_helpers.ss" "iu-match.ss")

#;(define p (cleanup-token-machine 
      '(deglobalize-lang 
	'(program
	  (bindings )
	  (socpgm (bindings ) (emit tok1))
	  (nodepgm
	   (tokens
	    [tok1 () (fun1)]
	    [tok1 () (fun2)])
	   (startup )
	   )))))

 #;  (define f (lambda (p)
	(match p 
	  [(cleanup-token-machine-lang
	    '(program (bindings )
		      (nodepgm (tokens ,toks ...))))
           (let* ([tok1 (assq 'tok1 toks)]
                  [body (rac tok1)])
	   (list tok1 body (deep-member? '(fun1) body)
		(deep-member? '(fun2) body)))])))

;   (f p)
   
;   (deep-member? '(foo) '(bar (foo) zoo))
     
     
;(require pass21_cleanup-token-machine)
;(require "regiment_helpers.ss")
