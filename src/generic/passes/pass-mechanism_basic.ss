
;;;; .title Basic mechanism for building pass-objects.
;;;; .author Ryan Newton
;;;; 
;;;; This is the basic mechanism for building the objects that
;;;; represent compiler passes within Regiment/WaveScript. <br><br>
;;;; 
;;;; The fancier syntactic sugar in pass-mechanism.ss is based on
;;;; this.

;; ================================================================================

(module pass-mechanism_basic mzscheme
    (require "../../plt/iu-match.ss"
             "../../plt/chez_compat.ss"
             "../grammars/grammar_checker.ss"
             "../constants.ss" )

  (provide     
	  build-compiler-pass
	  regiment-pass->name
	  )
  (chezimports)

;; This is the constructor for compiler passes.  It takes the main
;; function that does the real work of the compiler, and then wraps it
;; with some extra debugging code.
;; <br><br>
;;
;; Both the input and output specifiers can contain grammars that
;; constrain the input/output language, or predicate procedures
;; (assertions) which are gateways that the input and output must pass.
;;
;; .param input-spec must be of the form: <br>
;;   (input <(grammar <gram> <start>) | (assert <proc>)> ...)
;;
;; .param output-spec must be of the (similar) form:<br>
;;   (output <(grammar <gram> <start>) | (assert <proc>)> ...)
(define (build-compiler-pass name input-spec output-spec transform)  
  (match (list input-spec output-spec)
    [((input ,instuff ...) (output ,outstuff ...))
     (let ([mainclosure 
	    (lambda (prog)
	      (if (eq? prog 'get-expr-driver)
		  ;; This message requests that we return the "process-expr" function.  We pass it on.
		  (transform 'get-expr-driver)
		  (begin
		    (when (check-pass-grammars)
		     (for-each (lambda (inspec)
				 (match inspec
				   ;; The optional initial production may or may not be supplied:
				   [(grammar ,g ,initialprod ...)
				    (unless (apply check-grammar name prog g initialprod)
				      (error 'build-compiler-pass "Bad input to pass: \n ~s" prog))]
				   [(assert ,f)
				    (unless (f prog)
				      (set-top-level-value! 'failed-pass-input prog)
				      (error 'build-compiler-pass 
					     "pass ~s failed input-invariant ~s, failed input stored in 'failed-pass-input" name f))]
				   [,other (error 'build-compiler-pass "bad input spec: ~s" other)]
				   ))
		       instuff))	     
		    ;; Now execute the pass itself:
		    (let ((result (transform prog)))
		      (when (check-pass-grammars)
		       (for-each (lambda (outspec)
				   (match outspec
				     ;; Check output grammar:	   
				     [(grammar ,gram ,optional_initialprod ...)
				      (if (>= (regiment-verbosity) 2) 
					  (printf "~a: Got result, checking output grammar...\n" name))
				      (or (apply check-grammar `(,name ,result ,gram ,@optional_initialprod))
					;(inspect (apply list 'FOOFOO name result gram optional_initialprod))
					  (begin (pretty-print result) #f)
					  (error 'build-compiler-pass 
						 "Bad pass output from ~a, failed grammar try (analyze-grammar-failure failure-stack): \n ~s" 
						 name prog))
				      (if (>= (regiment-verbosity) 2)
					  (printf "~a: Output grammar passed.\n" name))]
				     [(assert ,f)
				      (unless (f prog)
					(set-top-level-value! 'failed-pass-output prog)
					(error 'build-compiler-pass 
					       "pass ~s failed output-invariant ~s pass output stored in 'failed-pass-output" name f))]
				     [,other (error 'build-compiler-pass "bad output spec: ~s" other)]
				     ))
			 outstuff))
		      ;; Return final result:
		      result))
		  ))])
       ;; Add pass to global pass-table and return:
       (set! regiment-pass-name-table
	     (cons `[,mainclosure ,name] regiment-pass-name-table))
       mainclosure
       )]))

;; This is a hidden table (association list) that maps closures to pass names.
;; When build-compiler-pass is used to construct a pass, it's added to
;; this table so that it's name can be retreived in the future.
;; (If the passes were objects, I'd simply have them support a get-name method.)
(define regiment-pass-name-table '())

;; This exposed function does a lookup in the table.
(define (regiment-pass->name pass)
  (ASSERT (procedure? pass))
  (cond
   [(assq pass regiment-pass-name-table) => cadr]
   ;; Otherwise, the best we can do is print the procedure to a string:
   [else (format "~s" pass)]))


)  ;; End module