    

;; Oops, cycles might wreck this???
(define (lift-graph g)
  (let ((collect '()))
    (letrec ([get-obj 
	      (lambda (node)
		(let ((entry (assq node collect)))
		  (if entry (cadar entry)
		      (add-obj node))))]
	     [add-obj 
	      (lambda (node)
		(let ((exists (get-obj node collect)))
		  (set! collect 
			(cons (make-sim-object 
			       node '() 
			       (map get-obj (node-neighbors node))))))))]
      
			     
		       
  (for-each (lambda (node) 
	      (if 

	      

(define world (lift-graph graph))

(define compile-simulate-nought
  (let ()

    (lambda (prog)
      (match prog
	     [(program (socpgm ,socbinds ,socstmts ...) 
		       (nodepgm ,nodebinds ,nodetoks))
	      (






(begin (let* () (begin (call result_2)))
       (let* ([tmp_4 (cons '40 '())] [tmp_1 (cons '30 tmp_4)])
         (letrec ([f_token_tmp_3 (lambda () (flood token_6))]
                  [token_6
                   (lambda ()
                     (if (< (locdiff (loc) tmp_1) 10.0)
                         (elect-leader m_token_tmp_3)))]
                  [m_token_tmp_3 (lambda () (call f_token_result_2))]
                  [f_token_result_2 (lambda () (emit m_token_result_2))]
                  [m_token_result_2
                   (lambda ()
                     (if (< (dist f_token_result_2) '50) (relay)))]
                  [handler
                   (lambda (msg args)
                     (case msg
                       [((f_token_tmp_3) (apply f_token_tmp_3 args))
                        ((token_6) (apply token_6 args))
                        ((m_token_tmp_3) (apply m_token_tmp_3 args))
                        ((f_token_result_2) (apply f_token_result_2 args))
                        ((m_token_result_2)
                         (apply m_token_result_2 args))]))])
           (f_token_tmp_3))
))
