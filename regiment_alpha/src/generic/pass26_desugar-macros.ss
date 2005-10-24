

;; Eventually I want to define a nice clean system for expressing TML macros.
;; Then elect-leader, flood, and maybe even gradients can just be
;; written up as seperate (and optional) macros in their own files.

(define desugar-macros
  (let ()

    (define (process-expr expr)
      (tml-generic-traverse
       (lambda (x autoloop)
	 (match x 


	   [(flood ,t)
	    (let ((newtok (unique-name 'floodtok)))
	      (vector
	       `(gemit (tok ,newtok (my-id)))
	       `([,newtok subid () 
			  (grelay (tok ,newtok subid))
			  (call ,t)])))]

	   [(elect-leader ,t)
	    (let* ((compete (unique-name 'compete))
		   (storagename (unique-name 'leaderstorage))
		   (storage `(tok ,storagename 0))
		  (cur-leader (unique-name 'cur-leader))
		  (check-winner (unique-name 'am-i-winner))
		  (id (unique-name 'subtokid)))
	      (vector
	       `(begin 
		  (gemit (tok ,compete (my-id)))
		  (timed-call 1000 ,check-winner)
		  )
	       `([,storagename () (stored [,cur-leader #f]) (set! ,cur-leader (my-id))]
		 [,compete ,id () 		 
		    (if (token-present? ,storage )
			(void)
			(subcall ,storage))
		    (if (< ,id (ext-ref ,storage ,cur-leader))
			(begin 
			  (printf '"(~a ~a) " id (ext-ref ,storage ,cur-leader)) 
			  (ext-set! ,storage ,cur-leader ,id)
			  (grelay (tok ,compete ,id)))
			(begin 
			  (printf '"~a "(ext-ref ,storage ,cur-leader))
			  ))]
		 [,check-winner ()
		    (if (= (ext-ref ,storage ,cur-leader) (my-id))
			(call ,t)
			(void))])
		 ))]

	   [,other (autoloop other)]))
       (lambda (subresults reconstruct)
	 (match subresults
	   [(#(,arg* ,newtbs*) ...)
	    (vector (apply reconstruct arg*)
		    (apply append newtbs*))]))
       expr))

    (define (process-tokbind tb)
      (mvlet ([(tok id args stored constbinds body) (destructure-tokbind tb)])
	(match (process-expr body)
	  [#(,newbod ,tbs)
	   (cons `[,tok ,id ,args (stored ,@stored) ,newbod]
		 tbs)])))

    (lambda (prog)
      (match prog
	[(,lang '(program (bindings ,constbinds ...)
		   (nodepgm (tokens ,toks ...))))          
	   `(desugar-macros-lang
	     '(program (bindings ,constbinds ...)
		(nodepgm (tokens 
			     ,@(apply append (map process-tokbind toks))))))]))
    ))


#!eof    
	'(tokens
	   [SOC-start () (timed-call 1000 open)
		         (timed-call 6000 close)]
	   [open () (printf "\n(final ")]
	   [close () (printf ")")]

	   [node-start () (stored [cur-leader #f])
	       (set! cur-leader (my-id))
	       (gemit (tok lead (my-id)))
	       (printf "(launch ~a) \n" (my-id))
	       (timed-call 5000 final-report)
	       ]
	   [lead id () 		 
		 (if (< id (ext-ref node-start cur-leader))
		     (begin 
		       (printf "(~a ~a) " id (ext-ref node-start cur-leader)) (flush-output-port)
		       (ext-set! node-start cur-leader id)
		       (grelay))
		     (begin 
		       (printf "~a " ;(my-id) 
			       (ext-ref node-start cur-leader)) 
		       (flush-output-port)
		       ;(gemit (tok lead (ext-ref node-start cur-leader)))
		       ))]
	   [final-report () (printf "~n   ~a " (ext-ref node-start cur-leader))]
	   )