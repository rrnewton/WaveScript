
;; [2005.03.10]  Just testing a few ideas wrt aggregation.

(load "simulator_nought.ss")

(init-world)

;; Transforms (a b c), representing { a->b, a->c }
;; into  (a b) and (a c)
(define (graph:horizontal->simple g)
  (apply append
	 (map (lambda (entry)
		(let ([head (car entry)])
		  (map (lambda (target) (list head target))
		       (cdr entry))))
	      g)))

;; Uses slib hashtab.scm interface:
(define graph:simple->horizontal 
  (let () 
    (define hashtab-get (hash-inquirer eq?))
    (define hashtab-set! (hash-associator eq?))

    (lambda (g)
      (let ([len (length g)])
	(let ([table (make-hash-table len)])
	  ;; Load up the hash table with all the outgoing edges.
	  (for-each
	   (lambda (entry)
	     (hashtab-set! table 
			   (car entry)
			   (cons (cadr entry)
				 (hashtab-get table (car entry)))))
	   g)
	  ;; Now offload them all:
	  (let ([acc '()])
	    (hash-for-each
	     (lambda (hd tls)
	       (set! acc (cons (cons hd tls) acc)))
	     table)
	    (reverse! acc)))))))



;; This uses the basic gradient-spreading algorithm.
;; VERY inefficient: depth first
(define spanning-tree
  (let () 
    (define hashtab-get (hash-inquirer eq?))
    (define hashtab-set! (hash-associator eq?))

    (lambda (graph start)
      (let* ([len (length graph)]
	     [table (make-hash-table len)])

	(let loop ([node start] [count 1])  
	  ;(printf "(~a . ~a)~n" (node-id node) count)
	  (for-each (lambda (nbr)
		 (define (hit!) (begin (hashtab-set! table nbr (cons count node))
				       ;; Relay the "gradient".
				       (loop nbr (add1 count))))
		 (match (hashtab-get table nbr)
			[#f  (hit!)]
			[(,dist . ,parent) (guard (> dist count)) (hit!)]
			[,else 
			 ;(printf ";; miss!~n")
			 (void)] ;;fizzle
			))
		    (graph-neighbors graph node)))
	(let ([graphacc '()])
	  (hash-for-each
	   (lambda (nd x)
	       (match x
		      [#f (error 'spanning-tree "this function is broken, no entry: ~s" nd)]
		      [(,dist . ,parent)
		       (let ([entry (assq parent graphacc)])
			 (if (not entry)
			     (begin (set! entry (list parent))
				    (set! graphacc (cons entry graphacc))))

			 (set-cdr! entry 
				   (cons nd (cdr entry)))



	table
	))))
	
	  
			      

