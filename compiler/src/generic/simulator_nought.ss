;; simulator_nought.ss
;;  -Ryan Newton [2004.05]
;; This is a simulator for the output of pass10_deglobalize.  
;; A simple simulator.
;; This file uses and abuses top-level bindings (like mad).
;; It should be loaded inside a module that only exports:
;;   (provide simulator-nought-language)


(define this-unit-description 
  "simplest simulator for nodal language")

;; This requires slib.
(slib:require 'tsort)

;; This is the simplest simulator ever.  Takes the output of pass "deglobalize".

(define-structure (node id pos))
(define-structure (sim-object node incoming neighbor-objs))

(define world-xbound 60)
(define world-ybound 60)
(define radius 30)
(define numprocs 10)

(define (random-node) 
  (make-node 
   (random (expt 2 32))
   (list (random world-xbound)
	 (random world-ybound))
   ))

(define (dist a b)
  (sqrt (+ (expt (- (car a) (car b)) 2)
	   (expt (- (cadr a) (cadr b)) 2))))
	
(define graph 
  (let ((seed (map (lambda (_) (random-node)) (iota numprocs))))
    ;; Connect the graph:
    (set! seed
					;      (let ((ids (map node-id graph)))
					;	(map 
	  (map (lambda (node)
		 (cons node (filter (lambda (n) (< (dist (node-pos node) (node-pos n)) radius))
				    seed)))
	       seed))
    seed))

;(list-get-radom 


(define (draw)
  (init-graphics)
  (for-each draw-point (map node-pos graph))
  (for-each draw-line (map (lambda (node) 
			     (map (lambda (neighb) 
				    (list (node-pos node) (node-pos neighb)))))
			   graph)))
  

(define (free-vars expr)
  (let loop ((env ()) (expr expr))
    (match expr	 
	   [,var (guard (symbol? var)) (if (memq var env) '() (list var))]   
	   [(quote ,x) '()]
	   [(,prim ,rand* ...) (regiment-primitive? prim)
	    (let ((frees (map (lambda (x) (loop env x)) rand*)))
	      (apply append frees))]
	   [(lambda (,formals) ,expr)
	    (loop (append formals env) expr)]
	   [,else (error 'free-vars "not simple expression: ~s" expr)])))

(define (process-statement stmt)
  stmt  
  )

(define (process-binds binds expr)
  (let* ([graph (map (lambda (bind)
		       (cons (car bind)
			     (free-vars (cadr bind))))
		     binds)]
	 [flat (reverse (tsort graph eq?))]
	 [binds (map (lambda (sym) (assq sym binds))
		     flat)])
    (if (cyclic? graph)
	(error 'process-binds "for now can't take a cyclic graph: ~s" binds))
    `(let* ,binds ,expr)))

(define (process-tokbinds tbinds expr)
  (disp "tbinds" tbinds)
  (let ([binds (map
		(lambda (tbind)
		  (match tbind 
			 [(,tok (,args ...) ,expr* ...)
			  `[,tok (lambda ,args ,expr* ...)]]))
		tbinds)]
	[handler `(lambda (msg args)
		    (case msg
		      ,(map (lambda (tok)
			      (disp "  GOT TOK" tok)
			      `[(,tok) (apply ,tok args)])
			    (map car tbinds))))])
  `(letrec (,@binds [handler ,handler]) ,expr)))


(define (compile-simulate-nought prog)
  (match prog
	 [(program (socpgm (bindings ,socbinds ...) ,socstmts ...)
		   (nodepgm (bindings ,nodebinds ...) (tokens ,nodetoks ...)))
	  `(,(process-binds socbinds
			    `(begin ,@(map process-statement socstmts)))
	    ,(process-binds nodebinds 
			    (process-tokbinds nodetoks
					      `(tempexpr))))]))

(define (simulator-nought-language expr)
  (void))

;========================================

(define these-tests
  `(
    [ (free-vars '(cons (quote 30) x)) '(x) ]
    ))


(define display-constrained
  (lambda args
    (for-each 
     (lambda (arg)
       (if (string? arg)
	   (display arg)
	   (let ([arg (car arg)]
		 [bound (cadr arg)]
		 [port (open-output-string)])
	     (pretty-print arg port)
	     (let ((str (get-output-string port)))
	       (if (> (string-length str) bound)
		   (begin (display (substring str 0 (max 0 (- bound 3))))
			  (display "..."))
		   (display str))))))	       
	      args)))

(define test-this
  (let ((these-tests these-tests))
    (lambda args 
      (let ([verbose (memq 'verbose args)]
	    [tests (map car these-tests)]
	    [intended (map cadr these-tests)]
	    [success #t])
	  (if verbose 
	      (printf "Testing ~s\n" this-unit-description))
	  (let ((results (map eval tests)))
	    (for-each 
	     (lambda (expr intended result) 
	       (display-constrained "  " `(,expr 40) 
				    " -> " `(,intended 20)
				    ": ")
	       (if (tester-equal? intended result)
		   (if verbose (display "#t\n"))
		   (begin (set! success #f)
			  (newline)
			  (display "FAIL: ")
			  (display-constrained `(,intended 40))
			  (newline) (newline))))
	     tests intended results))))))

(define testsim test-this)
(define testssim these-tests)


;;===============================================================================


(define t1
  '((shirt tie belt)
    (tie jacket)
    (belt jacket)
    (watch)
    (pants shoes belt)
    (undershorts pants shoes)
    (socks shoes)))

(define t2
  '((a b) (b c) (c a)
    (d e) (e f)))


(define p
  '(program
     (socpgm (bindings) (call result_2))
     (nodepgm
;       result_2
       (bindings (tmp_4 (cons '40 '())) (tmp_1 (cons '30 tmp_4)))
       (tokens
         ((f_token_tmp_3 () (flood token_6))
          (token_6
            ()
            (if (< (locdiff (loc) tmp_1) 10.0)
                (elect-leader m_token_tmp_3)))
          (m_token_tmp_3 () (call f_token_result_2))
          (f_token_result_2 () (emit m_token_result_2))
          (m_token_result_2
            ()
            (if (< (dist f_token_result_2) '50) (relay))))))))

(define csn  compile-simulate-nought)

(dsis tt (csn p))
