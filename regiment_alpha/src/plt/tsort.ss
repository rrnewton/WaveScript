;; [2004.06.02] RRN:
;; This is lifted straight from slib3a1.
;; I hacked it to use mzscheme's hashtables.


(module tsort mzscheme
;;  (require "helpers.ss")
  (require (lib "pretty.ss"))
  (provide tsort topological-sort test-this these-tests)

(define (tsort dag pred)
  (if (null? dag)
      '()
      (let* ((adj-table (make-hash-table))
	     (insert hash-table-put!)
	     (lookup hash-table-get)
	     (sorted '()))
	(letrec ((visit
		  (lambda (u adj-list)
;;                    (printf "visiting ~s and ~s~n" u adj-list) 
		    ;; Color vertex u
		    (insert adj-table u 'colored)
		    ;; Visit uncolored vertices which u connects to
		    (for-each (lambda (v)                                
                                (let ((val (lookup adj-table v
                                                   (lambda () (visit v '())))))
;;                                  (printf "  looked up ~s and got ~s~n" v val)
				  (if (not (eq? val 'colored))
                                      ;; RRN: Weird, how did this get void!
;;				      (visit v (if (or (void? val) (not val)) '() val))
                                      (or (void? val) (visit v (or val '())))
                                      )))
			      adj-list)
		    ;; Since all vertices downstream u are visited
		    ;; by now, we can safely put u on the output list
		    (set! sorted (cons u sorted)))))
	  ;; Hash adjacency lists
	  (for-each (lambda (def)
 ;;                     (printf "  inserting: ~s ~s~n" (car def) (cdr def)) 
		      (insert adj-table (car def) (cdr def)))
		    (cdr dag))
	  ;; Visit vertices
	  (visit (caar dag) (cdar dag))
	  (for-each (lambda (def)
		      (let ((val (lookup adj-table (car def))))
			(if (not (eq? val 'colored))
			    (visit (car def) (cdr def)))))
		    (cdr dag)))
	sorted)))
(define topological-sort tsort)

;;================================================================================
;;IGNORE.  TESTING STUFF:

(define (lenient-compare? o1 o2)
  (or (eq? o1 o2)
      ;; Strings are not deep structures according to eq-deep,
      ;; So we compare them with equal?
      (and (string? o1) (equal? o1 o2))
      (eq? o1 'unspecified)
      (eq? o2 'unspecified)))

;; This provides a weird sort of interface to a deep equal.  It walks
;; down the tree, applying the input comparator at every intermediate
;; node, only proceeding downward on negative comparisons.
(define eq-deep 
  (lambda (eq)
    (lambda (obj1 obj2)
      (let loop ((o1 obj1) (o2 obj2))
	(cond
	 [(eq o1 o2) #t]
	 [(and (list? o1) (list? o2))
	  (if (= (length o1) (length o2))
	      (andmap loop o1 o2)
	      #f)]
	 [(and (vector? o1) (vector? 02))
	  (andmap loop (vector->list o1) (vector->list o2))]
	 [else #f])))))

(define tester-eq? (eq-deep lenient-compare?))
(define tester-equal? (eq-deep lenient-compare?))

(define default-unit-tester
  (lambda (message these-tests . eq-fun)
    (let ((teq? (if (null? eq-fun)
		    tester-equal?
		    (eq-deep (car eq-fun)))))
    (lambda args 
    (call/cc
     (lambda (return)
       (let ([entries 	
	      (map 
	       (lambda (entry)
		 (cond
		  [(= 3 (length entry))  entry]
		  [(= 2 (length entry))
		   (list #f (car entry) (cadr entry))]
		  [else (error 'default-unit-tester 
			       " This is a bad test-case entry!: ~s~n" entry)]))
	       these-tests)])
	 (let ([verbose (memq 'verbose args)]
	       [descriptions (map car entries)]
	       [tests (map cadr entries)]
	       [intended (map caddr entries)]
	       [success #t])

	  (if verbose 
	      (printf "Testing module: ~a~n" message))
	    (for-each 
	     (lambda (num expr descr intended)
;	       (flush-output-port)
	       (if (and verbose descr) (printf "   ~s~n" descr))
	       (display-constrained `(,num 10) "  " `(,expr 40)
				    " -> ")
	       (if (procedure? intended)
		   (printf "Satisfy oracle? ~s: " intended)
		   (display-constrained `(,intended 20) ": "))

	       (let ((result (eval expr)))
	       
;	       (newline)
	       (if (or (and (procedure? intended) ;; This means its an oracle
			    (intended result))
		       (teq? intended result)) ;; Otherwise its an expected answer
		   (begin
		     (if (procedure? intended)
			 (printf "~s, " result))
		     (printf "PASS~n"))

		   (begin (set! success #f)
			  (newline)
			  (if (procedure? intended)
			      (printf "FAIL: Expected result to satisfy procedure: ~s~n" intended)
			      (begin 
				(printf "FAIL: Expected: ~n")			  
				(pretty-print intended)))
			  (printf "~n      Received: ~n")
			  (write result)
;			  (display-constrained `(,intended 40) " got instead " `(,result 40))  
			  (printf "~n~nFor Test: ~n")
			  (pretty-print expr)
			  (newline) 
			  (return (void))
			  ))))
	     (iota (length tests))
	     tests descriptions intended)
	    ))))))))

(define display-constrained
  (lambda args
    (for-each 
     (lambda (arg)
       (if (string? arg)
	   (display arg)
	   (let ([arg (car arg)]
		 [bound (cadr arg)]
		 [port (open-output-string)])
;	     (pretty-print arg port)
	     (write arg port)   (newline port)
	     (let ((str (get-output-string port)))
	       (if (> (string-length str) bound)
		   (begin (display (substring str 0 (max 0 (- bound 3))))
			  (display "..."))
		   ;; Gotta cut off the newline.
		   (display (substring str 0 (- (string-length str) 1))))))))
	      args)))


(define iota
  (case-lambda
    [(n) (iota 0 n)]
    [(i n)
     (if (= n 0)
         '()
         (cons i (iota (+ i 1) (- n 1))))]))
  
;;========================================

(define these-tests  
  `([(tsort '((shirt tie belt)
	     (tie jacket)
	     (belt jacket)
	     (watch)
	     (pants shoes belt)
	     (undershorts pants shoes)
	     (socks shoes))
            eq?)
    unspecified]
  [(tsort '((a b) (b c) (c a)
	     (d e) (e f))
           eq?)
    unspecified])
  )

(define test-this (default-unit-tester "" these-tests))

)

;(require tsort)
;(tsort '((a b) (b c) (c a)
;	     (d e) (e f))
;           eq?)

;(define x (make-hash-table))
;(define put hash-table-put!)
;(define get hash-table-get)

