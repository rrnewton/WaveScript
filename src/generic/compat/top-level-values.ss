
 
;; This defines a system of top-level "global" variables for R6RS
;; implementations which don't allow direct access to the top-level
;; environment.  

(define top-table (make-eq-hashtable 200))

(define (define-top-level-value var val) (hashtable-set! top-table var val))
(define (set-top-level-value! var val)   (hashtable-set! top-table var val))

(define (top-level-bound? var) (hashtable-ref top-table var #f))
(define (top-level-value var)  (hashtable-ref top-table var #f))

#;
(define default-environment
  (environment '(rnrs (6)) '(rnrs r5rs (6)) '(rnrs mutable-pairs (6)) '(rnrs mutable-strings (6))
	       '(main_r6rs)))

;; (Inefficient) This evaluates something inside the virtual top-level
;; environment.  This is unfinished, it also needs to import the
;; top-level WS/Regiment module.
(define reg:top-level-eval  
  (let ()
  ;; Here's a little hack that transforms (define v e) expressions into
    ;; explicit top-level-value calls.
    (define (eval-preprocess x)
      ;; UNLESS we do a little hack.
      ;; We manage a virtual top-level environment ourselves:
      (cond
       [(and (list? x) (= (length x) 3) (eq? (car x) 'define) (symbol? (cadr x)))
	(if (pair? (cadr x))
	    `(define-top-level-value ,(caadr x) (lambda ,(cdadr x) ,(caddr x)))
	    `(define-top-level-value ,(cadr x) ,(caddr x))
	    )]
       [else x]))
    (case-lambda 
      [(exp) (reg:top-level-eval 
	      exp 
	      ;;default-environment
	      (environment '(rnrs (6)) '(rnrs r5rs (6)) 
			   '(rnrs mutable-pairs (6)) '(rnrs mutable-strings (6)) 
			   '(main_r6rs) '(main))
	      )]
      [(exp env)
       (call-with-values (lambda () (hashtable-entries top-table))
	 (lambda (keys vals)
	   (define bound (vector-length keys))
	   (define bindsacc '())
	   (let loop ([i 0])
	     (if (fx=? i bound) (void)
		 (begin 
		   ;;(set! bindsacc (cons (list (vector-ref keys i) (vector-ref vals i)) bindsacc))
		   (set! bindsacc `((,(vector-ref keys i) ',(vector-ref vals i)) . ,bindsacc))
		   (loop (fx+ 1 i)))))
	   ;;(printf "Extending with bindings: ~s\n" bindsacc)
	   (eval `(let ,bindsacc ,(eval-preprocess exp)) env)))
       ])))

