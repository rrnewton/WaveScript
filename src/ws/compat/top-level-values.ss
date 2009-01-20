
 
;; This defines a system of top-level "global" variables for R6RS
;; implementations which don't allow direct access to the top-level
;; environment.  

(define top-table (make-eq-hashtable 200))

(define (define-top-level-value var val) (hashtable-set! top-table var val))
(define (set-top-level-value! var val)   (hashtable-set! top-table var val))

(define special-value (lambda (x) x))
(define (top-level-bound? var) (hashtable-ref top-table var #f))
(define (top-level-value var)  
  (let ([result (hashtable-ref top-table var special-value)])
    (if (eq? result special-value)
	(error 'top-level-value "unbound: ~a" var)
	result)))

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
      (if (pair? x)
	  (cond 
	   [(eq? (car x) 'define)
	    (if (pair? (cadr x))
		`(define-top-level-value ',(caadr x) (lambda ,(cdadr x) ,@(cddr x)))
		`(define-top-level-value ',(cadr x) ,(caddr x)))]
	   ;; Go inside begins:
	   ;[(eq? (car x) 'begin)    ]
	   [else x])
	  x))
    (case-lambda 
      [(exp) (reg:top-level-eval 
	      exp 
	      ;;default-environment
	      (environment '(except (rnrs (6)) error) '(rnrs r5rs (6)) 
			   '(rnrs mutable-pairs (6)) '(rnrs mutable-strings (6)) 
			   '(main_r6rs) '(main))
	      )]
      [(exp env)
       
       
       ;; FIXME FIXME : Redefine the existing bindings as
       ;; identifier-syntax that references the *current* value of
       ;; each binding.  However, even then there will be problems if
       ;; the code tries to define a new top-level value, and then
       ;; access it as a normal variable binding.
       (if (and (pair? exp) (eq? (car exp) 'begin))
	   ;; Go inside begins:
	   (for-each (lambda (e) (reg:top-level-eval e env))
	     (cdr exp))
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
	       ;(printf "Extending with bindings: ~s\n" bindsacc)
	       ;(printf "Expr: \n")
	       ;(pretty-print (eval-preprocess exp))
	       (eval `(let ,bindsacc ,(eval-preprocess exp)) env)
	       #;
	       (for-each (lambda (e)
			   (eval `(let ,bindsacc ,e) env))
		 (eval-preprocess exp)))))
       ])))

