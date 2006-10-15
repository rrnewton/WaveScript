
;; Pass: eta-primitives
;; [2004.06.24]

;; This simple pass eta expands all operand-context references to
;; primitives, hereafter primitive-names will only occur in operator
;; context.  This is just a simple, regularizing pass.

; (module pass01_eta-primitives mzscheme
; 	(require (lib "include.ss")
;                   "iu-match.ss"
;                   "prim_defs.ss"
; 		  (all-except "helpers.ss" test-this these-tests)
; 		  (all-except "regiment_helpers.ss" test-this these-tests)
;                   "grammar_checker.ss"
;                   (all-except "hm_type_inference.ss" test-this these-tests)                  
;                   )

; 	(provide eta-primitives 
; 		 ;test-this ;test000
; 		 )

;; DEPENDS: list-head, get-primitive-entry, regiment-primitive?

;; [2006.10.06] Rewriting to use generic-traversal:
(define eta-primitives
  (build-compiler-pass ;; This wraps the main function with extra debugging
   'eta-primitives
   `(input)
   ;; This insures (among other things) that any new lambda's we generate have types attached:
   `(output) ;(grammar ,eta_prim_gramar PassInput))
   (let ()
     (define process-expr*
       (lambda (expr* env)
	 (map (lambda (expr) (process-expr expr env)) expr*)))
     (define (process-expr expr env)
       (core-generic-traverse
	;; Driver
	(lambda (x fallthrough) 
	  (match x
	    ;; Variable References to primitives are rewritten:
	    [,var (guard (symbol? var) (regiment-primitive? var))
		(let* ([possible-formals '(a b c d e f g h i j)]
		       [args (cadr (get-primitive-entry var))])
		  (if (regiment-constant? var)
		      var
		      (let ([formals (list-head possible-formals (length args))])
			`(lambda ,formals 
			   ; Primitive types:
			   ,(map export-type (rdc (rdc (prim->type var))))
			   (,var ,@formals)))))]

	  ;; A bit of sugar.
          ;; This just expands "lists" into a bunch of cons cells.
	  [(list ,[rands] ...)
	   (process-expr (match rands
			   [() ''()]
			   [(,a . ,[b]) `(cons ,a ,b)])
			 env)]
	  ;; More sugar.
	  [(or ,[rands] ...)
	   (process-expr (match rands
			   [() ''#f]
			   [(,a . ,[b]) `(if ,a '#t ,b)])
			 env)]
	  [(and ,[rands] ...)
	   (process-expr (match rands
			   [() ''#t]
			   [(,a . ,[b]) `(if ,a ,b '#f)])
			 env)]

	  ;; I have mixed feelings about this (makes intermediate programs hard to read)
	  [(let ([,x ,t ,[y]] ...) ,[body])
	   `((lambda ,x ,t ,body) ,y ...)
	   ]

	  ;; Yucky, internal:
	  ;; Even more sugar.  This is convenient for doubletons:
	  [(tcar ,[v]) `(tupref '0 '2 ,v)]
	  [(tcdr ,[v]) `(tupref '1 '2 ,v)]

	  ;; Primitives that are applied with "app" have it taken away:
	  [(app ,prim ,[rands] ...)
	   (guard (regiment-primitive? prim))
	   `(,prim ,rands ...)]
	  
	  [,other (fallthrough other)]))
	;; Fuser
	(lambda (ls k) (apply k ls))
	;; Expression:
	expr))
     ;; Main pass body:
    (lambda (expr)
      (match expr
	     [(,input-language (quote (program ,body ,type)))
	      (let ([body (process-expr body '())])
		`(eta-primitives-language '(program ,body ,type)))])))))


(define test-this 
  (default-unit-tester " 1: Eta-Primitives: remove non-operator usages of primitive names."
    `(
      ["Simple test of eta-primitives"
       (eta-primitives '(base-language
			 '(program
			      (rfold + 0 (rmap nodeid (khood (anchor-at 50 10) 2)))
			    (Signal Integer))))
       (eta-primitives-language
	'(program
	     (rfold
	      (lambda (a b) (Integer Integer) (+ a b))
	      0
	      (rmap
	       (lambda (a) (Node) (nodeid a))
	       (khood (anchor-at 50 10) 2)))
	   (Signal Integer)))]

      )))

(define test01 test-this)
;)



