
;;;; .title Algebraic rewrite optimizations.
;;;; .author Ryan Newton

;;;; [2007.10.20] UNFINISHED, EXPERIMENTAL, UNDER DEVELOPMENT:

;;;; NOTE NOTE NOTE: WE NEED TO REMEMBER TO BE CAREFUL WITH PURITY.

;;;; This applies a set of rewrite rules to optimize the program.

;;;; Currently they are applied unidirectionally and unconditionally.
;;;; In the future we may use profiling information or other
;;;; heuristics to drive the rewrite process.

;;;; We use a simple little language for expressing the rewrite-rules
;;;; themselves.  Here's its grammar: <br>
;   Rule := [Pat Pat]
;   Pat  := (Oper Pat* ...)
;   Pat  := ,Var
;   Pat  := 'Const
;   Pat  := ,(PredName Var)
;
;   Oper := <symbol>
;   Var  := <symbol>
;   Const := number, etc
;   PredName  := symbol bound to function returning #t/#f
;
;; Actually, a Pred should only occur in the LHS of the rule...  We
;; can use (eval _) for getting to these preds, or we could look them
;; up in a table; either way is fine...  I would just put the closures
;; straight in the pattern representation but that would be ugly
;; because we've already used unquote ;).

;; ================================================================================
;; Rewrite rules

;; These are the built-in rewrite rules.
(define rewrite-rules 
  ;; We should actually be able to do this opt for any NONPOSITIVE gap:
  ;; But for now just limiting it to the zero-gap case.
  '([(rewindow (rewindow ,strm ,wid1 ,(nonpositive? gap1)) ,wid2 ,(nonpositive? gap2))
     (rewindow ,strm ,wid2 ,gap2)]

    ))

;; Unification will be triggered in the first place by the occurrence
;; of an application of the head operator (e.g. rewindow).


;; ================================================================================

(define-syntax mergesubsts
  (syntax-rules ()
    [(_ e) e]
    [(_ e1 e* ...)
     (let ([s1 e1])
       (and s1
	    (let ([rest (mergesubsts e* ...)])
	      (and rest 
		   (reallymerge s1 rest)))))]))

;; Tries to unify an expression with a pattern.
;; Should I use mutable cells like with type unification?
(define (unify expr pat env)
  (define (resolve expr)
    ;; Resolve the expression further if we can:
    (if (symbol? expr) ;; Resolve variables.
	(or (apply-env env expr) expr)
	expr))
  (match pat
    ;; Rule-supplied predicates:
    ;;a
    ;; If we wanted to optimize this pass we should maybe put off
    ;; checking the predicates until the end.  That is, make sure the
    ;; pattern matches otherwise.
    [(,uq (,pred ,name)) (guard (eq? uq 'unquote))
     (ASSERT (symbol? pred)) (ASSERT (symbol? name))
     (let ([value (get-value expr env)])
       ;; Could we get it down to a value?
       (if (not value) #f 
	   (let ([predfn (top-level-value pred)])
	     (inspect (predfn (unbox value))))))]

    ;; Pattern variable, unifies with anything:
    [(,uq ,name) (guard (eq? uq 'unquote)) 
     (ASSERT (symbol? name))
     `([,name ,expr])] ;; Return a substitution

    [,else 

     ;; Finally we try to combine them:
     (match (vector (resolve expr) pat)
       [#((app ,[resolve -> fn] ,x* ...) (,op ,pat* ...))
	(ASSERT symbol? op)
	(if (eq? fn op)
	    (unify* x* pat* env)
	    #f)]
       
       [,_ 	
	;(printf "   COULDNT MATCH: ~s\n" _)	
	#f] ;; Otherwise just fail.
       )]))

(define (unify* ex* pt* env)
  (if (null? ex*) (empty-subst)
      (mergesubsts (unify  (car ex*) (car pt*) env)
		   (unify* (cdr ex*) (cdr pt*) env))))


(define (reallymerge s1 s2)
  (append s1 s2)
  ;(union s1 s2)
  )
(define (empty-subst) ())

(define (empty-env) ())
(define (apply-env env v) (let ([entry (assq v env)]) (and entry (cadr entry))))



;; ================================================================================
;;; The Actual Compiler Pass





;; ================================================================================
;;; Predicates

;; Should predicates be on code or values?  If values, using what
;; value rep?  The one from interpret-meta?  The Scheme rep plus a
;; type?

;; Trying to use scheme value representations for the predicates:
(define (get-value expr env)
  (match expr
    [(quote ,val) (box val)] ;; NEED TO RETURN TYPE ALSO!
    [,vr (guard (symbol? vr))
	 (let ([entry (apply-env env expr)])
	   (if entry (get-value entry env)
	       #f))]
    [,oth (error 'rewrite-rules:get-value "don't know how to get a value from this expr: ~s" oth)]))

;; Currently using eval for pred names, so put these at top-level:
(define-top-level-value 'nonpositive? (lambda (n) (<= n 0)))

(define-testing rewrite-tests
`(

  [(,unify '(app foo '3 '4) ,''(foo ,x ,y) (,empty-env))
   ((x '3) (y '4))]

  [(,unify   '(app foo (app bar '3) (app baz '4))
	   ,''(foo (bar ,x) (baz ,y)) (,empty-env))
   ((x '3) (y '4))]

  ["Now test lookups in the environment."
   (,unify   '(app foo myvar (app baz '4)) 
	   ,''(foo (bar ,x) (baz ,y))
	  '([myvar (app bar '3)]))
   ((x '3) (y '4))]
  
  ))

(define-testing test-rewrite
  (default-unit-tester 
    "Rewrite-opts: Apply rewrite rules to optimize program." 
    rewrite-tests))
