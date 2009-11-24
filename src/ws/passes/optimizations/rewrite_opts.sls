#!r6rs

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


(library (ws passes optimizations rewrite_opts)
  (export special-rewrite-libfuns
	  rewrite-rule-table
	  rewrite-rules	   
	  test-rewrite
	  )
  (import (except (rnrs (6)) error) (ws common) 	   
	   ;"normalize_query/ws-remove-complex-opera.ss"
	   ;"../compiler_components/type_environments.ss"
	   )


;; ================================================================================
;; Rewrite rules


;; These are the built-in rewrite rules.
;;
;; FOR NOW DO NOT USE A PATTERN VARIABLE TWICE IN THE OUTPUT! (duplicates code)
;; In the future we could add some protection (extra let-bindings).
(define rewrite-rule-table 
  ;; We should actually be able to do this opt for any NONPOSITIVE gap:
  ;; But for now just limiting it to the zero-gap case.
  '(["rewindow/rewindow"
     (rewindow (rewindow ,strm ,wid1 ,(nonpositive? gap1)) ,wid2 ,(nonpositive? gap2))
     (rewindow ,strm ,wid2 ,gap2)]

    ["dewindow/window"  (dewindow (window ,strm ,wid)) ,strm]

    ["window/dewindow"  (window (dewindow ,strm) ,wid) (rewindow ,strm ,wid '0)]

    ["to-from freq"     (toFreq ,x (fromFreq ,y ,s))  ,s]
    ["from-to freq"     (fromFreq ,x (toFreq ,y ,s))  ,s]

    ))

;; Unification will be triggered in the first place by the occurrence
;; of an application of the head operator (e.g. rewindow).

(define special-rewrite-libfuns ;rewrite-heads  
  (ASSERT list-is-set? (map caadr rewrite-rule-table)))

(define rewrite-heads special-rewrite-libfuns)

;; ================================================================================
;;; Helpers:

;; Subst := #f | ([name . expr] ...)
;; Hack to short-circuit when we hit a #f (like 'and').
(define-syntax mergesubsts
  (syntax-rules ()
    [(_ e) e]
    [(_ e1 e* ...)
     (let ([s1 e1])
       (and s1
	    (let ([rest (mergesubsts e* ...)])
	      (and rest 
		   (reallymerge s1 rest)))))]))


(define (reallymerge s1 s2)
  (append s1 s2)
  ;(union s1 s2)
  )
(define (empty-subst) '())

(define (empty-env) '())
(define (apply-env env v) (let ([entry (assq v env)]) (and entry (cadr entry))))

;; ================================================================================
;; Unify code with the LHS of a rule.
;;

;; Should predicates be on code or values?  If values, using what
;; value rep?  The one from interpret-meta?  The Scheme rep plus a
;; type?

;; Trying to use scheme value representations for the predicates:
(define (get-value expr env)
  (match expr
    [(quote ,val) (box val)] ;; NEED TO RETURN TYPE ALSO!
    [(assert-type ,_ ,[x]) x]
    [(gint ,[c]) c]
    [,vr (guard (symbol? vr))
	 (let ([entry (apply-env env expr)])
	   (if entry (get-value entry env)
	       #f))]
    [,oth (error 'rewrite-rules:get-value "don't know how to get a value from this expr: ~s" oth)]))


;; Tries to unify an expression with a pattern.
;; Should I use mutable cells like with type unification?
(define (unify expr pat env)

  (define (resolve expr) (peel-annotations expr))
  ;; [2007.10.22] DISABLING this: smoosh-together gets rid of
  ;; let-bindings that might be in the way.
#;
  (define (resolve expr)
    ;; Resolve the expression further if we can:
    (if (symbol? expr) ;; Resolve variables.
	(or (apply-env env expr) expr)
	expr))
  (match pat
    ;; Rule-supplied predicates:
    ;; If we wanted to optimize this pass we should maybe put off
    ;; checking the predicates until the end.  That is, make sure the
    ;; pattern matches otherwise.
    [(,uq (,pred ,name)) (guard (eq? uq 'unquote))
     (ASSERT (symbol? pred)) (ASSERT (symbol? name))
     (let ([value (get-value expr env)])
       ;; Could we get it down to a value?
       (if (not value) #f 
	   (let ([predfn 
		  ;; [2008.04.30] Changing this, put those functions directly in!!
		  ;(top-level-value pred)
		  ;pred
		  ;; Eval the pred:
		  (reg:top-level-eval pred)
		  ])
	     ;; Check the actual predicate:
	     ;(printf "Checking predicate ~s against ~s\n" predfn expr)
	     (if (predfn (unbox value))
		 `([,name . ,expr])
		 #f))))]

    ;; Pattern variable, unifies with anything:
    [(,uq ,name) (guard (eq? uq 'unquote)) 
     (ASSERT (symbol? name))
     `([,name . ,expr])] ;; Return a substitution

    [,else 

     ;; Finally we try to combine them:
     (match (vector (resolve expr) pat)
       [#((,[resolve -> fn] ,x* ...) (,op ,pat* ...))
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

;; ================================================================================
;; Apply a substitution to the RHS of a rule.
;;
;; We currently lose any type or src-position annotationsin this
;; process... we could try to fix that at some point.
(define (instantiate-rule rulerhs subst)
  (match rulerhs
    [(,uq ,name) (guard (eq? uq 'unquote)) 
     (ASSERT (symbol? name))
     (cdr (ASSERT (assq name subst)))]
    [(quote ,c) `(quote ,c)]
    [(,op ,[args] ...) (guard (symbol? op))
     (cons op args)]))


;; ================================================================================
;;; The Actual Compiler Pass


(define-pass rewrite-rules
  [Expr (lambda (xp fallthr)
	  (match xp    
	    ;; Currently we apply rewrite rules from the inside out.
	    ;; We could do the opposite... there's no good principle to guide us.
	    [(,special ,[arg*] ...) 
	     (guard (symbol? special) (memq special rewrite-heads))
	     (define pos (list-find-position special rewrite-heads))
	     (define rule (list-ref rewrite-rule-table pos))
;;;	     (printf "Trying to unify ~s \nwith   ~s\n" (cons special arg*) (cadr rule))	     
	     (let ([subst (unify (cons special arg*) (cadr rule) (empty-env))])
	       (if subst 
		   (begin 
		     (printf " *** OPTIMIZATION: RULE-FIRED: ~s\n" (car rule))
		     (instantiate-rule (caddr rule) subst))
		   (cons special arg*)))]
	    [,oth (fallthr oth)]))])


;; ================================================================================
;; Currently using eval for pred names, so put these at top-level:

(define test-rewrite #f)
#;
(define-testing test-rewrite
  (default-unit-tester 
    "Rewrite-opts: Apply rewrite rules to optimize program." 
    `(      
      )))

;(define-top-level-value 'foobar (lambda (x) x))
(define-top-level-value 'nonpositive? "error-having-weird-ikarus-problems")
;(define ___  )
;; Side effect!
;(define-top-level-value 'nonpositive? (lambda (n) (<= n 0)))


#|
      [(',unify '(foo '3 '4) ,''(foo ,x ,y) (',empty-env))
       ((x . '3) (y . '4))]

      [(',unify '(foo (bar '3) (baz '4))
		,''(foo (bar ,x) (baz ,y)) (',empty-env))
       ((x . '3) (y . '4))]

      #;
      ["Now test lookups in the environment."
       (',unify   '(foo myvar (baz '4)) 
		 ,''(foo (bar ,x) (baz ,y))
		 '([myvar (bar '3)]))
       ((x '3) (y '4))]

|#



) ;; End module
