

;; ================================================================================ ;;
;; Pass properties: complete-ast-coverage
;; 
;; This pass makes sure that effectful constructs (set!, for loops,
;; effectful prims) only occur in effect context, and likewise for value primitives.
;; Let-bindings can occur in either effect or value context.
;;
;; Conditionals and wscase (redundant) are considered value constructs currently.
;;
;; ================================================================================ ;;

(module ws-normalize-context mzscheme
  (require "../../../plt/common.ss")
  (provide ws-normalize-context)
  (chezimports)


;; [2007.12.04] Hmm, this version looks like it naively lifts up
;; effectful constructs even if they are *already* in effect position.
;; It could do better.

(define-pass ws-normalize-context
  (define (effectform? sym) 
    (or (memq sym '(set! print for while))
	;(assq prim wavescript-effectful-primitives)
	))

  (define Value 
    (core-generic-traverse
     (lambda (x fallthru)
       (match x        
	 ;; This catches all effectful prims/constructs and puts them in effect context.
	 ;;[(emit ,[vq] ,[e]) `(begin (emit ,vq ,e) (tuple))]
	 [(,kwd . ,args) (guard (effectform? kwd))
	  `(begin ,(fallthru (cons kwd args)) 'UNIT)]
	 
	 [(,prim ,[simple*] ...) (guard (assq prim wavescript-effectful-primitives))
	  (let ([entry (assq prim wavescript-effectful-primitives)])
	    (match (caddr entry)	      
	      [(quote ,v) `(begin (,prim . ,simple*) 'BOTTOM)] ;; WSerror return value...
	      [#()        `(begin (,prim . ,simple*) 'UNIT)]))]

	 ;; Here we switch over to effect context:
	 [(begin ,[Effect -> e*] ... ,[last])  `(begin ,@e* ,last)]
	 [,x (fallthru x)]))
     (lambda (ls k) (apply k ls))))

  (define (Effect x)
    (define (lift-into-let code)
      (let ([tmp (unique-name "ignored_valInEffect")])
	`(let ([,tmp 'anyeffectlift ,code])
	   'UNIT)))
    (match x 
      [,v (guard (symbol? v)) ''UNIT]
      [(deref ,v) (ASSERT (symbol? v)) ''UNIT]
      [(tuple) ''UNIT]
      [(set! ,v ,[Value -> e]) `(set! ,v ,e)]
      [(print ,[Value -> e])   `(print ,e)]
      [(for (,i ,[Value -> st] ,[Value -> en]) ,[bod])
       `(for (,i ,st ,en) ,bod)]
      [(while ,[Value -> test] ,[bod]) `(while ,test ,bod)]
      [(,prim ,[Value -> simple*] ...)
       (guard (assq prim wavescript-effectful-primitives))
       (cons prim simple*)]
      [(let ([,lhs* ,ty* ,[Value -> rhs*]] ...) ,[bod])
       `(let ,(map list lhs* ty* rhs*) ,bod)]

;; Subsuming these two with a fallthrough case at the bottom.
#|
      ;; For simplicity we are not allowing conditionals directly in effect context.
      ;; [2007.12.06] Disabling this because it produces worse output code:
      [(if ,[Value -> a] ,[Value -> b] ,[Value -> c])
       (lift-into-let `(if ,a ,b ,c))]
      ;; Same for case statements as conditionals: 
      ;; (eventually it would be nice to compile conditions *to* case statements a la GHC)
      [(wscase ,[Value -> x] (,tag* ,[Value -> fun*]) ...)
       (lift-into-let `(wscase ,x ,@(map list tag* fun*)))] 
|#

      [(begin ,[e*] ...) (make-begin e*)]

      ;; Now we address normal value primitives.
      ;; Some of these may go away (optimization)
      [(,prim ,rand* ...) (guard (regiment-primitive? prim))
       (if (>= (ws-optimization-level) 3)
	   ;; [2007.12.22] This is the most extreme version: 
	   ;; Kill all value primitives, assume no error conditions.  (And by definition, no side effects.)
	   (make-begin (map Effect rand*))
	   (error 'ws-normalize-context "Value primitive in effect context, temporarily an error:\n ~s\n" (cons prim rand*)))]

      ;; Anything else we force into a value context by introducing a let binding
      ;; This might be dangerous, because there are cases we may be forgetting about.
      [,form (lift-into-let (Value form))]
      ;[,form (error 'ws-normalize-context "unhandled form in effect context: ~s" form)]
      ))
  
  ;; We start out in value context.
  [Expr (lambda (x _) (Value x))])


) ;; End module