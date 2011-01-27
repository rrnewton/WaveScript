

;; Pass: Add Control Flow
; =======================================================================
;; [2004.08.20] 

;; This creates a separate parallel control flow graph.

;; For now, control starts at the SOC, then hits the leaves and pours
;; through the program.  Basically the whole program is "Push".  In
;; the future we might have control flow moving to intermediate nodes
;; and then spreading up towards the leaves ("Pulling" them).


;; This sets us up to try to start figuring out when a control-flow
;; redirect means a spatial redirect.  (Next we've got to analyze
;; the "places" associated with expressions.)


;; (amap f (circle (anchor-at '(30 40)) 50))
;;  -> (soc anchor circle amap)

;; (amap f (union r1 r2))
;;  -> (soc r1 amap)
;;  -> (soc r2 amap)


;; Input language is Core plus edge classifications, plus heartbeats:

;;; <Pgm>  ::= (program (props <CatEntry>*) <Let>)
;;; <CatEntry>* ::= [<Name> <Prop>*]
;;; <Prop> ::= region | anchor | local | distributed | final | leaf
;;; <Let>  ::= (lazy-letrec (<Decl>*) <var>)
;;; <Decl> ::= (<var> <Heartbeat> <Exp>) 
;;; <Exp>  ::= <Simple>
;;;          | (if <Simple> <Simple> <Simple>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <Simple>*)
;;; <Formalexp> ::= (<var>*)
;;; <Simple> ::= (quote <Lit>) | <Var>

;;; Output language adds a control-flow graph also.
;;; <Pgm>  ::= (program (props <CatEntry>*) (control-flow <CFG>*) <Let>)
;;; <CFG>  ::= (<var>*)

; =======================================================================

;; [2004.12.06] Right now the control flow still isn't used by
;; anything downstream.  It's just a simple dependency graph for the
;; program.  

(module add-control-flow mzscheme
  (require "../../../plt/common.ss")
  (provide add-control-flow)
  (chezimports)

(define add-control-flow
  (let () 
          
  (lambda (expr)
    (match expr
	   [(add-heartbeats-language (quote (program (props ,proptable ...) ,letexpr ,type)))

	    (let ([check-prop 
		   (lambda (p s)
		     (let ((entry (assq s proptable)))
		       (if entry (memq p (cdr entry))
			   (error 'pass10_deglobalize:check-prop
				  "This should not happen!  ~nName ~s has no entry in ~s."
				  s proptable))))])

    ;; Returns control flow graph
    (define (process-let expr)
      (match expr
	 [(lazy-letrec ([,lhs* ,type* ,annots* ,[expr-dependencies -> deps*]] ...) ,expr)
	   (let ( )
	     (apply append
		    (map (lambda (lhs deps)
			   (map (lambda (x) `(,x ,lhs)) deps))
		      lhs* deps*)))]
	 [,other (error 'add-control-flow:process-let "bad lazy-letrec expression: ~s" other)]))

    (define expr-dependencies
      (lambda (expr)
        (match expr
          [(quote ,const) '()]
          [,var (guard (symbol? var) (not(regiment-constant? var)))
		(if (check-prop 'local var)
		    '()
		    (list var))]
	  ;; TODO:
          [(lambda ,formalexp ,types ,expr)
	   '() ;; CHECK UP ON THIS; MAYBE TAKE FREE-VARS??
	   ]
	  ;; Hmm... if I can tell at compile time I should narrow this!

	  ;; So should control flow from the test to the consequent?
          [(if ,[test] ,[conseq] ,[altern])
	   (append test conseq altern)]
	  
          [,prim (guard (regiment-constant? prim)) '()]
          [(,prim ,[rand*] ...)
           (guard (regiment-primitive? prim))	  
	   (apply append rand*)]
          [,unmatched
	   (error 'addplaces:process-let "invalid syntax ~s" unmatched)])))


    (let ([leaves (map car (filter (lambda (entry) (memq 'leaf entry)) proptable))])
      `(add-control-flow-language
	(quote (program (props ,proptable ...)
			(control-flow
			 ,@(graph:simple->vertical
			    (append (map (lambda (x) `(SOC ,x)) leaves)
				    (process-let letexpr))))
			,letexpr
			,type
			)))))]
	   ))))


'(add-control-flow '(add-heartbeats-language
		     '(program
		       (props (result_1 local final))
		       (lazy-letrec ((result_1 #f '3)) result_1)
		       )))

) ; End module
