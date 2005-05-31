
;; QUESTION: SHOULD let-stored bound variables be able to be referred
;; to externally with "ext-refs" or should they be local.  They
;; probably should be local-only, but I haven't implemented it that
;; way now.


;; Input Grammar:

;;;  <Pgm> ::= (program (bindings <Cbind>*) <NodePgm>)
;;;  <NodePgm> ::= (nodepgm (tokens <TokBinding>*))
;       NOTE: tokens will inclide a binding for SOC-start and node-start
;;;  <Cbind> ::= (<var> <Exp>)
;       NOTE: This expressions will be statically calculable -- constants.
;;;  <TokBinding> ::= (<TokName> <SubtokId> (<Var> ...) (bindings <Cbind>*) (stored <Stored>) <Expr>)
;;;  <TokName>   ::= <Symbol> 
;;;  <SubtokId>  ::= <Number>
;;;  <Token>     ::= (tok <TokName>) | (tok <Tokname> <Int>)
;;;  <DynToken>  ::= (tok <Tokname> <Expr>) | <Token>
;;;     NOTE: Either the whole token reference or just the sub-index can be dynamic.
;;;  <Expr>      ::= (quote <Constant>)
;;;                | <Var>
;;;                | <DynToken>
;;;                | (set! <Var> <Expr>)
;;;                | (ext-ref <Token> <Var>)
;;;                | (ext-set! <Token> <Var> <Expr>)
;       NOTE: These are static token refs for now.
;;;                | (begin <Expr> ...)
;;;                | (let ((<Symbol> <Expr>)) <Expr>)
;;;                | (if <Expr> <Expr> <Expr>)
;;;                | (subcall <DynToken> <Expr>...)
;;;                | (return <Expr>)
;;;                | (<Prim> <Expr> ...)
;;;                | (<Expr> ...)
;;;                | <Sugar> 
;;;                | (leds <Red|Yellow|Green> <On|Off|Toggle>)
;;;  <Prim> ::= <BasicPrim> 
;;;           | call | subcall | timed-call | bcast
;;;           | is_scheduled | deschedule | is_present | evict
;;;  <Sugar>  ::= (flood <Expr>)
;;;           | (elect-leader <Token> [<Token>])
              ;; <TODO> optional second argument.. decider

(define desugar-let-stored
  (let ()


    ;; This is confusing, but there are so many small traversals of
    ;; the program tree in this pass that it is much easier to reuse this tree walk:
    ;;
    ;; NOTE: A common failure mode when using this is invoking the
    ;; wrong loop when making a recursive pattern match.  Be careful.
    (define (generic-traverse driver fuse e)
      (let loop ((e e))
	(driver e 
	   (lambda (x)
	     (match x
;		    [,x (guard (begin (printf "~nGenTrav looping: ") (display-constrained (list x 50)) (newline) #f)) 3]
		    [,const (guard (constant? const)) (fuse () const)]
		    [(quote ,const)                (fuse ()   `(quote ,const))]
		    [,var (guard (symbol? var))    (fuse ()    var)]
		    [(tok ,tok)                    (fuse ()   `(tok ,tok))]
		    [(tok ,tok ,n) (guard (integer? n)) (fuse () `(tok ,tok ,n))]
		    [(tok ,tok ,[loop -> expr])    (fuse (list expr) `(tok ,tok ,expr))]
		    [(ext-ref ,tok ,var)           (fuse ()   `(ext-ref ,tok ,var))]
		    [(ext-set! ,tok ,var ,[loop -> expr])  
		                                   (fuse (list expr) `(ext-set! ,tok ,var ,expr))]
		    [(set! ,v ,[loop -> e])        (fuse (list e)    `(set! ,v ,e))]
		    [(leds ,what ,which)           (fuse () `(leds ,what ,which))]
		    [(begin ,[loop -> x] ...)      (fuse x           `(begin ,x ...))]
		    [(if ,[loop -> a] ,[loop -> b] ,[loop -> c])
		                                   (fuse (list a b c) `(if ,a ,b ,c))]
		    [(let ([,lhs ,[loop -> rhs]]) ,[loop -> bod])
		                                   (fuse (list rhs bod) `(let ([,lhs ,rhs]) ,bod))]
		    ;; "activate" and the gradient calls have already been desugared:
		    [(,call ,[loop -> rator] ,[loop -> rands] ...)
		     (guard (memq call '(bcast subcall call)))
		     (fuse (cons rator rands) `(,call ,rator ,rands ...))]
		    [(timed-call ,time ,[loop -> rator] ,[loop -> rands] ...)
		     (guard (number? time))		     
		     (fuse (cons rator rands) `(timed-call ,time ,rator ,rands ...))]
		    [(return ,[loop -> x])         (fuse (list x) `(return ,x))]
		    [(,prim ,[loop -> rands] ...)
		     (guard (or (token-machine-primitive? prim)
				(basic-primitive? prim)))
		     (fuse rands `(,prim ,rands ...))]
		    [(app ,[loop -> rator] ,[loop -> rands] ...)
		     (fuse (cons rator rands) `(app ,rator ,rands ...))]
		    [,otherwise
		     (error 'generic-traverse
			    "bad expression: ~s" otherwise)])))))


    ;; Get the free vars from an expression
    (define (free-vars e)
      (generic-traverse
       ;; driver, fuser, expression
       (lambda  (x loop) 
	 (match x
		[,v (guard (symbol? v)) (list v)]
		[(let ([,lhs ,[rhs]]) ,[bod])
		 (append rhs (remq lhs bod))
		 ]

;; FIXME TODO:
;		[(let-stored ...


		[,x (loop x)]))
       (lambda (ls _) (apply append ls))
       e))

#;(define (free-vars e)
  (match e
    [,const (guard (constant? const)) '()]
    [(quote ,const) '()]
    [,var (guard (symbol? var)) (list var)]
    [(tok ,tok ,[expr]) expr]
    [(ext-ref ,tok ,var) '()]
    [(ext-set! ,tok ,var ,[expr]) expr]
    [(begin ,[xs] ...) (apply append xs)]
    [(if ,[test] ,[conseq] ,[altern]) (append test conseq altern)]
    [(let ( (,lhs ,[rhs])) ,[body])
     (append rhs (remq lhs body))]
    [(leds ,what ,which) '()]
    [(,prim ,[rands] ...) 
	 (guard (or (token-machine-primitive? prim)
		    (basic-primitive? prim)))     
	 rands]
    [(,[rator] ,[rands] ...) `(apply append rator rands)]
    [,otherwise
     (error 'cps-tokmac:freevars 
	    "bad expression: ~s" otherwise)]))


(define process-expr 
  (lambda (env expr)
;  (trace-lambda PE (env expr)
  
  (match expr
;    [,x (guard (begin (disp "PEXP" x) #f)) 3]

    [(quote ,const)                    (values () `(quote ,const))]
    [,num (guard (number? num))        (values () num)]
    [(tok ,t ,n) (guard (number? n))   (values () `(tok ,t ,n))]
    [(tok ,t ,[st e])                  (values st `(tok ,t ,e))]
    ;; No renaming or anything here:
    [(ext-ref ,tok ,var)               (values () `(ext-ref ,tok ,var))]
    [(ext-set! ,tok ,var ,[st e])      (values st `(ext-set! ,tok ,var ,e))]

    [,var (guard (symbol? var))        (values () var)]
    [(begin ,[st xs] ...)
     (values (apply append st) (make-begin xs))]
    [(if ,[tst test] ,[cst conseq] ,[ast altern])
     (values (append tst cst ast) 
             `(if ,test ,conseq ,altern))]
    [(set! ,v ,[rst rhs])
     (values rst `(set! ,v ,rhs))]
    [(let ([,lhs ,[rst rhs]]) ,body)
     (mvlet ([(bst newbod) (process-expr (cons lhs env) body)])
	    (values (append bst rst)
		    `(let ([,lhs ,rhs]) ,newbod)))]

;; These are just primitives, they do not need their own form.
;    [(,call-style ,[st* args*] ...)
;     (guard (memq call-style '(call timed-call bcast )))
;     (values (apply append st*)
;             `(,call-style ,args* ...))]

    ;; The semantics of let-stored are that the first time the
    ;; expression is executed (and only the first), the RHS is
    ;; evaluated and stored.
    [(let-stored () ,[st body]) (values st body)]
    [(let-stored ([,lhs1 ,rhs1] [,lhs2 ,rhs2] [,lhs* ,rhs*] ...) ,body)
;     (let ([all-lhs (cons lhs1 (cons lhs2 lhs*))]
;	   [all-free (apply append (map free-vars (cons rhs1 (cons rhs2 rhs*))))])
;       (if (not (null? (intersection all-lhs all-free)))
;	   (error 'desugar-let-stored:process-expr
;		  "let-stored cannot have any recursive bindings"
;		  `(let-stored ([,lhs1 ,rhs1] [,lhs2 ,rhs2] [,lhs* ,rhs*] ...) ,body)
     (process-expr env `(let-stored ([,lhs1 ,rhs1]) 
			  (let-stored ([,lhs2 ,rhs2])
			     (let-stored ([,lhs* ,rhs*] ...) ,body))))]

    [(let-stored ([,lhs ,[rst rhs]]) ,body)
     (let ([newvar (unique-name 'stored-liftoption)])
       (mvlet ([(bst newbod) (process-expr (cons lhs env) body)])
               (values (append `([,lhs 'let-stored-uninitialized] [,newvar '#f]) rst bst)
                       (make-begin 
                        (list `(if ,newvar ;; If first time, initialize
				   (void)
                                   (begin 
                                     (set! ,newvar '#t)
                                     (set! ,lhs ,rhs)))
                               newbod)))))]

    [(leds ,what ,which) (values () `(leds ,what ,which))]
    [(,prim ,[rst* rands] ...)
     (guard (or (token-machine-primitive? prim)
                (basic-primitive? prim)))
     (values (apply append rst*)
             `(,prim ,rands ...))]
    [(app ,[rst1 rator] ,[rst* rands] ...)
     (warning 'desugar-let-stored
              "arbitrary application of rator: ~s~n" rator)
     (values (apply append rst1 rst*)
             `(app ,rator ,rands ...))]

    [,otherwise
	 (error 'desugar-let-stored:process-expr 
		"bad expression: ~s" otherwise)]
	)))
      

(define (process-tokbind tb)
  (mvlet ([(tok id args stored constbinds body) (destructure-tokbind tb)])
         (mvlet ([(newstored newbod) (process-expr (map car constbinds) body)])
                `[,tok ,id ,args (stored ,@stored ,@newstored) ,newbod])))

(lambda (prog)
  (match prog
    [(,lang '(program (bindings ,constbinds ...)
		      (nodepgm (tokens ,[process-tokbind -> toks] ...))))
     `(desugar-let-stored-lang
       '(program (bindings ,constbinds ...)
		 (nodepgm (tokens ,toks ...))))]))
))
