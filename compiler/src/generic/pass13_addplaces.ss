;; [2004.08.13]
;; Adding this to try to clean things up, and to try to start to
;; reason about events.

;; Input language is core plus edge annotations plus heartbeats.

;;; <Pgm>  ::= (program (props <CatEntry>*) <Let>)
;;; <CatEntry>* ::= [<Name> <Prop>*]
;;; <Prop> ::= region | anchor | local | distributed | final | leaf
;;; <Let>  ::= (lazy-letrec (<Decl>*) <var>)
;;; <Decl> ::= (<var> <Heartbeat> <Exp>) 
;;; <Heartbeat> ::= <Float> | #f | +inf.0
;;; <Exp>  ::= <Simple>
;;;          | (if <Simple> <Simple> <Simple>)
;;;          | (lambda <Formalexp> <Let>)
;;;          | (<primitive> <Simple>*)
;;; <Formalexp> ::= (<var>*)
;;; <Simple> ::= (quote <Lit>) | <Var>

;; Output language expands the <Decl> even more with formation and membership places
;;; <Decl> ::= (<var> <Heartbeat> <FormPlace> <MembPlace> <Exp>) 
;;; <FormPlace> = 
;;; <MembPlace> ::= X?       (Unknown place)
;;;               | _        (no place)
;;;               | X_<n>    (Some place...)

;;; [2004.08.13] Need to consider conditionals:
;;;   (if #t (circle...) (khood...))
;;; While they have the same "type", there will obviously be different
;;; places associated with different branches of a conditional.  So
;;; what should my analysis do?  Well if it's not known till runtime
;;; what will come out, this is really a place where it has to say
;;; *unknown*!

(define addplaces
  (lambda (expr)
    (match expr
	   [(,input-language (quote (program (props ,proptable ...) ,letexpr)))

    (define unknown-place '?) ;'X?)
    (define noplace '_)
	   
    (define (process-let expr)
      (disp "processing let" expr)
      (match expr
	 [ (lazy-letrec ([,lhs* ,heartbeat* ,[process-expr -> rhs* form* memb*]] ...) ,expr)	 

;	  (lazy-letrec ([,lhs* ,heartbeat* ,rhs*] ...) ,expr)

;	  (let ([stuff (map (lambda (rhs) (call-with-values (lambda () (process-expr rhs)) (lambda args args)))
;			    rhs*)]
;		[form* (map (lambda (_) 0) rhs*)]
;		[memb* (map (lambda (_) 0) rhs*)]
;		)
;	    (disp "got stuff" stuff)
;	    (disp "for rhs" rhs*)

	  `(lazy-letrec ([,lhs* ,heartbeat* ,form* ,memb* ,rhs*] ...) ,expr)]
	 [,other (error 'addplaces:process-let "bad lazy-letrec expression: ~s" other)]))
    
    (define (new-place) (unique-name 'X))

    (define (process-primapp prim args)
      (let ([expr (cons prim args)])
	(case prim
	  [(anchor-at) (values expr unknown-place (new-place))]
	  ;; Both of these start in the center and spread to some extent.
	  [(circle khood) (values expr (new-place) (list (new-place)))]
	  ;; Can we say something about cluster?  Disregarding the
	  ;; *type* cluster does not change the physical extent...
	  [(cluster) (let ([newp (list (new-place))])
		       (values expr newp newp))]
		  
	  [(cons) (values expr noplace noplace)]
	  [else (error 'addplaces:process-primapp "unhandled prim: ~s" prim)]
	  )))
	  
;	       [(anchor-at ,loc) (values expr '_ (new-place))]
;	       [(circle ,anch ) (values expr '_ (new-place))]

    (define process-expr
      (lambda (expr)
	(disp "process expr" expr)
        (match expr
          [(quote ,const) (values `(quote ,const) noplace noplace)]
          [,var (guard (symbol? var)) (values var noplace noplace)]
          [(lambda ,formalexp ,expr)
	   (values (process-let expr) noplace noplace)]
	  ;; Hmm... if I can tell at compile time I should narrow this!
          [(if ,test ,conseq ,altern)
	   (values `(if ,test ,conseq ,altern) unknown-place unknown-place)]
          [(,prim ,rand* ...)	   
           (guard (regiment-primitive? prim))
	   (process-primapp prim rand*)]
          [,unmatched
	   (error 'addplaces:process-let "invalid syntax ~s" unmatched)])))
    
    `(,input-language (quote (program (props ,proptable ...) 
				      ,(process-let letexpr))))])))
