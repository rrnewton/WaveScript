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

    (define unknown-place 'X?)
    (define noplace '_)
	   
    (define (process-let expr env)
      (disp "processing let" expr)
      (match expr
	 [(lazy-letrec ([,lhs* ,[process-expr -> rhs* form* memb*]] ...) ,expr)	 
	  (disp "got stuff from process expr back" rhs* form* memb*)
	  `(lazy-letrec ([,lhs* ,rhs*] ...) ,expr)]))
    
    (define (new-place) (unique-name 'X))

    (define (process-primapp prim args)
      (let ([expr (cons prim args)])
	(case prim
	  [(anchor-at) (values expr unknown-place (new-place))]
	  [(circle) (values expr (new-place) (list (new-place)))]
	  )))
	  
;	       [(anchor-at ,loc) (values expr '_ (new-place))]
;	       [(circle ,anch ) (values expr '_ (new-place))]

    (define process-expr
      (lambda (expr)
        (match expr
          [(quote ,const) (values `(quote ,const) noplace noplace)]
          [,var (guard (symbol? var)) (values var noplace noplace)]
          [(lambda ,formalexp ,expr)
	   (process-let expr (union formalexp env))]

	  ;; Hmm... if I can tell at compile time I should narrow this!
          [(if ,test ,conseq ,altern)
	   (values `(if ,test ,conseq ,altern) 
		   unknown-place unknown-place)]	  	 
          [(,prim ,rand* ...)	   
           (guard (regiment-primitive? prim))
	   (process-primapp prim rand*)]
          [,unmatched
	   (error 'TEMPLATE "invalid syntax ~s" unmatched)])))

    
    `(,input-language (quote (program (props ,proptable ...) 
				      ,(process-let letexpr))))])))
