

(module insert-refcounts mzscheme 
  (require  "../../../plt/common.ss"
	    ;(all-except (lib "list.ss") sort sort! filter)
	    ;(all-except "nominalize-types.ss" test-this these-tests)
	    ;"convert-sums-to-tuples.ss"
	    ;"../../compiler_components/c_generator.ss" 
	    )
  (provide insert-refcounts
	   heap-allocated?
	   gather-heap-types)
  (chezimports (except helpers test-this these-tests))

  (cond-expand [chez (import rn-match)] [else (void)])


;; The default wavescript scalar-type? predicate returns #t only for
;; numbers and characters.  It returns #f for tuples. For this backend
;; we have a somewhat tricker predicate heap-allocated?.
;; Currently, it reflects the decision that tuples are value types,
;; but they may contain pointers...
(define heap-allocated? 
  (case-lambda 
    ;[(ty) (heap-allocated? ty global-struct-defs)]
    [(ty struct-defs)
     (match ty
       [,scl (guard (scalar-type? scl)) #f]
       ;; The tuples are not themselves currently heap allocated, but they may contain pointers:
       [#() #f]
       ;[#(,[flds] ...) (ormap id flds)]
       [(Struct ,tuptyp) 
	(let ([entry (assq tuptyp struct-defs)])
	  (unless entry
	    (error 'heap-allocated? "no struct-def entry for type: ~s" tuptyp))
	  (ormap (lambda (x) (heap-allocated? x struct-defs)) 
		 (map cadr (cdr entry))))]
       [(Array ,_) #t]
       [(List ,_)  #t]
       [(Ref ,_)   #t] ;; ?? 
       [(Stream ,_) #t] ;; Meaningless answer.  No runtime representation...
       [(VQueue ,_) #t] ;; Meaningless answer.  No runtime representation...
       )]))

(define-pass gather-heap-types
    ;; This is a mutated accumulator:
    (define acc '())
    (define struct-defs '())
    (define (excluded? ty)
      (or (not (heap-allocated? ty struct-defs))
	  ;; HACKISH: 
	  (deep-assq 'Stream ty)
	  (deep-assq 'VQueue ty)))
    [Bindings 
     (lambda (vars types exprs reconstr exprfun)
       (for-each (lambda (ty)
		   (unless (or (excluded? ty) (member ty acc))
		     (set! acc (cons ty acc))))
	 types)
       (for-each exprfun exprs))]
    [Program 
     (lambda (pr Expr!)
      (fluid-let ([acc ()])
	(match pr
	  [(,lang '(program ,bod . ,meta*))
	   (fluid-let ([struct-defs (cdr (ASSERT (assq 'struct-defs meta*)))])
	     (Expr! bod)
	     `(gather-heap-types-lang '(program ,bod (heap-types ,@acc) ,@meta*))
	     )])))])


;; [2008.01.18] Experimenting with moving the refcounting into another
;; pass so we can start to think about optimizing away refcounts.
;;
;; This is NOT idempotent. It will keep inserting more and more
;; refcounts on repeat executions.  It uses 'let' to name both the
;; pre-refcounted and post-refcounted construct.
;; 

#;
(define insert-refcounts
  (let ()
    (cond-expand [chez (import rn-match)] [else (begin)])
    (define (WrapIncr exp ty)  
      (maybe-bind-tmp exp ty
        (lambda (tmp)
	  `(begin (incr-heap-refcount ,ty ,tmp)
		  ,tmp))))
    (define (Value xp tenv)
      (core-generic-traverse/types
       (lambda (xp tenv fallthru)
	 (match xp
	   ;;[',const (ASSERT simple-constant? const) `',const]

	   ;; No more type checking is allowed AFTER this pass.
	   ;; Here we treat the body of iterate as an EFFECT.
	   ;; (Rather than an expression returning a virtual queue).
	   [(iterate (annotations ,anot* ...) 
		     (let ([,lhs* ,ty* ,[rhs*]] ...) ,fun) ,[strm])
	    (define newenv (tenv-extend tenv lhs* ty*))
	    (define newfun (Effect fun newenv))
	    `(iterate (annotations ,anot* ...)
		      (let ,(map list lhs* ty* (map WrapIncr rhs* ty*))
			,newfun)
		      ,strm)]
	   
	   [(let ([,lhs ,ty ,[rhs]]) ,bod)
	    (define result (unique-name "result"))
	    (define newenv (tenv-extend tenv (list lhs) (list ty)))
	    (define newbod (Value bod newenv))

	    ;(inspect (vector newbod (map car (cdr newenv))))
	    ;(inspect (recover-type newbod newenv))

	    `(let ([,lhs ,ty ,rhs])
	       (begin
		 (incr-local-refcount ,ty ,lhs)
		 (let ([,result ,(recover-type bod newenv) ,newbod])  ;; Uh-oh, need type inference again.		   
		   (begin 
		     (decr-local-refcount ,ty ,lhs)
		     ,result))))]

	   [(assert-type (List ,elt) (cons ,[hd] ,[tl]))
	    (ASSERT simple-expr? hd)
	    (ASSERT simple-expr? tl)     
	    `(begin (incr-heap-refcount ,elt ,hd)
		    (incr-heap-refcount (List ,elt) ,tl)
		    (assert-type (List ,elt) (cons ,hd ,tl)))]
	   
	   ;; Safety net:
	   [(,form . ,rest) (guard (memq form '(let cons iterate)))
	    (error 'insert-refcounts "missed form: ~s" (cons form rest))]

	   ;; These jump us to effect context.
	   [(begin ,[(lambda (x) (Effect x tenv)) -> e*] ... ,[last])  `(begin ,@e* ,last)]

	   [(for (,i ,[st] ,[en]) ,bod)
	    `(for (,i ,st ,en) ,(Effect bod (tenv-extend tenv (list i) '(Int))))]
	   [(while ,[test] ,bod) `(while ,test ,(Effect bod tenv))]
	   
	   [(make-struct ,ty ,[fld*] ...) `(make-struct ,ty ,@fld*)]
	   [(struct-ref ,type ,fld ,[x]) `(struct-ref ,type ,fld ,x)]

	   [,oth (fallthru oth tenv)]))
       (lambda (ls k) (apply k ls))
       xp
       tenv))

 ;; Treating effect context differently because of let's:
 (define (Effect xp tenv)
   (define (Val x) (Value x tenv))
   (core-generic-traverse/types
    (lambda (xp tenv fallthru)
      (match xp
	
	[(let ([,lhs ,ty ,[Val -> rhs]]) ,bod)
	 (define newenv (tenv-extend tenv (list lhs) (list ty)))
	 (define newbod (Effect bod newenv))
	 `(let ([,lhs ,ty ,rhs])
	    (begin
	      (incr-local-refcount ,ty ,lhs)
	      ,newbod
	      (decr-local-refcount ,ty ,lhs)
	      (tuple)))]
	[(set! ,v (assert-type ,ty ,[e]))
	 `(begin 
	    (decr-heap-refcount ,ty ,v)
	    (set! ,v (assert-type ,ty ,e))
	    (incr-heap-refcount ,ty ,v))]
	[(Array:set (assert-type (Array ,elt) ,[arr]) ,[ind] ,[val])
	 (define tmp1 (unique-name "tmp"))
	 (define tmp2 (unique-name "tmp"))
	 (ASSERT simple-expr? ind)
	 (ASSERT simple-expr? arr)	 
	 `(begin 
	    (let ([,tmp1 ,elt (Array:ref (assert-type (Array ,elt) ,arr) ,ind)])
	      (decr-heap-refcount ,elt ,tmp1))  ;; Out with the old.
	    (Array:set (assert-type (Array ,elt) ,arr) ,ind ,val)
	    (let ([,tmp2 ,elt (Array:ref (assert-type (Array ,elt) ,arr) ,ind)])
	      (incr-heap-refcount ,elt ,tmp2))  ;; In with the new.
	    )]

	;; Control flow structures keep us in Effect context:
	[(begin ,[e*] ...) `(begin ,@e*)]
	[(for (,i ,[Val -> st] ,[Val -> en]) ,[bod]) `(for (,i ,st ,en) ,bod)]
	[(while ,[Val -> test] ,[bod]) `(while ,test ,bod)]
	[(if ,[Val -> test] ,[left] ,[right]) `(if ,test ,left ,right)]
	;; TODO WSCASE!
	[(wscase . ,rest) (error 'insert-refcounts "wscase not implemented yet")]

	
	[(,form . ,rest) (guard (memq form '(let Array:set set!)))
	 (error 'insert-refcounts "missed effect form: ~s" (cons form rest))]
	[(,form . ,rest) (guard (memq form '(make-struct struct-ref)))
	 (error 'insert-refcounts "shouldn't be in effect position: ~s" (cons form rest))]
	
	;; For handling the other leaves, go back to Value context:
	;[,oth (Value oth tenv)]
	[,oth (fallthru oth tenv)]
	))
    (lambda (ls k) (apply k ls))
    xp
    tenv))
 ;; Main pass body:
 (lambda (prog)
   (define (Expr x) (Value x (empty-tenv)))
   (cond-expand [chez (import iu-match)] [else (void)])
    (match prog
      [(,input-language 
	'(graph (const (,cbv* ,cbty* ,[Expr -> cbexp*]) ...)
		(init  ,[Expr -> init*] ...)
		(sources ((name ,nm) (output-type ,s_ty) (code ,[Expr -> scode]) (outgoing ,down* ...)) ...)
		(operators (iterate (name ,name) 
				    (output-type ,o_ty)
				    (code ,[Expr -> itercode])
				    (incoming ,o_up)
				    (outgoing ,o_down* ...)) ...)
		(sink ,base ,basetype)	,meta* ...))
       `(,input-language 
	 '(graph (const (,cbv* ,cbty* ,(map WrapIncr cbexp* cbty*)) ...)
		 (init ,@init*) 
		 (sources ((name ,nm) (output-type ,s_ty) (code ,scode) (outgoing ,down* ...)) ...)
		 (operators (iterate (name ,name) 
				     (output-type ,o_ty)
				     (code ,itercode)
				     (incoming ,o_up)
				     (outgoing ,o_down* ...)) ...)
		 (sink ,base ,basetype)
		 ,@meta*))]))))















;; I see there as being two philosophies for handling let's in the RHS
;; of other lets.  First, we could simply lift them out, but that
;; would increase their scope unnecessarily.  The approach we use is
;; to drive the refcount-incr from a var binding deep into the RHS, so
;; that it happens *inside* any scopes introduced by the RHS.
(define insert-refcounts
  (let ()
    (cond-expand [chez (import rn-match)] [else (begin)])
    ;; This is mutated below.
    (define global-struct-defs '())
    (define (WrapIncr exp ty)  
      (maybe-bind-tmp exp ty
        (lambda (tmp)
	  `(begin (incr-heap-refcount ,ty ,tmp)
		  ,tmp))))
    (define (not-heap-allocated? ty) (not (heap-allocated? ty global-struct-defs)))
    (define (DriveInside injection xp retty)
      (match xp
	[(let ([,lhs ,ty ,rhs]) ,[bod])
	 `(let ([,lhs ,ty ,rhs]) ,bod)]
	[(begin ,e* ... ,[last]) (make-begin `(begin ,@e* ,last))]
	[,oth 
	 (maybe-bind-tmp oth retty
	   (lambda (tmp)
	     `(begin ,(injection tmp) ,tmp)))]))

    (define Value 
      (core-generic-traverse
       (lambda (xp fallthru)
	 (match xp
	   ;;[',const (ASSERT simple-constant? const) `',const]

	   ;; No more type checking is allowed AFTER this pass.
	   ;; Here we treat the body of iterate as an EFFECT.
	   ;; (Rather than an expression returning a virtual queue).
	   [(iterate (annotations ,anot* ...) 
		     (let ([,lhs* ,ty* ,[rhs*]] ...) ,fun) ,[strm])
	    ;(define newfun (Effect fun))
	    (define newfun (Value fun))
	    `(iterate (annotations ,anot* ...)
		      (let ,(map list lhs* ty* (map WrapIncr rhs* ty*))
			,newfun)
		      ,strm)]
	   
	   [(let ([,lhs ,ty ,rhs]) ,[bod])
	    (define result (unique-name "result"))
	    ;; Drive the RC incr for the varbind inside:
	    (if (not-heap-allocated? ty) ; (simple-expr? bod)		
		`(let ([,lhs ,ty ,(Value rhs)]) ,bod)
		`(let ([,lhs ,ty ,(DriveInside (lambda (x) `(incr-local-refcount ,ty ,x))
					       (Value rhs) ty)])
		   ,(DriveInside (lambda (_) `(decr-local-refcount ,ty ,lhs)) bod 
				 '''unknown_result_ty)))]

	   [(assert-type (List ,elt) (cons ,[hd] ,[tl]))
	    (ASSERT simple-expr? hd)
	    (ASSERT simple-expr? tl)     
	    `(begin ,@(if (not-heap-allocated? elt) () `((incr-heap-refcount ,elt ,hd)))
		    (incr-heap-refcount (List ,elt) ,tl)
		    (assert-type (List ,elt) (cons ,hd ,tl)))]
	   
	   ;; Safety net:
	   [(,form . ,rest) (guard (memq form '(let cons iterate)))
	    (error 'insert-refcounts "missed form: ~s" (cons form rest))]

	   ;; These jump us to effect context.
	   [(begin ,[Effect -> e*] ... ,[last])  `(begin ,@e* ,last)]

	   [(for (,i ,[st] ,[en]) ,bod)
	    `(for (,i ,st ,en) ,(Effect bod))]
	   [(while ,[test] ,bod) `(while ,test ,(Effect bod))]
	   
	   [(make-struct ,ty ,[fld*] ...) `(make-struct ,ty ,@fld*)]
	   [(struct-ref ,type ,fld ,[x]) `(struct-ref ,type ,fld ,x)]

	   [,oth (fallthru oth)]))))
    ;; Treating effect context differently because of let's:
    (define Effect 
      (core-generic-traverse
       (lambda (xp fallthru)
	 (match xp
	   
	   [(let ([,lhs ,ty ,rhs]) ,[bod])
	    (if (not-heap-allocated? ty) ;; No reason to pollute things
		`(let ([,lhs ,ty ,(Value rhs)]) ,bod)
		`(let ([,lhs ,ty ,(DriveInside (lambda (x) `(incr-local-refcount ,ty ,x))
					       (Value rhs) ty)])
		   ,(make-begin
		     `(begin
			,bod
			(decr-local-refcount ,ty ,lhs)
			;;(tuple)
			))))]	   

	   [(set! ,v (assert-type ,ty ,[e]))
	    (if (not-heap-allocated? ty)
		`(set! ,v (assert-type ,ty ,e))
		`(begin 
		   (decr-heap-refcount ,ty ,v)
		   (set! ,v (assert-type ,ty ,e))
		   (incr-heap-refcount ,ty ,v)))]
	   [(Array:set (assert-type (Array ,elt) ,[arr]) ,[ind] ,[val])
	    (define tmp1 (unique-name "tmp"))
	    (define tmp2 (unique-name "tmp"))
	    (define setit `(Array:set (assert-type (Array ,elt) ,arr) ,ind ,val))
	    (ASSERT simple-expr? ind)
	    (ASSERT simple-expr? arr)	 
	    (if (not-heap-allocated? elt) setit
		`(begin 
		   (let ([,tmp1 ,elt (Array:ref (assert-type (Array ,elt) ,arr) ,ind)])
		     (decr-heap-refcount ,elt ,tmp1))  ;; Out with the old.
		   ,setit
		   (let ([,tmp2 ,elt (Array:ref (assert-type (Array ,elt) ,arr) ,ind)])
		     (incr-heap-refcount ,elt ,tmp2))  ;; In with the new.
		   ))]

	   ;; Control flow structures keep us in Effect context:
	   [(begin ,[e*] ...) `(begin ,@e*)]
	   [(for (,i ,[Value -> st] ,[Value -> en]) ,[bod]) `(for (,i ,st ,en) ,bod)]
	   [(while ,[Value -> test] ,[bod]) `(while ,test ,bod)]
	   [(if ,[Value -> test] ,[left] ,[right]) `(if ,test ,left ,right)]
	   ;; TODO WSCASE!
	   [(wscase . ,rest) (error 'insert-refcounts "wscase not implemented yet")]

	   
	   [(,form . ,rest) (guard (memq form '(Array:set set!)))
	    (error 'insert-refcounts "missed effect form: ~s" (cons form rest))]
	   [(,form . ,rest) (guard (memq form '(make-struct struct-ref)))
	    (error 'insert-refcounts "shouldn't be in effect position: ~s" (cons form rest))]
	   
	   ;; For handling the other leaves, go back to Value context:
	   [,oth (fallthru oth)]
	   ))))
    ;; Main pass body:
    (lambda (prog)
      (cond-expand [chez (import iu-match)] [else (void)])
      (fluid-let ([global-struct-defs (cdr (project-metadata 'struct-defs prog))])
	(match prog
	  [(,input-language 
	    '(graph (const (,cbv* ,cbty* ,[Value -> cbexp*]) ...)
		    (init  ,[Value -> init*] ...)
		    (sources ((name ,nm) (output-type ,s_ty) (code ,[Value -> scode]) (outgoing ,down* ...)) ...)
		    (operators (iterate (name ,name) 
					(output-type ,o_ty)
					(code ,[Value -> itercode])
					(incoming ,o_up)
					(outgoing ,o_down* ...)) ...)
		    (sink ,base ,basetype)	,meta* ...))	 
	   `(,input-language 
	     '(graph (const (,cbv* ,cbty* ,(map WrapIncr cbexp* cbty*)) ...)
		     (init ,@init*) 
		     (sources ((name ,nm) (output-type ,s_ty) (code ,scode) (outgoing ,down* ...)) ...)
		     (operators (iterate (name ,name) 
					 (output-type ,o_ty)
					 (code ,itercode)
					 (incoming ,o_up)
					 (outgoing ,o_down* ...)) ...)
		     (sink ,base ,basetype)
		     ,@meta*))])))))

) ;; End module
