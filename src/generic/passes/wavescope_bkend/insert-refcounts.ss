

(module insert-refcounts mzscheme 
  (require  "../../../plt/common.ss"
	    ;(all-except (lib "list.ss") sort sort! filter)
	    ;(all-except "nominalize-types.ss" test-this these-tests)
	    ;"convert-sums-to-tuples.ss"
	    ;"../../compiler_components/c_generator.ss" 
	    )
  (provide insert-refcounts
	   flag-static-allocate
	   heap-allocated?
	   gather-heap-types)
  (chezimports (except helpers test-this these-tests))

  (cond-expand [chez (import rn-match)] [else (void)])

(define (make-rc which ty exp)
  ;; [2008.04.05] We end up with BOTTOMs as a result of ws-normalize-context.
  ;; (They are the return value that follow a wserror control path.)
  (if (equal? exp ''BOTTOM)
      '(tuple)
      (list which ty exp)))

;; The default wavescript scalar-type? predicate returns #t only for
;; numbers and characters.  It returns #f for tuples. For this backend
;; we have a somewhat tricker predicate heap-allocated?.
;; Currently, it reflects the decision that tuples are value types,
;; but they may contain pointers...
(define heap-allocated? 
  (case-lambda 
    ;[(ty) (heap-allocated? ty global-struct-defs)]
    [(ty struct-defs union-types)
     (define (recur x) (heap-allocated? x struct-defs union-types))
     (match ty
       [,scl (guard (scalar-type? scl)) #f]
       ;; The tuples are not themselves currently heap allocated, but they may contain pointers:
       [#() #f]
       [String #t]
       ;[#(,[flds] ...) (ormap id flds)]
       [(Struct ,tuptyp) 
	(let ([entry (assq tuptyp struct-defs)])
	  (unless entry
	    (error 'heap-allocated? "no struct-def entry for type: ~s" tuptyp))
	  (ormap recur (map cadr (cdr entry))))]

       ;; Currently tagged unions are just like tuples:
       [(Union ,name)
	(define entry (ASSERT (assoc (list name) union-types)))
	(ormap recur (map cadr (cdr entry)))]

       ;; This is a function point, no closures at this point:
       [(,_ ... -> ,__) #f]

       [(Array ,_) #t]
       [(List ,_)  #t]
       [(Ref ,_)   #t] ;; ?? 

       [(Sigseg ,_) #t] ;; This shoudn't get to here with wsc2.
       [Timebase    #t] ;; This is conservative, we can't really say how timebase is implemented.
       
       [(Stream ,_) #t] ;; Meaningless answer.  No runtime representation...
       [(VQueue ,_) #t] ;; Meaningless answer.  No runtime representation...
       [Symbol #f] ;; Meaningless answer.  No runtime representation... used internally.
       )]))

;; Helper pass.
(define-pass gather-heap-types
    ;; This is a mutated accumulator:
    (define acc '())
    (define struct-defs '())
    (define union-types '())
    (define (excluded? ty)
      (or (not (heap-allocated? ty struct-defs union-types))
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
    [Expr 
     (lambda (xp fallthru)
       (match xp 
	 [(cast-variant-to-parent ,TC ,ty ,[exp])
	  `(cast-variant-to-parent ,TC ,ty ,exp)]
	 [,oth (fallthru oth)]))]
    [Program 
     (lambda (pr Expr!)
      (fluid-let ([acc ()])
	(match pr
	  [(,lang '(program ,bod . ,meta*))
	   (fluid-let ([struct-defs (cdr (ASSERT (assq 'struct-defs meta*)))]
		       [union-types (cdr (ASSERT (assq 'union-types meta*)))])
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
    (define global-union-types '())
    (define (not-heap-allocated? ty) (not (heap-allocated? ty global-struct-defs global-union-types)))
    ;; Adding a tricky value-needed? flag.  Basically, there are two
    ;; different scenarios for DriveInside, it can either push in a
    ;; refcount incr *to that value*, or it can push in a decr that
    ;; doesn't care about the value, but wants to be at the end of the
    ;; control flow.
    (define (DriveInside injection xp retty value-needed?)
      (match xp
	[(,lt ([,lhs ,ty ,rhs]) ,[bod]) (guard (memq lt '(let let-not-counted)))
	 `(,lt ([,lhs ,ty ,rhs]) ,bod)]
	[(begin ,e* ... ,[last]) (make-begin `(begin ,@e* ,last))]
	;; Go down both control paths:
	[(if ,test ,[left] ,[right]) `(if ,test ,left ,right)]
	
	;; If we get to a dead-end control path like this, just forget it.
	['BOTTOM ''BOTTOM]

	[,oth 
	 (define tmp (unique-name "tmprc"))
	 (if (or (simple-expr? oth)
		 ;; Also allowing some other expressions that we know
		 ;; will not affect reference counting as a side effect:
		 ;; This is an optimization that I'm not sure of yet:
		 ;(and (not value-needed?) (match? oth (Mutable:ref ,_)))
		 )
	     `(begin ,(if value-needed? (injection oth) (injection)) ,oth)
	     `(let-not-counted ([,tmp ,retty ,oth])
	       (begin ,(if value-needed? (injection tmp) (injection)) ,tmp)))
	 #;
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
		     (let ([,lhs* ,ty* ,rhs*] ...) ,fun) ,[strm])
	    ;(define newfun (Effect fun))
	    (define newbinds
	      (map list lhs* ty* (map Value (map TopIncr rhs* ty*))))
	    (define newfun (Value fun))
	    `(iterate (annotations ,@anot*)
		      (let ,newbinds
			,newfun)
		      ,strm)]

	   [(let ([,lhs ,ty ,rhs]) ,bod)
	    (define result (unique-name "result"))
	    
	    ;; TODO: *IF* RHS allocates, add to ZCT UNLESS it flows to a heap incr-point.

	    (if (not-heap-allocated? ty) ; (simple-expr? bod)		
		`(let ([,lhs ,ty ,(Value rhs)]) ,(Value bod))
		;; FIXME: INCORRECT:
		;; Does this work? DriveInside should come *before* recursion.
		;; BUT, that leads to an infinite loop.
		;; DriveInside introduces extra let-expressions, which then lead to extra calls to DriveInside.
		`(let ([,lhs ,ty ,(Value
				   (DriveInside (lambda (x) (make-rc 'incr-local-refcount ty x))
						rhs ty #t))])		   
		   ,(Value
		     (DriveInside (lambda () (make-rc 'decr-local-refcount ty lhs)) bod 
				 ''unknown_result_ty #f))))]
	   
	   [(let-not-counted ([,lhs ,ty ,[rhs]]) ,[bod])
	    `(let-not-counted ([,lhs ,ty ,rhs]) ,bod)]
	  
	   ;; Allocation routines -- add to ZCT?
	   [(assert-type (List ,elt) (cons ,[hd] ,[tl]))
	    (ASSERT simple-expr? hd)
	    (ASSERT simple-expr? tl)     
	    `(begin ,@(if (not-heap-allocated? elt) () (list (make-rc 'incr-heap-refcount elt hd)))
		    ,(make-rc 'incr-heap-refcount `(List ,elt) tl)
		    (assert-type (List ,elt) (cons ,hd ,tl)))]
	   ;; This could be more efficient, could bump it all at once
	   ;; without the for-loop.  incr-heap-refcount would have to
	   ;; take a numeric argument.  In the common case, the
	   ;; initialization will most likely be the null array, and
	   ;; getting rid of this for loop would do well.
	   [(assert-type (Array ,elt) (Array:make ,[len] ,[init]))
	    (guard (not (not-heap-allocated? elt)))
	    (define end  (unique-name "end"))
	    (define tmp  (unique-name "tmp"))
	    (define tmp2 (unique-name "tmp"))
	    (define ind  (unique-name "ind"))
	    `(let ([,tmp (Array ,elt) (assert-type (Array ,elt) (Array:make ,len ,init))])
	       (let ([,end Int (_-_ ,len '1)])
		 (begin (for (,ind '0 ,end)
			    (let ([,tmp2 ,elt (Array:ref (assert-type (Array ,elt) ,tmp) ,ind)])
			      ,(make-rc 'incr-heap-refcount elt tmp2)))
			,tmp)
		 ))]
	   
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
	   [(cast-variant-to-parent ,TC ,ty ,[exp]) `(cast-variant-to-parent ,TC ,ty ,exp)]
	  
	   [,oth (fallthru oth)]))))
    ;; Treating effect context differently because of let's:
    (define Effect 
      (core-generic-traverse
       (lambda (xp fallthru)
	 (match xp
	   
	   [(let ([,lhs ,ty ,rhs]) ,[bod])
	    (if (not-heap-allocated? ty) ;; No reason to pollute things
		`(let ([,lhs ,ty ,(Value rhs)]) ,bod)
		;; FIXME: INCORRECT:
		`(let ([,lhs ,ty ,(Value 
				   (DriveInside (lambda (x) (make-rc 'incr-local-refcount ty x))
						rhs ty #t))])
		   ,(make-begin
		     `(begin
			,bod
			,(make-rc 'decr-local-refcount ty lhs)
			;;(tuple)
			))))]

	   [(set! ,v (assert-type ,ty ,[Value -> e]))
	    (if (not-heap-allocated? ty)
		`(set! ,v (assert-type ,ty ,e))
		`(begin 
		   ,(make-rc 'decr-heap-refcount ty v)
		   (set! ,v (assert-type ,ty ,e))
		   ,(make-rc 'incr-heap-refcount ty v)))]
	   [(Array:set (assert-type (Array ,elt) ,[Value -> arr]) ,[Value -> ind] ,[Value -> val])
	    (define tmp1 (unique-name "tmp"))
	    (define tmp2 (unique-name "tmp"))
	    (define setit `(Array:set (assert-type (Array ,elt) ,arr) ,ind ,val))
	    (ASSERT simple-expr? ind)
	    (ASSERT simple-expr? arr)	 
	    (if (not-heap-allocated? elt) setit
		`(begin 
		   (let ([,tmp1 ,elt (Array:ref (assert-type (Array ,elt) ,arr) ,ind)])
		     ,(make-rc 'decr-heap-refcount elt tmp1))  ;; Out with the old.
		   ,setit
		   (let ([,tmp2 ,elt (Array:ref (assert-type (Array ,elt) ,arr) ,ind)])
		     ,(make-rc 'incr-heap-refcount elt tmp2))  ;; In with the new.
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

    ;; Value plus a hackish post-process step:
    (define (Value+ x)
      ;; Here we do the unthinkable: mutate an AST.
      (let* ([result (Value x)]
	     [hits (deep-assq-all 'let-not-counted result)])	  
	(for-each (lambda (hit) (set-car! hit 'let)) hits)
	result))

    ;; For "global" variables:
    ;; Must happen *BEFORE* the local decrements are inserted (before recurring, before Value).
    (define (TopIncr exp ty)  
      (DriveInside (lambda (x) (make-rc 'incr-heap-refcount ty x)) exp ty #t))

    (define (Operator op)
      (match op
	[(iterate (name ,name) (output-type ,o_ty)
		  (code ,[Value+ -> itercode])
		  (incoming ,o_up) (outgoing ,o_down* ...))
	 `(iterate (name ,name) (output-type ,o_ty)
		   (code ,itercode)
		   (incoming ,o_up) (outgoing ,@o_down*))]
	;; All of these have no code to speak of:
	[(_merge . ,_) op]
	[(__readFile . ,_) op]
	[(cutpoint . ,_) op]
	[(unionN . ,_) op]))

    ;; Main pass body:
    (lambda (prog)
      (cond-expand [chez (import iu-match)] [else (void)])    
      (fluid-let ([global-struct-defs (cdr (project-metadata 'struct-defs prog))]
		  [global-union-types (cdr (project-metadata 'union-types prog))])
	(match prog
	  [(,input-language 
	    '(graph (const (,cbv* ,cbty* ,cbexp*) ...)
		    (init  ,[Value+ -> init*] ...)
		    (sources ((name ,nm) (output-type ,s_ty) (code ,[Value+ -> scode]) (outgoing ,down* ...)) ...)
		    (operators ,[Operator -> oper*] ...)
		    (sink ,base ,basetype)	,meta* ...))
	   `(,input-language 
	     '(graph (const ,(map list cbv* cbty* (map Value+ (map TopIncr cbexp* cbty*)))
			    ...)
		     (init ,@init*) 
		     (sources ((name ,nm) (output-type ,s_ty) (code ,scode) (outgoing ,down* ...)) ...)
		     (operators ,@oper*)
		     (sink ,base ,basetype)
		     ,@meta*))])))))


;; This pass does not do a deep traversal of the program.  It simply
;; checks the constant bindings and the iterator state bindings to see
;; which can be statically allocated.
(define (flag-static-allocate prog) 

  ;; A binding that gets evaluated exactly once.
  (define (StaticBind lhs ty rhs)
    `(,lhs ,ty
	   ,(let loop ([rhs rhs])
	      (match (peel-annotations rhs) ;; no recursion!
		;; What kinds of things can we switch to static allocation?
		;; Currently just quoted constants:
		[',c `(static-allocate ,rhs)] 
		[(,make . ,_) (guard (eq-any? make 'Array:make 'Array:makeUNSAFE)) 
		 `(static-allocate ,rhs)]

#;
		;; [2008.04.13] Extending this to dig a little deeper.
		;; In particular, as long as there's no conditional
		;; control flow, it's still static...
		[(let ([,lhs ,ty ,[loop -> rhs]]) ,[loop -> bod])
		 ;(pp `(static-let `(let ([,lhs ,ty ,rhs]) ,bod)))
		 `(let ([,lhs ,ty ,rhs]) ,bod)]
		
;		[(assert-type ,ty ,[loop -> e]) `(assert-type ,ty ,e)]

		[,oth oth]))))

  (define (Operator op)
    (match op
      [(iterate (name ,name) (output-type ,o_ty)
		(code (iterate (annotations ,anot* ...)
			       (let ([,lhs* ,ty* ,rhs*] ...) ,fun) ,strm))
		(incoming ,o_up) (outgoing ,o_down* ...))
       (define newbinds
	 (map (lambda (lhs ty rhs) 
		(match ty
		  [(Ref ,_)
		   ;;(printf " *** mutated iterator state: ~s\n" lhs)
		   (list lhs ty rhs)]
		  [,_ 
		   ;;(printf " *** GOT UNMUTATED ITERATOR STATE: ~s\n" lhs) 
		   (StaticBind lhs ty rhs)]))
	   lhs* ty* rhs*))       
       `(iterate (name ,name) (output-type ,o_ty)
		 (code (iterate (annotations ,@anot*)
				(let ,newbinds ,fun)
				,strm))
		 (incoming ,o_up) (outgoing ,@o_down*))]
      ;; All of these have no code to speak of:
      [(_merge . ,_) op]
      [(__readFile . ,_) op]
      [(cutpoint . ,_) op]
      [(unionN . ,_) op]))

  (cond-expand [chez (import iu-match)] [else (void)])    
  (match prog
    [(,input-language 
      '(graph (const (,cbv* ,cbty* ,cbexp*) ...)
	      (init  ,init* ...)
	      (sources ,src* ...)
	      (operators ,[Operator -> oper*] ...)
	      (sink ,base ,basetype)	,meta* ...))
     `(,input-language 
       '(graph (const ,(map StaticBind cbv* cbty* cbexp*) ...)
	       (init ,@init*) 
	       (sources ,@src*)
	       (operators ,@oper*)
	       (sink ,base ,basetype)
	       ,@meta*))]))


#;
(define insert-zct
  (let ()
    
    (lambda (xp fall)
      (match xp
	[(begin ,effect ... ,truetail ,var) (guard (symbol? var))
	 
	 ]

	;; (add-to-zct ,v)

	;; (clear-zct)
	;; (emit-clear-zct ,vq ,x)
	
	)
      )
    

    0))

) ;; End module
