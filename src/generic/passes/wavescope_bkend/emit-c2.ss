

;;;; .title WaveScript EmitC Version TWO
;;;; .author Ryan Newton

;;;; This uses the generic C-generation libary (c_generator.ss).
;;;; Unlike the first version (emit-c.ss) this version does not rely on:

;;;;  (1) C++ features (templates etc)
;;;;  (2) XStream classes (Sigseg, etc)
;;;;  (3) Boost smart pointers

;;;; It produces "lower level" C code than the first version, meant to
;;;; be used with a custom garbage collector.

;;;; TODO: The pass that INSERTS refcount incr's and decr's can be
;;;; extracted from this pass.

(module emit-c2 mzscheme 
  (require  "../../../plt/common.ss"
	    (all-except (lib "list.ss") sort sort! filter)
	    (all-except "nominalize-types.ss" test-this these-tests)
	    "convert-sums-to-tuples.ss"
	    "../../compiler_components/c_generator.ss" )
  (provide emit-c2 
	   gather-heap-types
	   embed-strings-as-arrays)
  (chezprovide )  
  (chezimports (except helpers test-this these-tests))

;; These are just for sanity checking.  Disjoint types keep mix-ups from happening.
;; These should just be wrappers on the outside.  You should never find them nested.
(reg:define-struct (lines text))
;(reg:define-struct (expression text))
(define (append-lines . ls) 
  (DEBUGASSERT (andmap lines? ls))
  (make-lines (map lines-text ls)))
;(define (idk x) (ASSERT expression? x) x)
;(define (idk x) x)

;; This is mutated below with fluid-let to contain a table mapping
;; types to the free-function for that type.
(define free-fun-table '())

(define-syntax debug-return-contract
  (syntax-rules ()
    [(_ pred fun) (debug-return-contract "<unknownFun>" pred fun)]
    [(_ name pred fun) 
     (IFDEBUG (lambda args 
		(if (eq? 'name 'PrimAppK) (printf "IN PRIMAPPK"))
		(let ([result (apply fun args)])
			     ;(printf "Got debug contract result: ~s\n" 'name)
		  (if (pred result) result
		      (begin 
			(warning 'debug-return-contract "failed contract on return value, function: ~s\nvalue: ~s\nContinuation:\n"
				 'name result)
			(call/cc inspect)))))
	      fun)]))

;================================================================================
;;; Input grammar

;(define emit-c2-input-grammar)

;================================================================================

(define-pass gather-heap-types
    ;; This is a mutated accumulator:
    (define acc '())
    (define (excluded? ty)
      (or (and (symbol? ty) (memq ty num-types))
	  (eq? ty 'Bool)
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
	  [(,lang '(program ,bod . ,rest))
	   (Expr! bod)
	   `(gather-heap-types-lang '(program ,bod (heap-types ,@acc) ,@rest))])))])


(define-pass embed-strings-as-arrays
    (define (Type ty) 
      (match ty
	[String `(Array Char)]
	[,s (guard (symbol? s)) s]
	[(,qt ,tvar) (guard (memq qt '(NUM quote))) `(,qt ,tvar)]
	[#(,[t*] ...) (list->vector t*)]
	[(,[arg*] ... -> ,[res]) `(,@arg* -> ,res)]
	[,s (guard (string? s)) s] ;; Allowing strings for uninterpreted C types.
	[(,C ,[t*] ...) (guard (symbol? C)) (cons C t*)] ; Type constructor
	[,other (error 'embed-strings-as-arrays "malformed type: ~a" ty)]))
    [Expr (lambda (xp fallthru)
	    (match xp
	      ;; make a constant vector.
	      [',str (guard (string? str))
		     `',(list->vector (append ;(make-list 8 #\nul)
					      (string->list str)
					      (list #\nul)))]
	      [(string-append ,[s1] ,[s2])
	       (let ([a1 (unique-name "strarr1")]
		     [a2 (unique-name "strarr2")]
		     [i1 (unique-name "i")]
		     [i2 (unique-name "i")]
		     [len1 (unique-name "len")]
		     [result (unique-name "appendresult")])
		 `(let ([,a1 (Array Char) ,s1] 
			[,a2 (Array Char) ,s2])
		    (let ([,len1 Int (Array:length ,a1)]
			  [,result (Array Char) 
				   (assert-type (Array Char)
						(Array:makeUNSAFE (-_ (+_ ,len1 (Array:length ,a2)) '1)))])
		      (begin 
			(for (,i1 '0 (-_ ,len1 '1))
			    (Array:set ,result ,i1 (Array:ref ,a1 ,i1)))
			(for (,i2 '0 (-_ (Array:length ,a2) '1))
			    (Array:set ,result (+_ ,len1 ,i2) (Array:ref ,a2 ,i2)))
			;(app Array:blit ,result '0 ,a1 '0 (-_ (Array:length ,a1) '1))
			;(app Array:blit ,result (-_ (Array:length ,a1) '1) ,a2 '0 (Array:length ,a2))
			,result))))]

	      [(String:length ,[str]) `(Array:length ,str)]
	      
	      [(show ,[x]) `(__show_ARRAY ,x)]
	      [(wserror ,[x]) `(__wserror_ARRAY ,x)]

	      [(assert-type ,[Type -> ty] ,[e])  `(assert-type ,ty ,e)]

	      [,oth (fallthru oth)]

	      ;; Not implemented at runtime yet:
	      ;[(String:explode   (String) (List Char))]
	      ;[(String:implode   ((List Char)) String)]
	      )
	    )]  
    [Bindings
     (lambda (vars types exprs reconstr exprfun)
       (reconstr vars (map Type types) (map exprfun exprs)))])

;; This builds a set of top level function definitions that free all
;; the heap-allocated types present in the system.  It also builds a
;; table mapping types onto syntax-generating functions that call the
;; appropriate free-ing code.
;; 
;; Currently this routine mutates the table "free-fun-table" on the fly.
;;
;; This is this function that decides what freeing will be "open coded"
;; vs. relegated to separately defined functions.
(trace-define (build-free-fun-table! heap-types)
  (define (heap-allocated? ty)
    (match ty
      [,scl (guard (scalar-type? scl)) #f]
      [,v (guard (vector? v)) #f]
      [(Array ,_) #t]
      [(Ref ,_) #t] ;; ??      
      ))
  (define fun-def-acc '()) ;; mutated below
  (define (add-to-table! ty fun) 
    (set! free-fun-table (cons (cons ty (debug-return-contract lines? fun))
			       free-fun-table)))
  (for-each
      (lambda (ty) 
	 (let loop ([ty ty])
	   (match ty
	     ;; Tuples are not heap allocated for now (at least the bigger ones should be):
	     [,tup (guard (vector? tup)) #f] ;; No entry in the table.
	     [,scl (guard (scalar-type? scl)) #f]
	     [(Ref ,elt) (loop elt)]
	     [(Array ,elt)
	      (add-to-table! 
	       ty 	       
	       (if (not (heap-allocated? elt))
		   (lambda (ptr) (make-lines `("free((int*)",ptr" - 2);\n")))
		   (let ([name (type->name ty)]
			 [ind (Var (unique-name "i"))])
		     (set! fun-def-acc
			   (cons (make-lines 
				  (block `("void free_",name"(",(Type `(Array ,elt))" ptr)")
					 `("int ",ind";\n"
					   ,(block `("for (",ind" = 0; ",ind" < ((int*)ptr)[-2]; ",ind"++)")
						   ;;(lines-text ((cdr (loop elt)) `("ptr[",ind"]")))
						   (begin (loop elt) ;; For side effect
							  (lines-text (gen-decr-code elt `("ptr[",ind"]") default-free)))
						   )
					   "free((int*)ptr - 2);\n")))
				 fun-def-acc))
		     (lambda (ptr) (make-lines `("free_",name"(",ptr");\n"))))
		   ))])))
    heap-types)
  (apply append-lines 
	 ;; This will have to be per-thread...	 
	 ;(make-lines " void* ZCT[1000];\n int ZCT_count;\n")
	 fun-def-acc))


;================================================================================
;;; Hooks for garbage collection.

;; For the naive strategy we'de allow shared pointers but would need a CAS here.
(define (gen-incr-code ty ptr)
  (match ty
    [(Array ,elt) 
     (make-lines `("if (",ptr") ((int*)",ptr")[-1]++; /* incr refcount ",(format "~a" ty)" */\n"))]
    [(Ref ,[ty]) ty]
    ;; Other types are not heap allocated:
    [,_ (make-lines "")]
    ))

;; "ptr" should be text representing a C lvalue
(define (gen-decr-code ty ptr freefun)
  (match ty
    [(Array ,elt) 
     (make-lines 
      (block `("if (",ptr" && --(((int*)",ptr")[-1]) == 0) /* decr refcount ",(format "~a" ty)" */ ") 
	     (lines-text (freefun ty ptr))))]
    [(Ref ,[ty]) ty]
    [,_ (make-lines "")]))

;; Should only be called for types that are actually heap allocated.
(define default-free
  (debug-return-contract lines?
  (lambda (ty ptr)    
   ;(make-lines `("printf(\"FREEING ",(format "~a" ty)"\\n\");\n"))
    ((cdr (ASSERT (assoc ty free-fun-table))) ptr))))

;; This version represents plain old reference counting.
(begin
  (define (incr-local-refcount ty ptr) (gen-incr-code ty ptr))
  (define (decr-local-refcount ty ptr) (gen-decr-code ty ptr default-free))
  
  (define incr-heap-refcount gen-incr-code)
  (define (decr-heap-refcount ty ptr)
    (gen-decr-code ty ptr default-free))
  (define (potential-collect-point) (make-lines ""))
  )

#;
;; This version uses deferred reference counting.
(begin 
  (define (incr-local-refcount ty ptr) (make-lines ""))
  (define (decr-local-refcount ty ptr) (make-lines ""))

  (define incr-heap-refcount gen-incr-code)
  (define (decr-heap-refcount ty ptr)
    (gen-decr-code ty ptr default-free))

  (define (potential-collect-point)
    (make-lines ""))
  )


;; Do anything special that's required as a value is sent off through an emit.
;; Currently, with a pure depth-first strategy, reference count does not need to be affected by "emit".
(define (say-goodbye ty ptr) (make-lines ""))

;================================================================================
;;; Low level routines for generating syntax.

;(define (mangle-name n) (symbol->string n))
(define sym2str  symbol->string)

(define (cap x) (list x ";\n"))

(define (make-app rator rands)
  (list rator "("(insert-between ", " rands)")"))

(define (type->name ty)
  (match ty
    [,numt (guard (memq numt num-types)) (sym2str numt)]
    [(Ref ,[ty]) ty] ;; Doesn't affect the name currently...
    [(Array ,[ty]) (string-append "Array_" ty)]))

;(define (make-fundef type name rand bod) 0)

;; .param src* blocks of code (lines) for the body of each source.
(define (build-main-source-driver srcname*)
  (if (= 1 (length srcname*))
      ;(make-fundef "int" "main" ("return 0;\n"))
      (make-lines        
       (block "int main()" 
	      (list 
	       "initState();\n"
	       (block "while(1)"
		      (map (lambda (f) (list (make-app (Var f) ()) ";\n")) srcname*))
	       "return 0;\n")))
      (error 'emit-c2 "only single-sources supported right now")))


;; This is the simplest way to package a source.
(define (wrap-source-as-plain-thunk name code)
  (make-lines (block `("void ",(Var name) "()") (lines-text code))))


(define (wrap-iterate-as-simple-fun name arg argty code)
  (make-lines (block `("void ",(Var name) "(",(Type argty)" ",(Var arg)")") (lines-text code))))

;================================================================================
;;; "Continuations" used for syntax production.
;;; These are actually simple objects.

(define split-msg (gensym "split"))

(define (idk x)   (if (eq? x split-msg) (values (make-lines "") idk) (make-lines x)))
(define (nullk x) (if (eq? x split-msg) (values (make-lines "") idk) (make-lines "")))
(define (varbindk name typ)
  (define (split-k x)
    (if (eq? x 'split-msg) 
	;; Further splits have no effect:
	(values (make-lines "") split-k)
	;; After a split we mutate the variable rater than binding it.
	(make-lines `(,(Var name)" = ",x";\n"))))
  (lambda (x)      
    (if (eq? x split-msg)
	;; At the split point we bind the variable.
	;; And we return a new continuation.
	(values (make-lines `(,(Type typ)" ",(Var name)";\n"))
		split-k)
	;; Here we bind & set the variable.
	(make-lines `(,(Type typ)" ",(Var name)" = ",x";\n"))
	)))

;================================================================================

;; [2007.12.04] TEMP!!!!! SHOULD NOT DUPLICATE CODE
(define Const
  (lambda (datum wrap)
    ;; Should also make sure it's 32 bit or whatnot:
    (cond
     [(eq? datum 'BOTTOM) (wrap "0")] ;; Should probably generate an error.
     [(eq? datum 'UNIT) (wrap (Simple '(tuple)))]
     [(eq? datum #t) (wrap "TRUE")]
     [(eq? datum #f) (wrap "FALSE")]
     [(string? datum) (wrap (format "~s" datum))]
     ;; FIXME THIS WON'T HANDLE NON-PRINTING CHARACTERS YET!!
     [(char? datum) (wrap (format "'~a'" datum))]


     ;; Hacked this to handle NAN (not in a pretty way).
     [(flonum? datum) 
            ;(printf "GOT FLOAT: ~a ~a \n" datum (or (eq? datum +nan.0) (eq? datum -nan.0)))
      (wrap (format "(~a)~a" (Type 'Float)
		    (if (not (= datum datum)) ;(or (eq? datum +nan.0) (eq? datum -nan.0))
			"(0.0/0.0)" ;(inspect/continue datum);"(0.0/0.0)"
			datum)					 
		    ))]
     [(cflonum? datum) (wrap (format "(~a)(~a + ~afi)" (Type 'Complex)
				     (cfl-real-part datum)
				     (cfl-imag-part datum)))]
     [(eq? datum 'nulltimebase)  (wrap "WSNULLTIMEBASE")]
     [(integer? datum) (wrap (format "(~a)~a" (Type 'Int) datum))]

#;
     [(vector? datum)
      (ASSERT name)
      (let ([contenttype (if (zero? (vector-length datum))
			     ;; Throw in a default:
			     'Int
			     (type-const (vector-ref datum 0)))])
	`(,type" ",name" = makeArrayUnsafe(",(number->string (vector-length datum))
	       ", "
	       ;;"sizeof(",contenttype")"
	       "("(Type contenttype)")"(make-zero-for-type contenttype)
	       ");\n" 
	       ,(mapi (lambda (i x) (Const `("(",name"->data)[",(number->string i)"]")
					   "" x))
		      (vector->list datum))))]
     [else (error 'emitC2:Const "not a C-compatible literal: ~s" datum)])))

(define (Type ty)
  (match ty
    ;; Keeping typedefs for these on the C side:
;     [Bool    "wsbool_t"]
;     [Int     "wsint_t"]
;     [Int16   "wsint16_t"]
;     [Int64   "wsint64_t"]
;     [Double  "wsdouble_t"]
;     [Float   "wsfloat_t"]
;     [Complex "wscomplex_t"]
;     [Char    "wschar_t"]
;     [String  "wsstring_t"] ;; Not boosted.
;     [#()      "wsunit_t"]

    [Bool    "char"]
    [Int     "int"]
    [Int16   "int16_t"]
    [Int64   "int64_t"]
    [Double  "double"]
    [Float   "float"]
    [Complex "wscomplex_t"]
    [Char    "char"]
    [String  "char*"] ;; Not boosted.
    [#()      "char"]

    [(Array ,[elt]) (list elt "*")] ;[(Array ,[elt]) (list "(" elt "*)")]
    [(Ref ,[ty]) ty]
    ))

;; For the set of types that are printable at this point, we can use a simple printf flag.
(define (type->printf-flag ty)
  (match ty
    [String "%s"]
    [(Array Char) "%s"] ;; These are strings in disguise.
    [Bool   "%d"]
    [Int    "%d"]
    [Int16  "%hd"]
    [Int64  "%lld"]
    [Float  "%f"]	   
    [Double "%lf"]
    [(Pointer ,_) "%p"]))

;(define Var mangle-name)
(define Var sym2str)

;; These are the expressions that make valid operands (post flattening)
;;   .returns A string.  Could return an "expression".
(define (Simple expr)
  (match expr
    [,v (guard (symbol? v)) (ASSERT (not (regiment-primitive? v))) (Var v)]
    ;[',c (format "~a" c)] ;;TEMPTEMP
    [',c (Const c (lambda (x) x))]
    [(tuple) (list "(("(Type #())")0)")]
    [(deref ,var) (ASSERT (not (regiment-primitive? var))) (Var var)]
    ;[(assert-type ,t '())  (wrap (PolyConst '() t))]
    ;['() (error 'Simple "null list without type annotation")]
    ;[nulltimebase (Const #f #f 'nulltimebase)]    
       
    [(assert-type ,_ ,[x]) x]
    [,else (error 'Simple "not simple expression: ~s" else)]
    ))

;; Generates code for an emit.  (curried)
;; .param down*   A list of sinks (names of functions) to emit to.
;;                These can also be (NUM NAME) pairs for indexed ports.
;; .returns lines representing command-code
(define (Emit down*)
  ;;(ASSERT (not (null? down*)))
  (lambda (expr)
    (ASSERT simple-expr? expr)
    (let ([element (Simple expr)])
      (make-lines (map (lambda (down)
			 (cap (make-app (Var down) (list element))))
		    down*)))
    ;(make-lines `("emit ",(Simple expr)";"))
    ))

(define (emit-err where)
  (lambda (_) (error where "should not run into an emit!")))

;; This is used for local bindings.  But it does not increment the reference count itself.
(define (Binding emitter)
  (debug-return-contract Binding lines?
   (lambda (cb)
    ;(define val (Value (lambda (_) (error 'Binding "should not run into an emit!"))))
    (match cb
      [(,vr ,ty ,rhs) ;(,[Var -> v] ,[Type -> t] ,rhs) 
       ((Value emitter) rhs (varbindk vr ty))]
      [,oth (error 'Binding "Bad Binding, got ~s" oth)]))))

;; This is used for global and static bindings.
;; This separately returns the type decl and the initialization code.
;; This version also DOES inject a refcount incr.
(define (SplitBinding emitter)
  (lambda (cb)
    ;(define val (Value (lambda (_) (error 'Binding "should not run into an emit!"))))
    (match cb
      [(,vr ,ty ,rhs) 
       ;; We derive a setter continuation by "splitting" the varbind continuation:
       ;(define setterk (let-values ([(_ newk) ((varbindk vr ty) split-msg)]) newk))
       (define set-and-incr-k	 
	 (lambda (x)      
	   (printf "  CALLED SETTER K: ~a\n" x)
	   (if (eq? x split-msg)
	       (values (make-lines "")	set-and-incr-k)
	       (append-lines (make-lines `(" ",(Var vr)" = ",x";\n"))
			     (make-lines "/* I AM HEAP INCR */\n")
			     (incr-heap-refcount ty (Var vr))
			     ))))
       (printf "SPLITBINDING ~a\n" vr)

       (values (make-lines `(,(Type ty)" ",(Var vr)";\n"))
	       ((Value emitter) rhs set-and-incr-k))]
      [,oth (error 'SplitBinding "Bad Binding, got ~s" oth)])))

(define (Effect emitter)
  (debug-return-contract Effect lines?
  (lambda (xp)
    (match xp
      ['UNIT   (make-lines "")]
      [(tuple) (make-lines "")] ;; This should be an error.
      
      [(set! ,[Var -> v] ,[Simple -> e]) (make-lines `(,v " = " ,e ";\n"))]

      [(for (,[Var -> ind] ,[Simple -> st] ,[Simple -> en]) ,[bod])
       (make-lines
	(list `(,(Type 'Int)" ",ind";\n")
	      (block `("for (",ind" = ",st"; ",ind" <= ",en"; ",ind"++)")
		     (lines-text bod))))]
      ;; [2007.12.04] Would probably improve efficiency to at least handle scalars as well here:
      ;; Otherwise we needlessly allocate string objects.
      [(print (assert-type ,ty ,[Simple -> e]))
       ;(ASSERT (eq? t 'String))       
       (make-lines `("printf(\"",(type->printf-flag ty)"\", ",e");\n"))]
      
      [(set! ,[Var -> v] (assert-type ,ty ,[Simple -> x]))
       (append-lines 	
	;; Set! changes Ref objects, which are on the heap:
	(decr-heap-refcount ty v)  ;; Out with the old.
	(make-lines `(,v" = ",x";\n"))
	(incr-heap-refcount ty v)  ;; In with the new.
	)]

      [(Array:set (assert-type (Array ,elt) ,[Simple -> arr]) ,[Simple -> ind] ,[Simple -> val])
       (append-lines 	
	(make-lines "/* I AM ARRAY:SET DECR/INCR */\n")
	(decr-heap-refcount elt `(,arr"[",ind"]"))  ;; Out with the old.	
	(make-lines `(,arr"[",ind"] = ",val";\n"))
	(incr-heap-refcount elt `(,arr"[",ind"]"))  ;; In with the new.
	)]

      [(emit ,vq ,x) (emitter x)]
      [(begin ,[e*] ...) 
       (DEBUGASSERT (andmap lines? e*))
       (apply append-lines e*)]

      ;; DUPLICATED CASES WITH Value:
      ;; ========================================
      [(let ([,lhs ,ty ,rhs]) ,[bod])
       ;; Here we incr the refcount for a *local* reference
       (append-lines ((Binding emitter) (list lhs ty rhs))
		     (incr-local-refcount ty (Var lhs))
		     (ASSERT lines? bod)
		     (decr-local-refcount ty (Var lhs)))]
      ;; ========================================

       [(__wserror_ARRAY ,[Simple -> str]) (make-lines (list "wserror("str")"))]

      ))))

;; The continuation k is invoked on a piece of text representing the return expression.
;; k is expected to return text of the form "lines" that stores away this result.
(define (Value emitter)
  (debug-return-contract Value lines?
   (lambda (xp kont)
     ;; This is a debug wrapper that respects the funky interface to the continuation:
     ;(DEBUGMODE (set! k (debug-return-contract ValueK lines? k)))
     (match xp
       [,simp (guard (simple-expr? simp)) (kont (Simple simp))]

       ;; With the strings-as-arrays system we'll still get string
       ;; constants here, technically these are complex-constants.
       [',vec (guard (vector? vec));(assert-type (Array Char) ',vec)
        (ASSERT (vector-andmap char? vec))
	(kont (format "~a" (list->string (vector->list vec))))]
       
       ;; This doesn't change the runtime rep at all.
       [(Mutable:ref ,[Simple -> x]) (kont x)]
       [(begin ,[e]) e]
       [(begin ,[(Effect emitter) -> e1] ,e* ...)
	(define rest 
	  (begin 
	    ;(inspect (cons 'e1 e1))
	    ((Value emitter) `(begin ,@e*) kont)))
	;(inspect (cons 'rest rest))
	(ASSERT lines? e1)
	(ASSERT lines? rest)
	(append-lines e1 rest)]
       ;; This splits the continuation, using it twice.
       [(if ,[Simple -> test] ,conseq ,altern)
	(let-values ([(lines newk) (kont split-msg)])
	  (make-lines
	     `(,(lines-text lines)
	       "if (" ,test ") {\n"
	       ,(indent (lines-text ((Value emitter) conseq newk)) "  ")
	       "} else {\n"
	       ,(indent (lines-text ((Value emitter) altern newk)) "  ")
	       "}\n")))]       
       [(let ([,lhs ,ty ,rhs]) ,[bod])
	;; Here we incr the refcount for a *local* reference
	(append-lines ((Binding emitter) (list lhs ty rhs))
		      (incr-local-refcount ty (Var lhs))
		      (ASSERT lines? bod)
		      (decr-local-refcount ty (Var lhs)))]
       
       ;; PolyConstants:
       [(assert-type ,[Type -> ty] Array:null) (kont `("(",ty")0"))] ;; A null pointer.       

       [(,prim ,rand* ...) (guard (regiment-primitive? prim))
	(PrimApp (cons prim rand*) kont #f )]
       [(assert-type ,ty (,prim ,rand* ...)) (guard (regiment-primitive? prim))
	(PrimApp (cons prim rand*) kont ty)]
       
       [(assert-type ,ty ,[e]) e]
       ))))


(define PrimApp
  (debug-return-contract PrimApp lines?
   (lambda (app kontorig mayberetty)

     (define (kont x) (kontorig x)) ;; A place to throw in traces.

     ;; If we fall through to the "SimplePrim" case that means it's a
     ;; primitive with a one-to-one correspondence to a C function.
     (define SimplePrim
       (debug-return-contract SimplePrim string?
	(lambda (var)
	  (case var
	    [(not) "!"]
	    ;; These are the same as their C++ names:
	    [(cos sin tan acos asin atan max min)   (sym2str var)]
	    [(absF absD absI absI16 absI64)         "abs"]
	    [(roundF)                             "round"]
	    [(sqrtI sqrtF)                         "sqrt"]
	    ;; These are from the WS runtime library:
	    [(moduloI)                          "moduloI"]
	    [(sqrtC)                              "csqrt"]
	    ;;
	    ;; These use FFTW

#;
      [(fftR2C ifftC2R fftC ifftC memoized_fftR2C)
       (add-include! "<fftw3.h>")
       (add-include! (list "\"" (REGIMENTD) 
			   "/src/linked_lib/FFTW_wrappers.cpp\""))
       (add-link! "libfftw3f.so")
       (mangle var)]

 #;
      [(string-append 
        width start end joinsegs subseg toSigseg
        String:implode)
       (Var var)]
      [else (error 'emitC2:PrimApp "primitive not specifically handled: ~s" var)]
      ))))


     (define (wszero? ty obj)
       (match ty
	 [,ty (guard (memq ty num-types))
	      (match obj
		[',x (and (number? x) (= x 0))]
		[,_ #f])]
	 ;; This is arbitrary, false is "zero"
	 [Bool (not obj)]

	 ;; This is arbitrary, #\nul is "zero"
	 [Char (eq? obj (integer->char 0))]

	 ;; Vacillating on whether the null array should be a single
	 ;; object (a null pointer).
	 [(Array ,_)
	  (match obj
	    [Array:null #t]
	    [(assert-type ,_ ,[x]) x]
	    [,else #f])]
	 [,ty (error 'wszero? "Not yet handling zeros for this type: ~s" ty)]))
     (define (make-array len init ty)
       (let ([len (Simple len)])
	 (match ty
	   [(Array ,elt)
	   ;(k `("arrayMake(sizeof(",elt"), "len", "init")"))
	   (let* ([_elt (Type elt)]
		  [size `("sizeof(",_elt") * ",len" + 2*sizeof(int)")]
		  [tmp (Var (unique-name "arrtmp"))]
		  [alloc (if (wszero? elt init)
			     `("calloc(",size", 1)")
			     `("malloc(",size")"))]
		  [cast `("(",_elt"*)",tmp)])
	     (append-lines 
	      (make-lines `("int* ",tmp" = ((int*)",alloc" + 2);\n"
			    ,tmp"[-1] = 0;\n"
			    ,tmp"[-2] = ",len";\n"
			    ;; Now fill the array, if we need to:
			    ,(if (and init (not (wszero? elt init)))
				 (let ([i (unique-name "i")]
				       [tmp2 (unique-name "arrtmpb")]
				       [len2 (unique-name "lenmin1")])
				   (list `(,_elt"* ",(Var tmp2) " = ",cast";\n")
					 `("int ",(Var len2) " = ",len" - 1;\n")
					 (lines-text
					  ((Effect (emit-err 'array:make-constant))
					   `(for (,i '0 ,len2)
						(Array:set (assert-type (Array ,elt) ,tmp2) ,i ,init))))))
				 "")))
	      (kont cast))
	     )])))
     
     (match app
       ;; Refs and sets are pure simplicity:
       [(Array:ref (assert-type (Array ,[Type -> ty]) ,[Simple -> arr]) ,[Simple -> ind])
	(kont `(,arr"[",ind"]"))]
       [(Array:length ,[Simple -> arr])
	;; Length is -2 and refcount is -1:
	(kont `("((int*)",arr")[-2]"))]
       ;; This is the more complex part:

       [(Array:make ,len ,init) (make-array len init mayberetty)]
       [(Array:makeUNSAFE ,len) (make-array len #f   mayberetty)]
       
       ;; wsequal? should only exist for scalars at this point:
       [(wsequal? (assert-type ,ty ,[Simple -> left]) ,[Simple -> right])
	(ASSERT (memq ty '(Int Int16 Int64 Float Double Complex)))
	(kont `("(",left" == ",right")"))]

       [(__show_ARRAY (assert-type ,ty ,[Simple -> obj]))
	(match ty
	  [String       (kont obj)]
	  [(Array Char) (kont obj)]
	  [,_ 
	   (let* ([str (unique-name "str")]
		  [_str (Var str)])
	     (append-lines 
	      ((Binding (emit-err '__show_ARRAY)) 
	       (list str '(Array Char) '(assert-type (Array Char) (Array:makeUNSAFE '100))))
	      (make-lines (list "snprintf("_str", 100, "(type->printf-flag ty)", )"))
	      (kont _str)))])]
       
       [(,infix_prim ,[Simple -> left] ,[Simple -> right])
	(guard (memq infix_prim '(;+ - * /
				  +. -. *. /. 
				     +D -D *D /D
				     +_ *_ -_ /_
				     +: *: -: /:
				     +I16 *I16 -I16 /I16
				     +I64 *I64 -I64 /I64
				     < > <= >= =
				     ^_ ^. ^: ^D ^I16 ^I64
				     )))
	(let ([cname (case infix_prim
		       [(=) "=="]
		       [(;+ * - / 
			 < > <= >=) infix_prim]
		       [(+. *. -. /.
			    +_ *_ -_ /_
			    +D *D -D /D
			    +: *: -: /:
			    ^_ ^. ^D
			    ) ;; Chop off the extra character.
			(substring (sym2str infix_prim) 0 1)]
		       [(+I16 -I16 *I16 /I16 ^I16
			      +I64 -I64 *I64 /I64 ^I64
			      )
			(substring (sym2str infix_prim) 0 1)]
		       )])
	  (kont `("(" ,left ,(format " ~a " cname) ,right ")")))]


       [(,other ,[Simple -> rand*] ...)
	(kont `(,(SimplePrim other) "(" ,(insert-between ", " rand*) ")"))]
       ))))



;; .returns function def that executes the source (lines) and state decls (lines)
(define (Source xp)
   (match xp
    [((name ,nm) (output-type ,ty) (code ,cd)  (outgoing ,down* ...))
     (match cd 
       [(timer ,annot ',rate)
	(ASSERT integer? rate)
	;; For the basic backend, we simply call the downstream
	;; operator when we ourselves are asked for data (invoked).
	(values nm
	 ((Emit down*) ''UNIT) ;; Code
	 (make-lines ())) 	 ;; State 	
	])]))

;; .returns top-level decls (lines)
(define (Operator op)
  (match op
    [(iterate (name ,name) 
	      (output-type ,ty)
	      (code (iterate ,annot ,itercode ,_))
	      (incoming ,up)
	      (outgoing ,down* ...))
     (define emitter (Emit down*))
     (match itercode
       [(let (,[(SplitBinding (emit-err 'OperatorBinding)) -> bind* init*] ...) (lambda (,v ,vq) (,vty (VQueue ,outty)) ,bod))
	(values 
	 (wrap-iterate-as-simple-fun name v vty 
	    ((Value emitter) bod nullk))
	 bind* init*)]
       )]))

;================================================================================

(define-pass emit-c2
  [Program 
   (lambda (prog Expr)
     (fluid-let ([free-fun-table ()])
       (let ([freefundefs (build-free-fun-table! (cdr (ASSERT (project-metadata 'heap-types prog))))])
     ;(assq 'c-includes meta*)
     (match prog
       [(,lang '(graph (const ,[(Binding (emit-err 'TopBinding)) -> cb*] ...) ;; These had better be *constants* for GCC
		       (init  ,[(Effect (lambda _ (error 'top-level-init "code should not 'emit'"))) 
				-> init*] ...)
		       (sources ,[Source -> srcname* src*  state1*] ...)
		       (operators ,[Operator -> oper* state2** init**] ...)
		       (sink ,base ,basetype)
		       ,meta* ...))       
	  (define includes (string-append "#include<stdio.h>\n" 
					  "#include<stdlib.h>\n"
					  "#include \""(** (REGIMENTD) "/src/linked_lib/wsc2.h")"\"\n"
					  ))
	  (define allstate (text->string (map lines-text (apply append cb* state1* state2**))))
	  (define ops      (text->string (map lines-text (reverse oper*))))
	  (define srcfuns  (text->string (map lines-text (map wrap-source-as-plain-thunk srcname* src*))))
	  (define init     (begin (ASSERT (andmap lines? (apply append init**)))
			     (text->string (block "void initState()" (lines-text (apply append-lines (apply append init**)))))))
	  (define driver   (text->string (lines-text (build-main-source-driver srcname*))))
	  ;;(define toplevelsink "void BASE(int x) { }\n")	  
	  (define total (apply string-append 
			(insert-between "\n"
			  (list includes 
				(text->string (lines-text freefundefs))
				allstate
				;toplevelsink
				ops srcfuns 
				init driver))))

	  ;;(printf "====================\n\n")	
	  ;;(display total)
	  total]

       [,other ;; Otherwise it's an invalid program.
	(error 'emit-c2 "ERROR: bad top-level WS program: ~s" other)]))
	 ))]
  ) ;; End pass




) ;; End module

