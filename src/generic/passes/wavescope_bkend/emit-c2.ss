

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
            "insert-refcounts.ss"
            "nominalize-types.ss"
	    "../../compiler_components/c_generator.ss" )
  (provide emit-c2)
  (chezprovide )  
  (chezimports (except helpers test-this these-tests))

  (cond-expand [chez (import rn-match)] [else (void)])

;; Mutated below:
(define global-struct-defs '())


;; This is mutated below with fluid-let to contain a table mapping
;; types to a scheme function that generates free-code (when given
;; text representing the pointer to free).
(define free-fun-table '())

(define include-files ())
(define link-files    ())

(define (add-include! fn)
  (unless (member fn include-files)
    (set! include-files (cons fn include-files))))
(define (add-link! fn)
  (unless (member fn link-files)
    (set! link-files (cons fn link-files))))

;;========================================

;; These are just for sanity checking.  Disjoint types keep mix-ups from happening.
;; These should just be wrappers on the outside.  You should never find them nested.
(reg:define-struct (lines text))
;(reg:define-struct (expression text))
(define (append-lines . ls) 
  (DEBUGASSERT (andmap lines? ls))
  (make-lines (map lines-text ls)))
;(define (idk x) (ASSERT expression? x) x)
;(define (idk x) x)

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

(define (insert-c-string-escapes str)
  (list "\""
    (list->string
     (match (string->list str)
       [() ()]       
       [(#\nul     . ,[tl]) (list* #\\ #\0 #\0 #\0 tl)]
       [(#\newline . ,[tl]) (list* #\\ #\n tl)]
       [(#\tab     . ,[tl]) (list* #\\ #\t tl)]
       [(#\" . ,[tl])       (list* #\\ #\" tl)]
       [(#\\ . ,[tl])       (list* #\\ #\\ tl)]
       [(,a . ,[tl])        (cons a tl)]))
    "\""))

;================================================================================
;;; Input grammar

;(define emit-c2-input-grammar)


;(define-class <refcounter> (<class>) ())
;(define-class <emit-c2> (<class>) (rc))

; (specialise! initialise <emit-c2>
;   (lambda (call-next self . args) (call-next)
;     (slot-set! self 'rc (get-arg args 'rc))))
;(define-generic Emit)

;================================================================================


;; This builds a set of top level function definitions that free all
;; the heap-allocated types present in the system.  It also builds a
;; table mapping types onto syntax-generating functions that call the
;; appropriate free-ing code.
;; 
;; Currently this routine mutates the table "free-fun-table" on the fly.
;;
;; This is this function that decides what freeing will be "open coded"
;; vs. relegated to separately defined functions.
(define (build-free-fun-table! heap-types)
  (define fun-def-acc '()) ;; mutated below  
  (define (add-to-freefuns! ty fun)
    ;; Inefficient: should use hash table:
    (unless (assoc ty free-fun-table)
      (set! free-fun-table (cons (cons ty (debug-return-contract lines? fun))
				 free-fun-table))))  
  (define (add-to-defs! ty def)
    ;; Inefficient: should use hash table:
    (unless (assoc ty free-fun-table)
      (set! fun-def-acc (cons def fun-def-acc))))

  ;; First we add the freefuns name entries, because we might need
  ;; those to generate the defs.
  (for-each
      (lambda (ty) 
	(define name (type->name ty))
	(define default-fun_name `("void free_",name"(",(Type ty)" ptr)"))
	(define default-specialized_fun
	  (lambda (ptr) (make-lines `("free_",name"(",ptr");\n"))))
	(let loop ([ty ty])
	   (match ty ;; <- No match recursion!
	     ;; Tuples are not heap allocated for now (at least the bigger ones should be):
	     [,tup (guard (vector? tup)) #f] ;; No entry in the table.
	     [,scl (guard (not (heap-allocated? scl global-struct-defs))) #f]
	     [(Struct ,tuptyp) (add-to-freefuns! ty default-specialized_fun)]
	     [(Ref ,elt) (loop elt)]
	     [(List ,elt) (add-to-freefuns! ty default-specialized_fun)]
	     [(Array ,elt)
	      (if (not (heap-allocated? elt global-struct-defs))
		  (add-to-freefuns! ty (lambda (ptr) (make-lines `("free((int*)",ptr" - 2);\n"))))
		  (add-to-freefuns! ty default-specialized_fun))])))
    heap-types)

  ;; Now, for every name we've added to the table, we need to generate a definition:  
  (apply append-lines 
   (map (lambda (entry) 
	  (define ty (car entry))
	  (define name (type->name ty))
	  (define default-fun_name `("void free_",name"(",(Type ty)" ptr)"))
	  (define default-specialized_fun
	    (lambda (ptr) (make-lines `("free_",name"(",ptr");\n"))))
	  (let loop ([ty ty])
	       (match ty ;; <- No match recursion!
		 [(Struct ,tuptyp)
		  (let ([flds (cdr (ASSERT (assq tuptyp global-struct-defs)))])
		    (make-lines
		     (list
		     "/* Freeing struct: "(sym2str tuptyp)"*/\n"
		     (block default-fun_name
			    (map (match-lambda ((,fldname ,ty))
				   (lines-text (gen-decr-code ty (list "ptr."(sym2str fldname)) default-free)))
			      flds)))))]
		 [(List ,elt)
		  ;; We always build an explicit function for freeing list types:
		  ;; Here's a hack to enable us to recursively free the same type.
		  (make-lines 
		   (block default-fun_name
			  (block (list "if(ptr)") ;; If not null.
				 `(
				   ;; Recursively decr tail:
				   ,(Type `(List ,elt))" ptr2 = CDR(ptr);\n"
				   ,(lines-text (gen-decr-code `(List ,elt) "ptr2" default-free))
				   ;; If heap allocated, free the CAR:
				   ,(if (heap-allocated? elt global-struct-defs)
					(lines-text (gen-decr-code elt `("(*ptr)") default-free))
					"")
				   "free((int*)ptr - 2);\n"))))]
		 [(Array ,elt)
		  (if (not (heap-allocated? elt global-struct-defs))
		      (make-lines "")
		      (let ([ind (Var (unique-name "i"))])
			(make-lines 
			 (block default-fun_name
				`("int ",ind";\n"
				  ,(block `("for (",ind" = 0; ",ind" < ARRLEN(ptr); ",ind"++)")
					  ;;(lines-text ((cdr (loop elt)) `("ptr[",ind"]")))
					  (begin ;(loop elt) ;; For side effect
						 (lines-text (gen-decr-code elt `("ptr[",ind"]") default-free)))
					  )
				  "free((int*)ptr - 2);\n")))
			))])))
     free-fun-table)))



;================================================================================
;;; Hooks for garbage collection.

;; For the naive strategy we'de allow shared pointers but would need a CAS here.
(define (gen-incr-code ty ptr msg)
  (match ty
    ;; Both of these types just decr the -1 offset:
    [(,Container ,elt) (guard (memq Container '(Array List)))
     (make-lines `("INCR_RC(",ptr"); /* ",msg" type: ",(format "~a" ty)" */\n"))]
    [(Ref ,[ty]) ty]
    ;; Could make this a separate function:
    [(Struct ,name) 
     (apply append-lines
	    (make-lines (format "/* Incr tuple refcount, Struct ~a */\n" name))
	    (map (match-lambda ((,fldname ,ty))
		   (gen-incr-code ty (list ptr "." (sym2str fldname)) msg))
	      (cdr (assq name global-struct-defs))))]
    ;; Other types are not heap allocated:
    [,ty (guard (not (heap-allocated? ty global-struct-defs)))(make-lines "")]
    ))

;; "ptr" should be text representing a C lvalue
;; returns "lines"
(define (gen-decr-code ty ptr freefun)
  (match ty
    [(,Container ,elt) (guard (memq Container '(Array List)))
     (make-lines 
      (block `("if (DECR_RC_PRED(",ptr")) /* type: ",(format "~a" ty)" */ ")
	     (lines-text (freefun ty ptr))))]
    [(Ref ,[ty]) ty]
    ;; Could make this a separate function:
    [(Struct ,name) 
     (apply append-lines
	    (make-lines (format "/* Decr tuple refcount, Struct ~a */\n" name))
	    (map (match-lambda ((,fldname ,ty))
		   (gen-decr-code ty (list ptr "." (sym2str fldname)) freefun))
	      (cdr (assq name global-struct-defs))))]
    [,ty (guard (not (heap-allocated? ty global-struct-defs))) (make-lines "")]))

;; Should only be called for types that are actually heap allocated.
(define default-free
  (debug-return-contract lines?
  (lambda (ty ptr)    
    (define (strip-refs ty)
      (match ty [(Ref ,ty) ty] [,ty ty]))
    (let ([newty (strip-refs ty)])
      ((cdr (ASSERT (assoc newty free-fun-table))) ptr)))))

;; This version represents plain old reference counting.
(begin
  (define (incr-local-refcount ty ptr) (gen-incr-code ty ptr "local"))
  (define (decr-local-refcount ty ptr) (gen-decr-code ty ptr default-free))
  
  (define (incr-heap-refcount ty ptr) (gen-incr-code ty ptr "heap"))
  (define (decr-heap-refcount ty ptr)
    (gen-decr-code ty ptr default-free))
  (define (potential-collect-point) (make-lines ""))
  )

#;
;; This version uses deferred reference counting.
(begin 
  (define (incr-local-refcount ty ptr) (make-lines ""))
  (define (decr-local-refcount ty ptr) (make-lines ""))

  (define (incr-heap-refcount ty ptr) (gen-incr-code ty ptr "heap"))
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
    ;[#(,[flds] ...) (apply string-append "TupleOf_" flds)]
    [(Struct ,tuptyp) 
     (string-append
      (apply string-append "TupleOf_" 
			     (insert-between "_"
			      (map type->name (map cadr 
			       (cdr (ASSERT (assq tuptyp global-struct-defs)))))))
      "__")]
    [,scalt (guard (scalar-type? scalt)) (sym2str scalt)]
    [(Ref ,[ty]) ty] ;; Doesn't affect the name currently...
    [(Array ,[ty]) (string-append "Array_" ty)]
    [(List  ,[ty]) (string-append "List_"  ty)]
    [#() "Unit"]
    ))

;(define (make-fundef type name rand bod) 0)


;; This is the simplest way to package a source.
(define (wrap-source-as-plain-thunk name code)
  (make-lines (block `("void ",(Var name) "()") (lines-text code))))

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
     ;[(char? datum) (wrap (format "'~a'" datum))]
     [(char? datum) (wrap (format "~a" (char->integer datum)))]


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
    [Bool    "char"]
    [Int     "int"]
    [Int16   "int16_t"]
    [Int64   "int64_t"]
    [Double  "double"]
    [Float   "float"]
    [Complex "float complex"]
    [Char    "char"]
    [String  "char*"] ;; Not boosted.
    [#()      "char"]
    [(Struct ,name) (list "struct "(sym2str name))] ;; Value type.

    ;; An array looks like a C array, except the -1 word offset is a refcount and -2 is length.
    [(Array ,[elt]) (list elt "*")] ;[(Array ,[elt]) (list "(" elt "*)")]

    ;; A cons cell is just looks like a pointer, except the -1 offset is a refcount, and -2 is the CDR.
    [(List ,[elt]) (list elt "*")]

    [(Ref ,[ty]) ty]

    ;; This is an unused value.
    [(VQueue ,_) "char"]

    ))


;; Generate a struct definition.
(define (StructDef entry)
     (match entry
       [(,(sym2str -> name) (,[sym2str -> fld*] ,typ*) ...)
	(let ([tmpargs (map (lambda (_) (sym2str (unique-name 'tmp))) fld*)]
	      [ctype* (map Type typ*)])
	  `(,(block `("struct ",name)
		    (map (lambda (ctype fld) `[,ctype " " ,fld ";\n"])
		      ctype* fld*))
	    ";\n"
            ))]))

;; For the set of types that are printable at this point, we can use a simple printf flag.
(define (type->printf-flag ty)
  (match ty
    [#()    "()"]
    [String "%s"]
    [(Array Char) "%s"] ;; These are strings in disguise.
    [Bool   "%d"]
    [Int    "%d"]
    [Int16  "%hd"]
    [Int64  "%lld"]
    [Float  "%f"]	   
    [Double "%lf"]
    [#() "()"]
    [(Pointer ,_) "%p"]))

;(define Var mangle-name)
(define Var sym2str)

;; These are the expressions that make valid operands (post flattening)
;;   .returns A string.  Could return an "expression".
(define (Simple expr)
  (match expr
    [,v (guard (symbol? v)) (ASSERT (not (regiment-primitive? v))) (Var v)]
    ;[',c (format "~a" c)] ;;TEMPTEMP

    ;; PolyConstants:
    [(assert-type ,[Type -> ty] Array:null) `("(",ty")0")] ;; A null pointer. This choice is debatable.
    [(assert-type ,[Type -> ty] '())        `("(",ty")0")] ;; A null pointer.       
    ['() (error 'EmitC2:Simple "null list without type annotation")]

    ;; All other constants:
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
			 (cap 
			  (list (make-app (Var down) (list element))
				" /* emit */")))
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
	   (if (eq? x split-msg)
	       (values (make-lines "")	set-and-incr-k)
	       (append-lines (make-lines `(" ",(Var vr)" = ",x";\n"))
			     (make-lines "/* I AM HEAP INCR */\n")
			     #|(incr-heap-refcount ty (Var vr))|#
			     ))))
       (values (make-lines `(,(Type ty)" ",(Var vr)";\n"))
	       ((Value emitter) rhs set-and-incr-k))]
      [,oth (error 'SplitBinding "Bad Binding, got ~s" oth)])))

(define (Effect emitter)
  (debug-return-contract Effect lines?
  (lambda (xp)
    (match xp
      ['UNIT   (make-lines "")]
      [(tuple) (make-lines "")] ;; This should be an error.
      ;; Thanks to insert-refcounts this can get in here:
      [,v (guard (symbol? v)) (make-lines "")]

      [(incr-heap-refcount  ,ty ,[Simple -> val]) (incr-heap-refcount  ty val)]
      [(incr-local-refcount ,ty ,[Simple -> val]) (incr-local-refcount ty val)]
      [(decr-heap-refcount  ,ty ,[Simple -> val]) (decr-heap-refcount  ty val)]
      [(decr-local-refcount ,ty ,[Simple -> val]) (decr-local-refcount ty val)]

      [(for (,[Var -> ind] ,[Simple -> st] ,[Simple -> en]) ,[bod])
       (make-lines
	(list `(" ",(Type 'Int)" ",ind";\n")
	      (block `("for (",ind" = ",st"; ",ind" <= ",en"; ",ind"++)")
		     (lines-text bod))))]

      [(while ,test ,[bod])
       (let ([flag (unique-name "grosshack")])
	 (make-lines 
	  (block `("while (1)") 
		 (list 
		  (lines-text ((Value emitter) test (varbindk flag 'Bool)))
		  "if ("(Var flag)") {\n"
		  (indent (lines-text bod) "  ")
		  "} else break; \n"
		  ))))]

      ;; [2007.12.04] Would probably improve efficiency to at least handle scalars as well here:
      ;; Otherwise we needlessly allocate string objects.
      [(print (assert-type ,ty ,[Simple -> e]))
       ;(ASSERT (eq? t 'String))       
       (make-lines `("printf(\"",(type->printf-flag ty)"\", ",e");\n"))]
      
      [(set! ,[Var -> v] (assert-type ,ty ,[Simple -> x]))
       (append-lines 	
	;; Set! changes Ref objects, which are on the heap:
	#|(decr-heap-refcount ty v)  ;; Out with the old.|#
	(make-lines `(,v" = ",x";\n"))
	#|(incr-heap-refcount ty v)  ;; In with the new.|#
	)]

      [(Array:set (assert-type (Array ,elt) ,[Simple -> arr]) ,[Simple -> ind] ,[Simple -> val])
       (append-lines 	
	;(make-lines "/* I AM ARRAY:SET DECR/INCR */\n")
	#|(decr-heap-refcount elt `(,arr"[",ind"]"))  ;; Out with the old.|#
	(make-lines `(,arr"[",ind"] = ",val";\n"))
	#|(incr-heap-refcount elt `(,arr"[",ind"]"))  ;; In with the new.|#
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
		     #|(incr-local-refcount ty (Var lhs))|#
		     (ASSERT lines? bod)
		     #|(decr-local-refcount ty (Var lhs))|#
		     )]
      ;; ========================================

       [(__wserror_ARRAY ,[Simple -> str]) (make-lines (list "wserror("str");\n"))]

      ))))

;; The continuation k is invoked on a piece of text representing the return expression.
;; k is expected to return text of the form "lines" that stores away this result.
(define (Value emitter)
  (debug-return-contract Value lines?
   (lambda (xp kont)
     ;; This is a debug wrapper that respects the funky interface to the continuation:
     ;(DEBUGMODE (set! k (debug-return-contract ValueK lines? k)))
     (match xp

       ;; With the strings-as-arrays system we'll still get string
       ;; constants here, technically these are complex-constants.  We
       ;; could treat them the same way as other constant arrays, but
       ;; we get much less bloated, much more readable code if we do
       ;; the following:
       [',vec (guard (vector? vec));(assert-type (Array Char) ',vec)
        (ASSERT (vector-andmap char? vec))
	;; Strip out any null characters??
	(let* ([tmp (unique-name "tmpchararr")]
	       [ls (vector->list vec)]
	       [ls2 ;; Hack, a null terminator on the end is redundant for a string const:
		(if (fx= 0 (char->integer (vector-ref vec (sub1 (vector-length vec)))))
		    (rdc ls) 
		    ls)]
	       [newk (varbindk tmp '(Array Char))]
	       [str (list->string ls2)]
	       [copylen (add1 (string-length str)) ;; Copy the null terminator
		      ;(vector-length vec)
			])
	  (append-lines 
	   ((Value emitter) `(assert-type (Array Char) (Array:makeUNSAFE ',(vector-length vec))) newk)
	   (make-lines
	    (format "memcpy(~a, ~s, ~s);\n" (text->string (Var tmp))
		    str copylen))
	   (kont (Var tmp))))]

       [,simp (guard (simple-expr? simp)) (kont (Simple simp))]
       
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
		      #|(incr-local-refcount ty (Var lhs))|#
		      (ASSERT lines? bod)
		      #|(decr-local-refcount ty (Var lhs))|#
		      )]

       [(make-struct ,name ,[Simple -> arg*] ...)
	(kont `("{",(insert-between ", " arg*)"}"))]
       [(struct-ref ,type ,fld ,[Simple -> x])
	(kont `("(",x "." ,(sym2str fld)")"))]
       
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
	    [(cos sin tan acos asin atan)   (sym2str var)]
	    [(absF absD absI absI16 absI64)         "abs"]
	    [(roundF)                             "round"]
	    [(sqrtI sqrtF)                         "sqrt"]
	    ;; These are from the WS runtime library:
	    [(moduloI)                          "moduloI"]
	    [(sqrtC)                              "csqrt"]

	    #;
	    [(List:length List:ref List:append List:reverse) ;; List:make 
	     (list->string (remq-all #\: (string->list (sym2str var))))]

	    [else (error 'emitC2:PrimApp:SimplePrim "primitive not specifically handled: ~s" var)]
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

	 [(Struct ,_) #f]
	 
	 [,ty (error 'wszero? "Not yet handling zeros for this type: ~s, obj ~s" ty obj)]))
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
	      (make-lines 
	       (list
		`("int* ",tmp" = (int*)0;\n")
		(block `("if (",len")")
		       `(,tmp" = ((int*)",alloc" + 2);\n"
			     "CLEAR_RC(",tmp");\n"           
			     "SETARRLEN(",tmp", ",len");\n"  
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
				  "")))))
	      (kont cast)))])))
     
     (match app
       ;; Refs and sets are pure simplicity:
       [(Array:ref (assert-type (Array ,[Type -> ty]) ,[Simple -> arr]) ,[Simple -> ind])
	(kont `(,arr"[",ind"]"))]

       ;; Using some simple C macros here:
       [(car ,[Simple -> pr]) (kont `("CAR(",pr")"))]
       [(cdr ,[Simple -> pr]) (kont `("CDR(",pr")"))]

       [(List:is_null ,[Simple -> pr]) (kont `("(",pr" == 0)"))]

       [(cons ,[Simple -> hd] ,[Simple -> tl])
	(ASSERT mayberetty)
	(match mayberetty
	  [(List ,elt)
	   (let ([tmp (Var (unique-name "tmpcell"))]
		 [ty (Type mayberetty)])
	     (append-lines 
	      (make-lines `(,ty" ",tmp" = (",ty")CONSCELL(",(Type elt)");\n"
			       "CLEAR_RC(",tmp");\n"           
			       "SETCDR(",tmp", ",tl");\n"  
			       "SETCAR(",tmp", ",hd");\n"  
			       ;,tmp"[0] = ",hd";\n"
			       ))
	      ;; Increment cdr and car refcounts:
	      ;(make-lines " /* incr car: */ ")
	      #|(incr-heap-refcount elt hd) |#
	      ;(make-lines " /* incr cdr: */ ")
	      #|(incr-heap-refcount mayberetty tl) |#
	      (kont tmp)))])]
       
       [(Array:length ,[Simple -> arr])
	;; Length is -2 and refcount is -1:
	(kont `("ARRLEN(",arr")"))
	]
       ;; This is the more complex part:

       [(Array:make ,len ,init) (make-array len init mayberetty)]
       [(Array:makeUNSAFE ,len) (make-array len #f   mayberetty)]

       [(max ,[Simple -> a] ,[Simple -> b]) (kont `("(",a" > ",b" ? ",a" :",b")"))]
       [(min ,[Simple -> a] ,[Simple -> b]) (kont `("(",a" < ",b" ? ",a" :",b")"))]

       ;; These use FFTW currently
       [(,fft ,arr) ;(fftR2C ifftC2R fftC ifftC memoized_fftR2C)
	(guard (memq fft '(memoized_fftR2C fftR2C ifftC2R))) ;; TEMP: Just using memoized for the normal one too!
	(define inverse? (eq? fft 'ifftC2R))
	(define len0 (unique-name "len"))
	(define len1 (unique-name "len"))
	(define len2 (unique-name "len"))
	(define tmp (unique-name "tmp"))
	(define name (if inverse? "memoized_ifftC2R" "memoized_fftR2C"))
	(ASSERT simple-expr? arr)
	(add-include! "<fftw3.h>")
	(add-include! (list "\"" (REGIMENTD) "/src/linked_lib/fftw_wrappers.c\""))
	(add-link! "libfftw3f.so")		
	(append-lines ((Binding (emit-err 'fft)) (list len0 'Int `(Array:length ,arr)))
		      ((Binding (emit-err 'fft)) (list len1 'Int (if inverse? `(_-_ ,len0 '1) `(/_ ,len0 '2) )))
		      ((Binding (emit-err 'fft)) (list len2 'Int (if inverse? `(*_ ,len1 '2) `(_+_ ,len1 '1) )))
		      ((Binding (emit-err 'fft))
				   (list tmp 
					 (if inverse? '(Array Float) '(Array Complex))
					;`(assert-type (Array Complex) (Array:makeUNSAFE ,len))
					 (if inverse?
					     `(assert-type (Array Float)   (Array:makeUNSAFE ,len2))
					     `(assert-type (Array Complex) (Array:makeUNSAFE ,len2)))))
		      (make-lines `(,name"(",(Simple arr)", ",(Var tmp)");\n"))
		      (kont (Var tmp)))]
       
       ;; wsequal? should only exist for scalars at this point:
       [(wsequal? (assert-type ,ty ,[Simple -> left]) ,[Simple -> right])
	;(ASSERT (memq ty '(Int Int16 Int64 Float Double Complex)))
	(ASSERT scalar-type? ty)		
	(kont `("(",left" == ",right")"))]

       [(__show_ARRAY (assert-type ,ty ,[Simple -> obj]))
	(define max-show-size 100)
	(match ty
	  [String       (kont obj)]
	  [(Array Char) (kont obj)]
	  [,_ 
	   (let* ([str (unique-name "str")]
		  [_str (Var str)]
		  [_realsize (Var (unique-name "realsize"))]
		  [_max (number->string max-show-size)])
	     (append-lines 
	      ((Binding (emit-err '__show_ARRAY)) 
	       (list str '(Array Char) `(assert-type (Array Char) (Array:makeUNSAFE ',max-show-size))))
	      (make-lines (list 			   
			   "int "_realsize" = snprintf("_str", "
			           _max", \""
				   (type->printf-flag ty)"\", "obj");\n"
			   "if ("_realsize" >= "_max") { printf(\"Error, show overflowed fixed length "
			   _max" buffer\\n\"); exit(-1); }\n"
			   "SETARRLEN("_str", "_realsize" + 1); /* set virtual length */ \n"
			   ))
	      (kont _str)))])]
       
       [(,infix_prim ,[Simple -> left] ,[Simple -> right])	
	(guard (assq infix_prim infix-arith-prims))
	(define valid-outputs '("+" "-" "/" "*" "^" "<" ">" "==" "<=" ">="))
	(define result
	  (case infix_prim
	    [(=) "=="]
	    [(< > <= >=) (sym2str infix_prim)]
	    [else 
	     (match (string->list (sym2str infix_prim))	  
	       [(#\_ ,op ,suffix ...) (list->string (list op))]
	       [(,op ,suffix ...)     (list->string (list op))])]))
	(ASSERT (member result valid-outputs))
	(kont (list "(" left " " result " " right ")"))]


       ;;========================================
; 	[(realpart ,[v]) `("(" ,v ".real)")]
; 	[(imagpart ,[v]) `("(" ,v ".imag)")]
 	[(realpart ,[Simple -> v])   (kont `("__real__ (" ,v ")"))]
 	[(imagpart ,[Simple -> v])   (kont `("__imag__ (" ,v ")"))]

; 	;; Wait... do we need to cast to double here?
; 	[(,extractreal ,[Simple -> v]) 
; 	 (guard (memq extractreal '(realpart complexToFloat complexToDouble)))
; 	 (kont `("__real__ (" ,v ")"))]

	[(absC ,[Simple -> c]) (kont `("cNorm(",c")"))]

	[(intToChar ,[Simple -> e]) (kont `("(wschar_t)",e))]
	[(,toint     ,[Simple -> e]) 
	 (guard (memq toint '(int16ToInt int64ToInt floatToInt doubleToInt charToInt)))
	 (kont `("(int)",e))]
	[(,toint16     ,[Simple -> e]) 
	 (guard (memq toint16 '(intToInt16 int64ToInt16 floatToInt16 doubleToInt16)))
	 (kont `("(int16_t)",e))]
	[(,toint64    ,[Simple -> e]) 
	 (guard (memq toint64 '(int16ToInt64 intToInt64 floatToInt64 doubleToInt64)))
	 (kont `("(int64_t)",e))]
	[(,tofloat  ,[Simple -> e]) 
	 (guard (memq tofloat '(intToFloat int16ToFloat int64ToFloat doubleToFloat)))
	 (kont `("(float)",e))]
	[(,todouble  ,[Simple -> e]) 
	 (guard (memq todouble '(intToDouble int16ToDouble int64ToDouble floatToDouble)))
	 (kont `("(double)",e))]

 	[(complexToFloat ,e)   (PrimApp `(realpart ,e) kont mayberetty)]
 	[(complexToInt16 ,e)   (kont `("(int16_t)" ,(PrimApp `(complexToFloat ,e) id 'Float)))]
 	[(complexToInt64 ,e)   (kont `("(int64_t)" ,(PrimApp `(complexToFloat ,e) id 'Float)))]
 	[(complexToInt ,e)     (kont `("(int)"     ,(PrimApp `(complexToFloat ,e) id 'Float)))]
 	[(complexToDouble ,e)  (kont `("(double)"  ,(PrimApp `(complexToFloat ,e) id 'Float)))]
 	[(,ToComplex ,[Simple -> e])
 	 (guard (memq ToComplex 
 		      '(int16ToComplex int64ToComplex intToComplex floatToComplex doubleToComplex)))
     	 (kont `("(",e" + 0.0fi)"))]
 	[(makeComplex ,[Simple -> re] ,[Simple -> im])
 	 (kont `("(",re" + (",im" * 1.0fi))"))]
	;;========================================

	[(__stringToInt_ARRAY ,[Simple -> e]) 
	 (let ([tmp (Var (unique-name 'tmp))])
	   (make-lines 
	    `("int ",tmp";\n" 
	      "sscanf(",e", \"%d\", &",tmp");\n"
	      ,(lines-text (kont tmp)))))]
	[(__stringToFloat_ARRAY ,[Simple -> e]) 
	 (let ([tmp (Var (unique-name 'tmp))])
	   (make-lines 
	    `("float ",tmp";\n"
	      "sscanf(",e", \"%f\", &",tmp");\n"
	      ,(lines-text (kont tmp)))))]
	[(__stringToDouble_ARRAY ,[Simple -> e]) 
	 (let ([tmp (Var (unique-name 'tmp))])
	   (make-lines
	    `("double ",tmp";\n"
	      "sscanf(",e", \"%lf\", &",tmp");\n"
	      ,(lines-text (kont tmp)))))]
	[(__stringToComplex_ARRAY ,[Simple -> e]) 
	 (let ([tmp1 (Var (unique-name 'tmp))]
	       [tmp2 (Var (unique-name 'tmp))])
	   (make-lines
	    `("float ",tmp1";\n"
	      "float ",tmp2";\n"
	      "sscanf(",e", \"%f+%fi\", &",tmp1", &",tmp2");\n"
	      ,(lines-text (kont `(,tmp1"+(",tmp2"*1.0fi)"))))))]

	;;========================================

	[(clock) (kont "(clock() * 1000 / CLOCKS_PER_SEC)")]
	[(realtime) 
	 (define tmp (symbol->string (unique-name "tmp")))
	 `("struct timeval ",tmp";\n"
	   "gettimeofday(&",tmp", NULL);\n"
	   ,(kont `("(",tmp".tv_sec * 1000 + ",tmp".tv_usec / 1000)")))]
       
	[(,other ,[Simple -> rand*] ...)
	 (kont `(,(SimplePrim other) "(" ,(insert-between ", " rand*) ")"))]
       ))))



;; .param srccode* blocks of code (lines) for the body of each source.
(define (build-main-source-driver srcname* srccode* srcrates*)
  (printf "   TIMER RATES: ~s\n" srcrates*)
  (let  ([counter_marks
	  (cond
	   [(= 1 (length srcrates*)) '(1)]
	   [(andmap integer? srcrates*)
	    (let ([common-rate (apply lcm srcrates*)])	    
	      (map (lambda (rate)
		     (inexact->exact (quotient common-rate rate)))
		srcrates*))]
	   [else (error 'timer "non integer rates not handled yet: ~s" srcrates*)])])
    (make-lines        
     (block "int main()" 
	    (list 
	     "initState();\n"
	     (map (lambda (name) (format "int counter_~a = 0;\n" name)) srcname*)
	     (block "while(1)"		 
		    (list (map (lambda (name) (format "counter_~a++;\n" name)) srcname*)
			  (map (lambda (name code mark)
				 (block (format "if (counter_~a == ~a)" name mark)
					(list (lines-text code)
					      (format "counter_~a = 0;\n" name))))
			    srcname* srccode* counter_marks)
			  )
		  ;(map (lambda (f) (list (make-app (Var f) ()) ";\n")) srcname*)
		  )
	   "return 0;\n")))))

;; .returns function def that executes the source (lines) and state decls (lines)
(define (Source xp)
   (match xp
    [((name ,nm) (output-type ,ty) (code ,cd)  (outgoing ,down* ...))
     (match cd 
       [(timer ,annot ,rate)
	(match (peel-annotations rate)
	  [',rate
	   ;;(ASSERT integer? rate)
	   ;; For the basic backend, we simply call the downstream
	   ;; operator when we ourselves are asked for data (invoked).
	   (values nm
		   ((Emit down*) ''UNIT) ;; Code
		   (make-lines ()) ;; State 	
		   rate)
	   ])])]))

;; .returns top-level decls (lines)
(define (Operator op)
  (define (wrap-iterate-as-simple-fun name arg vqarg argty code)
    (make-lines 
     (list (block `("void ",(Var name) "(",(Type argty)" ",(Var arg)")")
		  (list
		   "char "(Var vqarg)";\n"
		   (lines-text code)))
	   "\n")))
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
	 (wrap-iterate-as-simple-fun name v vq vty ((Value emitter) bod nullk))
	 bind* init*)])]


    [(_merge (name ,name) (output-type (Stream ,elt))
	     (code ,__)
	     (incoming ,a ,b) (outgoing ,down* ...))
     (define emitter (Emit down*))
     (define arg (unique-name "arg"))
     (values (make-lines 
	      (list (block `("void ",(Var name) "(",(Type elt)" ",(Var arg)")")
			   (lines-text (emitter arg)))
		    "\n"))
	     '() '())]

    ;; This doesn't work for sigsegs, only arrays:
    [(__readFile (name ,name) (output-type (Stream ,elt))
		 (code (__readFile (annotations .  ,annot)
				   ',file          ,source
				   ',mode          ,[Simple -> repeats] 
				   ',skipbytes     ,[Simple -> offset] 
				   ',winsize      ',types_ignored))
		 (incoming ,in) (outgoing ,down* ...))
     (ASSERT symbol? source)  (ASSERT string? file)
     (let* (
	    ;; Do we output a struct of values or just a single value:
	    [structoutput? (match elt
			     [(Struct ,name) name] 
			     ;[(Sigseg (Struct ,name)) name] 
			     [(Array (Struct ,name)) name]
			     [,else #f])]
	    [tuptype (match elt
		       [(Struct ,structname) `("struct ",(sym2str structname))]
		       ;[(Sigseg ,[t]) t]
		       [(Array ,[t]) t]
		       [,other (ASSERT (not (eq? other 'String))) (Type other)])]
	    [types (if structoutput?
		       (map cadr (cdr (ASSERT (assq structoutput? global-struct-defs))))
		       (match elt
			 ;[(Sigseg ,t) (list t)]
			 [(Array ,t) (list t)]
			 [,oth        (list oth)]))]
	    ;[numstrings (length (filter (lambda (s) (eq? s 'String)) types))]

	    [binarymode (equal? mode "binary")]
	    [handle (Var (unique-name "_f"))]

	    [init (make-lines `(
		,handle" = fopen(\"",file"\", ",(if binarymode "\"rb\"" "\"r\"")");\n"
		"if (",handle" == NULL) {\n"
		"  printf(\"Unable to open data file %s: %m\", \"",file"\");\n"
		"  exit(1);\n"
		"}\n"
		"fseek(",handle", ",offset", SEEK_SET);\n"
		))]
	    [state (make-lines `("FILE* ",handle";\n"))]
	    [_buf (Var 'buf)]
	    [maintext ;; Code for the funcion that drives the file reading.
	     (list `(
		     ,(if (> winsize 0)
			  (lines-text ((Value (emit-err 'readFile)) `(assert-type ,elt (Array:makeUNSAFE ',winsize)) 
				       (varbindk 'buf elt)))
			  ;(Binding (list 'buf elt (make-zero-for-type elt)))
			  ;; A binding with no init:
			  (list (Type elt) " " _buf ";\n"))
		     
		     ;; TODO: HANDLE STRINGS:
; 		     ,(map (lambda (i) 
; 			     (list 
; 			      "// Cap of a 100 on length of strings read in:\n"
; 			      (format "char str~a[100];\n" i)))
; 			(cdr (iota (add1 numstrings))))

		     "int i, status = 0;\n"
		     ("// The binary format of tuples matches that in the file:\n"
		       ,(let ([readcmd 
			       (lambda (n dest)
				 `("status += fread((void*)",dest
				   ",sizeof(",tuptype"), ",(number->string n)", ",handle");\n"))])
			  (if (> winsize 0)
			      (if (> skipbytes 0)
				  ;; Have to interleave reading and skipping forward:
				  (block `("for (i=0; i<",(number->string winsize)"; i++)")
					 (list (readcmd 1 `("(",_buf"+i)"))
					       "fseek("handle", "(number->string skipbytes)", SEEK_CUR);\n"))
				  ;; Otherwise can do one read:
				  (readcmd winsize "buf"))
			      (readcmd 1 (** "&" _buf)))))
		     
		     ;; Now with that nasty scanf finished we still
		     ;; have to put the strings into the right fields:
; 		     ,(map (lambda (n fld ty)
; 			     (if (eq? ty 'String)
; 				 (format "tup.~a = str~a;\n" fld n)
; 				 '()))
; 			(cdr (iota (add1 (length types))))
; 			(list-head standard-struct-field-names (length types))
; 			types)
		     
		     ,(block `("if (status != ",(number->string (max 1 winsize))
			       " * ",(if binarymode "1" (number->string (length types)))")")
			     '("printf(\"dataFile EOF encountered (%d).\", status);\n"
			       "exit(0);\n"))
		     ,(lines-text ((Emit down*) 'buf))))])

       (values 
	(make-lines (block (list "void "(sym2str name)"("(Type '#())" ignored)") maintext))
         ;(wrap-iterate-as-simple-fun name 'ignored 'ignoredVQ (Type '#()) maintext)
	(list state) (list init)))] ;; End readFile
    ))

;================================================================================

(define-pass emit-c2
  [Program 
   (lambda (prog Expr)
     (fluid-let ([free-fun-table ()]
		 [global-struct-defs (cdr (project-metadata 'struct-defs prog))])
       (let ([freefundefs (build-free-fun-table! (cdr (ASSERT (project-metadata 'heap-types prog))))])
     ;(assq 'c-includes meta*)
     (match prog
       [(,lang '(graph (const ,[(SplitBinding (emit-err 'TopBinding)) -> cb* cbinit*] ...) ;; These had better be *constants* for GCC
		       (init  ,[(Effect (lambda _ (error 'top-level-init "code should not 'emit'"))) 
				-> init*] ...)
		       (sources ,[Source -> srcname* srccode*  state1* srcrate*] ...)
		       (operators ,[Operator -> oper* state2** init**] ...)
		       (sink ,base ,basetype)
		       ,meta* ...))
	(define includes (text->string 
			  (list "//WSLIBDEPS: "
				(map (lambda (fn) 
				       (let ([lib (extract-lib-name fn)])
					 (if lib (list " -l" lib) fn)))
				  link-files)
				"\n"
				
				"#include \""(** (REGIMENTD) "/src/linked_lib/wsc2.h")"\"\n"
				
				;; After the types are declared we can bring in the user includes:
				"\n\n" (map (lambda (fn) `("#include ",fn "\n"))
					 (reverse include-files)) "\n\n")))
	  (define allstate (text->string (map lines-text (apply append cb* state1* state2**))))
	  (define ops      (text->string (map lines-text (reverse oper*))))
	  ;(define srcfuns  (text->string (map lines-text (map wrap-source-as-plain-thunk srcname* srccode*))))
	  (define init     (begin (ASSERT (andmap lines? (apply append init**)))
			     (text->string (block "void initState()" 
				 (list (lines-text (apply append-lines (apply append init**)))
				       (lines-text (apply append-lines cbinit*)))))))
	  (define driver   (text->string (lines-text (build-main-source-driver srcname* srccode* srcrate*))))
	  ;;(define toplevelsink "void BASE(int x) { }\n")	  
	  (define total (apply string-append 
			(insert-between "\n"
			  (list includes 
				(text->string (map StructDef global-struct-defs))
				(text->string (lines-text freefundefs))
				allstate
				;toplevelsink
				ops ;srcfuns 
				init driver))))

	  ;;(printf "====================\n\n")	
	  ;;(display total)
	  total]

       [,other ;; Otherwise it's an invalid program.
	(error 'emit-c2 "ERROR: bad top-level WS program: ~s" other)]))
	 ))]
  ) ;; End pass




) ;; End module

