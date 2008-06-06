#!r6rs

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

;; Note on efficiency: 
;;   On demo3m for example, 
;;     Non-OOP:  chez: 266(15)   plt: 564(68)                                   47.4 mb alloced
;;     BOS version: chez: 293(15ms)   plt: 660(92)  (chastity chez from .boot)  52.7 mb alloced
;;     BOS version: chez: 385(105, 13 collections)   plt: 660(96)  (chastity chez from src)
;;     BOS version: 315(21) chez 950(180) plt (laptop core2duo)
;; Wow - are collections worse when running from source? (because more code is loaded?)
;; It's also probably fair to say that the extra allocation from BOS
;; makes the probability of length collections higher.

(library (ws passes wavescope_bkend emit-c2)
  (export emit-c2
	   ;emit-c2-generate-timed-code
	   <emitC2>
	   <emitC2-timed>
	   
	   ;; Have to export all the generic methods so that they can be subclassed:
	   build-free-fun-table!  heap-type?  add-include!
	   add-link!  gen-incr-code gen-decr-code gen-free-code
	   incr-local-refcount decr-local-refcount incr-heap-refcount
	   decr-heap-refcount potential-collect-point say-goodbye type->name
	   Const Var Simple TyAndSimple Type Let Value Effect StructDef Source
	   GenWorkFunction Operator SortOperators Cutpoint Emit type->printf-flag
	   Binding SplitBinding DummyInit StaticAllocate PrimApp Run
	   BuildTimerSourceDriver BuildOutputFiles varbindk
	   array-constructor-codegen
	   IterStartHook IterEndHook ForeignSourceHook

	   make-lines lines-text append-lines slot-cons! 

	   )
  (import (rnrs) (except (rnrs r5rs) force delay)
	  (rnrs mutable-strings)
	  (ws compat compat)
	  (ws common)
	  (ws passes wavescope_bkend insert-refcounts)
	  (ws passes wavescope_bkend emit-c)
	  (ws compiler_components c_generator)
	  (ws util bos_oop) 
	  ;(ws util rn-match) ;; TEMPTOGGLE
	  )

  ;; An object of this class is a compiler pass.  It is not reentrant,
  ;; because it maintains state for the program it is compiling.
  (define-class <emitC2> (<class>) 
    (theprog ;; <- holds on to the program we're compiling
     struct-defs union-types ;; <- cache these two pieces of metadata from theprog
     free-fun-table ;; <- This contains a table mapping
     ;;   types to a scheme function that generates free-code (when given
     ;;   text representing the pointer to free).
     include-files link-files ;; <- accumulate a list of files to include/link
     compile-flags ;; Accumulate a list of flags to send to gcc, datatype "text"
     server-cutpoints ;; <- names of cutpoints
     ))

  ;; We define a single top-level instance of our pass-class:
  ;(define-object obj <emitC2>)

  ;; These are the methods:
  (define-generic build-free-fun-table!)
  (define-generic heap-type?)
  (define-generic add-include!)
  (define-generic add-link!)

  (define-generic gen-incr-code)
  (define-generic gen-decr-code)
  (define-generic gen-free-code)
  (define-generic incr-local-refcount)
  (define-generic decr-local-refcount)
  (define-generic incr-heap-refcount)
  (define-generic decr-heap-refcount)


  (define-generic potential-collect-point)
  (define-generic say-goodbye)
  ;(define-generic make-app)
  (define-generic type->name)
  ;(define-generic wrap-source-as-plain-thunk)
;  (define-generic wrap-iterate-as-simple-fun)
  (define-generic Const)
  (define-generic Var)
  (define-generic Simple)
  (define-generic TyAndSimple)
  (define-generic Type)
  (define-generic Let)
  (define-generic Value)
  (define-generic Effect)
  (define-generic StructDef)
  (define-generic Source)
  (define-generic GenWorkFunction)
  (define-generic Operator)
  (define-generic SortOperators)
  (define-generic Cutpoint)
  (define-generic Emit)
  (define-generic type->printf-flag)
  (define-generic Binding)
  (define-generic SplitBinding)
  (define-generic DummyInit)
  (define-generic StaticAllocate)
  (define-generic PrimApp)
;  (define-generic Program)
  (define-generic Run)
  (define-generic BuildTimerSourceDriver)
  (define-generic BuildOutputFiles)
  (define-generic varbindk)
  (define-generic array-constructor-codegen)

  (define-generic IterStartHook)
  (define-generic IterEndHook)
  (define-generic ForeignSourceHook)

  (__spec add-include! <emitC2> (self fn)
    (define files (slot-ref self 'include-files))
    (unless (member fn files)
      (slot-set! self 'include-files (cons fn files))))
  (__spec add-link! <emitC2> (self fn)
    (define files (slot-ref self 'link-files))
    (unless (member fn files)
      (slot-set! self 'link-files (cons fn files))))
  
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
       [() '()]       
       [(#\nul     . ,[tl]) (cons* #\\ #\0 #\0 #\0 tl)]
       [(#\newline . ,[tl]) (cons* #\\ #\n tl)]
       [(#\tab     . ,[tl]) (cons* #\\ #\t tl)]
       [(#\" . ,[tl])       (cons* #\\ #\" tl)]
       [(#\\ . ,[tl])       (cons* #\\ #\\ tl)]
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
;;; Low level routines for generating syntax.

;(define (mangle-name n) (symbol->string n))
(define (sym2str x) ;(ASSERT x)
  (symbol->string x))

(define (slot-cons! obj fld x) (slot-set! obj fld (cons x (slot-ref obj fld))))

(define (cap x) (list x ";\n"))

(define (make-app rator rands)
  (list rator "("(insert-between ", " rands)")"))

(define (emit-err where)
  (lambda (_) (error where "should not run into an emit!")))

(__spec type->name <emitC2> (self ty)
  (match ty
    ;[#(,[flds] ...) (apply string-append "TupleOf_" flds)]
    [(Struct ,tuptyp) 
     (string-append
      (apply string-append "TupleOf_" 
			     (insert-between "_"
			      (map (curry type->name self) (map cadr 
			       (cdr (ASSERT (assq tuptyp (slot-ref self 'struct-defs))))))))
      "__")]
    [,scalt (guard (scalar-type? scalt)) (sym2str scalt)]
    [(Ref ,[ty]) ty] ;; Doesn't affect the name currently...
    [(Array ,[ty]) (string-append "Array_" ty)]
    [(List  ,[ty]) (string-append "List_"  ty)]
    [#() "Unit"]
    ))

(__spec ForeignSourceHook <emitC2> (self name callcode)
	callcode)

;; This is the simplest way to package a source.
(define (wrap-source-as-plain-thunk name code)
  (make-lines (block `("void ",(Var name) "()") (lines-text code))))

;================================================================================
;;; "Continuations" used for syntax production.
;;; These are actually simple objects.

(define split-msg (gensym "split"))

(define (idk x)   (if (eq? x split-msg) (values (make-lines "") idk) (make-lines x)))
(define (nullk x) (if (eq? x split-msg) (values (make-lines "") idk) (make-lines "")))
(__spec varbindk <emitC2> (self name typ)
  (define (split-k x)
    (if (eq? x 'split-msg) 
	;; Further splits have no effect:
	(values (make-lines "") split-k)
	;; After a split we mutate the variable rater than binding it.
	(make-lines `(,(Var self name)" = ",x";\n"))))
  (lambda (x)      
    (if (eq? x split-msg)
	;; At the split point we bind the variable.
	;; And we return a new continuation.
	(values (make-lines `(,(Type self typ)" ",(Var self name)";\n"))
		split-k)
	;; Here we bind & set the variable.
	(make-lines `(,(Type self typ)" ",(Var self name)" = ",x";\n"))
	)))

;; This is simpler, we just set it without binding it:
;(trace-define (setterk self vr ty) (let-values ([(_ newk) ((varbindk self vr ty) split-msg)]) newk))
(define (setterk self vr ty)
  (define thekont
    (lambda (x)      
      (if (eq? x split-msg)
	  (values (make-lines "")	thekont)
	  (append-lines (make-lines `(" ",(Var self vr)" = ",x";\n"))))))
  thekont)

;================================================================================

  
(__spec heap-type? <emitC2> (self ty) 
        (heap-allocated? ty (slot-ref self 'struct-defs) (slot-ref self 'union-types)))

;; This builds a set of top level function definitions that free all
;; the heap-allocated types present in the system.  It also builds a
;; table mapping types onto syntax-generating functions that call the
;; appropriate free-ing code.
;; 
;; Currently this routine mutates the table "free-fun-table" on the fly.
;;
;; This is this function that decides what freeing will be "open coded"
;; vs. relegated to separately defined functions.
(__spec build-free-fun-table! <emitC2> (self heap-types)
  (define fun-def-acc '()) ;; mutated below  
  (define proto-acc '()) ;; mutated below

  ;; This adds to a table of names for free-functions:
  (define (add-to-freefuns! ty fun)
    ;; Inefficient: should use hash table:
    (unless (assoc ty (slot-ref self 'free-fun-table))
      (slot-set! self 'free-fun-table 
                 (cons (cons ty (debug-return-contract lines? fun))
                       (slot-ref self 'free-fun-table)))))
  
  ;; First we add the freefuns name entries, because we might need
  ;; those to generate the defs.
  (for-each
      (lambda (ty) 
	(define name (type->name self ty))
	(define default-specialized_fun
	  (lambda (ptr) (make-lines `("free_",name"(",ptr");\n"))))
	(let loop ([ty ty])
	   (match ty ;; <- No match recursion!
	     ;; Tuples are not heap allocated for now (at least the bigger ones should be):
	     [,tup (guard (vector? tup)) #f] ;; No entry in the table.
	     [,scl (guard (not (heap-type? self scl))) #f]
	     [(Struct ,tuptyp) (add-to-freefuns! ty default-specialized_fun)]
	     [(Ref ,elt) (loop elt)]
	     [(List ,elt) (add-to-freefuns! ty default-specialized_fun)]
	     [(Array ,elt)
	      (if (not (heap-type? self elt))
		  (add-to-freefuns! ty (lambda (ptr) (make-lines `("FREEARR(",ptr");\n"))))
		  (add-to-freefuns! ty default-specialized_fun))])))
    heap-types)

  ;; Now, for every name we've added to the table, we need to generate a definition:  
  (let ([final-defs
	 (apply append-lines 
		(map (lambda (entry) 
		       (define ty (car entry))
		       (define name (type->name self ty))
		       (define default-fun_name `("void free_",name"(",(Type self ty)" ptr)"))
		       (define default-specialized_fun
			 (lambda (ptr) (make-lines `("free_",name"(",ptr");\n"))))
		       (let loop ([ty ty])
			 (match ty ;; <- No match recursion!
			   [(Struct ,tuptyp)
			    (set! proto-acc (cons default-fun_name proto-acc))
			    (let ([flds (cdr (ASSERT (assq tuptyp (slot-ref self 'struct-defs))))])
			      (make-lines
			       (list
				"/* Freeing struct: "(sym2str tuptyp)"*/\n"
				(block default-fun_name
				       (map (match-lambda ((,fldname ,ty))
					      (lines-text (gen-decr-code self ty (list "ptr."(sym2str fldname)))))
					 flds)))))]
			   [(List ,elt)
			    (set! proto-acc (cons default-fun_name proto-acc))
			    ;; We always build an explicit function for freeing list types:
			    ;; Here's a hack to enable us to recursively free the same type.
			    (make-lines 
			     (block default-fun_name
				    (block (list "if(ptr)") ;; If not null.
					   `(
					     ;; Recursively decr tail:
					     ,(Type self `(List ,elt))" ptr2 = CDR(ptr);\n"
					     ,(lines-text (gen-decr-code self `(List ,elt) "ptr2"))
					     ;; If heap allocated, free the CAR:
					     ,(if (heap-type? self elt)
						  (lines-text (gen-decr-code self elt `("(*ptr)")))
						  "")
					     "FREECONS(ptr);\n"))))]
			   [(Array ,elt)
			    (if (not (heap-type? self elt))
				(make-lines "")
				(let ([ind (Var self (unique-name "i"))])
				  (set! proto-acc (cons default-fun_name proto-acc))
				  (make-lines 
				   (block default-fun_name
					  `("int ",ind";\n"
					    ,(block `("for (",ind" = 0; ",ind" < ARRLEN(ptr); ",ind"++)")
						    ;;(lines-text ((cdr (loop elt)) `("ptr[",ind"]")))
						    (begin ;(loop elt) ;; For side effect
						      (lines-text (gen-decr-code self elt `("ptr[",ind"]")))))
					    "FREEARR(ptr);\n")))
				  ))])))     
		  (slot-ref self 'free-fun-table)))])
    (append-lines 
     (make-lines (map (lambda (x) (list x ";\n")) proto-acc))
     final-defs)))




;================================================================================
;;; Hooks for garbage collection.

;; These underlying methods do the actual code generation:
;; For the naive strategy we'de allow shared pointers but would need a CAS here.
(__spec gen-incr-code <emitC2> (self ty ptr msg)
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
		   (gen-incr-code self ty (list ptr "." (sym2str fldname)) msg))
	      (cdr (assq name (slot-ref self 'struct-defs)))))]
    ;; Other types are not heap allocated:
    [,ty (guard (not (heap-type? self ty)))(make-lines "")]
    ))

;; "ptr" should be text representing a C lvalue
;; returns "lines"
(__spec gen-decr-code <emitC2> (self ty ptr)
  (match ty
    [(,Container ,elt) (guard (memq Container '(Array List)))
     (make-lines 
      (block `("if (DECR_RC_PRED(",ptr")) /* type: ",(format "~a" ty)" */ ")
	     (lines-text (gen-free-code self ty ptr))))]
    [(Ref ,[ty]) ty]
    ;; Could make this a separate function:
    [(Struct ,name) 
     (apply append-lines
	    (make-lines (format "/* Decr tuple refcount, Struct ~a */\n" name))
	    (map (match-lambda ((,fldname ,ty))
		   (gen-decr-code self ty (list ptr "." (sym2str fldname))))
	      (cdr (assq name (slot-ref self 'struct-defs)))))]
    [,ty (guard (not (heap-type? self ty))) (make-lines "")]))

;; This generates free code for a type (using free-fun-table).
;; Should only be called for types that are actually heap allocated.
(define _ig 
  (specialise! gen-free-code <emitC2>
     (debug-return-contract lines?
       (lambda (next self ty ptr)
	 (next)
	 (let* ([strip-refs (lambda(ty)
			      (match ty [(Ref ,ty) ty] [,ty ty]))]
		[newty (strip-refs ty)])
	   ((cdr (ASSERT (assoc newty (slot-ref self 'free-fun-table))))
	    ptr))))))


;; These methods represent the actions to take when encountering local or heap refs.
;; The default version represents plain old reference counting.
(__spec incr-local-refcount <emitC2> (self ty ptr) (gen-incr-code self ty ptr "local"))
(__spec decr-local-refcount <emitC2> (self ty ptr) (gen-decr-code self ty ptr))

(__spec incr-heap-refcount <emitC2> (self ty ptr) (gen-incr-code self ty ptr "heap"))
(__spec decr-heap-refcount <emitC2> (self ty ptr) (gen-decr-code self ty ptr))

;; TODO -- not used yet
(__spec potential-collect-point <emitC2> (self) (make-lines ""))


;; TODO -- not used yet
;; Do anything special that's required as a value is sent off through an emit.
;; Currently, with a pure depth-first strategy, reference count does not need to be affected by "emit".
(__spec say-goodbye <emitC2> (self ty ptr) (make-lines ""))


;================================================================================

;; [2007.12.04] TEMP!!!!! SHOULD NOT DUPLICATE CODE
(__spec Const <emitC2> (self datum wrap)
    ;; Should also make sure it's 32 bit or whatnot:
    (cond

     ;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
     ;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
     ;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
     ;; [2008.04.13] TEMP HACK: PUTTING BACK IN TEMPORARILY
     [(eq? datum 'BOTTOM) (wrap "0")] ;; Should probably generate an error.
     ;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
     ;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
     ;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
     ;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME

     [(eq? datum 'UNIT) (wrap (Simple self '(tuple)))]
     [(eq? datum #t) (wrap "TRUE")]
     [(eq? datum #f) (wrap "FALSE")]
     [(string? datum) (wrap (format "~s" datum))]
     ;; FIXME THIS WON'T HANDLE NON-PRINTING CHARACTERS YET!!
     ;[(char? datum) (wrap (format "'~a'" datum))]
     [(char? datum) (wrap (format "~a" (char->integer datum)))]


     ;; Hacked this to handle NAN (not in a pretty way).
     [(flonum? datum) 
            ;(printf "GOT FLOAT: ~a ~a \n" datum (or (eq? datum +nan.0) (eq? datum -nan.0)))
      (wrap (format "(~a)~a" (Type self 'Float)
		    (if (not (= datum datum)) ;(or (eq? datum +nan.0) (eq? datum -nan.0))
			"(0.0/0.0)" ;(inspect/continue datum);"(0.0/0.0)"
			datum)					 
		    ))]
     [(cflonum? datum) (wrap (format "(~a)(~a + ~afi)" (Type self 'Complex)
				     (real-part datum)
				     (imag-part datum)))]
     [(eq? datum 'nulltimebase)  (wrap "WSNULLTIMEBASE")]
     [(integer? datum) 
      ;(wrap (format "(~a)~a" (Type self 'Int) datum))
      (wrap (number->string datum))]

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
	       "("(Type self contenttype)")"(make-zero-for-type contenttype)
	       ");\n" 
	       ,(mapi (lambda (i x) (Const `("(",name"->data)[",(number->string i)"]")
					   "" x))
		      (vector->list datum))))]
     [else (error 'emitC2:Const "not a C-compatible literal: ~s" datum)]))



(__spec Type <emitC2> (self ty)
  (match ty ;; No recursion!
    [Bool    "char"]
    [Int     "int"]
    [Int16   "int16_t"]
    [Int32   "int32_t"]
    [Int64   "int64_t"]
    [Uint16  "uint16_t"]
    [Double  "double"]
    [Float   "float"]
    [Complex "float complex"]
    ;[Complex "__complex float"]
    [Char    "char"]
    ;[String  "const char*"] 
    [String  "char*"] 
    [#()      "char"]
    [(Struct ,name) (list "struct "(sym2str name))] ;; Value type.

    ;; An array looks like a C array, except the -1 word offset is a refcount and -2 is length.
    [(Array ,elt) (list (Type self elt) "*")] 

    ;; A cons cell is just looks like a pointer, except the -1 offset is a refcount, and -2 is the CDR.
    [(List ,elt) (list (Type self elt) "*")]

    [(Ref ,ty) (Type self ty)]

    ;; Values of this type aren't really used.
    [(,_ ... -> ,__) "char"]

    ;; This is an unused value.
    [(VQueue ,_) "char"]

    ))


;; Generate a struct definition.
(__spec StructDef <emitC2> (self entry)
     (match entry
       [(,(sym2str -> name) (,[sym2str -> fld*] ,typ*) ...)
	(let ([tmpargs (map (lambda (_) (sym2str (unique-name 'tmp))) fld*)]
	      [ctype* (map (curry Type self) typ*)])
	  `(,(block `("struct ",name)
		    (map (lambda (ctype fld) `[,ctype " " ,fld ";\n"])
		      ctype* fld*))
	    ";\n"
            ))]))

;; For the set of types that are printable at this point, we can use a simple printf flag.
(__spec type->printf-flag <emitC2> (self ty)
  (match ty
    [#()    "()"]
    [String "%s"]
    [(Array Char) "%s"] ;; HACK: These are strings in disguise.
    [Bool   "%d"]
    [Int    "%d"]
    [Int16  "%hd"]
    [Int32  "%ld"]
    [Int64  "%lld"]
    [Uint16  "%hu"]
    [Float  "%g"]
    [Double "%lf"]
    [#() "()"]
    [(Pointer ,_) "%p"]
    [(,args ... -> ,ret)     
     (error 'type->printf-flag 
	    "Cannot print a function type! ~s" ty)]))


;(define Var mangle-name)
(__spec Var <emitC2> (self x) (sym2str x))

;; These are the expressions that make valid operands (post flattening)
;;   .returns A string.  Could return an "expression".
(__spec Simple <emitC2> (self expr)
  (match expr
    [,v (guard (symbol? v)) (ASSERT (not (regiment-primitive? v))) (Var self v)]
    ;[',c (format "~a" c)] ;;TEMPTEMP

    ;; PolyConstants:
    [(assert-type ,ty Array:null) `("((",(Type self ty)")0) /* Array:null */")] ;; A null pointer. This choice is debatable.
    [(assert-type ,ty '())        `("((",(Type self ty)")0)")] ;; A null pointer.       
    ['() (error 'EmitC2:Simple "null list without type annotation")]

    ;; All other constants:
    [',c (Const self c (lambda (x) x))]

    [(tuple) (list "(("(Type self '#())")0)")]
    [(deref ,var) (ASSERT (not (regiment-primitive? var))) (Var self var)]
    ;[(assert-type ,t '())  (wrap (PolyConst '() t))]
    ;['() (error 'Simple "null list without type annotation")]
    ;[nulltimebase (Const #f #f 'nulltimebase)]    
       
    [(assert-type ,_ ,[x]) x]
    [,else (error 'Simple "<emitC2> not simple expression: ~s" else)]
    ))

(__spec TyAndSimple <emitC2> (self)
  (lambda (expr)
    (match expr
      [(assert-type ,ty ,_)  (values ty (Simple self expr))])))



;; Generates code for an emit.  (curried)
;; .param down*   A list of sinks (names of functions) to emit to.
;;                These can also be (NUM NAME) pairs for indexed ports.
;; .returns lines representing command-code
(__spec Emit <emitC2> (self down*)
  ;;(ASSERT (not (null? down*)))
  (lambda (expr)
    (ASSERT simple-expr? expr)
    (let ([element (Simple self expr)])
      (make-lines (map (lambda (down)
			 ;(if (list? down) (set! down (ASSERT symbol? (cadr down)))) ;; HACK
			 (cap 
			  (list (make-app (Var self down) (list element))
				" /* emit */")))
		    down*)))
    ;(make-lines `("emit ",(Simple expr)";"))
    ))

;; This is used for local bindings.  But it does not increment the reference count itself.
(__spec Binding <emitC2> (self emitter)
  (debug-return-contract Binding lines?
   (lambda (cb)
    ;(define val (Value (lambda (_) (error 'Binding "should not run into an emit!"))))
    (match cb
      [(,vr ,ty ,rhs) ;(,[Var -> v] ,[Type -> t] ,rhs) 
       ((Value self emitter) rhs (varbindk self vr ty))]

      ;; HACK FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME 
      ;; [2008.04.13] Trying to catch static-allocate even elsewhere in the code:
#;
      [(,vr ,ty ,static) (guard (match? (peel-annotations static) (static-allocate ,rhs)))
       (define-values (binds init) (StaticAllocate self vr ty static))
       ;; De-split the binding right here:
       (inspect/continue
	(append-lines binds init))]

      [,oth (error 'Binding "Bad Binding, got ~s" oth)]))))

;; This is there for the benefit of the java backend.  It needs to
;; have an initialized value of some kind, even if its null.
(__spec DummyInit <emitC2> (self ty) "")

;; This is used for global and static bindings.
;; This separately returns the type decl and the initialization code.
;; This version also DOES inject a refcount incr.
(__spec SplitBinding <emitC2> (self emitter)
  (lambda (cb)
    ;(define val (Value (lambda (_) (error 'Binding "should not run into an emit!"))))
    (match cb      
      [(,vr ,ty ,rhs)
       (let loop ([rhs rhs])
	 (match rhs
	   ;; Currently [2008.02.14] we don't allow the static-allocate
	   ;; annotation to float around in arbitrary code.  We catch it right here.
	   [(static-allocate ,rhs) (StaticAllocate self vr ty rhs)]

	   ;; Here we flatten out lets:
	   ;; [2008.04.13] This runs into a problem right now because of struct constants;
#;
	   [(let (,bind) ,[bodbnds bodinit])	  
	    ;;(define-values (bnds init) (SplitBinding self (list vr )))
	    (match bind
	      ;; HACK FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME 
	      ;; HACK FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME 
	      ;; HACK FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME 
	      [(,lhs ,ty (make-struct . ,_)) 
	       (Binding bind)]
	      #;
	      [,else
	       (define-values (bnds init) ((SplitBinding self emitter) bind))
	       (values (append-lines bnds bodbnds)
		       (append-lines init bodinit))
	       ]
	      [,else 
	       (values (append-lines ((Binding self emitter) bind) bodbnds)
		       bodinit)
	       ]
	      )]

	   	   ;; HACK FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME 
	   ;; HACK FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME 
	   [(make-struct . ,rest)
	    (inspect (cons 'make-struct rest))
	    ;; Here we just do the binding immediately, and return nothing for the initialization:
	    (values ((Binding self emitter) (list vr ty rhs))
		    (make-lines "")
		    )]
	   [(assert-type ,ty (make-struct . ,rest))
	    (inspect (cons 'ASSERTmake-struct rest))
	    ;; Here we just do the binding immediately, and return nothing for the initialization:
	    (values ((Binding self emitter) (list vr ty rhs))
		    (make-lines "")
		    )]

	   ;; Flip
	   [(assert-type ,ty (static-allocate ,rhs)) 
	    (loop `(static-allocate (assert-type ,ty ,rhs)))]
#;
	   [(assert-type ,ty ,[b i]) (values b i)]
	   

	   [,rhs
	    ;; We derive a setter continuation by "splitting" the varbind continuation:       
	    (values (make-lines `(,(Type self ty)" ",(Var self vr)" ",(DummyInit self ty)";\n"))
					;((Value self emitter) rhs set-and-incr-k)
		    ((Value self emitter) rhs (setterk self vr ty)))]))]
      [,oth (error 'SplitBinding "Bad Binding, got ~s" oth)]
      )))


;; For the x86 version we currently don't statically allocate anything.
(__spec StaticAllocate <emitC2> (self lhs ty rhs)
  (define-values (decl initcode)
    ((SplitBinding self (emit-err 'splitbind)) (list lhs ty rhs)))
  ;; We do need to add a refcount increment.  Something statically
  ;; allocated will never reach zero.  
  (values decl
	  (append-lines initcode
	     (gen-incr-code self ty (Var self lhs) "static top-incr"))))

#;
(__spec Let <emitC2> (self form emitter recur)
  (match form     
    [(([,lhs ,ty ,rhs]) ,[recur -> bod])
     (append-lines ((Binding self emitter) (list lhs ty rhs))
		   (ASSERT lines? bod))]))


(__spec Let <emitC2> (self form emitter recur)
  (match form         
    [(([,lhs ,ty ,rhs]) ,_bod)

     ;; ANOTHER HACK: HANDLE make-struct DIFFERENTLY IN THE BODY:
     (define bod 
       (match (peel-annotations _bod)
	 [(make-struct ,sname ,args ...)
	  (define tmp (unique-name 'hackage))
	  ;; Here we introduce another temporary at the last second.
	  ;; To get around C's limited {a,b,c} syntax for structs...
	  (make-lines 
	   (block "" 
		 (list (lines-text ((Binding self emitter) (list tmp `(Struct ,sname) _bod)))
		       (lines-text (recur tmp)))))
	  ]
	 [,oth (recur oth)]))

     ;; A hack on a hack, make-struct must be handled differently:

;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME      
;; [2008.04.13] MIGRATING THIS HACK TO SPLITBINDING
     #;	
     (let-values ([(bind* init*) ((SplitBinding self emitter)
				     (list lhs ty rhs))])
	  (let ([result (append-lines bind* init* bod)])
	    (make-lines (block "" (lines-text result)))))
;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME      

     (match (peel-annotations rhs)
       [(make-struct . ,_)
	(let ([result (append-lines ((Binding self emitter) (list lhs ty rhs))
				    bod)])
	  (make-lines (block "" (lines-text result))))]
       [,_ 
	(let-values ([(bind* init*) ((SplitBinding self emitter)
				     (list lhs ty rhs))])
	  (let ([result (append-lines bind* init* bod)])
	    (make-lines (block "" (lines-text result)))))])]))

(__spec Effect <emitC2> (self emitter)
  (debug-return-contract Effect lines?
  (lambda (xp)
    (define (Loop x) ((Effect self emitter) x)) ;; Eta reducing this BREAKS things.
    (define (Simp x) (Simple self x))
    (define (Vr x)   (Var self x))
    (match xp ;; no recursion
      ['UNIT   (make-lines "")]
      [(tuple) (make-lines "")] ;; This should be an error.
      ;; Thanks to insert-refcounts this can get in here:
      [,v (guard (symbol? v)) (make-lines "")]

      [(incr-heap-refcount  ,ty ,[Simp -> val]) (incr-heap-refcount  self ty val)]
      [(incr-local-refcount ,ty ,[Simp -> val]) (incr-local-refcount self ty val)]
      [(decr-heap-refcount  ,ty ,[Simp -> val]) (decr-heap-refcount  self ty val)]
      [(decr-local-refcount ,ty ,[Simp -> val]) (decr-local-refcount self ty val)]

      [(for (,[Vr -> ind] ,[Simp -> st] ,[Simp -> en]) ,[Loop -> bod])
       (make-lines
	(block ""
	       (list `(" ",(Type self 'Int)" ",ind";\n")
		     (block `("for (",ind" = ",st"; ",ind" <= ",en"; ",ind"++)")
			    (lines-text bod)))))]

      [(while ,test ,[Loop -> bod])
       (let ([flag (unique-name "grosshack")])
	 (make-lines 
	  (block `("while (",(Const self #t id)")")
		 (list 
		  ;(lines-text ((Value self emitter) test (varbindk self flag 'Bool)))
		  (let-values ([(bind* init*) ((SplitBinding self emitter)
					       (list flag 'Bool test))])
		    (list (lines-text bind*)
			  (lines-text init*)))
		  "if ("(Var self flag)") {\n"
		  (indent (lines-text bod) "  ")
		  "} else break; \n"
		  ))))]

      ;; [2007.12.04] Would probably improve efficiency to at least handle scalars as well here:
      ;; Otherwise we needlessly allocate string objects.
      [(print ,[(TyAndSimple self) -> ty x])
       ;(ASSERT (eq? t 'String))       
       (make-lines `("printf(\"",(type->printf-flag self ty)"\", ",x");\n"))]
      
      [(set! ,[Vr -> v] ,[(TyAndSimple self) -> ty x])
       (append-lines 	
	;; Set! changes Ref objects, which are on the heap:
	#|(decr-heap-refcount ty v)  ;; Out with the old.|#
	(make-lines `(,v" = ",x";\n"))
	#|(incr-heap-refcount ty v)  ;; In with the new.|#
	)]

      [(Array:set ,[(TyAndSimple self) -> ty arr] ,[Simp -> ind] ,[Simp -> val])
       (let-match ([(Array ,elt) ty])
	 (append-lines 	
	  ;;(make-lines "/* I AM ARRAY:SET DECR/INCR */\n")
	  #|(decr-heap-refcount elt `(,arr"[",ind"]"))  ;; Out with the old.|#
	  (make-lines `(,arr"[",ind"] = ",val";\n"))
	  #|(incr-heap-refcount elt `(,arr"[",ind"]"))  ;; In with the new.|#
	  ))]

      [(emit ,vq ,x) (emitter x)]
      [(begin ,[Loop -> e*] ...) 
       (DEBUGASSERT (andmap lines? e*))
       (apply append-lines e*)]

      ;; DUPLICATED CASES WITH Value:
      ;; ========================================
      [(let . ,_) (Let self _ emitter (lambda (x) ((Effect self emitter) x)))]
      ;; ========================================

      [(__wserror_ARRAY ,[Simp -> str]) 
       (make-lines (list "wserror("str")\n"))]

      ))))

;; The continuation k is invoked on a piece of text representing the return expression.
;; k is expected to return text of the form "lines" that stores away this result.
(__spec Value <emitC2> (self emitter)
  (debug-return-contract Value lines?
   (lambda (xp kont)
     (define (recur x) ((Value self emitter) x kont))
     ;; This is a debug wrapper that respects the funky interface to the continuation:
     ;(DEBUGMODE (set! k (debug-return-contract ValueK lines? k)))
     (define (Simp x) (Simple self x))
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
	       [newk (varbindk self tmp '(Array Char))]
	       [str (list->string ls2)]
	       [copylen (add1 (string-length str)) ;; Copy the null terminator
		      ;(vector-length vec)
			]
	       [_tmp (Var self tmp)])
	  (append-lines 
	   ((Value self emitter) `(assert-type (Array Char) (Array:makeUNSAFE ',(vector-length vec))) newk)
	   (make-lines
	    (format "memcpy(~a, ~s, ~s);\n" (text->string _tmp)
		    str copylen))
	   (kont _tmp)))]
       
       ;; This means we're at a end of a control path we will never reach.
       ;; HACK: There's surely a nicer way to do this....
       ;; [2008.04.05] But currently I just throw out the continuation.
       ['BOTTOM (make-lines "/* BOTTOM CTRL PATH */\n")]

       [,simp (guard (simple-expr? simp)) (kont (Simple self simp))]
       
       ;; This doesn't change the runtime rep at all.
       [(Mutable:ref ,x) (kont (Simple self x))]
       [(begin ,e) (recur e)]
       [(begin ,e1 ,e* ...)
	(define first ((Effect self emitter) e1))
	(define rest 
	  (begin 
	    (recur `(begin ,@e*))))
	;(inspect (cons 'rest rest))
	(ASSERT lines? first)
	(ASSERT lines? rest)
	(append-lines first rest)]
       ;; This splits the continuation, using it twice.
       [(if ,[Simp -> test] ,conseq ,altern)
	(let-values ([(lines newk) (kont split-msg)])
	  (make-lines
	     `(,(lines-text lines)
	       "if (" ,test ") {\n"
	       ,(indent (lines-text ((Value self emitter) conseq newk)) "  ")
	       "} else {\n"
	       ,(indent (lines-text ((Value self emitter) altern newk)) "  ")
	       "}\n")))]

       [(let . ,_) (Let self _ emitter recur)]

       ;; Should only occur as the rvalue in a binding, which is ok.
       ;; But this is not a general expression in C:
       [(make-struct ,name ,[Simp -> arg*] ...)
	(kont `("{",(insert-between ", " arg*)"}"))]
       [(struct-ref ,type ,fld ,[Simp -> x])
	(kont `("(",x "." ,(sym2str fld)")"))]
       
       [(__foreign ,_ ...) (kont "0")] ;; Does nothing atm.
       
       ;; [2008.02.26] Had a problem with substituting this pattern for 'ty': (,argty* ... -> ,retty)
       [(foreign-app ',name (assert-type ,ty ,ignored) ,[Simp -> rand*] ...)
	(match ty 
	  [(,argty* ... -> ,retty)
	   ;; Here's a hack for when the return type is unit/void.  This will let us slip "call" forms through.
	   (if (equal? retty '#())
	       ;; The problem is that you can't do anything with a "void" value in C:
	       (append-lines (make-lines `(,name"(",(insert-between ", " rand*)");\n"))
			     (kont (Const self 'UNIT (lambda (x) x))))	    
	       (kont `(,name"(",(insert-between ", " rand*)")")))])]
       [(foreign-app . ,_) (error 'emitC2 "foreign-app without type annotation: ~s" (cons 'foreign-app _))]

       
       [(,prim ,rand* ...) (guard (regiment-primitive? prim))
	(PrimApp self (cons prim rand*) kont #f )]
       [(assert-type ,ty (,prim ,rand* ...)) (guard (regiment-primitive? prim))
	(PrimApp self (cons prim rand*) kont ty)]
              
       [(assert-type ,ty ,e) (recur e)]
       ))))


;(define (array-constructor-codegen self len init ty kont)
(__spec array-constructor-codegen <emitC2> (self len init ty kont)
  (match ty
    [(Array ,elt)
					;(k `("arrayMake(sizeof(",elt"), "len", "init")"))
     
					;(when (and (heap-allocated? elt global-struct-defs) (not init))
					;(error array-constructor-codegen "DANGER DANGER: ~s" elt))
     
     ;; We fill with zeros if we've got Array:make with a zero scalar.
     ;; OR if we've got a makeUNSAFE with pointers (have to put in null pointers).
     (define zero-fill?
       (or (and init (wszero? elt init))
	   (and (not init) (heap-type? self elt))))

     (let* ([_elt (Type self elt)]
	    [size `("(sizeof(",_elt") * ",len") + RCSIZE + ARRLENSIZE")]
	    [tmp (Var self (unique-name "arrtmp"))]
	    [alloc (if zero-fill?
		       `("calloc(",size", 1)")
		       `("malloc(",size")"))]
	    [cast `("(",_elt"*)",tmp)])
       (append-lines 
	(make-lines 
	 (list
	  `("int* ",tmp" = (int*)0;\n")
	  ;; This will handle Array:null:
	  (block `("if (",len" > 0)")
		 `(,tmp" = (int*)((char*)",alloc" + RCSIZE + ARRLENSIZE);\n"
		       "CLEAR_RC(",tmp");\n"           
		       "SETARRLEN(",tmp", ",len");\n"  
		       ;; Now fill the array, if we need to:
		       ,(if (and init (not zero-fill?))
			    (let ([i (unique-name "i")]
				  [tmp2 (unique-name "arrtmpb")]
				  [lensub1 (unique-name "lenmin1")])
			      (list `(,_elt"* ",(Var self tmp2) " = ",cast";\n")
				    `("int ",(Var self lensub1) " = ",len" - 1;\n")
				    (lines-text
				     ((Effect self (emit-err 'array:make-constant))
				      `(for (,i '0 ,lensub1)
					   (Array:set (assert-type (Array ,elt) ,tmp2) ,i ,init))))))
			    "")))))
	(kont cast)))]))
  
(define (wszero? ty obj)
       (match obj
	 [(assert-type ,_ ,[x]) x]
	 [,obj
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
	    [(Array ,_) (eq? obj 'Array:null)]
	    [(List ,_)  (eq? obj '())]

	    [(Struct ,_) #f]
	    
	    [,ty (error 'wszero? "Not yet handling zeros for this type: ~s, obj ~s" ty obj)])]))  
  
(define __
  (specialise! PrimApp <emitC2> 
   ;; [2008.05.10] FIXME: This failed the return contract for the __real__ case.
   (debug-return-contract PrimApp lines?
     (lambda (next self app kontorig mayberetty)
       (define ___ (next))
     
     (define (kont x) (kontorig x)) ;; A place to throw in traces.

     (define (Simp x) (Simple self x))

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
	    [(logF logD)                            "log"]
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
     
         
     (match app
       ;; Refs and sets are pure simplicity:
       [(Array:ref ,[Simp -> arr] ,[Simp -> ind])
	(kont `(,arr"[",ind"]"))]

       ;; Using some simple C macros here:
       [(car ,[Simp -> pr]) (kont `("CAR(",pr")"))]
       [(cdr ,[Simp -> pr]) (kont `("CDR(",pr")"))]

       [(List:is_null ,[Simp -> pr]) (kont `("(",pr" == 0)"))]

       [(cons ,[Simp -> hd] ,[Simp -> tl])
	(ASSERT mayberetty)
	(match mayberetty
	  [(List ,elt)
	   (let ([tmp (Var self (unique-name "tmpcell"))]
		 [ty (Type self mayberetty)])
	     (append-lines 
	      (make-lines `(,ty" ",tmp" = (",ty")CONSCELL(",(Type self elt)");\n"
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
       
       [(Array:length ,[Simp -> arr])
	;; Length is -2 and refcount is -1:
	(kont `("ARRLEN(",arr")"))
	]
       ;; This is the more complex part:

       [(Array:make ,[Simp -> len] ,init) (array-constructor-codegen self len init mayberetty kont)]
       [(Array:makeUNSAFE ,[Simp -> len]) (array-constructor-codegen self len #f   mayberetty kont)]

       [(max ,[Simp -> a] ,[Simp -> b]) (kont `("(",a" > ",b" ? ",a" :",b")"))]
       [(min ,[Simp -> a] ,[Simp -> b]) (kont `("(",a" < ",b" ? ",a" :",b")"))]



       [(lshiftI16 ,[Simp -> a] ,[Simp -> b]) (kont `("(",(Type self 'Int16)")(",a" << ",b")"))]
       [(lshiftI32 ,[Simp -> a] ,[Simp -> b]) (kont `("(",(Type self 'Int32)")(",a" << ",b")"))]
       [(lshiftI64 ,[Simp -> a] ,[Simp -> b]) (kont `("(",(Type self 'Int64)")(",a" << ",b")"))]
       [(rshiftI16 ,[Simp -> a] ,[Simp -> b]) (kont `("(",(Type self 'Int16)")(",a" >> ",b")"))]
       [(rshiftI32 ,[Simp -> a] ,[Simp -> b]) (kont `("(",(Type self 'Int32)")(",a" >> ",b")"))]
       [(rshiftI64 ,[Simp -> a] ,[Simp -> b]) (kont `("(",(Type self 'Int64)")(",a" >> ",b")"))]
       [(logandI16 ,[Simp -> a] ,[Simp -> b]) (kont `("(",(Type self 'Int16)")(",a" & ",b")"))]
       [(logandI32 ,[Simp -> a] ,[Simp -> b]) (kont `("(",(Type self 'Int32)")(",a" & ",b")"))]
       [(logandI64 ,[Simp -> a] ,[Simp -> b]) (kont `("(",(Type self 'Int64)")(",a" & ",b")"))]
       [(logorI16  ,[Simp -> a] ,[Simp -> b]) (kont `("(",(Type self 'Int16)")(",a" | ",b")"))]
       [(logorI32  ,[Simp -> a] ,[Simp -> b]) (kont `("(",(Type self 'Int32)")(",a" | ",b")"))]
       [(logorI64  ,[Simp -> a] ,[Simp -> b]) (kont `("(",(Type self 'Int64)")(",a" | ",b")"))]
       [(logxorI16 ,[Simp -> a] ,[Simp -> b]) (kont `("(",(Type self 'Int16)")(",a" & ",b")"))]
       [(logxorI32 ,[Simp -> a] ,[Simp -> b]) (kont `("(",(Type self 'Int32)")(",a" & ",b")"))]
       [(logxorI64 ,[Simp -> a] ,[Simp -> b]) (kont `("(",(Type self 'Int64)")(",a" & ",b")"))]

#|
       [(,lshift ,[Simp -> a] ,[Simp -> b]) 
	(guard (memq lshift '(lshiftI16 lshiftU16 lshiftI32))) (ASSERT mayberetty)
	(kont `("(",(Type self mayberetty)")(",a" << ",b")"))]
       [(,rshift ,[Simp -> a] ,[Simp -> b]) 
 	(guard (memq rshift '(rshiftI16 rshiftU16 rshiftI32))) (ASSERT mayberetty)
	(kont `("(",(Type self mayberetty)")(",a" >> ",b")"))]
       [(,logand ,[Simp -> a] ,[Simp -> b]) 
	(guard (memq logand '(logandI16 logandU16 logandI32))) (ASSERT mayberetty)
	(kont `("("(",(Type self mayberetty)"),a" & ",b")"))]
       [(,logor ,[Simp -> a] ,[Simp -> b]) 
	(guard (memq logor '(logorI16 logorU16 logorI32)))     (ASSERT mayberetty)
	(kont `("(",(Type self mayberetty)")(",a" | ",b")"))]
       [(,logxor ,[Simp -> a] ,[Simp -> b]) 
	(guard (memq logxor '(logxorI16 logxorU16 logxorI32))) (ASSERT mayberetty)
	(kont `("(",(Type self mayberetty)")(",a" & ",b")"))]
|#

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
	(add-include! self "<fftw3.h>")
	(add-include! self (list "\"" (REGIMENTD) "/src/linked_lib/fftw_wrappers.c\""))
	(add-link! self "libfftw3f.so")		
	(append-lines ((Binding self (emit-err 'fft)) (list len0 'Int `(Array:length ,arr)))
		      ((Binding self (emit-err 'fft)) (list len1 'Int (if inverse? `(_-_ ,len0 '1) `(/_ ,len0 '2) )))
		      ((Binding self (emit-err 'fft)) (list len2 'Int (if inverse? `(*_ ,len1 '2) `(_+_ ,len1 '1) )))
		      ((Binding self (emit-err 'fft))
				   (list tmp 
					 (if inverse? '(Array Float) '(Array Complex))
					;`(assert-type (Array Complex) (Array:makeUNSAFE ,len))
					 (if inverse?
					     `(assert-type (Array Float)   (Array:makeUNSAFE ,len2))
					     `(assert-type (Array Complex) (Array:makeUNSAFE ,len2)))))
		      (make-lines `(,name"(",(Simp arr)", ",(Var self tmp)");\n"))
		      (kont (Var self tmp)))]
       
       ;; wsequal? should only exist for scalars at this point:
       [(wsequal? ,[(TyAndSimple self) -> ty left] ,[Simp -> right])
	(ASSERT scalar-type? ty)		
	(kont `("(",left" == ",right")"))]

       [(__show_ARRAY ,[(TyAndSimple self) -> ty obj])
	(define max-show-size 100)
	(match ty
	  [String       (kont obj)]
	  [(Array Char) (kont obj)]
	  [,_ 
	   (let* ([str (unique-name "str")]
		  [_str (Var self str)]
		  [_realsize (Var self (unique-name "realsize"))]
		  [_max (number->string max-show-size)])
	     (append-lines 
	      ((Binding self (emit-err '__show_ARRAY)) 
	       (list str '(Array Char) `(assert-type (Array Char) (Array:makeUNSAFE ',max-show-size))))
	      (make-lines (list 			   
			   "int "_realsize" = snprintf("_str", "
			           _max", \""
				   (type->printf-flag self ty)"\", "obj");\n"
			   "if ("_realsize" >= "_max") { printf(\"Error, show overflowed fixed length "
			   _max" buffer\\n\"); exit(-1); }\n"
			   "SETARRLEN("_str", "_realsize" + 1); /* set virtual length */ \n"
			   ))
	      (kont _str)))])]
       
       [(,infix_prim ,[Simp -> left] ,[Simp -> right])	
	(guard (assq infix_prim infix-arith-prims))
	(if (equal? "^" (substring (sym2str infix_prim) 0 1))
	    (kont (list "pow("left", "right")")) ;; Actually this doesn't work for integers
	    (let ()
	      (define valid-outputs '("+" "-" "/" "*" "<" ">" "==" "<=" ">=")) ; "^"
	      (define result
		(case infix_prim
		  [(=) "=="]
		  [(< > <= >=) (sym2str infix_prim)]
		  [else 
		   (match (string->list (sym2str infix_prim))	  
		     [(#\_ ,op ,suffix ...) (list->string (list op))]
		     [(,op ,suffix ...)     (list->string (list op))])]))
	      (ASSERT (member result valid-outputs))
	      (kont (list "(" left " " result " " right ")"))))]


       ;;========================================
; 	[(realpart ,[v]) `("(" ,v ".real)")]
; 	[(imagpart ,[v]) `("(" ,v ".imag)")]
 	[(realpart ,[Simp -> v])   (kont `("__real__ (" ,v ")"))]
 	[(imagpart ,[Simp -> v])   (kont `("__imag__ (" ,v ")"))]

; 	;; Wait... do we need to cast to double here?
; 	[(,extractreal ,[Simp -> v]) 
; 	 (guard (memq extractreal '(realpart complexToFloat complexToDouble)))
; 	 (kont `("__real__ (" ,v ")"))]

	[(absC ,[Simp -> c]) (kont `("cNorm(",c")"))]

	[(intToChar ,[Simp -> e]) (kont `("(wschar_t)",e))]
       
	[(__cast_num ',from ',to ,[Simp -> e])
	 ;(inspect (vector from to))
	 (kont `("(",(Type self to)")",e))]

	[(,toint     ,[Simp -> e]) 
	 (guard (memq toint '(int16ToInt int64ToInt floatToInt doubleToInt charToInt)))
	 (kont `("(int)",e))]
	[(,toint16     ,[Simp -> e]) 
	 (guard (memq toint16 '(intToInt16 int64ToInt16 floatToInt16 doubleToInt16)))
	 (kont `("(int16_t)",e))]
	[(,toint64    ,[Simp -> e]) 
	 (guard (memq toint64 '(int16ToInt64 intToInt64 floatToInt64 doubleToInt64)))
	 (kont `("(int64_t)",e))]
	[(,tofloat  ,[Simp -> e]) 
	 (guard (memq tofloat '(intToFloat int16ToFloat int64ToFloat doubleToFloat)))
	 (kont `("(float)",e))]
	[(,todouble  ,[Simp -> e]) 
	 (guard (memq todouble '(intToDouble int16ToDouble int64ToDouble floatToDouble)))
	 (kont `("(double)",e))]

 	[(complexToFloat ,e)   (PrimApp self `(realpart ,e) kont mayberetty)]
 	[(complexToInt16 ,e)   (kont `("(int16_t)" ,(PrimApp self `(complexToFloat ,e) id 'Float)))]
 	[(complexToInt64 ,e)   (kont `("(int64_t)" ,(PrimApp self `(complexToFloat ,e) id 'Float)))]
 	[(complexToInt ,e)     (kont `("(int)"     ,(PrimApp self `(complexToFloat ,e) id 'Float)))]
 	[(complexToDouble ,e)  (kont `("(double)"  ,(PrimApp self `(complexToFloat ,e) id 'Float)))]
 	[(,ToComplex ,[Simp -> e])
 	 (guard (memq ToComplex 
 		      '(int16ToComplex int64ToComplex intToComplex floatToComplex doubleToComplex)))
     	 (kont `("(",e" + 0.0fi)"))]
 	[(makeComplex ,[Simp -> re] ,[Simp -> im])
 	 (kont `("(",re" + (",im" * 1.0fi))"))]
	;;========================================

	[(__stringToInt_ARRAY ,[Simp -> e]) 
	 (let ([tmp (Var self (unique-name 'tmp))])
	   (make-lines 
	    `("int ",tmp";\n" 
	      "sscanf(",e", \"%d\", &",tmp");\n"
	      ,(lines-text (kont tmp)))))]
	[(__stringToFloat_ARRAY ,[Simp -> e]) 
	 (let ([tmp (Var self (unique-name 'tmp))])
	   (make-lines 
	    `("float ",tmp";\n"
	      "sscanf(",e", \"%f\", &",tmp");\n"
	      ,(lines-text (kont tmp)))))]
	[(__stringToDouble_ARRAY ,[Simp -> e]) 
	 (let ([tmp (Var self (unique-name 'tmp))])
	   (make-lines
	    `("double ",tmp";\n"
	      "sscanf(",e", \"%lf\", &",tmp");\n"
	      ,(lines-text (kont tmp)))))]
	[(__stringToComplex_ARRAY ,[Simp -> e]) 
	 (let ([tmp1 (Var self (unique-name 'tmp))]
	       [tmp2 (Var self (unique-name 'tmp))])
	   (make-lines
	    `("float ",tmp1";\n"
	      "float ",tmp2";\n"
	      "sscanf(",e", \"%f+%fi\", &",tmp1", &",tmp2");\n"
	      ,(lines-text (kont `(,tmp1"+(",tmp2"*1.0fi)"))))))]

	;;========================================

	[(clock) (kont "(clock() * 1000 / CLOCKS_PER_SEC)")]
	[(realtime) 
	 (define tmp (symbol->string (unique-name "tmp")))
	 (make-lines
	  `("struct timeval ",tmp";\n"
	    "gettimeofday(&",tmp", NULL);\n"
	    ,(lines-text (kont `("(",tmp".tv_sec * 1000 + ",tmp".tv_usec / 1000)")))))]

	[(getID) (kont "0 /* NodeID of PC-server */")]
       
	[(,other ,[Simp -> rand*] ...)
	 (kont `(,(SimplePrim other) "(" ,(insert-between ", " rand*) ")"))]
       )  
       ))))



;; .param srccode* blocks of code (lines) for the body of each source.
(__spec BuildTimerSourceDriver <emitC2> (self srcname* srccode* srcrates*)

   (ASSERT "BuildTimerSourceDriver: must have same number of names, code, and rates"
	   all-equal?
	   (list (length srcname*) (length srccode*) (length srcrates*)))
  
   ;; There are three options here.
   ;;  (1) We run a system of virtual timers.
   ;;  (2) We are driven by a serial port connected to a mote.
   ;;  (3) We are driven by "foreign_source".
   (cond 
    ;; Option (1):
    [(not (null? srcname*)) ;; TODO: IN THE FUTURE SHOULD BE ABLE TO COMBINE MOTE SOURCES OR FOREIGN SOURCES WITH TIMERS. FIXME  FIXME FIXME  FIXME  
     (ASSERT (null? (slot-ref self 'server-cutpoints)))
      ;; If the rate is #f we don't build in that entry:
      (let-match ([([,srcname* ,srccode* ,srcrates*] ...)
		   (filter id
		     (map (lambda (nm code rate)
			    (if rate (list nm code rate) #f))
		       srcname* srccode* srcrates*))])   
	(printf "   TIMER RATES: ~s\n" srcrates*)
	;; This is a hack: rather than maintain a sorted list of
	;; upcoming events, we simply compute a common tick frequency
	;; and go tick by tick:
	(let  ([counter_marks
		(cond
		 [(= 1 (length srcrates*)) '(1)]
		 [(andmap integer? srcrates*)
		  (let ([srcrates* (map exact srcrates*)])
		    (let ([common-rate (apply lcm srcrates*)])
		      (map (lambda (rate)
			     (exact (quotient common-rate rate)))
			srcrates*)))]
		 [else (error 'timer "non integer rates not handled yet: ~s" srcrates*)])])
	  (make-lines        
	   (block (list "int main(int argc, "(Type self `(Array (Array Char)))" argv)" )
		  (list 
		   "parseOptions(argc,argv);\n"
		   (map (lambda (name) (format "int counter_~a = 0;\n" name)) srcname*)
		   "initState();\n"
		   (Type self 'Bool)" dummy ="(Const self #t id)";\n" ;; Hack for java.
		   (block "while(dummy)"
			  (list (map (lambda (name) (format "counter_~a++;\n" name)) srcname*)
				(map (lambda (name code mark)
				       (block (format "if (counter_~a == ~a)" name mark)
					      ;; Execute the code for this source.
					      (list (lines-text code)
						    (format "counter_~a = 0;\n" name))))
				  srcname* srccode* counter_marks)
				)
					;(map (lambda (f) (list (make-app (Var f) ()) ";\n")) srcname*)
			  )
		   "return 0;\n")))))]

      ;; Option (2): serial port connected to mote:
      [(not (null? (slot-ref self 'server-cutpoints)))
       (unless (= (length (slot-ref self 'server-cutpoints)) 1)
	  (error 'BuildTimerSourceDriver 
		 "Currently can't handle multiple return streams multiplexed onto serial port"))
	
	;; This is SUPER hackish, just sticking all the WSQueryMsg.c files in the flags!
	;(slot-cons! self 'compile-flags " -I$TOSROOT/support/sdk/c -L$TOSROOT/support/sdk/c -lmote -I$TOSROOT/support/sdk/c/tos/types/")
	;; ACK: fixing the TINYOS install dir at compile time.  Can't get quoting/env-vars to work out:
	(slot-cons! self 'compile-flags 
		    (format " -I~a/support/sdk/c  -L~a/support/sdk/c -lmote "
			    (getenv "TOSROOT") (getenv "TOSROOT")))
	(for i = 0 to (sub1 (length (slot-ref self 'server-cutpoints)))
	     (add-include! self (format "\"WSQueryMsg~a.c\"" i)))
	
	;; -lmote
	;(add-link! "libmote.a")

	(make-lines (list "
#include <message.h>
//#include \"/opt/tinyos-2.x/support/sdk/c/serialsource.h\"
//#include \"/opt/tinyos-2.x/support/sdk/c/message.h\"
#include \"serialsource.h\"
#include \"serialpacket.h\"
#include \"serialprotocol.h\"
#include \"message.h\"

static char *msgs[] = {
  \"unknown_packet_type\",
  \"ack_timeout\"	,
  \"sync\"	,
  \"too_long\"	,
  \"too_short\"	,
  \"bad_sync\"	,
  \"bad_crc\"	,
  \"closed\"	,
  \"no_memory\"	,
  \"unix_error\"
};
void stderr_msg(serial_source_msg problem) {
  fprintf(stderr, \"Note: %s\\n\", msgs[problem]);
}


void hexprint(uint8_t *packet, int len)
{
  int i;

  for (i = 0; i < len; i++)
    printf(\"%02x \", packet[i]);
}


// This is a driver that takes serial messages (from a mote) and pumps them into WS.
int main(int argc, char **argv)
{
  serial_source src;
  initState();
  if (argc < 3)  {
      fprintf(stderr, \"Usage: %s <device> <rate> - get packets from a serial port\\nExample: device=/dev/ttyUSB0, rate=telosb\\n\", argv[0]);
      exit(2);
  }
  parseOptions(argc-2,argv+2);

  src = open_serial_source(argv[1], platform_baud_rate(argv[2]), 0, stderr_msg);
  if (!src)  {
      fprintf(stderr, \"Couldn't open serial port at %s:%s\\n\", argv[1], argv[2]);
      exit(1);
  }
  for (;;) {
      int len, i;
      const unsigned char *packet = read_serial_packet(src, &len);
      if (!packet) exit(0);

      //for ( i = 0; i < len; i++ ) printf(\"%02x \", packet[i]); putchar('\\n');

      if (len >= 1 + SPACKET_SIZE &&
	  packet[0] == SERIAL_TOS_SERIAL_ACTIVE_MESSAGE_ID)
	{
/*
	  tmsg_t *msg = new_tmsg((void*)packet + 1, len - 1);
	  if (!msg) exit(0);

	  printf(\"dest %u, src %u, length %u, group %u, type %u\\n  \",
		 spacket_header_dest_get(msg),
		 spacket_header_src_get(msg),
		 spacket_header_length_get(msg),
		 spacket_header_group_get(msg),
		 spacket_header_type_get(msg));
	  hexprint((uint8_t *)tmsg_data(msg) + spacket_data_offset(0),
		   tmsg_length(msg) - spacket_data_offset(0));

	  printf(\"\\n\");
	  free(msg);
*/
	  // We shouldn't HAVE to do this:
	  // The MIG generated code uses offset 0, so we hack this:
	  //msg->data = msg->data + spacket_data_offset(0);
          tmsg_t *msg2 = new_tmsg((void*)packet + 1 + spacket_data_offset(0), len - 1);

          "(Var self (car (slot-ref self 'server-cutpoints)))"(msg2);
	  free(msg2);
	}
      else
	{
	  printf(\"non-AM packet: \");
	  hexprint(packet, len);
	}
     
      //tmsg_t* incoming_msg = new_tmsg((void*)packet,len);
      free((void *)packet);
      //free((void *)incoming_msg);
    }
}
"))];"

  ;; Option 3: driven by foreign_source
  [else
   (make-lines "
  int main(int argc, char** argv) {
    wsmain(argc, argv);
    return 0;
  }
")
   ]

)) 

;; These types define different pieces of code.  The type indicates
;; the context where the code should end up.  The code generator
;; weaves these together, putting each piece in the appropriate place.
(reg:define-struct (c-timer name code state rate ))
(reg:define-struct (c-toplvl lines))
(reg:define-struct (c-init   lines))
(reg:define-struct (c-state  lines))
(reg:define-struct (c-proto  lines))

;; .returns a function def in several parts: name, code, state, rate, init-code
;; code, state, init-code are all "lines" structures
;; If "rate" is non-false, then the result represents a timer entry.

;; Returns a list of code pieces, which can be any of the "c-" datatypes.
(__spec Source <emitC2> (self xp)
   (match xp
    [((name ,nm) (output-type ,ty) (code ,cd)  (outgoing ,down* ...))
     (match cd 
       [(timer ,annot ,rate)
	(match (peel-annotations rate)
	  [',rate
	   ;;(ASSERT integer? rate)
	   ;; For the basic backend, we simply call the downstream
	   ;; operator when we ourselves are asked for data (invoked).
	   (list 
	    (make-c-timer nm
		   ((Emit self down*) ''UNIT) ;; Code
		   (make-lines '()) ;; State 	
		   rate))])]

       ;; Allowing normal inline_C and treating it as a special case of inline_TOS:
       [(inline_C ',top ',initfun) 
	(list (make-c-state (make-lines (list "\n" top "\n")))
	      (make-c-init  (make-lines (if (equal? initfun "") "" `(,initfun "();\n")))))]

       [(__foreign_source ',name ',filels '(Stream ,type))
	(define ty (Type self type))
	(define arg (unique-name "tmp"))
	(for-each (lambda (file)
		    (let ([ext (extract-file-extension file)])
		      (cond
		       [(member ext '("c" "cpp" "h" "so" "a" "o"))
			(add-include! (list "\"" file "\""))]
		       [else (error 'emit-c:foreign
				    "cannot foreign source from this type of file: ~s" file)])))
	  (cdr filels))
	;; Create a function for the entrypoint.
	(let* ([proto `("void ",name"(",ty");\n")]
	       [bod (ForeignSourceHook self name
				       (lines-text ((Emit self down*) arg)))]
	       [impl (make-lines 
		      (block `("void ",name"(",ty" ",(Var self arg)")")
			    bod))])
	  (list (make-c-toplvl impl)
		(make-c-proto  proto)))]
       )]))



(__spec IterStartHook <emitC2> (self name arg argty) "")
(__spec IterEndHook   <emitC2> (self name arg argty) "")

;; Returns two values both of type 'lines'
;;  (1) A function prototype
;;  (2) A function definition
(__spec GenWorkFunction <emitC2> (self name arg vqarg argty code)
  (define _arg (Var self arg))
  (define _argty (Type self argty))
  (values
   (make-lines `("void ",(Var self name) "(",_argty" ",_arg"); // Iter prototype\n"))
   (make-lines 
    (list (block `("void ",(Var self name) "(",_argty" ",_arg")")
		 (list
		  "char "(Var self vqarg)";\n"
		 (IterStartHook self name arg argty)
		 (lines-text code)
		 (IterEndHook self name arg argty)
		 ))
	  "\n"))))

;; A cut point on the server, currently only coming FROM the network.
;; Returns decls, top lvl binds, init code
(__spec Cutpoint <emitC2> (self type in out)
   (define local (unique-name "local"))
   (define _local (Var self local))
   ;(define AM_NUM (number->string (+ AM_OFFSET (slot-ref self 'amsender-count))))
   (slot-cons! self 'server-cutpoints in)   
   (match type
     [(Stream ,elt)
      (define _elt (Type self elt))
      (define root (format "cuttype~a" 10)) ;; TEMP HACK FIXME
      ;; This is extremly annoying.  MIG won't build us a struct, it will
      ;; just give us accessor methods.  Therefore we need to build our
      ;; own bit of code that will build up the struct using these accessors.
      (define (copycode dest src)
	(let loop ([elt elt] [destpos dest] [srcpos (format "~a_theitem" root)])
	  (match elt
	    [,scl (guard (scalar-type? scl))
		  (list destpos " = " srcpos "_get(" src ");\n")]
	    [(Struct ,name)
	     (map (match-lambda ((,fldname ,fldty))
		    (loop fldty (list destpos "." (sym2str fldname)) (list srcpos "_" (sym2str fldname))))
	       (cdr (assq name (slot-ref self 'struct-defs))))]
	    ;; For a single array we can handle this (but not yet packed inside a tuple)
	    [(Array ,elt)
	     (define ty `(Array ,elt))
	     (define _elt (Type self elt))
	     (list
	      ;; Could use nx_ types here and then we could unpack this ourselves (without MIG):
	      "int i;\n"
	      "int arrsize = "root"_len_get(" src ");\n"
	      ;;"char* ptr = ((char*)tmsg_data(" src ")) + sizeof(uint16_t);\n"
	      ;;;;"int arrsize = ((uint16_t*)ptr)[-1];\n"
	      ;;;;"char* ptr = ((char*)"src") + " srcpos "_offset(0);\n"
	      (lines-text (array-constructor-codegen self "arrsize" #f ty (setterk self local ty)))
	      ;;"printf(\"arrsize %d %x tmsg addr %p Offset addr %p\\n\", arrsize, arrsize, "src", tmsg_data("src"));\n"
	      ;;"memcpy("_local", ((uint16_t*)ptr)+1, sizeof("_elt") * ARRLEN("_local"));\n"
	      (block "for (i=0; i<arrsize; i++)"
		     (list _local"[i] = " srcpos "_get("src", i);\n"))
	      )]
	    )))
      (values (make-lines
	       (list 
		"
  // CutPoint from node side:
  void "(Var self in)"(tmsg_t* nodeobj) {
    "_elt" "_local";
"(indent (copycode _local "nodeobj") "    ")"
    "(Var self out)"("_local");
  }
"))
'() '()
)]))



;; .returns top-level decls (lines)
(__spec Operator <emitC2> (self op)
  (define (Simp x) (Simple self x))
  (match op
    [(iterate (name ,name) 
	      (output-type ,ty)
	      (code (iterate ,annot ,itercode ,_))
	      (incoming ,up)
	      (outgoing ,down* ...))
     (define (emitter x) ((Emit self down*) x))
     (match itercode
       [(let (,[(SplitBinding self (emit-err 'OperatorBinding)) -> bind* init*] ...)
	  (lambda (,v ,vq) (,vty (VQueue ,outty)) ,bod))
	(let-values ([(proto def)
		      (GenWorkFunction self name v vq vty ((Value self emitter) bod nullk))])	  
	  (values def (cons proto bind*) init*))])]

    [(cutpoint (name ,_) (output-type ,type) (code ,__) (incoming ,in) (outgoing ,out))
     (ASSERT out)
     (Cutpoint self type in out)]

    [(_merge (name ,name) (output-type (Stream ,elt))
	     (code ,__)
	     (incoming ,a ,b) (outgoing ,down* ...))
     (define (emitter x) ((Emit self down*) x))
     (define arg (unique-name "arg"))
     (values (make-lines 
	      (list (block `("void ",(Var self name) "(",(Type self elt)" ",(Var self arg)")")
			   (lines-text (emitter arg)))
		    "\n"))
	     '() '())]

    [(unionN . ,_) (error 'emitC2 "doesn't support unionN/unionList right now")]

    ;; This doesn't work for sigsegs, only arrays:
    [(__readFile (name ,name) (output-type (Stream ,elt))
		 (code (__readFile (annotations .  ,annot)
				   ',file          ,source
				   ',mode          ,[Simp -> repeats] 
				   ',skipbytes     ,[Simp -> offset] 
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
		       [,other (ASSERT (not (eq? other 'String))) (Type self other)])]
	    [types (if structoutput?
		       (map cadr (cdr (ASSERT (assq structoutput? (slot-ref self 'struct-defs)))))
		       (match elt
			 ;[(Sigseg ,t) (list t)]
			 [(Array ,t) (list t)]
			 [,oth       (list oth)]))]
	    ;[numstrings (length (filter (lambda (s) (eq? s 'String)) types))]

	    [binarymode (equal? mode "binary")]
	    [handle (Var self (unique-name "_f"))]

	    [init (make-lines `(
		,handle" = fopen(\"",file"\", ",(if binarymode "\"rb\"" "\"r\"")");\n"
		"if (",handle" == NULL) {\n"
		"  printf(\"Unable to open data file %s: %m\", \"",file"\");\n"
		"  exit(1);\n"
		"}\n"
		"fseek(",handle", ",offset", SEEK_SET);\n"
		))]

	    [skipcmd (list "fseek("handle", "(number->string skipbytes)", SEEK_CUR);\n")]
	    
	    [state (make-lines `("FILE* ",handle";\n"))]
	    [_buf (Var self 'buf)]
	    [maintext ;; Code for the funcion that drives the file reading.
	     (list `(
		     ,(if (> winsize 0)
			  (lines-text ((Value self (emit-err 'readFile)) 
				       `(assert-type ,elt (Array:makeUNSAFE ',winsize)) 
				       (varbindk self 'buf elt)))
			  ;(Binding (list 'buf elt (make-zero-for-type elt)))
			  ;; A binding with no init:
			  (list (Type self elt) " " _buf ";\n"))
		     
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
					       skipcmd))
				  ;; Otherwise can do one read:
				  (readcmd winsize "buf"))
			      (list (readcmd 1 (** "&" _buf))
				    (if (> skipbytes 0) skipcmd ""))
			      )))
		     
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
		     ,(lines-text ((Emit self down*) 'buf))))])

       (values 
	(make-lines (block (list "void "(sym2str name)"("(Type self '#())" ignored)") maintext))
         ;(wrap-iterate-as-simple-fun name 'ignored 'ignoredVQ (Type self '#()) maintext)
	(list state) (list init)))] ;; End readFile
    ))


;================================================================================

;; This has a quirky return type.  Ugly abstraction boundaries.
;; Returns a vector of two elements:
;;   (1) Association list of file-name, file-contents to write.
;;   (2) Thunk to execute after files are written.
(__spec BuildOutputFiles <emitC2> (self includes freefundefs state ops init driver)
  (define text
    (apply string-append 
         (insert-between "\n"
           (list includes 
                 (text->string (map (curry StructDef self) (slot-ref self 'struct-defs)))
                 (text->string (lines-text freefundefs))
                 state
                 ;toplevelsink
                 ops ;srcfuns 
                 init driver))))
  ;; Return an alist of files:
  (vector (list (list "query.c" text))
	  void))

(define (cutpoint? op)  (eq? (car op) 'cutpoint))

;; For now the only sorting we do is to bring Cutpoints to the front:
(__spec SortOperators <emitC2> (self ops) 
 (define-values (cps other) (partition cutpoint? ops))
 ;; Hack, also reverse the others for now... 
 ;; In the future we should topologically sort them here:
 (append cps (reverse other)))
  
;; Run the pass and generate C code:
;; Same return type as BuildOutputFiles
(__spec Run <emitC2> (self)
  (let* ([prog (slot-ref self 'theprog)]
	 [freefundefs (build-free-fun-table! self (cdr (ASSERT (project-metadata 'heap-types prog))))])
     (match prog
       [(,lang '(graph 
		 ;; These had better be *constants* for GCC:
		 (const ,[(SplitBinding self (emit-err 'TopBinding)) -> cb* cbinit*] ...)
		 (init  ,[(Effect self (lambda _ (error 'top-level-init "code should not 'emit'")))
			  -> init*] ...)
		 ;(sources ,[(curry Source self) -> srcname* srccode* state1* srcrate* srcinit*] ...)
		 (sources ,[(curry Source self) -> pieces**] ...)
		 (operators ,oper* ...)
		 (sink ,base ,basetype)
		 ,meta* ...))

	(define pieces* (apply append pieces**))
	;; Break out the different pieces:
	(define timers        (filter c-timer? pieces*))
	(define toplvl-pieces (map c-toplvl-lines (filter c-toplvl? pieces*)))
	(define init-pieces   (map c-init-lines (filter c-init? pieces*)))
	(define state-pieces  (map c-state-lines (filter c-state? pieces*)))
	(define proto-pieces  (map c-proto-lines (filter c-proto? pieces*)))

	(let-match ([(,[(curry Operator self) -> oper* state2** opinit**] ...) (SortOperators self oper*)])

	;; This must be called early, for side effects to the object's fields:
	(define driver   (text->string (lines-text 
					;(BuildTimerSourceDriver self srcname* srccode* srcrate*)
					(BuildTimerSourceDriver self 
								(map c-timer-name timers)
								(map c-timer-code timers)
								(map c-timer-rate timers))
					)))
	(define includes (text->string 			  
			  (list "//WSLIBDEPS: "
				;; This is a bit hackish, throw the flags in there also:
				(slot-ref self 'compile-flags)
				(map (lambda (fn) 
				       (let ([lib (extract-lib-name fn)])
					 (if lib (list " -l" lib) fn)))
				  (slot-ref self 'link-files))
				"\n"
							       
				#;
				(map (lambda (definc) (list "#include \""definc"\"\n"))
				  (slot-ref self 'default-includes))		
				
				;; After the types are declared we can bring in the user includes:
				"\n\n" (map (lambda (fn) `("#include ",fn "\n"))
					 (reverse (slot-ref self 'include-files))) "\n\n")))
	(define allstate (text->string (map lines-text (apply append cb* state-pieces state2**))))
	(define ops      (text->string (map lines-text oper*)))
	  ;(define srcfuns  (text->string (map lines-text (map wrap-source-as-plain-thunk srcname* srccode*))))
	(define init     (begin ;(ASSERT (andmap lines? (apply append opinit**)))
			   (text->string (block "void initState()" 
						(list 
						 (lines-text (apply append-lines init*))
						 (lines-text (apply append-lines cbinit*))
						 ;; This will be where the inline_C initializers go:
						 ;(lines-text (apply append-lines srcinit*))
						 (map lines-text init-pieces)
						 (lines-text (apply append-lines (apply append opinit**))))))))	
	  ;;(define toplevelsink "void BASE(int x) { }\n")	  
	  
	  ;; This is called last:
	  (BuildOutputFiles self includes freefundefs allstate ops init driver)
	  )]

       [,other ;; Otherwise it's an invalid program.
	(error 'emit-c2 "ERROR: bad top-level WS program: ~s" other)])))


;; Constructor, parse out the pieces of the program.
(__spec initialise <emitC2> (self prog)
  (slot-set! self 'free-fun-table '())
  (slot-set! self 'struct-defs (cdr (project-metadata 'struct-defs prog)))
  (slot-set! self 'union-types (cdr (project-metadata 'union-types prog)))
  (slot-set! self 'theprog prog)
  (slot-set! self 'link-files '())
  (slot-set! self 'compile-flags '())
  ;; Our default includes
  (slot-set! self 'include-files (list (** "\"" (REGIMENTD) "/src/linked_lib/wsc2.h\"")))
  (slot-set! self 'server-cutpoints '())
  )

(define (emit-c2 prog class)
  ;(define obj (make-object <zct> prog))
  (define obj (make-object class prog))
  (Run obj))

;;================================================================================

(define-class <zct> (<emitC2>) ())

#;
(specialise! Var <zct> 
 (lambda (next self v)
   (string-append (next) "____")
   ))


;;================================================================================


;;================================================================================


;; ================================================================================

(define-class <emitC2-timed> (<emitC2>) ())

(define (print-w-time2 prefix)
  ;;(list "printf(\"("prefix" %f)\\n\", (double)clock());\n")
#;
  (list "printf(\"("prefix" %lld)\\n\", clock());\n")
  (let ([tmp (sym2str (unique-name "tmp"))])
    `("struct timeval ",tmp";\n"
      "gettimeofday(&",tmp", NULL);\n"
      "printf(\"(",prefix" %ld)\\n\", ",tmp".tv_usec);\n"))
  #;
  (list 
   "print(\"("prefix"\"); "
   "print(clock()); "
   "print(\")\\n\");\n"))

(define ____IterStartHook
  (specialise! IterStartHook <emitC2-timed>
    (lambda (next self name arg argty)
      (list (next)	  	    
	    (print-w-time2 (list "Start "(sym2str name)" "))))))

(define ____IterEndHook
  (specialise! IterEndHook <emitC2-timed>
    (lambda (next self name arg argty) 
    (list (next)
	  (print-w-time2 (list "End "(sym2str name)" "))
	  ))))

;; Wrap timers around the whole Source call:
(define ____Source
  (specialise! Source <emitC2-timed>
  (lambda (next self xp)
    (define-values (nm code state rate init) (next))
    (values nm 
	    (append-lines (make-lines (print-w-time2 "StartTraverse "))
			  code
			  (make-lines (print-w-time2 "EndTraverse ")))
	    state rate init))))


) ;; End module
