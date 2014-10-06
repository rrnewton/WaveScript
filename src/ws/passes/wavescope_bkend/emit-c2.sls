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

;; This pass is using a substantial amount of runtime.  It could be
;; because of the inefficient OOP system.  This pass returns a "text"
;; structure, so the string append cost shouldn't be paid until afterwards.
;; (Except for the cost of indentation -- could test that.)
;;
;; Note on efficiency: 
;;   On demo3m for example, 
;;     Non-OOP:  chez: 266(15)        plt: 564(68 collecting)                   47.4 mb alloced
;;     BOS version: chez: 293(15ms)   plt: 660(92)  (chastity chez from .boot)  52.7 mb alloced
;;     BOS version: chez: 385(105, 13 collections)   plt: 660(96)  (chastity chez from src)
;;     BOS version: chez: 315(21)                    plt: 950(180) (laptop core2duo)
;; Wow - are collections worse when running from source? (because more code is loaded?)
;; It's also probably fair to say that the extra allocation from BOS
;; makes the probability of length collections higher.

(library (ws passes wavescope_bkend emit-c2)
  (export emit-c2 ;; The main entrypoint.
	   ;emit-c2-generate-timed-code
	   <emitC2>
	   <emitC2-base>
	   <emitC2-zct>
	   <emitC2-nogc>
	   <emitC2-timed>

	   emitC2-output-target
	   
	   ;; Have to export all the generic methods so that they can be subclassed:
	   build-free-fun-table!  heap-type?  add-include!
	   add-link!  gen-incr-code gen-decr-code gen-free-code

	   incr-local-refcount decr-local-refcount incr-heap-refcount  decr-heap-refcount  incr-queue-refcount  decr-queue-refcount 

	   potential-collect-point say-goodbye type->name
	   Const Var Simple TyAndSimple Type Let Value Effect StructDef Source
	   GenWorkFunction Operator SortOperators Cutpoint Emit type->printf-flag
	   Binding SplitBinding DummyInit StaticAllocate PrimApp Run
	   BuildTimerSourceDriver BuildOutputFiles varbindk
	   array-constructor-codegen
	   IterStartHook IterEndHook ForeignSourceHook
	   ExtraKernelArgsHook

	   make-lines lines-text append-lines slot-cons! 

	   ;; More datatypes:
	   make-c-timer c-timer-name c-timer-code c-timer-state c-timer-rate c-timer?
	   make-c-toplvl c-toplvl-lines c-toplvl?
	   make-c-init c-init-lines c-init?
	   make-c-state c-state-lines c-state?
	   make-c-proto c-proto-lines c-proto?
	   
	   test-emit-c2
	   )
  (import (except (rnrs (6)) error) (except (rnrs r5rs) force delay)
	  (rnrs mutable-strings)
	  (ws compat compat)
	  (ws common)
	  (ws passes wavescope_bkend insert-refcounts)
	  (ws passes wavescope_bkend emit-c)
	  (ws passes partition-graph)
	  (ws compiler_components c_generator)
	  (ws util bos_oop) 

	  (ws passes wavescope_bkend convert-sums-to-tuples)

	  ;(ws util rn-match) ;; TEMPTOGGLE
	  )

  ;; An object of this class is a compiler pass.  It is not reentrant,
  ;; because it maintains state for the program it is compiling.
  (define-class <emitC2-base> (<class>) 
    (theprog ;; <- holds on to the program we're compiling
     struct-defs union-types ;; <- cache these two pieces of metadata from theprog
     free-fun-table ;; <- This contains a table mapping
     copy-fun-table ;; <- Analogous t the free-fun-table
     ;;   types to a scheme function that generates free-code (when given
     ;;   text representing the pointer to free).
     include-files link-files ;; <- accumulate a list of files to include/link
     compile-flags ;; Accumulate a list of flags to send to gcc, datatype "text"
     hash-defs     ;; Accumulate flags to put at the top of the output file, before includes, datatype "text"
     server-cutpoints ;; <- names of cutpoints
     ))

  ;; We define a single top-level instance of our pass-class:
  ;(define-object obj <emitC2-base>)

  ;; These are the methods:
  (define-generic build-free-fun-table!)
  (define-generic build-copy-fun-table!)
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
  (define-generic incr-queue-refcount)
  (define-generic decr-queue-refcount)

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
  (define-generic AllocHook)
  (define-generic ExtraKernelArgsHook)

  (define-generic InvokePerOpCollector)

  ;; Add a filename (string) to the list of includes.
  (__spec add-include! <emitC2-base> (self fn)
    (define files (slot-ref self 'include-files))
    ;; [2009.12.01] It looks like at some point I was accepting lists of strings here.  Dunno why:
    (ASSERT string? fn)
    (unless (member fn files)
      (when (>= (wavescript-verbosity) 1)
	(printf "     --> adding include ~a \n" fn))
      (slot-set! self 'include-files (cons fn files)))
      )
  (__spec add-link! <emitC2-base> (self fn)
    (define files (slot-ref self 'link-files))
    (unless (member fn files)
      (slot-set! self 'link-files (cons fn files))))

  (define (add-file! self)
    (lambda (file)
      ;; Add to global list of includes if it's not already there.
      (let ([ext (extract-file-extension file)])
	(cond
	 [(member ext '("c" "cpp" "h" "hpp"))
	  (add-include! self (** "\"" file "\""))]
	 [(or (member ext '("so" "a" "o"))
	      ;; A hack:
	      (substring? ".so." file))
	  ;; Note: If you try to load a pre-compiled object, you must also provide a header!
	  (add-link! self file)]
	 [else (error 'emit-c:foreign "cannot load C extension from this type of file: ~s" file)]))))
  
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

(define null-lines (make-lines ""))

(define (slot-cons! obj fld x) (slot-set! obj fld (cons x (slot-ref obj fld))))

(define (cap x) (list x ";\n"))
(define (make-app rator rands)
  (list rator "("(insert-between ", " rands)")"))

(define (make-fun-header name args argty* return)
  (list return " " name "(" 
	(insert-between ", " (map (lambda (arg argty) (list argty " " arg)) args argty*))
	")"))

(define (emit-err where)
  (lambda (_) (error where "should not run into an emit!")))

(__spec type->name <emitC2-base> (self ty)
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
    
    [(Pointer ,name) (string-append "Pointer_" )]    
    [Timebase "Timebase"]

    [(Union ,name) (string-append "Union_" (sym2str name))]
    
    ))


(__spec ForeignSourceHook <emitC2-base> (self name callcode)
	callcode)

;; This will be where we will add fresh allocations to the ZCT.
;; Takes a type and a 'text' object representing a simple expression.
;; Returns a 'lines' object.
(__spec AllocHook <emitC2-base> (self ty simple-xp) null-lines)

;; This is the simplest way to package a source.
(define (wrap-source-as-plain-thunk name code)
  (make-lines (block `("void ",(Var name) "()") (lines-text code))))

;================================================================================
;;; "Continuations" used for syntax production.
;;; These are actually simple objects.

(define split-msg (gensym "split"))

(define (idk x)   (if (eq? x split-msg) (values null-lines idk) (make-lines x)))
(define (nullk x) (if (eq? x split-msg) (values null-lines idk) null-lines))
(__spec varbindk <emitC2-base> (self name typ)
  (define (split-k x)
    (if (eq? x 'split-msg) 
	;; Further splits have no effect:
	(values null-lines split-k)
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
	  (values null-lines	thekont)
	  (make-lines `(" ",(Var self vr)" = ",x";\n")))))
  thekont)

(define returnk
  (let ()
    (define thekont
      (lambda (x)      
	(if (eq? x split-msg)
	    (values null-lines	thekont)
	    (make-lines `("return ",x";\n")))))
    thekont))

;================================================================================

  
(__spec heap-type? <emitC2-base> (self ty) 
        (c-heap-allocated? ty (slot-ref self 'struct-defs) (slot-ref self 'union-types)))

;; This builds a set of top level function definitions that free all
;; the heap-allocated types present in the system.  It also builds a
;; table mapping types onto syntax-generating functions that call the
;; appropriate free-ing code.
;; 
;; Currently this routine mutates the field "free-fun-table" on the
;; fly, which stores the table of syntax-generating functions.  It
;; also returns a 'lines' datatype containing the prototypes and
;; definitions.
;;
;; This is this function that decides what freeing will be "open coded"
;; vs. relegated to separately defined functions.
;; 
;; [2008.11.07] This should be factored out into an earlier pass.  It
;; can generate WS code.
(__spec build-free-fun-table! <emitC2-base> (self heap-types)
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
		  (add-to-freefuns! ty default-specialized_fun))]
	     [(Union ,name)
	      (add-to-freefuns! ty default-specialized_fun)]
	     ;[Timebase #f]
	     )))
    heap-types)

  ;; free-fun-table is fully populated as of here.
  ;; Note, this is before we first call gen-decr-code.
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
					      (lines-text (gen-decr-code self ty (list "ptr."(sym2str fldname)) "")))
					 flds)))))]
			   [(List ,elt)
			    (define _elt (Type self elt))
			    (set! proto-acc (cons default-fun_name proto-acc))
			    ;; We always build an explicit function for freeing list types:
			    ;; Here's a hack to enable us to recursively free the same type.
			    (make-lines 
			     (block default-fun_name
				    (block (list "if(ptr)") ;; If not null.
					   `(
					     ;; Recursively decr tail:
					     ,(Type self `(List ,elt))" ptr2 = CDR(ptr, ",_elt");\n"
					     ,(lines-text (gen-decr-code self `(List ,elt) "ptr2" ""))
					     ;; If heap allocated, free the CAR:
					     ,(if (heap-type? self elt)
						  (lines-text (gen-decr-code self elt `("(*ptr)") ""))
						  "")
					     "FREECONS(ptr,",_elt ");\n"))))]
			   [(Array ,elt)
			    (if (not (heap-type? self elt))
				null-lines
				(let ([ind (Var self (unique-name "i"))])
				  (set! proto-acc (cons default-fun_name proto-acc))
				  (make-lines 
				   (block default-fun_name
					  `("int ",ind";\n"
					    ,(block `("for (",ind" = 0; ",ind" < ARRLEN(ptr); ",ind"++)")
						    ;;(lines-text ((cdr (loop elt)) `("ptr[",ind"]")))
						    (begin ;(loop elt) ;; For side effect
						      (lines-text (gen-decr-code self elt `("ptr[",ind"]") ""))))
					    "FREEARR(ptr);\n")))
				  ))]

			   ;; [2009.06.10] This case should work for unions too:
			    ;(guard (eq-any? structish 'Struct 'Union))
			   [(Union ,name) 
			    (set! proto-acc (cons default-fun_name proto-acc))
			    (let ([variants (get-variants self name)])
			      (make-lines 
			       (list 
				"/* Freeing union: "(sym2str name)"*/\n"
				;; TODO: If we generated C++ we could pass a reference here:
				(block default-fun_name
				  (block "switch(ptr.tag)"
				    (map (match-lambda ((,variantname ,ty))
					   (list 
					    "case "(sym2str variantname)" :\n"
					    (lines-text (gen-decr-code self ty (list "ptr.payload."(sym2str variantname)) ""))
					    "   break;\n"))
					 variants))))))]
			   )))
		  (slot-ref self 'free-fun-table)))])
    (append-lines 
     (make-lines (map (lambda (x) (list x ";\n")) proto-acc))
     final-defs)))

;; Get the variants of a particular union type.
(define (get-variants self name)
  (let loop ([ls (slot-ref self 'union-types)])
    (ASSERT (not (null? ls)))
    (if (eq? (caaar ls) name)
	(cdr (car ls))
	(loop (cdr ls)))))

;; [2008.11.07] This is not ideal, but for now I'm doing this in just
;; the same way as the free functions.  It would be better to do it in
;; a previous pass.  But doing as good of a job would require throwing
;; a bunch of new stuff into the intermediate language (for example,
;; tuple mutation...).
(__spec build-copy-fun-table! <emitC2-base> (self queue-types)
	
	)




;================================================================================
;;; Hooks for garbage collection.

;; These underlying methods do the actual code generation:
;; For the naive strategy we'de allow shared pointers but would need a CAS here.
;; Returns lines representing a block of code to do refcount incr.
;; 
;; TODO: We should really generate incr/decr *functions* for more
;; complex types, like we do with free functions.
(__spec gen-incr-code <emitC2-base> (self ty ptr msg)
  (match ty
    ;; Both of these types just decr the -1 offset:
    [(Array ,elt) (make-lines `("INCR_ARR_RC(" ,ptr"); /* ",msg", type: ",(format "~a" ty)" */\n"))]
    [(List ,elt)  (make-lines `("INCR_CONS_RC(",ptr"); /* ",msg", type: ",(format "~a" ty)" */\n"))]

    [(Ref ,[ty]) ty]
    ;; Could make this a separate function:
    [(Struct ,name) 
     (apply append-lines
	    (make-lines (format "/* Incr tuple refcount, Struct ~a */\n" name))
	    (map (match-lambda ((,fldname ,ty))
		   (gen-incr-code self ty (list ptr "." (sym2str fldname)) msg))
	      (cdr (assq name (slot-ref self 'struct-defs)))))]

    [(Union ,name)
     (let ([variants (get-variants self name)])
       (make-lines 
	(list (format "/* Incr union refcount, Union ~a */\n" name)
	      ;; TODO: If we generated C++ we could pass a reference here:
	      (block (list "switch("ptr".tag)")
		     (map (match-lambda ((,variantname ,ty))
			    (list 
			     "case "(sym2str variantname)" :\n"
			     (lines-text (gen-incr-code self ty (list ptr ".payload."(sym2str variantname)) msg))
			     "   break;\n"))
		       variants)))))]

    ;; Other types are not heap allocated:
    [,ty (guard (not (heap-type? self ty))) null-lines]
    ))

;; "ptr" should be text representing a C lvalue
;; returns "lines"
(__spec gen-decr-code <emitC2-base> (self ty ptr msg)
  (match ty
    [(,Container ,elt) (guard (memq Container '(Array List)))
     (define decr (if (eq? Container 'Array) "DECR_ARR_RC_PRED" "DECR_CONS_RC_PRED"))
     (make-lines 
      (block `("if (",decr"(",ptr")) /* ",msg", type: ",(format "~a" ty)" */ ")
	     (let ([freecode (lines-text (gen-free-code self ty ptr))])	      
	       (if #f ;; DEBUGGING TOGGLE.
		   (list "printf(\"  WS Freeing, type: "(Type self ty)", Pointer %p\\n\"," ptr");\n" freecode)
		   freecode))))]
    [(Ref ,[ty]) ty]
    ;; Could make this a separate function:
    [(Struct ,name) 
     (apply append-lines
	    (make-lines (format "/* Decr ~a tuple refcount, Struct ~a */\n" msg name))
	    ;; [2009.06.10] This also looks the same as the free code:
	    ;; Consider this:
	    ;;   "free_"(type->name `(Struct ,name))
	    ;; But it had better inline or this will copy the struct!
	    (map (match-lambda ((,fldname ,ty))
		   (gen-decr-code self ty (list ptr "." (sym2str fldname)) msg))
	      (cdr (assq name (slot-ref self 'struct-defs)))))]

    [(Union ,name) 
     ;; [2009.06.10] Can we just call the same code from build-free...
     (make-lines (list (format "/* Decr union refcount, Union ~a */\n" name)
		       "free_"(type->name self `(Union ,name))"("ptr");\n"
		       ))
     #;
     (let ([variants (let loop ([ls (slot-ref self 'union-types)])
		       (ASSERT (not (null? ls)))
		       (if (eq? (caaar ls) name)
			   (cdr (car ls))
			   (loop (cdr ls))))])
       (make-lines 
	(list 
	 ;; TODO: If we generated C++ we could pass a reference here:
	 (block default-fun_name
		(block "switch(ptr.tag)"
		       (map (match-lambda ((,variantname ,ty))
			      (list 
			       "case "(sym2str variantname)" :\n"
			       (lines-text (gen-decr-code self ty (list "ptr.payload."(sym2str variantname)) ""))
			       "   break;\n"))
			 variants))))))
     ]    


    [,ty (guard (not (heap-type? self ty))) null-lines]))

;; This generates free code for a type (using free-fun-table).
;; Should only be called for types that are actually heap allocated.
(define (strip-refs ty) (match ty [(Ref ,ty) ty] [,ty ty]))
(define _ignrd
  (specialise! gen-free-code <emitC2-base>
     (debug-return-contract lines?
       (lambda (next self ty ptr)
	 (next)
	 ;; Look up and apply the function for generating free code syntax:
	   ((cdr (ASSERT (assoc (strip-refs ty) (slot-ref self 'free-fun-table))))
	    ptr)))))

(define gen-copy-code 
  (lambda (self ty ptr)
    ;; Look up and apply the function for generating free code syntax:
    "// COPY IT\n"
    #;
    ((cdr (ASSERT (assoc (strip-refs ty) (slot-ref self 'copy-fun-table))))
     ptr)))

;; These methods represent the actions to take when encountering local or heap refs.
;; All return a block of code in a "lines" datatype.
;; The default version represents plain old reference counting.
(__spec incr-local-refcount <emitC2-base> (self ty ptr) (gen-incr-code self ty ptr "local"))
(__spec decr-local-refcount <emitC2-base> (self ty ptr) (gen-decr-code self ty ptr "local"))

(__spec incr-heap-refcount <emitC2-base> (self ty ptr) (gen-incr-code self ty ptr "heap"))
(__spec decr-heap-refcount <emitC2-base> (self ty ptr) (gen-decr-code self ty ptr "heap"))


(define (ifthreads block)
  (append-lines (make-lines "#ifdef WS_THREADED\n") 
		block
		(make-lines "#endif\n")
		))
;; We don't need to worry about queue refcounts in the queue-free depth-first single thread context.
(__spec incr-queue-refcount <emitC2-base> (self ty ptr) (ifthreads (gen-incr-code self ty ptr "queue")))
(__spec decr-queue-refcount <emitC2-base> (self ty ptr) (ifthreads (gen-decr-code self ty ptr "queue")))

;; TODO -- not used yet
(__spec potential-collect-point <emitC2-base> (self) null-lines)


;; TODO -- not used yet
;; Do anything special that's required as a value is sent off through an emit.
;; Currently, with a pure depth-first strategy, reference count does not need to be affected by "emit".
(__spec say-goodbye <emitC2-base> (self ty ptr) null-lines)


;================================================================================

;; [2007.12.04] TEMP!!!!! SHOULD NOT DUPLICATE CODE
(__spec Const <emitC2-base> (self datum wrap)

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
      (wrap 
       (if (not (= datum datum)) ;(or (eq? datum +nan.0) (eq? datum -nan.0))
	   "(0.0/0.0)" ;(inspect/continue datum);"(0.0/0.0)"
	   (format "~aF" datum)))]
     [(double? datum)      
      (let ([datum (double-val datum)])
	(DEBUGASSERT number? datum)
	(if (not (= datum datum)) "(0.0L/0.0L)" (format "~aL" datum)))]
     [(cflonum? datum) (wrap (format "(~a)(~a + ~afi)" (Type self 'Complex)
				     (real-part datum)
				     (imag-part datum)))]
     [(eq? datum 'nulltimebase)  (wrap "WSNULLTIMEBASE")]
     [(integer? datum) 
      ;; [2012.02.02] Well this commented line looks wrong, but it
      ;; probably is necessary to do something here.  For one thing,
      ;; should comply with C's conventions for suffixes on numeric
      ;; literals:
      ;; 
      ;(wrap (format "(~a)~a" (Type self 'Int) datum))
      (wrap (number->string datum))
      ]

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



(__spec Type <emitC2-base> (self ty)
  (match ty ;; No recursion!
    [Bool    "ws_bool_t"]
    [Int     "int"]
    [Int16   "int16_t"]
    [Int32   "int32_t"]
    [Int64   "int64_t"]
    [Uint16  "uint16_t"]
    [Uint8   "uint8_t"]
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

    [(Pointer ,cname) 
     (if (string=? cname "")
	 (error 'emitC2:Type "Pointer type cannot have \"\" as its C name")
	   cname)]

    ;; This is an unused value.
    [(VQueue ,_) "char"]
    [Timebase    "ws_timebase_t"]

    [(Union ,name) `("struct ",(sym2str name))]
    ))


;; Generate a struct definition.
(__spec StructDef <emitC2-base> (self entry)
     (match entry
       [(,(sym2str -> name) (,[sym2str -> fld*] ,typ*) ...)
	(let ([tmpargs (map (lambda (_) (sym2str (unique-name 'tmp))) fld*)]
	      [ctype* (map (curry Type self) typ*)])
	  `(,(block `("struct ",name)
		    (map (lambda (ctype fld) `[,ctype " " ,fld ";\n"])
		      ctype* fld*))
	    ";\n"
            ))]))

(define (UnionDef self)
  (define tagty  (Type self tag-type)) ;; from convert-sums-to-tuples
  (lambda (def)
    (define (Ty x) (Type self x))
    (match def
      [((,[sym2str -> name]) (,[sym2str -> tc*] ,[Ty -> ty*]) ...)      
       (text->string
	(list (block `("struct ",name)
		      (list
		       tagty" tag;\n"
		       (block "union"
			      (map (lambda (tc ty) (list ty " " tc ";\n"))
				tc* ty*))
		       " payload;\n"))
	       ";\n"
	       (block (list "enum "name"_enum")
		      (list (insert-between ", " tc*) "\n"))
	       ";\n"
	       ;"uint32_t getTotalByteSize(const struct "name" &e) {\n"
	       ;"  return sizeof(e.tag) + sizeof(e.payload);\n"
	       ;"}\n"
	       ))
       ])))

;; For the set of types that are printable at this point, we can use a simple printf flag.
(__spec type->printf-flag <emitC2-base> (self ty)
  (match ty
    [#()    "()"]
    [String "%s"]
    [Char   "%c"]
    [(Array Char) "%s"] ;; HACK: These are strings in disguise.
    [Bool   "%d"]
    [Int    "%d"]
    [Int16  "%hd"]
    [Int32  "%ld"]
    [Int64  "%lld"]
    [Uint16 "%hu"]
    [Uint8  "%hu"]
    [Float  "%g"]
    [Double "%.15g"]
    [#() "()"]
    [(Union ,_) "<union>"]
    [Timebase "<timebase>"]
    [(Pointer ,_) "%p"]
    [(,args ... -> ,ret) ;"%p"
     (error 'type->printf-flag 
	    "Cannot print a function type! ~s" ty)]))


;(define Var mangle-name)
(__spec Var <emitC2-base> (self x) (sym2str x))

;; These are the expressions that make valid operands (post flattening)
;;   .returns A string.  Could return an "expression".
(__spec Simple <emitC2-base> (self expr)
  (match expr
    [nulltimebase (Const self 'nulltimebase id)]
    [,v (guard (symbol? v)) (ASSERT (not (wavescript-primitive? v))) (Var self v)]
    ;[',c (format "~a" c)] ;;TEMPTEMP

    ;; PolyConstants:
    [(assert-type ,ty Array:null) `("((",(Type self ty)")0) /* Array:null */")] ;; A null pointer. This choice is debatable.
    [(assert-type ,ty '())        `("((",(Type self ty)")0)")] ;; A null pointer.       
    ['() (error 'EmitC2:Simple "null list without type annotation")]

    ;; All other constants:
    [',c (Const self c (lambda (x) x))]

    [(tuple) (list "(("(Type self '#())")0)")]
    [(deref ,var) (ASSERT (not (wavescript-primitive? var))) (Var self var)]
    ;[(assert-type ,t '())  (wrap (PolyConst '() t))]
    ;['() (error 'Simple "null list without type annotation")]
       
    [(assert-type ,_ ,[x]) x]
    [,else (error 'Simple "<emitC2-base> not simple expression: ~s" else)]
    ))

(__spec TyAndSimple <emitC2-base> (self)
  (lambda (expr)
    (match expr
      [(assert-type ,ty ,_)  (values ty (Simple self expr))])))



;; Generates code for an emit.  (curried)
;; .param down*   A list of sinks (names of functions) to emit to.
;;                These can also be (NUM NAME) pairs for indexed ports.
;; .param ty      Type of outgoing values.
;; .param incr?   Should the refcount be incremented on the way out?
;; .returns lines representing command-code
(__spec Emit <emitC2-base> (self down* ty)
  ;;(ASSERT (not (null? down*)))
  (lambda (expr)
    (ASSERT simple-expr? expr)
    (let ([element (Simple self expr)])
      (make-lines (map (lambda (down)
			 (cap (make-app "EMIT" (list element (Type self ty) (Var self down)))))
		    down*)))))

;; This is used for local bindings.  But it does not increment the reference count itself.
(__spec Binding <emitC2-base> (self)
  (debug-return-contract Binding lines?
   (lambda (cb)
    ;(define val (Value (lambda (_) (error 'Binding "should not run into an emit!"))))
    (match cb
      [(,vr ,ty ,rhs) ;(,[Var -> v] ,[Type -> t] ,rhs) 
       ((Value self) rhs (varbindk self vr ty))]

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
(__spec DummyInit <emitC2-base> (self ty) "")

;; This is used for global and static bindings.
;; This separately returns the type decl and the initialization code.
;; This version also DOES inject a refcount incr.
(__spec SplitBinding <emitC2-base> (self)
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
	       (define-values (bnds init) ((SplitBinding self) bind))
	       (values (append-lines bnds bodbnds)
		       (append-lines init bodinit))
	       ]
	      [,else 
	       (values (append-lines ((Binding self) bind) bodbnds)
		       bodinit)
	       ]
	      )]

	   	   ;; HACK FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME 
	   ;; HACK FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME 
	   [(make-struct . ,rest)
	    ;(inspect (cons 'make-struct rest))
	    ;; Here we just do the binding immediately, and return nothing for the initialization:
	    (values ((Binding self) (list vr ty rhs))
		    null-lines
		    )]
	   [(assert-type ,ty (make-struct . ,rest))
	    ;(inspect (cons 'ASSERTmake-struct rest))
	    ;; Here we just do the binding immediately, and return nothing for the initialization:
	    (values ((Binding self) (list vr ty rhs))
		    null-lines
		    )]

	   ;; Flip
	   [(assert-type ,ty (static-allocate ,rhs)) 
	    (loop `(static-allocate (assert-type ,ty ,rhs)))]

	   ;; [2008.08.06] Here we begin to support monomorphic closed functions:
	   [(lambda (,arg* ...) ,_ ,bod)
	    (match ty 
	      [(,argty* ... -> ,retty)
	       (printf "EMITTING LAMBDA, args ~s\n" arg*)
	       (values 
		(make-lines 
		 (list "/* WS function definition */\n"
		  (block (make-fun-header (Var self vr) (map (curry Var self) arg*)
					  (map (curry Type self) argty*) (Type self retty)
			 ;self vr argty* retty 
					  )
			 (lines-text ((Value self) bod returnk)))))
		;; Don't need any initialization code:
		null-lines)])
	    ;(error 'emitC2:SplitBinding "got a function, not supported yet.")
	    ]
	  
	   [,rhs
	    ;; We derive a setter continuation by "splitting" the varbind continuation:       
	    (values (make-lines `(,(Type self ty)" ",(Var self vr)" ",(DummyInit self ty)";\n"))
		    ((Value self) rhs (setterk self vr ty)))]))]
      [,oth (error 'SplitBinding "Bad Binding, got ~s" oth)]
      )))


;; For the x86 version we currently don't statically allocate anything.
(__spec StaticAllocate <emitC2-base> (self lhs ty rhs)
  (define-values (decl initcode)
    ((SplitBinding self) (list lhs ty rhs)))
  ;; We do need to add a refcount increment.  Something statically
  ;; allocated will never reach zero.  
  (values decl
	  ;; [2008.07.30] Trying without this increment:
	  ;initcode
	  (append-lines initcode
	     (gen-incr-code self ty (Var self lhs) "static top-incr"))
	  ))

#;
(__spec Let <emitC2-base> (self form recur)
  (match form     
    [(([,lhs ,ty ,rhs]) ,[recur -> bod])
     (append-lines ((Binding self) (list lhs ty rhs))
		   (ASSERT lines? bod))]))
(__spec Let <emitC2-base> (self form recur)
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
		 (list (lines-text ((Binding self) (list tmp `(Struct ,sname) _bod)))
		       (lines-text (recur tmp)))))
	  ]
	 [,oth (recur oth)]))

     ;; A hack on a hack, make-struct must be handled differently:

;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME      
;; [2008.04.13] MIGRATING THIS HACK TO SPLITBINDING
     #;	
     (let-values ([(bind* init*) ((SplitBinding self)
				     (list lhs ty rhs))])
	  (let ([result (append-lines bind* init* bod)])
	    (make-lines (block "" (lines-text result)))))
;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME      

     (match (peel-annotations rhs)
       [(make-struct . ,_)
	(let ([result (append-lines ((Binding self) (list lhs ty rhs))
				    bod)])
	  (make-lines (block "" (lines-text result))))]
       [,_ 
	(let-values ([(bind* init*) ((SplitBinding self)
				     (list lhs ty rhs))])
	  (let ([result (append-lines bind* init* bod)])
	    (make-lines (block "" (lines-text result)))))])]))

(__spec Effect <emitC2-base> (self)
  (debug-return-contract Effect lines?
  (lambda (xp)
    (define (Loop x) ((Effect self) x)) ;; Eta reducing this BREAKS things.
    (define (Simp x) (Simple self x))
    (define (Vr x)   (Var self x))
    (match xp ;; no recursion
      ['UNIT   null-lines]
      [(tuple) null-lines] ;; This should be an error.
      ;; Thanks to insert-refcounts this can get in here:
      [,v (guard (symbol? v)) null-lines]

      ;; Call the eponymous method:
      [(incr-heap-refcount  ,ty ,[Simp -> val]) (incr-heap-refcount  self ty val)]
      [(decr-heap-refcount  ,ty ,[Simp -> val]) (decr-heap-refcount  self ty val)]
      [(incr-local-refcount ,ty ,[Simp -> val]) (incr-local-refcount self ty val)]
      [(decr-local-refcount ,ty ,[Simp -> val]) (decr-local-refcount self ty val)]
      [(incr-queue-refcount ,ty ,[Simp -> val]) (incr-queue-refcount self ty val)]
      [(decr-queue-refcount ,ty ,[Simp -> val]) (decr-queue-refcount self ty val)]

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
		  (let-values ([(bind* init*) ((SplitBinding self)
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
       ;; Set! changes Ref objects, which are on the heap:
       (make-lines `(,v" = ",x";\n"))]

      [(Array:set ,[(TyAndSimple self) -> ty arr] ,[Simp -> ind] ,[Simp -> val])
       (let-match ([(Array ,elt) ty]) (make-lines `(,arr"[",ind"] = ",val";\n")))]

      [(_emit_to ,to ,props ,vq ,x)
       (match vq
	 [(assert-type (VQueue ,elt) ,_)
	  ((Emit self (list to) elt) x)])]

      [(begin ,[Loop -> e*] ...) 
       (DEBUGASSERT (andmap lines? e*))
       (apply append-lines e*)]

      ;; DUPLICATED CASES WITH Value:
      ;; ========================================
      [(let . ,_) (Let self _ (lambda (x) ((Effect self) x)))]
      ;; ========================================

       [(app ,[Simp -> rator] ,[Simp -> rands] ...)
	;(ASSERT symbol? rator)
	(make-lines (list (make-app rator rands) ";\n"))]

      [(__wserror_ARRAY ,[Simp -> str]) 
       (make-lines (list "wserror_builtin("str");\n"))]

      ;; Note: ASSUMES __BYTE_ORDER == __LITTLE_ENDIAN
      [(__type_unsafe_write (assert-type ,ty ,[Simp -> xp]) ,[Simp -> buf] ,[Simp -> offset])
       ;; Simply cast and assign:
       (make-lines `("*((",(Type self ty)" *)(",buf" + ",offset")) = ",xp"; /* type unsafe write */ \n"))]
      [(__type_unsafe_write . ,_) (error 'emitC2:Prim "invalid form: ~s" (cons '__type_unsafe_write _))]

      [(fifo-copy-outgoing ,elt ,down)
       (define ff (list "& "(symbol->string down) "_queue"))
       (make-lines
	(list 
	 "#ifdef WS_DISJOINT_HEAPS\n"
	 (block ""
	       (list 
"  // Copy & Release enqueued FIFO contents.
  int i;
  int pending = wsfifo_pending("ff"); 
  for(i=0; i < pending ; i++) { 
    void* ptr = wsfifo_recheck("ff");
    "(gen-copy-code self elt "ptr")"
    wsfifo_release_one("ff");
  }
"))
	 "#endif\n"))]

      ))))

(define (valid-foreign-type! ty)
  (unless
      (match ty
	[(Array ,[ty]) ty]
	[,num (guard (scalar-type? num)) #t]
	[#() #t]
	[(Pointer ,ty) #t]
	[,_ #f])
    (error 'emitC2:foreign "type not currently supported for foreign functions: ~s" ty)))

;; The continuation k is invoked on a piece of text representing the return expression.
;; k is expected to return text of the form "lines" that stores away this result.
(__spec Value <emitC2-base> (self)
  (debug-return-contract Value lines?
   (lambda (xp kont)
     (define (recur x) ((Value self) x kont))
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
        (ASSERT (curry vector-andmap char?) vec)
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
	   ((Value self) `(assert-type (Array Char) (Array:makeUNSAFE ',(vector-length vec))) newk)
	   (make-lines
	    (format "memcpy(~a, ~s, ~s);\n" (text->string _tmp)
		    str copylen))
	   (kont _tmp)))]

       ;; This means we're at a end of a control path we will never reach.
       ;; HACK: There's surely a nicer way to do this....
       ;; [2008.04.05] But currently I just throw out the continuation.
       ['BOTTOM (make-lines "/* BOTTOM CTRL PATH */\n")]
       [',tb (guard (timebase? tb)) 	     
	     (kont (format "TIMEBASE(~a)" (timebase-num tb)))]

       [,simp (guard (simple-expr? simp)) (kont (Simple self simp))]
       
       ;; This doesn't change the runtime rep at all.
       [(assert-type ,_ (Mutable:ref ,x)) (kont (Simple self x))]
       [(begin ,e) (recur e)]










       [(begin ,e1 ,e* ...)
	(define first ((Effect self) e1))
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
	       ,(indent (lines-text ((Value self) conseq newk)) "  ")
	       "} else {\n"
	       ,(indent (lines-text ((Value self) altern newk)) "  ")
	       "}\n")))]

       [(let . ,_) (Let self _ recur)]

       ;; Should only occur as the rvalue in a binding, which is ok.
       ;; But this is not a general expression in C:
       [(make-struct ,name ,[Simp -> arg*] ...)
	(kont `("{",(insert-between ", " arg*)"}"))]
       [(struct-ref ,type ,fld ,[Simp -> x])
	(kont `("(",x "." ,(sym2str fld)")"))]

       [(app ,[Simp -> rator] ,[Simp -> rands] ...)
	;(ASSERT symbol? rator)
	(kont (make-app rator rands))]
       
       [(__foreign ',cname ',files ',ty)
	(for-each (add-file! self) files)
	;; The "value" returned is ignored:
	(kont "0")]
       [(__foreign . ,_)
	(error 'emit-c2 "unhandled foreign form: ~s" `(__foreign ,@_))]
              
       ;; [2009.06.08] Transplanted from emit-c.ss
       [(wscase ,[Simp -> x] ((,tag* . ,tc*) (lambda (,v*) (,ty*) ,bod*)) ...)
	(let-values ([(lines newk) (kont split-msg)])
	  (make-lines
	   (list (lines-text lines)
		 (block `("switch (",x".tag)")
			(map (lambda (tc v ty bod)
			       (define TC (sym2str tc))
			       `("case ",TC": {\n"
				 ;; Simply bind a reference to the struct:  	   
				 ;; In the orig backend we bound this as a reference!!
				 "  // This should be a reference... could maybe use a hack like #define\n"
				 "  ",(Type self ty)" ",(sym2str v) " = ",x".payload.",TC";\n"
				 ,(lines-text ((Value self) bod newk))
				 
			      ;(Block (tenv-extend tenv (list v) (list ty)))
			   ;,(indent ( name "" bod) "  ")
			   "  }  break;\n"))
		    tc* v* ty* bod*)
		  ))))]
       
       [(cast-variant-to-parent ,[sym2str -> tc] ,ty 
				(assert-type ,variant-ty ,e))	
	(let* ([ty (Type self ty)]
	       [newvar (unique-name 'sumbld)]
	       ;; TODO: check if the C compiler can optimize away spurious copies with (=) on struct types:
	       ;; If it can't, we should not introduce a new variable sumresult if we already have a varbindk.
	       ;; (But that still leaves us with the question of how we ge at the var.)
	       [name   (sym2str (unique-name 'sumresult))])
	  ;; First build the variant itself:
	  (make-lines
	   (list (lines-text ((Value self) e (varbindk self newvar variant-ty)))
		 ;; Build the parent container:
		 ty" "name"; // Casting to union parent type\n" ;; decl
		 ;; Next store the result within the parent union type.
		 name".payload."tc" = "(sym2str newvar)";\n"
		 ;; And store the tag:
		 name".tag = "tc";\n"
		 (lines-text (kont name))
		 )))]

       ;; [2008.02.26] Had a problem with substituting this pattern for 'ty': (,argty* ... -> ,retty)
       ;;
       ;; For the time being there is no conversion of the arguments.
       ;; All WS values are passed directly to C, which had better be
       ;; able to deal with their representations.
       [(foreign-app ',name (assert-type ,ty ,ignored) ,[Simp -> rand*] ...)
	(match ty 
	  [(,argty* ... -> ,retty)
	   (for-each valid-foreign-type! argty*)
	   (valid-foreign-type! retty)
	   
	   ;; Here's a hack for when the return type is unit/void.  This will let us slip "call" forms through.
	   (match retty 
	     [#() ;; The problem is that you can't do anything with a "void" value in C:
	      (append-lines (make-lines `(,name"(",(insert-between ", " rand*)");\n"))
			    (kont (Const self 'UNIT (lambda (x) x))))]
	     ;; [2008.07.03] Tuples are handled in a special way.
	     ;; This is not an official, supported feature of the FFI,
	     ;; but I'm allowing C functions to return "tuples" to WS.
	     ;; Basically, they must return a void* to some kind of
	     ;; struct with the same memory layout as the WS tuple.
	     ;; The WS code then frees that memory.  This introtudes a
	     ;; portability problem, however.  This won't be fun to
	     ;; mimic in Scheme or ML.
	     [(Struct ,str)
	      (let ([tmp (Var self (unique-name "tmptup"))])
		(append-lines (make-lines `(,(Type self retty)"* ",tmp" = ",name"(",(insert-between ", " rand*)"); /* foreign app, return tuple */ \n"))
			      (kont `("(*",tmp")"))
			      (make-lines `("free(",tmp");\n"))))] ;; Does NOT use WSFREE, this is external.
	     [,else (kont `(,name"(",(insert-between ", " rand*)") /* foreign app */ "))])])]
       [(foreign-app . ,_) (error 'emitC2 "foreign-app without type annotation: ~s" (cons 'foreign-app _))]

       [(,prim ,rand* ...) (guard (wavescript-primitive? prim))
	(PrimApp self (cons prim rand*) kont #f )]
       [(assert-type ,ty (,prim ,rand* ...)) (guard (wavescript-primitive? prim))
	(PrimApp self (cons prim rand*) kont ty)]
              
       [(assert-type ,ty ,e) (recur e)]
       ))))


;(define (array-constructor-codegen self len init ty kont)
(__spec array-constructor-codegen <emitC2-base> (self len init ty kont)
  (match ty
    [(Array ,elt)
					;(k `("arrayMake(sizeof(",elt"), "len", "init")"))
     
					;(when (and (heap-allocated? elt global-struct-defs) (not init))
					;(error array-constructor-codegen "DANGER DANGER: ~s" elt))
     
     ;; We fill with zeros if we've got Array:make with a zero scalar.
     ;; OR if we've got a makeUNSAFE with pointers (have to put in null pointers).
     (define scalar? (not (heap-type? self elt)))
     (define zero-fill?
       (or (and init (wszero? elt init))
	   (and (not init) (heap-type? self elt))))

     ;; TODO: There's duplication between this and the allocation
     ;; routines in ws.h This should use those allocation routines,
     ;; which in turn will mean generalizing them a bit more (malloc/calloc).
     (let* ([_elt (Type self elt)]
	    [size `("(sizeof(",_elt") * ",len") + RCSIZE + ARRLENSIZE")]
	    [tmp (Var self (unique-name "arrtmp"))]
	    [postfix (if scalar? "_SCALAR" "")]
	    [alloc (if zero-fill?
		       `("WSCALLOC",postfix"(",size", 1)")
		       `("WSMALLOC",postfix"(",size")"))]
	    [cast `("(",_elt"*)",tmp)])
       (append-lines 
	(make-lines 
	 (list
	  `("int* ",tmp" = (int*)0;\n")
	  ;; This will handle Array:null:
	  (block `("if (",len" > 0)")
		 `(,tmp" = (int*)((char*)",alloc" + RCSIZE + ARRLENSIZE);\n"
		       ,(if (eq-any? (wsc2-gc-mode) 'none 'boehm)  ""
			    (list "CLEAR_ARR_RC("tmp");\n"))
		       "SETARRLEN(",tmp", ",len");\n"  
		       ;; Now fill the array, if we need to:
		       ,(if (and init (not zero-fill?))
			    (let ([i (unique-name "i")]
				  [tmp2 (unique-name "arrtmpb")]
				  [lensub1 (unique-name "lenmin1")])
			      (list `(,_elt"* ",(Var self tmp2) " = ",cast";\n")
				    `("int ",(Var self lensub1) " = ",len" - 1;\n")
				    (lines-text
				     ((Effect self)
				      `(for (,i '0 ,lensub1)
					   (Array:set (assert-type (Array ,elt) ,tmp2) ,i ,init))))))
			    "")))))
	(AllocHook self ty cast)
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
	    
	    ;; [2009.06.10] Actually, given our implementation of structs, it seems like this could be zero:
	    [(Struct ,_) #f]
	    [(Union ,_) #f]

	    [#() #t]
	    
	    [,ty (error 'wszero? "Not yet handling zeros for this type: ~s, obj ~s" ty obj)])]))  
  
(define __
  (specialise! PrimApp <emitC2-base> 
   ;; [2008.05.10] FIXME: This failed the return contract for the __real__ case.
   (debug-return-contract PrimApp lines?
     (lambda (next self app kont mayberetty)
       (define ___ (next))
     
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
	    [(roundF roundD)                      "round"]
	    [(sqrtI sqrtF sqrtD)                   "sqrt"]
	    ;; These are from the WS runtime library:
	    [(moduloI)                          "moduloI"]
	    [(sqrtC)                              "csqrt"]

	    [(Secret:newTimebase)              "TIMEBASE"]

	    [else (error 'emitC2:PrimApp:SimplePrim "primitive not specifically handled: ~s" var)]
	    ))))
     
     ;; Handle primitives:
     (match app
       ;; Refs and sets are pure simplicity:
       [(Array:ref ,arr ,ind)
	;; TODO: Insert bounds checks!!
	(PrimApp self `(Array:refUNSAFE ,arr ,ind) kont mayberetty)]
       [(Array:refUNSAFE ,[Simp -> arr] ,[Simp -> ind])
	(kont `(,arr"[",ind"]"))]

       ;; Using some simple C macros here:
       [(car (assert-type (List ,elt) ,[Simp -> pr])) (kont `("CAR(",pr", ",(Type self elt)")"))]
       [(cdr (assert-type (List ,elt) ,[Simp -> pr])) (kont `("CDR(",pr", ",(Type self elt)")"))]

       [(List:is_null ,[Simp -> pr]) (kont `("(",pr" == 0)"))]

       [(cons ,[Simp -> hd] ,[Simp -> tl])
	(ASSERT mayberetty)
	(match mayberetty
	  [(List ,elt)
	   (let ([_elt (Type self elt)]
		 [tmp (Var self (unique-name "tmpcell"))]
		 [ty (Type self mayberetty)])
	     (append-lines 
	      (make-lines `(,ty" ",tmp" = (",ty")CONSCELL(",_elt");\n"
			       ,(if (eq-any? (wsc2-gc-mode) 'none 'boehm)  ""
				    (list "CLEAR_CONS_RC("tmp");\n"))
			       "SETCDR(",tmp", ",tl", ",_elt");\n"  
			       "SETCAR(",tmp", ",hd", ",_elt");\n"
			       ))
	      (AllocHook self mayberetty tmp)
	      (kont tmp)))])]
       [(Array:make ,[Simp -> len] ,init) (array-constructor-codegen self len init mayberetty kont)]
       [(Array:makeUNSAFE ,[Simp -> len]) (array-constructor-codegen self len #f   mayberetty kont)]

       [(Array:length ,[Simp -> arr]) (kont `("ARRLEN(",arr")"))]

       [(max ,[Simp -> a] ,[Simp -> b]) (kont `("(",a" > ",b" ? ",a" :",b")"))]
       [(min ,[Simp -> a] ,[Simp -> b]) (kont `("(",a" < ",b" ? ",a" :",b")"))]

       [(randomI ,[Simp -> bound]) (kont `("(rand() % ",bound")"))]

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
	(add-include! self (** "\"" (WAVESCRIPTD) "/src/linked_lib/fftw_wrappers.c\""))
	(add-link! self "libfftw3f.so")		
	(append-lines ((Binding self) (list len0 'Int `(Array:length ,arr)))
		      ((Binding self) (list len1 'Int (if inverse? `(_-_ ,len0 '1) `(/_ ,len0 '2) )))
		      ((Binding self) (list len2 'Int (if inverse? `(*_ ,len1 '2) `(_+_ ,len1 '1) )))
		      ((Binding self)
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
	(unless (or (scalar-type? ty) (eq? ty 'Timebase))
	  (error 'emitC2 "wsequal? should not be used at this type at this late phase: ~a" ty))
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
	      ((Binding self) 
	       (list str '(Array Char) `(assert-type (Array Char) (Array:makeUNSAFE ',max-show-size))))
	      (make-lines (list 			   
			   "int "_realsize" = snprintf("_str", "
			           _max", \""
				   ;; [2012.02.02] Hit an error, adding a cast here.
				   ;; Probably the underlying problem is with "Const":
				   (type->printf-flag self ty)"\", ("(Type self ty)")"obj");\n"
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

	[(intToChar ,[Simp -> e]) (kont `("(ws_char_t)",e))]
       
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
 	[(complexToInt16 ,e)   (kont `("(int16_t)" ,(lines-text (PrimApp self `(complexToFloat ,e) make-lines 'Float))))]
 	[(complexToInt64 ,e)   (kont `("(int64_t)" ,(lines-text (PrimApp self `(complexToFloat ,e) make-lines 'Float))))]
 	[(complexToInt ,e)     (kont `("(int)"     ,(lines-text (PrimApp self `(complexToFloat ,e) make-lines 'Float))))]
 	[(complexToDouble ,e)  (kont `("(double)"  ,(lines-text (PrimApp self `(complexToFloat ,e) make-lines 'Float))))]
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

	;; Causes overflow problems if clock_t is integral:
	;[(clock) (kont "(clock() * 1000 / CLOCKS_PER_SEC)")]
	[(clock) (kont "(clock() * ((double)1000 / CLOCKS_PER_SEC))")]
	[(realtime) 
	 (define tmp (symbol->string (unique-name "tmp")))
	 (make-lines
	  `("struct timeval ",tmp";\n"
	    "gettimeofday(&",tmp", NULL);\n"
	    ,(lines-text (kont `("(",tmp".tv_sec * 1000 + ",tmp".tv_usec / 1000)")))))]

	[(__type_unsafe_read ,[Simp -> buf] ,[Simp -> offset]) (ASSERT mayberetty)
	 (kont `("(*((",(Type self mayberetty)" *)(",buf" + ",offset"))) /* type_unsafe_read */"))]

	[(getID) (kont "0 /* NodeID of PC-server */")]

	[(ptrMakeNull) (kont "0 /* ptrMakeNull */")]
	[(ptrIsNull ,[Simp -> p]) (kont `("(",p " == 0)" ))]

	;; This is kind of silly, but we need to reallocate it just so that it has the metadata!
	[(ptrToArray ,[Simp -> ptr] ,[Simp -> len])	 
	 (match mayberetty
	   [(Array ,elt)	    
	    (define tmp (unique-name "newarrtmp"))
	    (define _tmp (Var self tmp))
	    (unless (scalar-type? elt)
	      (error 'emitC2:PrimApp "ptrToArray is only implemented for arrays of scalar types"))
	    (make-lines 
	     (block ""
	     (list 
	      (lines-text (array-constructor-codegen self len #f `(Array ,elt) (varbindk self tmp `(Array ,elt))))
	      (list "memcpy("_tmp", "ptr", "len" * sizeof("(Type self elt)"));\n")
	      (lines-text (kont _tmp)))))])]
	
       [(Array:makeUNSAFE ,[Simp -> len]) (array-constructor-codegen self len #f   mayberetty kont)]       
       [(Array:make ,[Simp -> len] ,init) (array-constructor-codegen self len init mayberetty kont)]       

	[(,other ,[Simp -> rand*] ...)
	 (kont `(,(SimplePrim other) "(" ,(insert-between ", " rand*) ")"))]
       )  
       ))))



;; .param srccode* blocks of code (lines) for the body of each source.
(__spec BuildTimerSourceDriver <emitC2-base> (self srcname* srccode* srcrates*)
   (define (normal-rate? r) (and r (> r 0)))
   (ASSERT "BuildTimerSourceDriver: must have same number of names, code, and rates"
	   all-equal?
	   (list (length srcname*) (length srccode*) (length srcrates*)))
  
   ;; There are three options here.
   ;;  (1) We run a system of virtual timers.
   ;;  (2) We are driven by a serial port connected to a mote.
   ;;  (3) We are driven by "foreign_source".
   (cond 
    ;; Option (1): Virtual timers.
    [(not (null? srcname*))

      ;; If the rate is #f we don't build in that entry:
     (let-match (;; As a special convention, we treat timers with zero rates differently:
		 [([,zerosrc* ,zerosrccode*] ...)
		  (filter id 
		    (map (lambda (nm code rate)
			   (if (zero? rate) (list nm code) #f))
		      srcname* srccode* srcrates*))]

		 ;; As ANOTHER special convention we do the same for negative rates [2011.05.20].
		 ;; They run, not infinitely fast, but "at least as fast as anybody else".
		 [([,negsrc* ,negsrccode*] ...)
		  (filter id 
		    (map (lambda (nm code rate) (if (< rate 0) (list nm code) #f))
		      srcname* srccode* srcrates*))]

		 ;; Normal, positive timer rates:
		 [([,srcname* ,srccode* ,srcrates*] ...)
		  (filter id
		    (map (lambda (nm code rate)
			   (if (normal-rate? rate) (list nm code rate) #f))
		      srcname* srccode* srcrates*))]
		 )
	(when (>= (wavescript-verbosity) 1)  ;; TEMP 
	  (printf "   TIMER RATES: ~s\n" srcrates*)
	  (printf "   ZERO TIMERS ~s\n" zerosrc*)
	  (printf "   NEG  TIMERS ~s\n" negsrc*))
	;; This is a hack: rather than maintain a sorted list of
	;; upcoming events, we simply compute a common tick frequency
	;; and go tick by tick:
	(let-values  
	    ([(counter_marks timestep_ms)
	      (cond
	       [(= 1 (length srcrates*)) (values '(1) (inexact (/ 1000 (car srcrates*))))] ;; Simple case.
	       [(andmap integer? srcrates*)
		(let ([srcrates* (map exact srcrates*)])
		  (let ([common-rate (apply lcm srcrates*)])
		    (values (map (lambda (rate)
				   (exact (quotient common-rate rate)))
			      srcrates*)
			    (inexact (/ 1000 common-rate)))))]
	       [else (error 'timer "non integer rates not handled yet: ~s" srcrates*)])])
	  (make-lines        
	   (block (list "int main(int argc, "(Type self `(Array (Array Char)))" argv)" )
		  (list 
		   (map (lambda (name) (format "int counter_~a = 0;\n" name)) srcname*)
		   "  initState();\n"
		   "ws_parse_options(argc,argv); /* [2008.08.27] Moving to after initState */ \n" 
		   (Type self 'Bool)" dummy ="(Const self #t id)";\n" ;; Hack for java.
		   
		   "// Insert calls to those timers executing only once (with zero rates)\n"
		   (map lines-text zerosrccode*)

		   "// Next, run in a timer loop indefinitely\n"
		   (block "while(dummy && !stopalltimers)"
			  (list (map (lambda (name) (format "counter_~a++;\n" name)) srcname*)
				"VIRTTICK();\n"

				"int fired = 0;\n"

				"// And finally call the normal-timers on ticks where it is appropriate:\n"
				(map (lambda (name code mark)
				       (block (format "if (counter_~a == ~a)" name mark)
					      ;; Execute the code for this source.
					      (list (lines-text code)
						    (format "counter_~a = 0;\n" name)
						    "fired = 1;\n"
						    )))
				  srcname* srccode* counter_marks)

				"// The timesteps where we fire are the only 'real' ones:\n"
				(block (if (null? srcname*) "if (1)" "if (fired)")
				  (if (null? negsrc*)
				      (format "WAIT_TICKS(~a);\n" timestep_ms)
				      (list 
				       "// Use do-while here to make sure that the negative guys go at least ONCE:\n"
				       (block "do"
					      (list "// Insert calls to those (negative rate) timers that run max speed:\n"
						    "// This substitutes for a call to WAIT_TICKS:\n"
						    (map lines-text negsrccode*)))
				       (format "while (TIME_DEBT(~a) > 0.0);\n" timestep_ms)
				       )
				      ))
				))
		   ;"wsShutdown();\n"
		   "// We keep the main function going for other tuples to come through.\n"
		   ;"printf(\"Out of main loop.\\n\");\n"
		   ;"print_queue_status();\n"
		   "while (print_queue_status()) { sleep(1); }\n"
		   "return 0;\n")))))]

      ;; Option (2): serial port connected to mote:
      [(not (null? (slot-ref self 'server-cutpoints)))
       (unless (= (length (slot-ref self 'server-cutpoints)) 1)
	  (error 'BuildTimerSourceDriver 
		 "Currently can't handle multiple return streams multiplexed onto serial port"))
	
	;; This is SUPER hackish, just sticking all the WSQueryMsg.c files in the flags!
	;(slot-cons! self 'compile-flags " -I$TOSROOT/support/sdk/c -L$TOSROOT/support/sdk/c -lmote -I$TOSROOT/support/sdk/c/tos/types/")
	;; ACK: fixing the TINYOS install dir at compile time.  Can't get quoting/env-vars to work out:
       (slot-set! self 'compile-flags 
		  (list (format " -I~a/support/sdk/c  -L~a/support/sdk/c -lmote "
				(getenv "TOSROOT") (getenv "TOSROOT"))
			(slot-ref self 'compile-flags)))
	(for i = 0 to (sub1 (length (slot-ref self 'server-cutpoints)))
	     (add-include! self (format "\"WSQueryMsg~a.c\"" i)))
	
	;; -lmote
	;(add-link! "libmote.a")
	(add-include! self "<message.h>")
	(add-include! self "\"serialsource.h\"")
	(add-include! self "\"serialpacket.h\"")
	(add-include! self "\"serialprotocol.h\"")
	;(add-include! self "\"message.h\"")
	
	(make-lines (list "
//#include <message.h>
////#include \"/opt/tinyos-2.x/support/sdk/c/serialsource.h\"
////#include \"/opt/tinyos-2.x/support/sdk/c/message.h\"
//#include \"serialsource.h\"
//#include \"serialpacket.h\"
//#include \"serialprotocol.h\"
//#include \"message.h\"

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
  ws_parse_options(argc-2,argv+2);

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
	  free(msg); // Does not use WSFREE
*/
	  // We shouldn't HAVE to do this:
	  // The MIG generated code uses offset 0, so we hack this:
	  //msg->data = msg->data + spacket_data_offset(0);
          tmsg_t *msg2 = new_tmsg((void*)packet + 1 + spacket_data_offset(0), len - 1);

          "(Var self (car (slot-ref self 'server-cutpoints)))"(msg2);
	  free(msg2); // Does not use WSFREE
	}
      else
	{
	  printf(\"non-AM packet: \");
	  hexprint(packet, len);
	}
     
      //tmsg_t* incoming_msg = new_tmsg((void*)packet,len);
      free((void *)packet); // Does not use WSFREE
      //free((void *)incoming_msg);
    }
}
"))];"

  ;; Option 3: driven by foreign_source
  ;; TODO: Catch the case where there are no timers or foreign sources (an error).
  [else
   (make-lines "
  int main(int argc, char** argv) {
    wsinit(argc, argv);
    initState();
    ws_parse_options(argc, argv);
    wsmain(argc, argv);
    wsShutdown();
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

(define (grab_fifos down*)
  (map (lambda (down)
	 (list "GRAB_WRITEFIFO("(symbol->string down)");\n"))
    down*))
(define (release_fifos down*)
  (map (lambda (down)
	 (list "RELEASE_WRITEFIFO("(symbol->string down)");\n"))
    down*))

;; Returns a list of code pieces, which can be any of the "c-" datatypes above.
(__spec Source <emitC2-base> (self xp)
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
		   (append-lines (make-lines (grab_fifos down*))
				 ((Emit self down* '#()) ''UNIT) ;; Code
				 (make-lines (release_fifos down*)))
		   (make-lines '()) ;; State 	
		   rate))])]

       ;; Allowing normal inline_C and treating it as a special case of inline_TOS:
       [(inline_C ',top ',initfun) 
	(list (make-c-state (make-lines (list "\n" top "\n")))
	      (make-c-init  (make-lines (if (equal? initfun "") "" `(,initfun "();\n")))))]

       [(__foreign_source ',name ',filels '(Stream ,type))
	(define ty (Type self type))
	(define arg (unique-name "tmp"))
	(for-each (add-file! self) filels)
	(valid-foreign-type! type)

	;; Create a function for the entrypoint.
	(let* ([proto `("extern void ",name"(",ty");\n")]
	       [bod (ForeignSourceHook self name
				       (list (grab_fifos down*)
					     (lines-text ((Emit self down* type) arg))
					     (release_fifos down*)))]
	       [impl (make-lines 
		      (block `("void ",name"(",ty" ",(Var self arg)")")
			    bod))])
	  (list (make-c-toplvl impl)
		(make-c-proto  (make-lines proto))
		))]
       )]))


;; Two hooks that return 'text':
(__spec IterStartHook <emitC2-base> (self name arg argty down*) 
	(grab_fifos down*))
;; Takes symbols and types (not text) as arguments.
(__spec IterEndHook   <emitC2-base> (self name arg argty down*) 
	(release_fifos down*))

(__spec ExtraKernelArgsHook <emitC2-base> (self) '())

;; A work function for each iterate.
;; Returns two values both of type 'lines'
;;  (1) A function prototype
;;  (2) A function definition
(__spec GenWorkFunction <emitC2-base> (self name arg vqarg argty code down*)
  (define _arg (Var self arg))
  (define _argty (Type self argty))
  (define extra (map (lambda (x) (list x ", ")) (ExtraKernelArgsHook self)))
  (values
   (make-lines `("void ",(Var self name) "(",extra ,_argty" ",_arg"); // Iter prototype\n"))
   (make-lines 
    (list (block `("void ",(Var self name) "(",extra ,_argty" ",_arg")")
		 (list
		  "char "(Var self vqarg)";\n"
		 (IterStartHook self name arg argty down*)
		 (lines-text code)
		 (IterEndHook self name arg argty down*)))
	  "\n"))))

;; A cut point on the server, currently only coming FROM the network.
;; Returns decls, top lvl binds, init code
(__spec Cutpoint <emitC2-base> (self type in out)
   ;; Cutpoint from tinyOS node:
   (let* ([local (unique-name "local")]
	  [_local (Var self local)])
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
		(lines-text (array-constructor-codegen self "arrsize" #f ty (setterk self local ty)))
		(block "for (i=0; i<arrsize; i++)"
		       (list _local"[i] = " srcpos "_get("src", i);\n")))]
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
		)])))

;; Returns "lines":
(__spec InvokePerOpCollector <emitC2-base> (self) null-lines)

;; .returns three top-level code blocks: definition (lines), prototypes (list of lines), init-code
(__spec Operator <emitC2-base> (self op)
  (define (Simp x) (Simple self x))
  (match op
    [(iterate (name ,name) 
	      (output-type (Stream ,elt))
	      (code (iterate ,annot ,itercode ,_))
	      (incoming ,up)
	      (outgoing ,down* ...))
     (match itercode
       [(let (,[(SplitBinding self) -> bind* init*] ...)
	  (lambda (,v ,vq) (,vty (VQueue ,outty)) ,bod))
	(let-values ([(proto def)
		      (GenWorkFunction self name v vq vty ((Value self) bod nullk) down*)])
	  (values def (cons proto bind*) init*))])]

    [(cutpoint (name ,_) (output-type ,type) (code ,__) (incoming ,in) (outgoing ,out))
     (ASSERT out)
     (Cutpoint self type in out)]

    [(_merge (name ,name) (output-type (Stream ,elt))
	     (code ,__)
	     (incoming ,a ,b) (outgoing ,down* ...))
     ;; This emit should not increment/decrement reference counts:
     ;; We are just redirecting something already in flight.
     (define arg (unique-name "arg"))
     (define extra (map (lambda (x) (list x ", ")) (ExtraKernelArgsHook self)))
     (define header `("void ",(Var self name) "(",extra ,(Type self elt)" ",(Var self arg)")"))
     (values (make-lines 	      
	      (list "// merge operator: \n"
		    (block header 
			   (list (grab_fifos down*)
				 (lines-text ((Emit self down* elt) arg))
				 (release_fifos down*))) "\n"))
	     (list (make-lines (list header ";\n")))
	     '())]

    [(unionN . ,_) (error 'emitC2 "doesn't support unionN/unionList right now")]

    ;; It would be nicer to be able to write this as a library
    ;; function.  That would probably require at least type-case..  Or
    ;; perhaps the functionality of demarshaling could be factored out
    ;; of here without introducing inefficiency?

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
	    ;[numstrings (length (filter (lambda (sA) (eq? s 'String)) types))]

	    [binarymode (equal? mode "binary")]
	    [__ (unless binarymode (error 'emit-c2:readFile "text mode reading not supported yet"))]

	    [handle (Var self (unique-name "_f"))]
	    [eofhit (Var self (unique-name "_eofhit"))]
	    	    
	    [init (make-lines `(
		,handle" = fopen(\"",file"\", ",(if binarymode "\"rb\"" "\"r\"")");\n"
		"if (",handle" == NULL) {\n"
		"  printf(\"Unable to open data file %s: %m\", \"",file"\");\n"
		"  exit(1);\n"
		"}\n"
		"fseek(",handle", ",offset", SEEK_SET);\n"
		))]

	    [skipcmd (list "fseek("handle", "(number->string skipbytes)", SEEK_CUR);\n")]	    
	    [state (make-lines `("FILE* ",handle";\n"
				 "int ",eofhit" = 0;\n"))]
	    [_buf (Var self 'buf)]
	    [_elt (Type self elt)]
	    [maintext ;; Code for the funcion that drives the file reading.
	     (list `(
		     "if (",eofhit") return;\n"

		     ,(IterStartHook self name 'ignored '#() down*)

		     ,(if (> winsize 0)
			  (lines-text ((Value self) 
				       `(assert-type ,elt (Array:makeUNSAFE ',winsize)) 
				       (varbindk self 'buf elt)))
			  ;; A binding with no init:
			  (list _elt " " _buf ";\n"))

		     ;,(IterStartHook self name 'ignored '#() down*)
		    
		     ;; TODO: HANDLE STRINGS:
; 		     ,(map (lambda (i) 
; 			     (list 
; 			      "// Cap of a 100 on length of strings read in:\n"
; 			      (format "char str~a[100];\n" i)))
; 			(cdr (iota (add1 numstrings))))

		     "int i, status = 0;\n"
		     ("// readFile: The binary format of tuples matches that in the file:\n"
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
			     `("fprintf(stderr, \"dataFile EOF encountered (%d).\", status);\n"
			       ;; [2008.11.06] Removing the shutdown on EOF behavior:
			       ;"wsShutdown();\n"
			       "fflush(stderr);\n"
			       ,eofhit" = 1;\n"
			       ,(IterEndHook self name 'ignored '#() down*)
			       ;(InvokePerOpCollector self)
			       "return;\n"
			       ))
		     
		     ,(list ;(grab_fifos down*)
		             (lines-text (incr-queue-refcount self elt _buf))
			     (lines-text ((Emit self down* elt) 'buf))
			     ;(release_fifos down*)
			     ;(lines-text (InvokePerOpCollector self))
			     (IterEndHook self name 'ignored '#() down*)
			     )))]
	    [extra (map (lambda (x) (list x ", ")) (ExtraKernelArgsHook self))])

       (values 
	(make-lines (list "// readFile operator: \n"
			  (block (list "void "(sym2str name)"("extra (Type self '#())" ignored)") maintext)))
         ;(wrap-iterate-as-simple-fun name 'ignored 'ignoredVQ (Type self '#()) maintext)
	(list state) (list init)))] ;; End readFile
    ))


;================================================================================

;; Assemble the pieces into output files:
;; This has a quirky return type.  Ugly abstraction boundaries.
;; Inputs STRINGS.
;; Returns a vector of two elements:
;;   (1) Association list of file-name, file-contents to write, datatype "text"
;;   (2) Thunk to execute after files are written.
(define (BuildOutputFiles_helper self includes freefundefs state ops init driver)
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
  (vector (list (list (emitC2-output-target) text))
	  void))
(__spec BuildOutputFiles <emitC2-base> (self includes freefundefs state ops init driver)
	(BuildOutputFiles_helper self includes freefundefs state ops init driver))

(define emitC2-output-target (make-parameter "query.c"))

(define (cutpoint? op)  (eq? (car op) 'cutpoint))

;; For now the only sorting we do is to bring Cutpoints to the front:
(__spec SortOperators <emitC2-base> (self ops) 
 (define-values (cps other) (partition cutpoint? ops))
 ;; Hack, also reverse the others for now... 
 ;; In the future we should topologically sort them here:
 (append cps (reverse other)))
  
;; Run the pass and generate C code:
;; Same return type as BuildOutputFiles
(__spec Run <emitC2-base> (self)
  (let* ([prog (slot-ref self 'theprog)]
	 ;; Run build-free-fun-table! before doing other codegen:
	 [freefundefs (build-free-fun-table! self (cdr (ASSERT (project-metadata 'heap-types prog))))])
     (match prog
       [(,lang '(graph 
		 ;; These had better be *constants* for GCC:
		 (const ,[(SplitBinding self) -> cb* cbinit*] ...)
		 (init  ,[(Effect self)
			  -> init*] ...)
		 ;(sources ,[(curry Source self) -> srcname* srccode* state1* srcrate* srcinit*] ...)
		 (sources ,[(curry Source self) -> pieces**] ...)
		 (operators ,oper* ...)
		 (sink ,base ,basetype)
		 ,meta* ...))
	;; We only need to skim the meta-data off each operator to get the sum total of types that travel through queues.
	(define queue-types 
	  (list->set
	   (apply append 
		  (map (lambda (op) '())
		    oper*))))
	(define __ (build-copy-fun-table! self queue-types))
	
	(define pieces* (apply append pieces**))
	;; Break out the different pieces:
	(define timers        (filter c-timer? pieces*))
	(define toplvl-pieces (map c-toplvl-lines (filter c-toplvl? pieces*)))
	(define init-pieces   (map c-init-lines (filter c-init? pieces*)))
	(define state-pieces  (map c-state-lines (filter c-state? pieces*)))
	(define proto-pieces  (map c-proto-lines (filter c-proto? pieces*)))

	;; Extract the names of all the iterates, these are our workers.
	(define opnames (map op->name oper*)) ;; May be #f for cutpoints.
	(define opinputs (map (lambda (x) (match (op->inputtype x #f) [(Stream ,elt) elt])) oper*))
	#;
	(define iterates (filter id (map (lambda (x) (match x [(iterate (name ,n) . ,_) n] [,_ #f])) oper*)))
	#;(define iterates-input-types
	  (filter id (map (lambda (x) (match x [(iterate . ,rest) 
						(match (cadr (ASSERT (assq 'code rest)))
						  [(iterate ,annot ,[f] ,_) f]
						  [(let ,_ ,[b]) b]
						  [(lambda (,x ,vq) (,xty ,vqty) ,_) xty])]
					       [,_ #f])) oper*)))

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
				(insert-between " "
				 (map (lambda (fn) 
				       (let ([lib (extract-lib-name fn)])
					 (if lib (list "-l" lib) fn)))
				   (slot-ref self 'link-files)))
				"\n"			      	
				(slot-ref self 'hash-defs)
				
				;; After the types are declared we can bring in the user includes:
				"\n\n" (map (lambda (fn) `("#include ",fn "\n"))
					 (reverse (slot-ref self 'include-files))) "\n\n")))
	(define allstate (text->string (list "int stopalltimers = 0;\n"
					     (map lines-text (apply append cb* state-pieces state2**)))))
	(define ops      (text->string (map lines-text oper*)))
	  ;(define srcfuns  (text->string (map lines-text (map wrap-source-as-plain-thunk srcname* srccode*))))
	(define init     (begin ;(ASSERT (andmap lines? (apply append opinit**)))
			   (text->string (block "void initState()"
					        ;(list "void initState("(insert-between ", " (ExtraKernelArgsHook self))")")
						(list 
						 "/* We may need to start up the Boehm GC or do other standard WS init: */ \n"
						 "wsInternalInit();\n"
						 (make-app "TOTAL_WORKERS" (list (number->string 
										  (add1 (length opnames)))))
						 ";\n"

						 "// [2008.11.07] The static data gets allocated against a never-cleared ZCT: \n"
						 "#ifdef WS_THREADED \n"
						 "#ifdef WS_USE_ZCT \n"
						 " zct_t* zct = WSCALLOC(sizeof(zct_t), 1);\n"
						 "#endif\n"
						 "#endif\n"
						 ;; FIXME: NUMBERS WILL NOT BE CONSECUTIVE:
						 "REGISTER_WORKER(0, "(Type self '#())", BASE);\n"
						 (map (lambda (i name ty) 
							(if name
							    (format "REGISTER_WORKER(~a, ~a, ~a);\n"
								    (add1 i) (text->string (Type self ty)) name)
							    ""))
						   (iota (length opnames)) opnames opinputs)
						 
						 (lines-text (apply append-lines init*))
						 (lines-text (apply append-lines cbinit*))
						 ;; This will be where the inline_C initializers go:
						 ;(lines-text (apply append-lines srcinit*))
						 (map lines-text init-pieces)
						 (lines-text (apply append-lines (apply append opinit**)))
						 "// We will never need to clear this ZCT, so we can throw it out:\n"
						 "#ifdef WS_THREADED \n"
						 "#ifdef WS_USE_ZCT \n"
						 "WSFREE(zct);\n"
						 "#endif\n"
						 "#endif\n"
						 ;; Finally, register all the work functions.							
						 "START_WORKERS();\n"
						 )))))
	  ;;(define toplevelsink "void BASE(int x) { }\n")	  

	(define union-defs (make-lines (map (UnionDef self) (slot-ref self 'union-types))))

	;; This is called last:
	(BuildOutputFiles self includes 
			  ;; Put the union definitions with the free-functions:
			  (append-lines union-defs freefundefs)
			  ;; Stick state and prototypes together:
			  (** (text->string (map lines-text proto-pieces)) ;; Put the prototypes early.				
				allstate
				
				(if #f ""
				    (** "DECLARE_WORKER(0, "(Type self '#())", BASE)\n"
					(text->string (map (lambda (i name ty) 
							     (if name
								 (list "DECLARE_WORKER("(number->string (add1 i))
								   ", "(Type self ty)", "(symbol->string name)")\n" 
								   )
								 '()))
						;(iota (length iterates)) iterates iterates-input-types
						(iota (length opnames)) opnames opinputs
						))))
				)
			  (** (text->string (map lines-text toplvl-pieces))
				ops) ;; Put these top level defs before the iterate defs.
			  init driver)
	  )]

       [,other ;; Otherwise it's an invalid program.
	(error 'emit-c2 "ERROR: bad top-level WS program: ~s" other)])))


;; Constructor, parse out the pieces of the program.
(__spec initialise <emitC2-base> (self prog)
  (slot-set! self 'free-fun-table '())
  (slot-set! self 'copy-fun-table '())
  (slot-set! self 'struct-defs (cdr (project-metadata 'struct-defs prog)))
  (slot-set! self 'union-types (cdr (project-metadata 'union-types prog)))
  (slot-set! self 'theprog prog)
  (slot-set! self 'link-files '())
  (slot-set! self 'compile-flags '())
  (slot-set! self 'hash-defs '())
  ;; Our default includes
  (slot-set! self 'include-files (list (** "\"" (WAVESCRIPTD) "/src/linked_lib/wsc2.h\"")))
  (slot-set! self 'server-cutpoints '())
  )

;; Takes a class as argument, makes an object, calls the "Run" method.
(define (emit-c2 prog class)
  (define obj (make-object class prog))
  (Run obj))


;;================================================================================

;; TODO: bring specifics down from the emitC2-base class to this one.
;; We need to get these out of the way of other sibling classes (e.g. for Java).
(define-class <emitC2> (<emitC2-base>) 
  ())

;;================================================================================


;; This is for use with a conservative collector.  No reference counting.
(define-class <emitC2-nogc> (<emitC2>) ())
;; Very simple, just don't insert any refcounting code:
(__specreplace incr-local-refcount <emitC2-nogc> (self ty ptr) null-lines)
(__specreplace decr-local-refcount <emitC2-nogc> (self ty ptr) null-lines)
(__specreplace incr-heap-refcount  <emitC2-nogc> (self ty ptr) null-lines)
(__specreplace decr-heap-refcount  <emitC2-nogc> (self ty ptr) null-lines)
(__specreplace incr-queue-refcount  <emitC2-nogc> (self ty ptr) null-lines)
(__specreplace decr-queue-refcount  <emitC2-nogc> (self ty ptr) null-lines)

(__specreplace gen-incr-code <emitC2-nogc> (self ty ptr msg) null-lines)
(__specreplace gen-decr-code <emitC2-nogc> (self ty ptr msg) null-lines)

(__specreplace build-free-fun-table! <emitC2-nogc> (self heap-types) null-lines)

(__spec initialise <emitC2-nogc> (self prog)
  ;; Add this to the include list:
  ;;(slot-set! self 'include-files (cons (** "\"gc/gc.h\"") (slot-ref self 'include-files)))
  ;;(slot-set! self 'compile-flags (list " -lgc " (slot-ref self 'compile-flags)))
  ;(slot-set! self 'link-files (cons "libgc.so" (slot-ref self 'link-files)))
  (match (wsc2-gc-mode)
    [boehm 
     ((add-file! self) "libgc.so")
     ; ((add-file! self) "libgc.a") ;; Had better statically link the collector.
     (slot-set! self 'hash-defs (list "#define USE_BOEHM\n" (slot-ref self 'hash-defs)))]
    [none (void)]))


;;================================================================================
;;; UNFINISHED: this will enable deferred refcounting using a ZCT (zero-count-table)
(define-class <emitC2-zct> (<emitC2>) (zct-types zct-init?))

(__spec initialise <emitC2-zct> (self prog)
  (slot-set! self 'zct-types '())
  (slot-set! self 'zct-init? #f)  
  (slot-cons! self 'compile-flags " -DWS_USE_ZCT ")
  )

(__specreplace incr-local-refcount <emitC2-zct> (self ty ptr) null-lines)
(__specreplace decr-local-refcount <emitC2-zct> (self ty ptr) null-lines)

(define (ensure-zct-types-computed self)
  (unless (slot-ref self 'zct-init?)
    (slot-set! self 'zct-init? #t)
    (slot-set! self 'zct-types
	       (filter (lambda (pr) 
			 ;; Structs and Unions are not heap allocated:
			 (match (car pr)
			   [(Struct ,_) #f]
			   [(Union ,_)  #f]
			   [,else       #t]))
		 (slot-ref self 'free-fun-table)))))

(define (get-zct-type-tag self ty)
  ;; This is hackish, but free-fun-table must be populated by the time
  ;; we get here.  The first time through we populate zct-types.
  (ensure-zct-types-computed self)
  (let ([ind (list-index (lambda (pr) (equal? (car pr) ty))
			 (slot-ref self 'zct-types))])
    (or ind
	(error 'get-zct-type-tag 
	       "could not lookup type ~s in: \n~s" ty (slot-ref self 'zct-types)))))

;; We use the index in the free-fun-table as the numeric tag for that type.
(__specreplace AllocHook <emitC2-zct> (self ty smpl) 
  ;;(make-lines (format "/* Alloc spot ~a ~a */\n" ty (text->string simple-xp)))   
  (define tag (number->string (get-zct-type-tag self ty)))
  (make-lines `("PUSH_ZCT(zct, ",tag", ",smpl");\n")))

(__spec InvokePerOpCollector <emitC2-zct> (self) 
	(make-lines "BLAST_ZCT(zct, DECR_ITERATE_DEPTH());\n"))

;; Clear the ZCT at the end of an operator execution.
(define ___IterEndHook
  (specialise! IterEndHook <emitC2-zct>
    (lambda (next self name arg argty down*) 
      (list #;(map (lambda (down)
		   (list "FIFO_COPY_OUTGOING("(symbol->string down)");\n")
		   ;(build-fifo-copy down elt)
		   )
	      down*)
	    (next)	  
	    (lines-text (InvokePerOpCollector self))
	    ))))

;; For testing purposes allowing multiple iterate work functions to be
;; active (depth first calls).  iterate_depth lets us know when it's
;; safe to blast the ZCT.
(define ___IterStartHook
  (specialise! IterStartHook <emitC2-zct>
    (lambda (next self name arg argty down*)
    (list "INCR_ITERATE_DEPTH();\n"	  
	  (next)))))

(define gen-decr-called-from-free #f) ;; modified with fluid-let
  
;; This must also build a dispatcher for freeing based on numeric type tag.
(define __build-free-fun-table!
  (specialise! build-free-fun-table! <emitC2-zct> 
    (lambda (next self heap-types)
      (fluid-let ([gen-decr-called-from-free #t])
       (let* ([table (next)])	
	 (ensure-zct-types-computed self)
	 (append-lines table
	 (make-lines 
	  (block "void free_by_numbers(typetag_t tag, void* ptr)"
		 (block "switch (tag)"
			(list (mapi (match-lambda (,i (,ty . ,syngen))
				      (list (format "case ~a: " i)
					    (lines-text (syngen "ptr"))
					    " break;\n"))
				    (slot-ref self 'zct-types))
			      "default: printf(\"tag %d, \", tag); wserror_builtin(\"invalid tag for ZCT entry\");\n"
			 ))))))
       ))))


;; A normal decrement will PUSH to the ZCT if the RC hits zero.
;; HOWEVER, a decrement that happens FROM the freeing process, should
;; just free immediately.  Thus the behavior of this depends on where
;; we call it from.  We achieve this with a hack: a fluid-let bound flag.

(define __gen-free-code 
  (specialise! gen-free-code <emitC2-zct>
    (lambda (next self ty ptr)
      ;; Yuck, so imperative.  We call get-zct-type-tag just to
      ;; generate the zct-types table even if we don't need the result!
      (let  ([tag (get-zct-type-tag self ty)])
	(if gen-decr-called-from-free	  
	    (next)
	    (make-lines `("PUSH_ZCT(zct, ",(number->string tag)", ",ptr");\n"))
	    )))))

;; DUPLICATED CODE:
;; This is here just to so we can slip in the global state bindings for the ZCT.

;; FIXME: Change the interface to BuildOutputFiles so that this can be done away with:
(define __BuildOutputFiles
  (specialise! BuildOutputFiles <emitC2-zct>
	       (lambda (next self includes freefundefs state ops init driver)
      (BuildOutputFiles_helper self includes freefundefs 
			       (text->string 
				(list state
; 				     "typetag_t zct_tags[ZCT_SIZE];\n"
; 				     "void*     zct_ptrs[ZCT_SIZE];\n"
; 				     "int       zct_count;\n"
				     
				     "int       iterate_depth = 0;\n\n"
				     "#ifdef WS_THREADED\n"
				     "  zct_t** all_zcts;\n"
				     "  pthread_mutex_t zct_lock = PTHREAD_MUTEX_INITIALIZER;\n"
				     "#else\n"
				     "  zct_t the_zct;\n"
				     "  zct_t* zct = &the_zct;\n"
				     "#endif\n"))
			       ops init driver))))


(__specreplace ExtraKernelArgsHook <emitC2-zct> (self) (list "zct_t* zct"))
;; This is unnecessary duplicated code:
;; It's annoying that I can't change the arguments when calling the parent method (next).
#;
(__specreplace GenWorkFunction <emitC2-zct> (self name arg vqarg argty code down*)
  (define _arg (Var self arg))
  (define _argty (Type self argty))
  (values
   (make-lines `("void ",(Var self name) "(zct_t* zct, ",_argty" ",_arg"); // Iter prototype\n"))
   (make-lines 
    (list (block `("void ",(Var self name) "(zct_t* zct, ",_argty" ",_arg")")
		 (list
		  "char "(Var self vqarg)";\n"
		 (IterStartHook self name arg argty down*)
		 (lines-text code)
		 (IterEndHook self name arg argty down*)))
	  "\n"))))

;;================================================================================


;;================================================================================


;; ================================================================================

;; This variant enables profiling.
(define-class <emitC2-timed> (<emitC2>) ())

(define (print-w-time2 prefix)
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
    (lambda (next self name arg argty down*)
      (list (next)	  	    
	    (print-w-time2 (list "Start "(sym2str name)" "))))))

(define ____IterEndHook
  (specialise! IterEndHook <emitC2-timed>
    (lambda (next self name arg argty down*) 
    (list (next)
	  (print-w-time2 (list "End "(sym2str name)" "))
	  ))))

;; Wrap timers around the whole Source call:
(define ____Source
  (specialise! Source <emitC2-timed>
  (lambda (next self xp)    
    (map (lambda (piece)
	   (if ( piece)
	       (make-c-timer 
		(c-timer-name piece)
		(append-lines (make-lines (print-w-time2 "StartTraverse "))
			      (c-timer-code piece)
			      (make-lines (print-w-time2 "EndTraverse ")))
		(c-timer-state piece)
		(c-timer-rate  piece))
	       piece))
      (next)))))

(define-testing test-emit-c2
  (let ()
    (define testprog
      '(explicit-stream-wiring-language
       '(graph
	 (const)
	 (init)
	 (sources)
	 (operators)
	 (sink tmpsmp_223 'hmm) (union-types)
	 (heap-types)
	 (struct-defs)
	 (input-parameters ()))))
    (default-unit-tester "emit-c2" 
      `(
	
	[(let ((obj (make-object <emitC2> ',testprog)))
	   obj)
	 unspecified]

	))
    ))

) ;; End module
