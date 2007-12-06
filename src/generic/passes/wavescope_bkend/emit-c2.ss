

;;;; .title WaveScript EmitC Version TWO
;;;; .author Ryan Newton

;;;; This uses the generic C-generation libary (c_generator.ss).
;;;; Unlike the first version (emit-c.ss) this version does not rely on:

;;;;  (1) C++ features (templates etc)
;;;;  (2) XStream classes (Sigseg, etc)
;;;;  (3) Boost smart pointers

;;;; It produces "lower level" C code than the first version, meant to
;;;; be used with a custom garbage collector.

(module emit-c2 mzscheme 
  (require  "../../../plt/common.ss"
	    (all-except (lib "list.ss") sort sort! filter)
	    (all-except "nominalize-types.ss" test-this these-tests)
	    "convert-sums-to-tuples.ss"
	    "../../compiler_components/c_generator.ss" )
  (provide emit-c2)
  (chezprovide )  
  (chezimports (except helpers test-this these-tests))

;; These are just for sanity checking.  Disjoint types keep mix-ups from happening.
;; These should just be wrappers on the outside.  You should never find them nested.
(reg:define-struct (lines text))
;(reg:define-struct (expression text))
(trace-define (append-lines . ls) (make-lines (map lines-text ls)))
;(define (idk x) (ASSERT expression? x) x)
;(define (idk x) x)

;================================================================================
;;; Input grammar

;(define emit-c2-input-grammar)

;================================================================================
;;; Hooks for garbage collection.

(define-syntax debug-return-contract
  (syntax-rules ()
    [(_ pred fun) (IFDEBUG (lambda args (let ([result (apply fun args)]) (pred result) result))
			   fun)]))

#;
(define-syntax debug-return-contract
  (syntax-rules ()
    [(_ pred fun) (lambda args (let ([result (apply fun args)]) (pred result) result))]))

;; For the naive strategy we'de allow shared pointers but would need a CAS here.
(define (gen-incr-code ty ptr)
  (match ty
    [(Array ,elt) (make-lines `("(int*)",ptr"[-1]++; /* incr refcount */\n"))]
    ;; Other types are not heap allocated:
    [,_ (make-lines "")]
    ))

;; "ptr" should be text representing a C lvalue
(define (gen-decr-code ty ptr ifzero)
  (match ty
    [(Array ,elt) (make-lines (block `("if (--(((int*)",ptr")[-1]) == 0) /* decr refcount */ ") (lines-text ifzero)))]
    [,_ (make-lines "")]))

(define (default-free ty ptr)
  (let ([offset (match ty 
		  [(Array ,elt) " - 2"]
		  [,_ ""])]
	[debugprint `("printf(\"  FREEING %p\\n\", ",ptr");\n")])
    (make-lines `(  ;,debugprint
		  "free(",ptr,offset");\n"))))

(define incr-local-refcount gen-incr-code)
(define (decr-local-refcount ty ptr)  
  (gen-decr-code ty ptr (default-free ty ptr)))

(define incr-heap-refcount gen-incr-code)
(define (decr-heap-refcount ty ptr)
  (gen-decr-code ty ptr (default-free ty ptr)))

;; Do anything special that's required as a value is sent off through an emit.
;; Currently, with a pure depth-first strategy, reference count does not need to be affected by "emit".
(define (say-goodbye ty ptr) (make-lines ""))

;================================================================================
;;; Low level routines for generating syntax.

(define (mangle-name n) (symbol->string n))
(define sym2str  symbol->string)


(define (cap x) (list x ";\n"))

(define (make-app rator rands)
  (list rator "("(insert-between ", " rands)")"))

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
(trace-define Const
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

(define Var mangle-name)

;; These are the expressions that make valid operands (post flattening)
;;   .returns A string.  Could return an "expression".
(trace-define (Simple expr)
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
    [,else (error 'Simple "not simple expression: ~s" x)]
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
      (append-lines 
       ;(say-goodbye TYPE? element)
       (make-lines (map (lambda (down)
			  (cap (make-app (Var down) (list element))))
		     down*))))
    ;(make-lines `("emit ",(Simple expr)";"))
    ))

(define (emit-err where)
  (lambda (_) (error where "should not run into an emit!")))

(define (Binding emitter)
  (lambda (cb)
    ;(define val (Value (lambda (_) (error 'Binding "should not run into an emit!"))))
    (match cb
      [(,vr ,ty ,rhs) ;(,[Var -> v] ,[Type -> t] ,rhs) 
       ((Value emitter) rhs (varbindk vr ty))]
      [,oth (error 'Binding "Bad Binding, got ~s" oth)])))
;; This separately returns the type decl and the initialization code.
(define (SplitBinding emitter)
  (lambda (cb)
    ;(define val (Value (lambda (_) (error 'Binding "should not run into an emit!"))))
    (match cb
      [(,vr ,ty ,rhs) 
       ;; We derive a setter continuation by "splitting" the varbind continuation:
       (define setterk (let-values ([(_ newk) ((varbindk vr ty) split-msg)]) newk))
       (values (make-lines `(,(Type ty)" ",(Var vr)";\n"))
	       ((Value emitter) rhs setterk))]
      [,oth (error 'SplitBinding "Bad Binding, got ~s" oth)])))

(define (Effect emitter)
  (debug-return-contract lines?
  (lambda (xp)
    (match xp
      ['UNIT   (make-lines "")]
      [(tuple) (make-lines "")] ;; This should be an error.
      
      ;;[(set! )]
      ;;[(for)]
      ;; [2007.12.04] Would probably improve efficiency to at least handle scalars as well here:
      ;; Otherwise we needlessly allocate string objects.
      [(print (assert-type ,ty ,[Simple -> e]))
       ;(ASSERT (eq? t 'String))
       (define flag
	 (match ty
	   [String "%s"]
	   [Int    "%d"]))
       (make-lines `("printf(\"",flag"\", ",e");\n"))]
      
      [(set! ,[Var -> v] (assert-type ,ty ,[Simple -> x]))
       (append-lines 	
	;; Set! changes Ref objects, which are on the heap:
	(decr-heap-refcount ty v)  ;; Out with the old.
	(make-lines `(,v" = ",x";\n"))
	(incr-heap-refcount ty v)  ;; In with the new.
	)]

      [(Array:set (assert-type (Array ,[Type -> ty]) ,[Simple -> arr]) ,[Simple -> ind] ,[Simple -> val])
       (append-lines 	
	(decr-heap-refcount ty `(,arr"[",ind"]"))  ;; Out with the old.	
	(make-lines `(,arr"[",ind"] = ",val";\n"))
	(incr-heap-refcount ty `(,arr"[",ind"]"))  ;; In with the new.
	)]

      [(emit ,vq ,x) (emitter x)]
      [(begin ,[e*] ...) (apply append-lines e*)]

      ;; DUPLICATED CASES WITH Value:
      ;; ========================================
      [(let ([,lhs ,ty ,rhs]) ,[bod])
       ;; Here we incr the refcount for a *local* reference
       (append-lines ((Binding emitter) (list lhs ty rhs))
		     (incr-local-refcount ty (Var lhs))
		     bod
		     (decr-local-refcount ty (Var lhs)))]
      ;; ========================================
      ))))

;; The continuation k is invoked on a piece of text representing the return expression.
;; k is expected to return text of the form "lines" that stores away this result.
(define (Value emitter)
  (debug-return-contract lines?
   (trace-lambda V (xp k)
     ;; This is a debug wrapper that respects the funky interface to the continuation:
     (IFDEBUG (set! k (let ([oldk k]) (lambda (x) 
			  (call-with-values (lambda () (oldk x))
			    (lambda args (ASSERT lines? (car args)) (apply values args)))))) (void))
     (match xp
       [,simp (guard (simple-expr? simp)) (k (Simple simp))]
       ;; This doesn't change the runtime rep at all.
       [(Mutable:ref ,[Simple -> x]) (k x)]
       [(begin ,[e]) e]
       [(begin ,[(Effect emitter) -> e1] ,e* ...)
	(define rest ((Value emitter) `(begin ,@e*) k))
	(append-lines e1 rest)]
       ;; This splits the continuation, using it twice.
       [(if ,[Simple -> test] ,conseq ,altern)
	(let-values ([(lines newk) (k split-msg)])
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
		      bod
		      (decr-local-refcount ty (Var lhs)))]
       
	[(,prim ,rand* ...) (guard (regiment-primitive? prim))
	 (PrimApp (cons prim rand*) k #f )]
	[(assert-type ,ty (,prim ,rand* ...)) (guard (regiment-primitive? prim))
	 (PrimApp (cons prim rand*) k ty)]
       ))))


(define PrimApp
  (debug-return-contract lines?
   (lambda (app k mayberetty)
     ;; If we fall through to the "SimplePrim" case that means it's a
     ;; primitive with a one-to-one correspondence to a C function.
     (define (SimplePrim var)
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
      ;[(toArray)                (fromlib "toArray")]      
      ;; These use FFTW

#;
      [(fftR2C ifftC2R fftC ifftC memoized_fftR2C)
       ;(if (eq? var  'memoized_fftR2C) (inspect 'gotit))
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
      ))

     (match app
       ;; Refs and sets are pure simplicity:
       [(Array:ref (assert-type (Array ,[Type -> ty]) ,[Simple -> arr]) ,[Simple -> ind])
	(k `(,arr"[",ind"]"))]
       [(Array:length ,[Simple -> arr])
	;; Length is -2 and refcount is -1:
	(k `("((int*)",arr")[-2]"))]
       ;; This is the more complex part:
       [(Array:make ,[Simple -> len] ,[Simple -> init])       
	(match mayberetty
	  [(Array ,[Type -> elt])
	   ;(k `("arrayMake(sizeof(",elt"), "len", "init")"))
	   (let ([size `("sizeof(",elt") * ",len" + 2")]
		 [tmp (Var (unique-name "arrtmp"))])
	     (append-lines 
	      (make-lines `("int* ",tmp" = ((int*)malloc(",size") + 2);\n"
			    ,tmp"[-1] = 0;\n"
			    ,tmp"[-2] = ",len";\n"))
	      (k `("(",elt"*)",tmp)))
	     )])]
       
       ;; wsequal? should only exist for scalars at this point:
       [(wsequal? (assert-type ,ty ,[Simple -> left]) ,[Simple -> right])
	(ASSERT (memq ty '(Int Int16 Int64 Float Double Complex)))
	(k `("(",left" == ",right")"))]

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
	  (k `("(" ,left ,(format " ~a " cname) ,right ")")))]


       [(,other ,[Simple -> rand*] ...)
	(k `(,(SimplePrim other) "(" ,(insert-between ", " rand*) ")"))]
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
       [(let (,[(SplitBinding (emit-err 'Binding)) -> bind* init*] ...) (lambda (,v ,vq) (,vty (VQueue ,outty)) ,bod))
	(values 
	 (wrap-iterate-as-simple-fun name v vty 
	    ((Value emitter) bod nullk))
	 bind* init*)]
       )]))

;================================================================================

(define-pass emit-c2
  [Program 
   (lambda (prog Expr)
     (match prog
       [(,lang '(graph (const ,[(Binding (emit-err 'Binding)) -> cb*] ...) ;; These had better be *constants* for GCC
		       (init  ,[(Effect (lambda _ (error 'top-level-init "code should not 'emit'"))) 
				-> init*] ...)
		       (sources ,[Source -> srcname* src*  state1*] ...)
		       (operators ,[Operator -> oper* state2** init**] ...)
		       (sink ,base ,basetype)
		       ,_))	
	;(assq 'c-includes meta*)

	(define includes (string-append "#include<stdio.h>\n" 
					"#include<stdlib.h>\n"
					"#include \""(++ (REGIMENTD) "/src/linked_lib/wsc2.h")"\"\n"
					))
	(define allstate (text->string (map lines-text (apply append cb* state1* state2**))))
	(define ops      (text->string (map lines-text (reverse oper*))))
	(define srcfuns  (text->string (map lines-text (map wrap-source-as-plain-thunk srcname* src*))))
	(define init     (text->string (block "void initState()" (lines-text (apply append-lines (apply append init**))))))
	(define driver   (text->string (lines-text (build-main-source-driver srcname*))))
	;(define toplevelsink "void BASE(int x) { }\n")

	(define total (apply string-append 
			(insert-between "\n"
			  (list includes allstate
				;toplevelsink
				ops srcfuns 
				init driver))))

	;(printf "====================\n\n")	
	;(display total)
	total]

       [,other ;; Otherwise it's an invalid program.
	(error 'emit-c2 "ERROR: bad top-level WS program: ~s" other)]))]
  ) ;; End pass




) ;; End module

