#!r6rs

;;;; .title WaveScript EmitMLton (SML)
;;;; .author Ryan Newton [2007.06.13]

(library (ws passes mlton_bkend emit-mlton)
  (export emit-mlton-wsquery test-emit-mlton)
  (import (except (rnrs (6)) error) (rnrs mutable-strings)
	  (ws compat compat)
	  (ws common)
	  (ws compiler_components c_generator)
	  (ws passes ocaml_bkend shared-emit-ml))

;; MUTABLE
;(define union-edges 'union-edges-uninit)

(define foreign-includes 'uninit-fi)
(define (add-file! file)
  (set! foreign-includes (cons file foreign-includes)))

;; If there are any foreign sources, this becomes true, and that tells
;; us to call into the foreign "wsmain" instead of starting our own
;; scheduler.
(define driven-by-foreign #f)

(define extraCdecls 'uninit-ecd)
(define CinitCalls  'uninit-cic)

;; Experimenting with both of these:
(define int-module 'Int32)
;(define int-module 'Int)

;; Mutated below:
(define union-types 'utuninit)

;======================================================================
;======================================================================

(define (coerce-id x) (if (symbol? x) (Var x) x))

;;; These are the functions that differ from their caml countparts.

;; Curried version:
(define (make-fun formals body)
  (if (null? formals)
      (list "(fn () => "body")")
      (let loop ([formals (map coerce-id formals)])
	(if (null? formals)
	    body
	    (list "(fn "(car formals)" => "(loop (cdr formals)) ")")))))
;; Curried version:
(define (make-app rator rands) (list "(" rator " "(insert-between " " rands) ")"))

(define (make-prim-app f x*) (make-app f (list (apply make-tuple-code x*))))


;; This is LAME, but having problems with "let val rec" syntax in MLton.
;; This produces the "f x = bod" snippet .
#;
(define (make-fun-binding name formals funbody)
  (list " " (coerce-id name) " "
	(if (null? formals) "()"
	    (insert-between " " (map coerce-id formals)))
	" = "funbody))
(define (make-fun-binding name formals funbody)
  (list " " (coerce-id name) " = fn "
	(if (null? formals) "()"
	    (insert-between " " (map coerce-id formals)))
	" => "funbody))

;; Tuple version:
;; ...

;; Prints a floating point constant in a manner acceptable to SML.
#;
;; This fails to be an identity... some digits change:
(define (format-float fl)
  (let ((v (decode-float fl)))
    (let ((m (vector-ref v 0))
	  (e (vector-ref v 1))
	  (s (vector-ref v 2)))     

      (let* ([exact (* s m (expt 2 e))]
	     [n   (truncate (/ (log exact) (log 10)))]
	     [expsign (if (< n 0) -1 1)]
	     [n2  (* expsign (add1 (abs n)))]
	     [rem (/ exact (expt 10 n2))])
	(format "~aE~a" rem (inexact->exact n2))
	))))
;; This is the same as above but faster:
#;
(define (format-float fl)
  (let* ([n   (truncate (/ (log fl) (log 10)))]
	 [expsign (if (< n 0) -1 1)]
	 [n2  (* expsign (add1 (abs n)))]
	 [rem (/ fl (expt 10 n2))])
    (format "~aE~a" rem (inexact->exact n2))
    ))

;; This is a hack, but is much simpler.
(define (format-float fl)
  (if (not (= fl fl))
      "(0.0/0.0)"
      (let* ((str (format "~a" fl))
	     (len (string-length str)))
	(do ((i 0 (fx+ i 1)))
	    ((fx= i len) str)
	  (case (string-ref str i)
	    [(#\-) (string-set! str i #\~)]
	    [(#\e) (string-set! str i #\E)])))))


(define (make-for i st en bod)
  (list
   "(let val "i" = ref "st" in \n"
   " while !"i" <= "en" do\n"
   "   (let val "i" = !"i" in"bod"\n end; "i" := !"i" + 1) end)"))
(define (make-while tst bod) `("(while ",tst" do\n ",bod")\n"))

(define (make-let binds body . extra)
  (list "(let val " extra
	(insert-between "\n val "
	   (map make-bind binds)
	   )
	" in \n"
	(indent body "  ")
	"\n end)"))

(define (make-letrec binds body) (make-let binds body "rec "))

;; In SML this is different bc it allows recursion.
;; Would have to do letrec for same effect.
#;
(define (with-fun-binding name formals funbody body)
  (list "(let fun " name
	(if (null? formals) "()"
	    (insert-between " " (map coerce-id formals)))
	" = \n" funbody
	" in \n"
	(indent body "  ")
	"\n end)"))




; make-conditional
(define (ln . args)     (list args "\n"))
(define (lnfst . args)  (list "\n" args))
(define (lnboth . args) (list "\n" args "\n"))

;; If the type needs a specialized hashfun, returns its name,
;; otherwise returns #f.
;(define (HashType k v) )
;(define (SharedPtrType t))

;; These are the (mlton) types that WS types have when they are traveling abroad.
(define (ForeignType ty)
  (match ty
;    [Timebase (Type 'Int)]
    [,oth (Type oth)]))

(define (marshal-to-foreign ty expr)
  (match ty
;    [Timebase (list "(let val Timebase tb = "expr" in tb end)")]
    [,oth expr]))
(define (marshal-from-foreign ty expr)
  (match ty
;    [Timebase (list "(Timebase "expr")")]
    [,oth expr]))

;; This should give you an idea of the mapping between types:
(define (Type t)
  (match t
    ;; These are used just for the type variables of user Sum types.
    [(quote ,v) (ASSERT symbol? v) (** "'" (symbol->string v))]

    [Float     "Real32.real"]
    [Double    "Real64.real"]
    
    [Bool    "bool"]
    [Int     (format "~a.int" int-module)]
    [Int16   "Int16.int"] ;; Not standard SML.
    [Int32   "Int32.int"]
    [Int64   "Int64.int"] 

    [Uint8    "Word8.word"]
    [Uint16   "Word16.word"]

    [Complex  "Complex.complex"]
    [String   "string"]
    [Char     "char"]

    [(Ref ,[t]) `("(",t ") ref")]
    [(VQueue ,_) "unit"]
  
    [(Pointer ,_) "MLton.Pointer.t"]

    [Timebase "SigSeg.timebase"] ;; This will change.

    [(Sum ,name ,[args] ...)
     (list "(" (if (null? args) "" (apply make-tuple-code args)) " " (SumName name) ")")]

    [#() "unit "]

    [#(,[t*] ...) `("(",(insert-between " * " t*)")")]

    [(,[args] ... -> ,[ret])
     (list "(" (if (null? args)
		   "unit"
		   (insert-between " * " args))
	   " -> " ret ")")]

    ;; Went back and forth on whether this should be a pointer:
    [(Sigseg ,t) `(,(Type t) " sigseg")]

    [(Array ,[t])  `("(",t") array")]
    [(List ,[t]) `("(",t") list")]
    ;[(HashTable ,kt ,vt) (SharedPtrType (HashType kt vt))]
    [,other (error 'emit-mlton:Type "Not handled yet.. ~s" other)]))

(define (TypeDecl td)
  (match td
    [((,name ,[Type -> args] ...) [,tag* ,ty*] ...)     
     (let ([params (if (null? args) ""
		       (apply make-tuple-code args))])
       (list "and "params" "(SumName name)" = \n  "
	   (insert-between "  |\n  "
	    (map (lambda (tag ty)
		   (list (VariantName tag) " of " (Type ty)))
	     tag* ty*))
	   "\n")
       )]))

(define flush "TextIO.flushOut TextIO.stdOut")

(define (build-BASE type) 
  (define printer 
    (if (suppress-main-stream-printing) "()"
	(if #f ;(equal? type '#()) 
	    flush 
	    `("(print_endline (",(build-show type)" x) "
	      ;; "; " ,flush
	      ")"))))
  `(" baseSink = "
     ,(make-let `(["counter" "ref 0"])
       (make-fun '("x") 
	 (make-seq 
	  printer
	  "counter := !counter + 1"
	  `("if !counter  = element_limit "
	    " then OS.Process.exit OS.Process.success"
	    " else () "))))
     "\n"))

(define (build-show t)
#;
  (define (intprint mod)
    (format (** "(fn n => if n < ~a.fromInt 0 "
	       " then \"-\" ^ ~a.toString (~a.-(~a.fromInt 0, n))"
	       " else ~a.toString n)")
	    mod mod mod mod mod))
  ;; Simpler:
  (define (intprint mod)  (format "~a.toString" mod))

  (match t

    [Int16 (intprint 'Int16)]
    [Int64 (intprint 'Int64)] 
    [Int   (intprint int-module)]

    [Uint8  (intprint 'Word8)]
    [Uint16 (intprint 'Word16)]

    ;; ERROR: FIXME:
    [(quote ,_) (make-fun '(_) "(\"POLYMORPHIC_OBJECT\")")]

    [Timebase  (make-fun '("x") (list "\"<Timebase \" ^ "(intprint 'Int)" x ^ \">\""))]

    [String     (make-fun '("x") "x")]
    [Char       (make-fun '("x") "String.implode [x]")]
    [Float  "Real32.toString"]
    [Double "Real64.toString"]
    [Bool   "Bool.toString"]
    [Complex "(fn {real,imag} => Real32.toString real ^ \"+\" ^ Real32.toString imag ^ \"i\")"]

    [(List ,[t]) (make-fun '("ls") (list "(\"[\" ^ concat_wsep \", \" (List.map "t" ls) ^ \"]\")"))]
    [(Array ,[t]) (make-fun '("a") (list "(\"#[\" ^ concat_wsep \", \" (List.map "t" (arrayToList a)) ^ \"]\")"))]

    [(Pointer ,_) "(fn _ => \"<pointer>\")"]

#;
    [(Sum ,name ,args ...) (format "(fn _ => \"<uniontype ~a>\")" name)]
    [(Sum ,sumname ,arg-ty* ...)     
     (car (apply append 
		 (map (lambda (x)
			(match x
			  [((,name ,ty*1 ...) (,variant* ,ty*2) ...)
			   (if (not (eq? name sumname)) '()
			       (let ([instance (instantiate-type `(MAGIC ,(list->vector ty*1) ,(list->vector ty*2)))])
				 (types-equal! instance
					       (instantiate-type `(MAGIC ,(list->vector arg-ty*) ',(unique-name "any")))
					       #f "")
				 (let ([new-varty* (vector->list (export-type (caddr instance)))])
				   (list 
				    (make-fun (list (Var 'sumt))
					      (WScase 'sumt 
						      variant*
						      (map (lambda (varname argty)
								   `(lambda (x) (,argty)
									    (string-append 
									     ;',(format "~s" (VariantName varname))
									     ',(** (VariantName varname) "(")
									     (string-append
									      (show (assert-type ,argty x))
									      '")"))))
							variant* new-varty*)
						      #f))))))]))
		   union-types)))]

    ;; Just print range:
    [(Sigseg ,t) 
     (let ([Int (format "~a" int-module)])
       (make-fun '("ss") 
	       (list 
		"(\"[\"^ Int64.toString ("  (DispatchOnArrayType 'start t)
		" ss) ^\", \"^ Int64.toString ("      (DispatchOnArrayType 'end t)
		" ss + 1) ^ \")\")")))]

    [#(,[t*] ...)
     (let ([flds (map Var (map unique-name (make-list (length t*) 'fld)))])
       (list 
	"(fn "(apply make-tuple-code flds)" =>\n"
	(indent 
	 (list "\"(\" ^"
	       (insert-between " \", \" ^"
			       (map (lambda (printer fld)
				      `("((",printer") ",fld") ^ \n")) 
				 t* flds)
			       )
	       " \")\"")
	 "  ")")"))]
    ))


(define (build-equality-test t)
  (match t
    
    [Float  "Real32.=="]
    [Double "Real64.=="]
    [Complex "(fn ({real=r1,imag=i1}, {real=r2,imag=i2}) => Real32.==(r1,r2) andalso Real32.==(i1,i2))"]

    [(List   ,[t])  (list "(fn (l1, l2) => List.length l1 = List.length l2 andalso ListPair.all "t" (l1, l2))" )]
    [(Array  ,[t])  (list "(arrayEqual "t")")]
    [(Sigseg ,[t])  (list "(SigSeg.eq "t")")]

    [#()    "(fn ((),()) => true)"]
    [#(,[t*] ...)
     (let ([flds1 (map Var (map unique-name (make-list (length t*) 'a)))]
	   [flds2 (map Var (map unique-name (make-list (length t*) 'b)))])
       (list 
	"(fn "(make-tuple-code (apply make-tuple-code flds1) (apply make-tuple-code flds2))" =>\n"
	(indent
	 (insert-between " andalso "
	   (map (lambda (comp a b) (list comp" "(make-tuple-code a b)))
	     t* flds1 flds2))
	 "  ")")"))]

    ;; Otherwise fall through on builtin equality:       
    [,else ;(make-fun (list (make-tuple "x" "y")) "(x = y)")
     (make-fun (list (make-tuple-code "x" "y")) "(x = y)")
	   ]
    ))


;======================================================================

;; CHANGED HEAVILY FOR MLTON:

;; This is the only entry point to the file.  A complete query can
;; be transformed into a complete query file.
;;
;; .param prog  The wsquery to process.
(define emit-mlton-wsquery
  (lambda (prog)
    ;; Lame, requires REGIMENTD:
    (define header1 (file->string (** (REGIMENTD) "/src/ws/passes/mlton_bkend/scheduler.sml")))
    (define header2 (file->string (** (REGIMENTD) "/src/ws/passes/mlton_bkend/prims.sml")));
    (define header3a (file->string (** (REGIMENTD) "/src/ws/passes/mlton_bkend/sigseg.sig")))
    (define header3b (file->string (** (REGIMENTD) "/src/ws/passes/mlton_bkend/sigseg.sml")))
    (define header4 (file->string (** (REGIMENTD) "/src/ws/passes/mlton_bkend/data_reader.sml")))
    (define header5 (file->string (** (REGIMENTD) "/src/ws/passes/mlton_bkend/foreign.sml")))

    (define complex1 (file->string (** (REGIMENTD) "/src/ws/passes/mlton_bkend/Complex.sig")))
    (define complex2 (file->string (** (REGIMENTD) "/src/ws/passes/mlton_bkend/Complex.sml")))

    (fluid-let ([driven-by-foreign #f]
		[extraCdecls      '()]
		[CinitCalls       '()]
		[foreign-includes '()]
		[union-types (cdr (ASSERT (project-metadata 'union-types prog)))])
      (match prog
	[(,lang '(graph (const ,[ConstBind -> cb*] ...)
			(init  ,[Effect -> init*] ...)
			(sources . ,[Sources -> src* state1** init1*])
			(operators ,[Operator -> oper* state2**] ...)
					;		      (unions ,[Union -> union*] ...)
			(sink ,base ,basetype)			
			,meta* 		 ...
			))
	 		 
	 ;; If there was any inlined C-code we need to dump it to a file here and include it in the link.
	 (let ([tmpfile "__temp__inlinedC.c"])
	 (unless (null? CinitCalls)
	   (set! extraCdecls
		 (snoc (format "\nvoid ws_inlinedC_initialization() {\n~a\n}\n\n"
			       (apply string-append
				      (map (lambda (init) (format "  ~a();\n" init))
					CinitCalls)))
		       extraCdecls)))
	 (unless (null? extraCdecls)
	   (string->file (apply string-append extraCdecls) tmpfile)
	   (set! foreign-includes (cons tmpfile foreign-includes))))
       
	 ;; Just append this text together.
	 (let ([result (list 
                           ;; Very first thing is 
		           (let ([deps (filter (lambda (fn) (not (equal? (extract-file-extension fn) "h")))
					 foreign-includes)])
			     (if (null? deps) '()
				 (list 
				  (format "(*WSLIBDEPS: ~a\n" 
					  (apply string-append
						 (insert-between " " 
								 (list-rem-dups deps equal?))))
				  "*)\n")))

			   "(* This is set by the compiler to reflect the flags passed to wsmlton: *)\n"
			   "val oPTLVL = "(number->string (ws-optimization-level))"\n\n"
			   
                           complex1 complex2 
			   ;; [2008.08.29] Moving foreign to after prims.
		           header1 header2 header5   header3a header3b header4  "\n" 
			   
			   "datatype Ignored = Ignored of int\n"
			   ;(map TypeDecl user-type-decls*)
			   (map TypeDecl union-types)

			   ;; wsinit happens before the individual inits below, and before wsmain:
			   (if driven-by-foreign
			       '("val wsinit = _import \"wsinit\" : (int * string array) -> unit; \n"
				 "val raw_foreign_wserror = _import \"wserror\" : string -> unit;\n"
				 "fun foreign_wserror str = raw_foreign_wserror (str ^  nullChar)\n"
				 "val cmdargs = CommandLine.name() :: CommandLine.arguments()\n"
				 "val argc = length cmdargs\n"
				 "val argv = Array.fromList cmdargs\n"
				 "val _ = wsinit(argc, argv)\n"
				 )
			       '())

			   ;; Block of constants first:
			   "\n(* First a block of constants *)\n\n"
			   (map (lambda (cb) (list "val " cb " ; \n")) cb*)

			   "\n(* Then initialize the global bindings: *)\n"
			   "fun initglobals () = "(indent (apply make-seq init*) "        ")"\n\n"
			   "val _ = run_w_handlers initglobals print errprnt exit_process\n"

			   ;; Handle iterator state in the same way:
			   "\n(* Next the state for all iterates/sources. *)\n\n"
			   (map (lambda (iterstatebind)
				  (list "val " iterstatebind " ; \n"))
			     (apply append (append state1** state2**)))
			   
			   "\n(* Third, function bindings for iterates, sources, unions. *)\n\n"
			   " val "(build-BASE basetype)
			   
			   " fun ignored () = () \n" ;; starts off the block of mutually recursive defs
			   (map (lambda (x) (list "\n val " x "\n"))
			     ;; We reverse it because we wire FORWARD
			     (reverse oper*))

			   src*
			  
			   " \n\n"

			   ;; We need to call init functions for inlined C code, if it exists:
			   (if (null? CinitCalls) '()
			       '("val ws_inlinedC_initialization = _import \"ws_inlinedC_initialization\" : unit -> unit;\n"
				 "val _ = ws_inlinedC_initialization()\n"
				 ))

			   ;; We either call the foreign wsmain or start our scheduler.
			   (if driven-by-foreign
			       '("val wsmain = _import \"wsmain\" : (int * string array) -> unit; \n"
				 "val _ = wsmain(argc, argv)")
			       (list 
				"val _ = (\n"
				"\n(*  Initialize the scheduler. *)\n"
				(map (lambda (x) (if (null? x) '() (list x ";\n"))) init1*)
				"\n\n(*  Then run it *)\n"			   
				"runMain runScheduler\n"
				")\n"
				))

;			   "try runScheduler()\n"
;			   "with End_of_file -> Printf.printf \"Reached end of file.\n\";;\n"
;			   "\nPrintf.printf \"Query completed.\\n\";;\n"


			   )])
	   result)]
	   [,other ;; Otherwise it's an invalid program.
	    (error 'emit-mlton-wsquery "ERROR: bad top-level WS program: ~s" other)]))))


; ======================================================================
;;; Helper functions for handling different program contexts:


#;
;; The incoming values already have indices on them, this just needs
;; to pass them through.
(define (Union union)
  (match union
    [(union (name ,name) (output-type ,ty) (incoming ,up* ...) (outgoing ,down* ...))
     (let ([emitter (Emit down*)])
       ;(list " "(Var name)" = " (make-fun '("x") ((Emit down*) "x")) " \n")
       (make-fun-binding name '("x")((Emit down*) "x") )
       ;(list " "(Var name)" = " (make-fun '("x") ) " \n")
       )]
    
    ;; For MLTON, this could probably be implemented on top of unionList/unionN with no loss of efficiency.
    [(merge (name ,name) (output-type ,ty) (incoming ,astrm ,bstrm) (outgoing ,down* ...))
     (list "\n  (* Merge operator: *)\n"
      (make-fun-binding name '("x") ((Emit down*) "x") ))]
    
))

        
(define (Var var)
  (ASSERT symbol? var)
  ;; This is the place to do any name mangling. 
  ;; We prefix with an underscore to avoid names beginning in capital letters.
  (if (real-primitive? var)
      (symbol->string var)
      (string-append "var_" (symbol->string var))))

;; Really the RHS could be any expression, it just does not have a stream type.
(define (ConstBind b)
  (match b

    [(,[Var -> v] ,t ,rhs)
     (make-bind
      `[,v ,t ,(Expr rhs 
		 (lambda (_) 
		   (error 'ConstBind 
			  "shouldn't have any 'emit's within a constant (non-stream) expression")))])]

#|
    [(,[Var -> v] ,t (quote ,[Const -> rhs]))
     (list v " = " rhs)]

    [(,[Var -> v] ,t ,rhs)
     (guard (symbol? rhs))
     ;; This is messed up:
     (list v " = " (Var rhs))]

    ;; Could have assert-type:
    [(,v ,t (,ann ,_ ,rhs))
     (guard (memq ann '(assert-type )))
     (ConstBind `(,v ,t ,rhs))]
|#
    [,oth (error 'ConstBind "Bad ConstBind, got ~s" oth)]))

;; This handles null characters correctly.
;; (VERY INEFFICIENT CURRENTLY .. BAD FOR OUR LARGE INLINEC STRINGS)
(define (print-mlton-string str)
  (list "\""
    (list->string
     (match (string->list str)
       [() '()]
       ;; Something that's already quoted.
      ; [(#\\ ,c . ,[tl]) (list* #\\ c tl)]
       
       [(#\nul     . ,[tl]) (cons* #\\ #\0 #\0 #\0 tl)]
       [(#\newline . ,[tl]) (cons* #\\ #\n tl)]
       [(#\tab     . ,[tl]) (cons* #\\ #\t tl)]
     ;	   [(#\r)       (list #\\ #\r)]
       [(#\" . ,[tl])       (cons* #\\ #\" tl)]
;       [(#\' . ,[tl])       (cons* #\\ #\' tl)]
       [(#\\ . ,[tl])       (cons* #\\ #\\ tl)]
       [(,a . ,[tl]) (cons a tl)]
       ))
    "\""))

(define Const
  (case-lambda
    [(datum) (Const datum #f)]
   [(datum ty)
    (cond
     ;; This indicates that we have a forall a.a value...
     [(eq? datum 'BOTTOM) 
      (wrap "(print_endline \"Tried to evaluate BOTTOM value!\"; wserror \"BOTTOM\")")] ;; Should probably generate an error.

     [(eq? datum 'UNIT) "()"]     
     [(null? datum) "[]"]
     [(eq? datum #t) "true"]
     [(eq? datum #f) "false"]
     [(string? datum) (print-mlton-string datum)]
     [(double? datum)  (format-float (double-val datum))]
     [(flonum? datum)  (format-float datum)]
     [(cflonum? datum) (format "{real=~a, imag=~a }" 
			       (real-part datum)
			       (imag-part datum))]
     [(integer? datum)  
      (let ([txt (if (< datum 0)
		      (format "(~a~s)" #\~ (* -1 datum))
		      (format "(~s)" datum))])
	(case ty	
	  [(Int Int16 Int32 Int64) txt]
	  [(Uint8)  (format "(Word8.fromInt ~a)" txt)]
	  [(Uint16) (format "(Word16.fromInt ~a)" txt)]
	  ;; [2008.08.27] This may need to change in the future, but
	  ;; for now unannotated ints are allowed, and they are "Int"s.
	  [(#f) txt]
	  [else (error 'emit-mlton:Const "unhandled type annotation ~s on integer constant: ~s" ty datum)]))]

     [(eq? datum 'nullseg) 
      ;(error 'emit-mlton:Const "Got 'nullseg in Const...")
      "(nullseg())"]
     ;[(sigseg? datum) ]

     [(char? datum) (list "#" (print-mlton-string (list->string (list datum))))]

     [(eq? datum 'nulltimebase) "(nullTimebase())"]

     ;[(eq? datum 'nulltimebase)  (wrap "WSNULLTIMEBASE")]     
     [(list? datum)
      (list "[" (insert-between ", " (map Const datum)) "]")]
     [(vector? datum)
      (ASSERT (not (zero? (vector-length datum))))
      (list "(Array.fromList "(Const (vector->list datum))")")]

     [else (error 'emit-mlton:Const "not a Mlton-compatible literal (currently): ~s" datum)])]))


;; This handles one of the statements that are part of query initialization.
;; (Initializing global state.)
(define (Effect expr)
  (Expr expr 
	(lambda (_) 
	  (error 'Effect
		 "shouldn't have any 'emit's within an initialization expression"))))

;; ================================================================================

;; Converts hertz to microseconds:
(define (rate->timestep freq)
  (when (zero? freq) (error 'rate->timestep "sampling rate of zero is not permitted"))
  (flonum->fixnum (* 1000000 (/ 1.0 freq))))

;; Returns: a binding, top state (also bindings), and initialization code.
(define (Source src only-one-source?)
    (define (E x) (Expr x 'noemits!))
  (match src
    [((name ,[Var -> v]) (output-type ,ty) (code ,app) (outgoing ,downstrm ...))
     (match app
       [(timer ,annot ,[peel-annotations -> arg])
	(if only-one-source?
	    ;; With only one timer, things are simple:
	    (values 
	     (list "fun " v "() = " 
		   (make-seq
		    (make-while "true" ((Emit downstrm) "()"))
		    "SE(0, fn _ => wserror(\"should never happen\"))\n"))
	     '() 
	     ;; We put ourselves in the scheduling queue only once.  We run forever when called:
	     `("(* Seed the schedule with timer datasource: *)\n"
	       "schedule := SE(0,",v") :: !schedule\n")
	     )
	    (match arg
	      [',rate 
	       (let ([r ;(Expr rate 'noemitsallowed!)
		      (number->string (rate->timestep rate))]
		     [vtime (Var (unique-name 'virttime))])
		 (values 	
		  ;; First, a function binding that drives the source.
		  (list "val " v " = let fun "v" () = " 
			(indent  
			 (make-seq
			  `(,vtime " := Int64.+ (!",vtime", ",r")")
			  ((Emit downstrm) "()")
			  `("SE (!",vtime",",v")\n"))
			 "    ")
			"\n in "v" end"
			"\n\n")

		  ;; Second, top level state bindings.
		  (list (make-bind `(,vtime (Ref Int64) "ref 0")))
		  
		  ;; Third, initialization statement:
		  `("(* Seed the schedule with timer datasource: *)\n"
		    "schedule := SE(0,",v") :: !schedule\n")))]))]

       [(inline_C ',decls ',init)
	;; This returns nothing... it adds the C code to a top-level accumulator.
	(set! extraCdecls (cons decls extraCdecls))
	(when (not (equal? init ""))
	  (set! CinitCalls  (cons init CinitCalls)))
	(values '() '() '())
	]

       [(__foreign_source ',name ',includes ',types)
	(for-each add-file! includes)
	(set! driven-by-foreign #t)
	(let ([tupty 
	       (match types
		 ;; Only allowing simple scalar types for now (Int, Float)
		 [(Stream #(,ty* ...)) (Type (list->vector ty*))]
		  ;(insert-between " * " (map symbol->string types))
		 [(Stream ,ty) (Type ty)])
	       ])
	  (values
	   ;; A function that pushes data into the system.
	   ;; Don't use the scheduler at all.
	   ;; Do catch errors however.
	   (list "\nfun " v " elt = \n"
		 "  run_w_handlers (fn () => "((Emit downstrm) "elt")") foreign_wserror foreign_wserror exit_foreignsrc  \n\n"
		 `("val _ = (_export \"",name"\" : (",tupty" -> int) -> unit;) ",v" \n")
		 ;"val wsmain = _import \"wsmain\" : unit -> unit; \n"
		 ;(map (lambda (init) `("val ",init" = _import \"",init"\" : unit -> unit ;\n")) initCcalls)
		 )
	   ;; No top-level state bindings. 
	   '()
	   ;; We call into C to get the thing started.
	   ;(map (lambda (init) (make-app init (list (make-tuple-code)))) initCcalls))
	   ;"wsmain()\n"
	   '()
	   ))]

       ;; Reading from ensbox hardware.
       [(ensBoxAudioAll)
	;; Don't worry about the so-called "scheduler", just hook up
	;; the hardware source to the data flow entry point.
	(values
	 ;; Register this exported callback.

	 (let ([bod (make-let `(["v" (Array Int16) "(unpack_int16_array_Pointer p n)"])
  	              (make-seq
		       ((Emit downstrm) 
			"toSigseg(v, !stamp, 0)")
		       "stamp := Int64.+ (!stamp, n)"))])
	   (list "val _ = "
	    (make-app "ensbox_entry"
	      (list (make-let '(["stamp" "ref (Int64.fromInt 0)"])
		    (make-fun (list (make-tuple-code "p" "n"))
		      (if #t
			  bod 
			  (make-app "runMain" (list (make-fun '() bod)))
			  )))))))
	 '() ;; Second, top level bindings	 
	 '("(runMain init_ensbox)") ;; Third, initialization statement.
	 )]
       
       ;[,other (values "UNKNOWNSRC\n" "UNKNOWNSRC\n")]
       
       )]))

(define (Sources src*)
  ;; Should not count inline-C:
  (define only1? (= 1 (length src*)))
  (match src*
    [(,[(lambda (x) (Source x only1?)) -> src* state1** init1*] ...)
     (values src* state1** init1*)]))

(define (ReadFile name code upstream downstrm)
    (set! name (Var name))
  (match code
    [(__readFile ,annot ,[(lambda (x) (Expr x #f)) -> file] ,[Var -> source] ',mode ',repeats ',skipbytes ',offset ',winsize ',types)
     (cond
      [(equal? mode "text") 	  
       (if (not (zero? repeats))
	   (error 'emit-mlton "MLton text mode reader doesn't support replaying the file yet."))
       (if (not (zero? winsize))
	   (error 'emit-mlton "MLton text mode reader doesn't support windowing right now."))
       
       ;; This builds a call to textFileReader
       (let* (
	      [names (map (lambda (_) (Var (unique-name 'elmt))) types)]
	      [lspat (list "[" (insert-between ", " names) "]")]
	      [desome (lambda (x) (list 
				   "(case "x" of SOME x => x | NONE => raise WSError \"could not parse data from file\")"))]
	      [tuppat (list "("
			    (insert-between ", "
					    (map (lambda (name ty)
						   (match ty
						     [Float  (desome (list "Real32.fromString "name))]
						     [Double (desome (list "Real64.fromString "name))]
						     [Int    (desome (format "~a.fromString ~a" int-module name))]
						     [Int16  (desome (list "Int16.fromString "name))]
						     [Int64  (desome (list "Int64.fromString "name))]
						     [String name]
						     ))
					      names types))
			    ")")])
	 ;; Builds a function representing the stream
         (list 	  
	   name" = textFileReader ("file", "
	   (number->string (length names))", fn "lspat" => "tuppat") " 
	   (indent (make-fun '("x") ((Emit downstrm) "x")) "      ")
	   )
	 )]
      
      [(equal? mode "binary")
       (let ([homogenous? (homogenous-sizes? types)]
	     [tuptyp (if (= 1 (length types))
			 (car types)
			 (list->vector types ))])
	  (list name " = "
		(make-let `(["binreader"
			       ,(format "BinIO.vector -> int -> ~a" (Type tuptyp))
			       ,(indent (build-binary-reader types homogenous?) "    ")])
		    (list "    "
			  (if (> winsize 0) "dataFileWindowed" "dataFile")
			  (make-tuple-code file 
					   (list "\"" mode "\"")
					   (number->string repeats)
					   )
			  " \n"
			  (make-tuple-code "binreader"
					   (number->string (apply + (map type->width types)))
					   (number->string skipbytes)
					   (number->string offset))
			  " \n"
			  (indent (make-fun '("x") ((Emit downstrm) "x")) "      ")
			  ;; Also an additional arguments for
			  ;; dataFileWindowed.  One that has the window size.
			  ;; And one that bundles up array create/set and
			  ;; toSigseg.
			  (if (> winsize 0)
			      (begin 
				;; This is necessary for now:
				;; Only allowing windowed reads for ONE-TUPLES.
				(ASSERT (= (length types) 1))
				(list 
				 " "(number->string winsize)" "	   

				 ;; Next a multiplier for indices... this is a hack to get around SML's *LAME* "pack" functionality.
			  ;;; HACK HACK HACK:
				 (if homogenous? " 1 "
			      ;;; If not using *exclusively* the "wordIndexed" functions, this should be:
				     (list " "(number->string (type->width (car types)))" "))

				 (let ()
				   (make-tuple-code 
				    (DispatchOnArrayType 'Array:makeUNSAFE tuptyp)
				    (DispatchOnArrayType 'Array:set        tuptyp)
				    (DispatchOnArrayType 'toSigseg         tuptyp)))
				 ))
			      "")
			  "\n"
			  ))))]
      [else (error 'readFile "mode not handled yet in MLton backend: ~s" mode)]
      )]))


(define (type->reader t) 
  (match t
    [Int    "read_int32"]
    [Int16  "read_int16"]
    [Int64  "read_int64"]
    [Float  "read_real32"]
    ))

(define (homogenous-sizes? types) 
  (cond
   [(null? types) (error 'homogenous-sizes? "expects at least one type")]
   [(null? (cdr types)) #t]
   [else (apply = (map type->width types))]))

(define (build-binary-reader types homogenous?)
  (define widths (map type->width types))
  (make-fun '("vec" "ind")
  (list 
   ;"  let pos = ref ind in \n"
   (apply make-tuple-code
     (mapi (lambda (i t)
	     (list (if homogenous? 
		       (format "~a_wordIndexed" (type->reader t))
		       (type->reader t))
		   " vec (ind + "
		   (if homogenous? 
		       ;; If they're homogenous we do position (word) indexed rather than byte indexed.
		       (number->string i) 
		       (number->string (apply + (list-head widths i))))
		   ")"))
	   types))
   "\n")))

(define (build-text-reader types)
  (match types
    [(Int)
     "8888888"
     ]))


;; It is error prone to keep writing this:
(define (sigseg-prim? p)
  (memq p '(joinsegs subseg width toSigseg toArray timebase start end seg_get)))

;; Converts an operator based on the array element type.
;; For mlton this does the same thing irrespective of element type.
(define (DispatchOnArrayType op elt)
  (case op
    [(Array:null) "(Array.fromList [])"]
    [(Array:make) "Array.array"]
    ;; Just makes a normal array with zeros:
    ;; Cut/paste from above:

    ;; Can't do anything smart with this right now:
    ;; Could use MONO Arrays... (Like we do in Caml)
    [(Array:makeUNSAFE)
     (lnboth (make-fun '("n") 
	 (make-app (DispatchOnArrayType 'Array:make elt)
		   (list 
		    (make-tuple-code "n" 
		     (make-mlton-zero-for-type elt))))))]

    [(Array:length) "Array.length"]
    [(Array:set)    "Array.update"]
    [(Array:ref)    "Array.sub"]

    [(nullseg) "(nullseg())"]
    ;; We just use the normal name conversion:
    [else (if (sigseg-prim? op)
	      (ASSERT (PrimName op)) ;; Defined in the libray.
	      (error 'DispatchOnArrayType "don't know how to dispatch this operator: ~s" op))]
    ))

(define (make-mlton-zero-for-type t)
    (match (match t
	     [Int   ''0]
	     [Bool  "false"]
	     [String "\"\""]

	     [Uint8  "(Word8.fromInt 0)"]
	     [Uint16 "(Word16.fromInt 0)"]

	     [Int16 "(Int16.fromInt 0)"]
	     [Int32 "(Int32.fromInt 0)"]
	     [Int64 "(Int64.fromInt 0)"]
	     [Float ''0.0]
	     [Double ''0.0]
	     [Complex `',(make-rectangular  0.0 0.0);''0.0+0.0i
	      ]
	     [#(,[make-mlton-zero-for-type -> t*] ...) 
	      (apply make-tuple-code t*)]

	     ;; Well this is downright nasty:
	     ;[(Char) "#\" \""]
	     [Char "#\"\\000\""]

	     [(Sigseg ,_) "(nullseg())"]
	     [(Array ,_) "(Array.fromList [])"]
	     [(List ,_) "[]"]

	     [,oth (error 'make-mlton-zero-for-type "unhandled type: ~s" oth)])
      [(quote ,c) (Const c)]
      [,str       str]))


;(define (ForeignApp ls)
(define (ForeignApp realname type rator rand*)
  (match type 
    [(,argty* ... -> ,retty)
     (marshal-from-foreign retty
      (make-app (Var rator) 
	       (list (apply make-tuple-code 
			    (map marshal-to-foreign argty* rand*)))))
     ]))

(define (ForeignEntry cname files ty)
  (match ty
    [(,argty* ... -> ,retty)	
     (for-each add-file! files)
     `(" _import \"",cname"\" : ",(ForeignType ty)";\n")]))


(define (SumName s) (** "ty_" (symbol->string s)))
(define (VariantName s) (symbol->string s))

(define (WScase x names funs emitter)
  (define (myExpr x) (Expr x emitter))
  (let ([x (myExpr x)])
    (list "case " x " of\n"
	  (insert-between "\n | "
	    (map (lambda (name fun)
		   (match fun
		     [(lambda (,[Var -> v*] ...) ,ty* ,[myExpr -> bod])
		      (list (VariantName name) " "(apply make-tuple-code v*) " => " bod)
		      ]))
	      names funs))
	  )))

(define (Prim expr emitter)
  (define (myExpr x) (Expr x emitter))
  (match expr

    ;; Handle equality tests.  This is more laborious than in Caml.
    [(,eq (assert-type ,ty ,[myExpr -> x]) ,[myExpr -> y])
     (guard (memq eq '(wsequal? =)))
     (make-app (build-equality-test ty) (list (make-tuple-code x y)))]

    ;; Print is required to be pre-annotated with a type.
    ;; (We can no longer do recover-type.)
    [(print (assert-type ,t ,e))    
     `("(print ",(Prim `(show (assert-type ,t ,e)) emitter)")")]
    [(print ,_) (error 'emit-c:Effect "print should have a type-assertion around its argument: ~s" _)]
    [(show (assert-type ,t ,[myExpr -> e]))
     `("((",(build-show t)") ",e")")]
    [(wserror ,[myExpr -> s])
     ;; Should declare a special WSException or something:
     `("(wserror ",s")")]

    ;; This is the new style for converting between numeric types.
    [(__cast_num ',frm ',to ,[myExpr -> e])
     ;; HACK: don't have string-downcase in ikarus yet.
     ;(string-downcase (symbol->string frm))
     (define (my-downcase sym)
       (match sym
	 [Int    "int"]
	 [Int16 "int16"]
	 [Int32 "int32"]
	 [Int64 "int64"]
	 [Uint8 "uint8"]
	 [Uint16 "uint16"]
	 [Float "float"]
	 [Double "double"]
	 [Complex "Complex"]))
     ;; HACK: this only works if there is a legacy conversion at these types:          
     ;; I haven't implemented the general method yet.
     (let* ([convert (string->symbol (** (my-downcase frm) "To" (symbol->string to)))]
	    [prim (PrimName convert)])
       (unless prim (error 'emit-mlton:Prim "no conversion implementation for ~a" convert))
       (make-prim-app prim (list e)))]

    ;; This is annoying, but we use different sigseg prims based on the type of Array that they use.
    [(,prim (assert-type (,tc ,elt) ,first) ,rest ...)
     (guard (memq tc '(Array Sigseg)) (sigseg-prim? prim))
     (make-prim-app (DispatchOnArrayType prim elt)
	       (map myExpr (cons first rest)))]
    ;; Safety net:
    [(,prim ,_ ...) (guard (sigseg-prim? prim))
     (error 'emit-mlton:Prim "missed this sigseg prim: ~s" prim)]

    ;; Now do array prims in much the same way:
    [(assert-type (Array ,elt) (,prim ,[myExpr -> arg*] ...))
     (guard (memq prim '(Array:make Array:makeUNSAFE)))
     (make-prim-app (DispatchOnArrayType prim elt) arg*)]
    [(,prim ,first ,[myExpr -> rest] ...)
     (guard (memq prim '(Array:ref Array:set Array:length)))
     (match first 
       [(assert-type (Array ,elt) ,_)
	(make-prim-app (DispatchOnArrayType prim elt)
		       (cons (myExpr first) rest))])]

    [(ptrIsNull ,[myExpr -> ptr]) 
     (list "(EQUAL = MLton.Pointer.compare (MLton.Pointer.null, "ptr"))")]
    [(ptrMakeNull) "(MLton.Pointer.null)"]

    ;; This unpacks a foreign array into a WS array:
    [(assert-type (Array ,elt) (ptrToArray ,[myExpr -> ptr] ,[myExpr -> len]))
     (let ([getter 
	    (lambda ()
	      (case elt
		[(Int16)  "getInt16"]
		[(Int)    "getInt32"]
		[(Int64)  "getInt64"]
		[(Float)  "getReal32"]
		[(Double) "getReal64"]
		
		;; TODO, complex numbers can be done but are a tad trickier.
		[else (error 'ptrToArray "Unsupported element type for wsmlton: ~s" elt)]
		))])
       (let ([fun (if (eq? elt 'Char)
		      "(fn (ptr, offset) => Char.chr (Word8.toInt (MLton.Pointer.getWord8 (ptr, offset))))"
		      (list "MLton.Pointer."(getter)))])
	 (list "Array.tabulate ("len", "
	       "fn i => "fun" ("ptr", i))")))]

    ;val raw_fftR2C = _import "raw_fftR2C" : (Real32.real array * Word64.word array * int) -> unit;
    [(fftR2C ,[myExpr -> arr])
     (list 
      "let val inbuf  = "arr" \n"
      "    val len    = Array.length inbuf \n"
      "    val len2   = len div 2 + 1      \n"
      "    val outbuf = Array.array (len2, (Word64.fromInt 0)) \n"
      "in \n"
      "  (raw_fftR2C (inbuf,outbuf, len); \n"
      "   Array.tabulate (len2, fn i => unpack_complex (Array.sub (outbuf,i)))) \n"
      "end \n")]

    [(ifftC2R ,[myExpr -> arr])
     (list 
      "let val inbuf  = pack_complex_array "arr" \n"
      "    val len    = Array.length inbuf \n"
      "    val len2   = (len - 1) * 2      \n"
      "    val outbuf = Array.array (len2, (Real32.fromInt 0)) \n"
      "in \n"
      "  (raw_ifftC2R (inbuf,outbuf, len); \n"
      "   outbuf)\n"
;      "   Array.tabulate (len2, fn i => unpack_complex (Array.sub (outbuf,i)))) \n"
      "end \n")]


    [(memoized_fftR2C ,[myExpr -> arr])
     (list 
      "let val inbuf  = "arr" \n"
      "    val len    = Array.length inbuf \n"
      "    val len2   = len div 2 + 1      \n"
;      "    val outbuf = Array.array (len2, (Word64.fromInt 0)) \n"
      "    val ptr = memoized_fftR2C (inbuf, len) \n"
      "in \n"
;      "   (* It is vital that memoized_fftR2C not be called again until we unpack the outbuf: *) \n"
      "   ( \n"
      "  unpack_complex_array_Pointer ptr len2 \n"
;;      "    print (\"Outbuf size: \"^ (Int.toString (Array.length outbuf)) ); \n"
;      "    Array.tabulate (len2, fn i => unpack_complex (Array.sub (outbuf,i))) \n"
      "    ) \n"
      "end \n")]
;    [(memoized_fftR2C ,[myExpr -> arr]) (list "(memoized_fftR2C_wrapper "arr")")]


    ;; Probably should be able to specialize these earlier....
    ;; But in other backends (C, caml), they remain polymorphic.
    [(,op (assert-type ,ty ,[myExpr -> x]) ,[myExpr -> y])
     (guard (memq op '(< <= >= > max min)))
     (make-app (case ty
		 [(Int)    (format "~a.~s" int-module op)]
		 [(Int16)  (format "Int16.~s" op)]
		 [(Int32)  (format "Int32.~s" op)]
		 [(Int64)  (format "Int64.~s" op)]
		 [(Uint16) (format "Word16.~s" op)]
		 [(Uint8)  (format "Word8.~s" op)]
		 [(Float)  (format "Real32.~s" op)]
		 [(Double) (format "Real64.~s" op)]
		 [else (error 'emit-mlton "unhandled type for comparison operator ~s: ~s" op ty)]
		 )
	       (list (make-tuple-code x y)))]

    ;; Safety net:
    [(,prim ,_ ...)     
     (guard (memq prim '(Array:make Array:makeUNSAFE Array:ref Array:set Array:length
  	                 < <= >= > max min
			 )))
     (error 'emit-mlton:Prim "missed this array prim: ~s" (cons prim _))]
 
    [(assert-type ,t ,[primapp]) primapp]

    [(tuple . ,_) (error 'emit-mlton:Prim "got a tuple: ~s" `(tuple . ,_))]

    [(,prim ,[myExpr -> rands] ...) (guard (real-primitive? prim))
     (make-prim-app 
      (cond
       [(PrimName prim) => (lambda (x) x)]
       [else (error 'emit-mlton:Prim "currently unhandled: ~s" prim)])
      rands)]
))



;;================================================================================
;; Import the rest of our functionality from the shared module.

;; For this, we use a little bit of an inheritance hack, we don't want
;; to actually bother with some real OOP system just for this.

;; This packages up the MLton specific functionality to pass back up to the parent module.
;; This is not complete, just what I happen to be using.
(define MLtonSpecific 
  (lambda args
    (apply
     (make-dispatcher (car args)
		      
        make-let 
        make-letrec
	make-tuple-code 
	make-fun
	make-app
	make-for
	make-while
	make-fun-binding
	
	Var Prim Const 
	DispatchOnArrayType
	Type ReadFile
	ForeignApp ForeignEntry
	WScase SumName VariantName
	)
     (cdr args))))

;; Here we import bindings from the "superclass"
;; We pass upwards our "method table"
(define-values (Expr Operator Emit make-bind) 
  (sharedEmitCases MLtonSpecific))




;;================================================================================

;; This just converts the name of the primitive, for those primitives
;; that map directly onto Mlton functions:
;;
;; SML CONVENTION: ALL PRIMITIVES TAKE MULTIPLE ARGUMENTS AS TUPLES!
(define PrimName
  (let ()
    
  (define (compose . ls)
    (make-fun '("x")
       (let loop ([ls ls])
	 (if (null? ls)
	     "x"
	     (make-app (car ls) (list (loop (cdr ls))))))))
  (define sametable ;; Prims with the same name:
    '(
      joinsegs subseg width toSigseg toArray timebase
      not 
      
      m_invert 
      ;;wserror ;generic_hash 
      ))
  (define doubleToFloat (make-fun '("x") "Real32.fromLarge IEEEReal.TO_NEAREST (Real64.toLarge x)"))
  (define aliastable
    `(
      
      [_+I16 "( Int16.+)"]
      [_-I16 "( Int16.-)"] 
      [*I16 "( Int16.* )"] 
      [/I16 "( Int16.quot )"]
      [^I16 "powInt16"]

      [_+I32 "( Int32.+)"]
      [_-I32 "( Int32.-)"] 
      [*I32 "( Int32.* )"] 
      [/I32 "( Int32.quot )"]
      [^I32 "powInt32"]
      
      [_+I64 "( Int64.+)"]
      [_-I64 "( Int64.-)"] 
      [*I64 "( Int64.* )"] 
      [/I64 "( Int64.quot )"]
      [^I64 "powInt64"]

      [_+_ ,(format "(~s.+)" int-module)]  
      [_-_ ,(format "(~s.-)" int-module)] 
      [*_ ,(format "(~s.*)" int-module)]
      [/_ ,(format "(~s.quot)" int-module)]
      [^_ powInt] ;; Defined in prims.sml

      [_+U8 "( Word8.+)"]
      [_-U8 "( Word8.-)"] 
      [*U8 "( Word8.* )"] 
      [/U8 "( Word8.div )"]
      ;[^U8 "powWord8"]

      [_+U16 "( Word16.+)"]
      [_-U16 "( Word16.-)"] 
      [*U16 "( Word16.* )"] 
      [/U16 "( Word16.div )"]
      ;[^U16 "powWord16"]

      [_+. "( Real32.+ )"]
      [_-. "( Real32.- )"] 
      [*. "( Real32.* )"] 
      [/. "( Real32./ )"]
      [^. "( Real32.Math.pow )"]

      [_+D "( Real64.+ )"]
      [_-D "( Real64.- )"] 
      [*D "( Real64.* )"] 
      [/D "( Real64./ )"]
      [^D "( Real64.Math.pow )"]

      ;; UHH UNFORTUNATELY REAL32 != REAL
      [cos  Real32.Math.cos]
      [sin  Real32.Math.sin]
      [tan  Real32.Math.tan] 
      [acos Real32.Math.acos]
      [asin Real32.Math.asin]
      [atan Real32.Math.atan]

      [absI16 Int16.abs]
      [absI64 Int64.abs]
      [absI   (format "~s.abs" int-module)]
      [absF   Real32.abs]
      [absD   Real64.abs]
      [absC   Complex.magnitude]

      [logF Real32.Math.ln]
      [logD Real64.Math.ln]

      [randomI "(fn n => Int.mod (Word.toInt (MLton.Random.rand ()),  n))"]

      [List:append List.@]
      [List:zip ListPair.zip]
      [List:toArray  Array.fromList]

;; TODO ==========================


      [_+: "Complex.+"]
      [_-: "Complex.-"] 
      [*: "Complex.*"] 
      [/: "Complex./"]
      [^: "Complex.pow"]


;; SHARED ==========================

      [Mutable:ref   "ref"]
      [deref         "!"]

      [sqrtI "(fn x => (Int32.fromLarge (Real32.toLargeInt IEEEReal.TO_ZERO (Real32.Math.sqrt (Real32.fromLarge (Int32.toLarge x))))))"]
      [sqrtF Real32.Math.sqrt]
      [sqrtD Real64.Math.sqrt]
      [sqrtC Complex.sqrt]

      [realpart "(fn {real, imag} => real)"]
      [imagpart "(fn {real, imag} => imag)"]

      [cons ,(make-fun (list (make-tuple-code "x" "y")) "x::y")]
      [car List.hd]
      [cdr List.tl]
      [List:length  List.length]
      [List:reverse List.rev]
      [List:ref     List.nth]
      [List:make    "(fn (n,elm) => List.tabulate(n, (fn _ => elm)))"]


      [makeComplex  ,(make-fun (list (make-tuple-code "r" "i")) "({real= r, imag= i})")]

      ;; [2007.08.10] We found that getting rid of conversions toLarge is enormously good:

      ;[int16ToInt     ,(compose (format "~a.fromLarge" int-module) "Int16.toLarge")]
      [int16ToInt     "Int16.toInt"]
      [int16ToInt32   ,(compose "Int32.fromLarge" "Int16.toLarge")]
      [int16ToInt64   ,(compose "Int64.fromLarge" "Int16.toLarge")]
      [int16ToFloat   ,(compose "Real32.fromInt" "Int16.toInt")]
      [int16ToDouble  ,(compose "Real64.fromInt" "Int16.toInt")]
      [int16ToComplex  ,(make-fun '("n") "({real= Real32.fromInt (Int16.toInt n), imag= Real32.fromInt 0})")]

      ;[int64ToInt     ,(compose (format "~a.fromLarge" int-module) "Int64.toLarge")]
      [int64ToInt     "Int64.toInt"] ;; ASSUMING Int = Int32, MLTON SPECIFIC
      [int64ToInt32   ,(compose "Int32.fromLarge" "Int64.toLarge")]
      [int64ToInt16   ,(compose "Int16.fromLarge" "Int64.toLarge")]
      [int64ToFloat   ,(compose "Real32.fromLargeInt" "Int64.toLarge")]
      [int64ToDouble  ,(compose "Real64.fromLargeInt" "Int64.toLarge")]
      [int64ToComplex  ,(make-fun '("n") "({real= Real32.fromLargeInt (Int64.toLarge n), imag= Real32.fromInt 0})")]


      [int32ToInt     "Int32.toInt"] ;; ASSUMING Int = Int32, MLTON SPECIFIC
      [int32ToInt16   ,(compose "Int16.fromLarge" "Int16.toLarge")]
      [int32ToFloat   ,(compose "Real32.fromLargeInt" "Int32.toLarge")]
      [int32ToDouble  ,(compose "Real32.fromLargeInt" "Int32.toLarge")]
      [int32ToComplex  ,(make-fun '("n") "({real= Real32.fromLargeInt (Int32.toLarge n), imag= Real32.fromInt 0})")]


      ;[intToInt16     ,(compose "Int16.fromLarge"     (format "~a.toLarge" int-module))]
;      [intToInt64     ,(compose "Int64.fromLarge"     (format "~a.toLarge" int-module))]
;      [intToFloat     ,(compose "Real32.fromLargeInt" (format "~a.toLarge" int-module))]
      ;[intToDouble    ,(compose "Real64.fromLargeInt" (format "~a.toLarge" int-module))]
      [intToInt16     "Int16.fromInt" ]
      [intToInt32     "Int32.fromInt" ]
      [intToInt64     "Int64.fromInt"]
      [intToFloat     "Real32.fromInt"]
      [intToDouble    "Real64.fromInt"]
      [intToComplex  ,(make-fun '("n") "({real= Real32.fromInt n, imag= Real32.fromInt 0})")]

      [intToUint16     "Word16.fromInt" ]
      [intToUint8      "Word8.fromInt" ]

      [uint16ToDouble ,(compose "Real64.fromInt" "Word16.toInt")]

      [uint8ToInt    "Word8.toInt"]
      [uint8ToDouble ,(compose "Real64.fromInt" "Word8.toInt")]

      ;[floatToInt     ,(make-fun '("x") "Int32.fromLarge (Real32.toLargeInt IEEEReal.TO_ZERO x)")]
      [floatToInt     "Real32.toInt IEEEReal.TO_ZERO"]
      [floatToInt16   ,(make-fun '("x") "Int16.fromInt   (Real32.toInt IEEEReal.TO_ZERO x)")]
      [floatToInt32   ,(make-fun '("x") "Int32.fromLarge (Real32.toLargeInt IEEEReal.TO_ZERO x)")]
      [floatToInt64   ,(make-fun '("x") "Int64.fromLarge (Real32.toLargeInt IEEEReal.TO_ZERO x)")]
      [floatToDouble  ,(make-fun '("x") "Real64.fromLarge IEEEReal.TO_NEAREST (Real32.toLarge x)")]
      [floatToComplex ,(make-fun '("n") "({real= n, imag= Real32.fromInt 0})")]

      ;[doubleToInt    ,(compose (format "~a.fromLarge" int-module) "(Real64.toLargeInt IEEEReal.TO_ZERO)")]
      [doubleToInt     "Real64.toInt IEEEReal.TO_ZERO"]
      [doubleToInt16  ,(make-fun '("x") "Int16.fromInt   (Real64.toInt IEEEReal.TO_ZERO x)")]
      [doubleToInt32  ,(make-fun '("x") "Int32.fromLarge (Real64.toLargeInt IEEEReal.TO_ZERO x)")]
      [doubleToInt64  ,(make-fun '("x") "Int64.fromLarge (Real64.toLargeInt IEEEReal.TO_ZERO x)")]
      [doubleToFloat  ,doubleToFloat]
      [doubleToComplex ,(make-fun '("n") (list "({real= "doubleToFloat" n, imag= Real32.fromInt 0})"))]

      [complexToInt16 "(fn {real,imag} => Int16.fromInt (Real32.toInt IEEEReal.TO_ZERO real))"]
      [complexToInt32 "(fn {real,imag} => Int32.fromLarge (Real32.toLargeInt IEEEReal.TO_ZERO real))"]
      [complexToInt64 "(fn {real,imag} => Int64.fromLarge (Real32.toLargeInt IEEEReal.TO_ZERO real))"]
      [complexToInt   "(fn {real,imag} => (Real32.toInt IEEEReal.TO_ZERO real))"]
      [complexToFloat "(fn {real,imag} => real)"]
      [complexToDouble "(fn {real,imag} => (Real64.fromLarge IEEEReal.TO_ZERO (Real32.toLarge real)))"]

      ;; Should return option type:
      [stringToInt    ("(fn s => case ",(format "~s.fromString" int-module)" s of SOME x => x)")]
      [stringToFloat  "(fn s => case Real32.fromString s of SOME x => x)"]
      [stringToDouble "(fn s => case Real64.fromString s of SOME x => x)"]
;      [stringToComplex "(fun s -> Scanf.sscanf \"%f+%fi\" (fun r i -> {Complex.re=r; Complex.im=i}))"]


      ;; This is a hack to support the uses of the '!' operator:
      [intToInt "(fn x => x)"]
      [int16ToInt16 "(fn x => x)"]
      [int32ToInt32 "(fn x => x)"]
      [int64ToInt64 "(fn x => x)"]
      [floatToFloat "(fn x => x)"]
      [uint16ToUInt16 "(fn x => x)"]
      [uint32ToUInt32 "(fn x => x)"]

      [string-append "(String.^)"] 
      [String:length "String.size"]
      [String:ref    "String.sub"]
      [String:toList   "String.explode"]
      [String:fromList "String.implode"]
      [String:fromArray "string_fromArray"]
      [String:toArray   "string_toArray"]

      [intToChar "Char.chr"]
      [charToInt "Char.ord"]
      
      ;; FIXME: These need to be tested:
      ;; Also, should abstract away these conversions:
      [rshiftI32 ,(make-fun (list (make-tuple-code "i" "n"))
			   "Int32.fromLarge (Word32.toLargeInt (Word32.>> (Word32.fromLargeInt (Int32.toLarge i), n)))")]
      [lshiftI32 ,(make-fun (list (make-tuple-code "i" "n"))
			   "Int32.fromLarge (Word32.toLargeInt (Word32.<< (Word32.fromLargeInt (Int32.toLarge i), n)))")]      

      [logandI32 ,(make-fun (list (make-tuple-code "a" "b"))
			   "Int32.fromLarge (Word32.toLargeInt (Word32.andb (Word32.fromLargeInt (Int32.toLarge a), Word32.fromLargeInt (Int32.toLarge b))))")]


      [rshiftI16 ,(make-fun (list (make-tuple-code "i" "n"))
			   "Int16.fromInt (Word32.toInt (Word32.>> (Word32.fromInt (Int16.toInt i), n)))")]
      [lshiftI16 ,(make-fun (list (make-tuple-code "i" "n"))
			   "Int16.fromInt (Word32.toInt (Word32.<< (Word32.fromInt (Int16.toInt i), n)))")]


      
      [clock    "(fn () => (Time.toReal (#usr (Timer.checkCPUTimer (Timer.totalCPUTimer())))) * 1000.0)"]
      [realtime "(fn () => (Int64.fromLarge (Time.toMilliseconds (Timer.checkRealTimer (Timer.totalRealTimer())))))"]

      [Secret:newTimebase newTimebase]    ;; ADT breakage!

      [roundF  ,(make-fun '("x") "Real32.fromInt (Real32.floor (x + 0.5))")]
      [roundD  ,(make-fun '("x") "Real64.fromInt (Real64.floor (x + 0.5))")]

      [start   ss_start]
      [end     ss_end]
      [seg_get ss_get]
      ))

    (lambda (sym) 
      (cond 
       [(memq sym sametable) (Var sym)]
       [(assq sym aliastable) => (lambda (x) (format "~a" (cadr x)))]
       [else #f])
      )))

;;================================================================================

(define-testing test-emit-mlton
  (default-unit-tester "wavescript_emit-mlton.ss: generating WaveScript Mlton code." 
    `(
      [3 3]

      ;; This makes sure we can generate *something* for all the primitives.
      ,@(map
	 (match-lambda ([,prim ,argtypes ,rettype])
	   `[(',Prim '(,prim ,@(map (lambda (_) (unique-name 'x)) argtypes))
		    (lambda (_) #f)) unspecified]
	   )
	 ;; Quadratic:
	 (let ([exceptions 
		(append 
		 '(;; These are obsolete:
		   eq? locdiff nodeid getID sense even? odd? 
		   ;; These weren't really primitives:    
		   tuple tupref ref deref static statref __foreign foreign foreign_box foreign_source
		   empty-wsrecord wsrecord-select wsrecord-extend wsrecord-restrict
		   ;; These were desugared or reduced to other primitives:
		   ws:or ws:and dataFile 
		   ;; These were resolved into the w/namespace versions:
		   head tail map append fold
		   List:head List:tail  List:is_null 

		   ;; This are handled specially by DispatchOnArrayType.
		   ;; This is due to the dual-representation for arrays.
		   Array:toList Array:make Array:makeUNSAFE Array:get Array:ref Array:length
		   joinsegs subseg width toSigseg toArray timebase start end seg_get
		   
		   ;; These have a special syntax, requiring an assert-type or whatnot:
		   ptrToArray
		   cons car cdr null? prim_window 
		   List:ref List:append List:reverse List:length List:make 
		   < <= >= > max min = 
		   		   
		   randomI

		   ensBoxAudio ensBoxAudioF ensBoxAudioAll
		   		   
		   wsequal? print show seg_get toArray __show_ARRAY __wserror_ARRAY __backtoSTR
		   __stringToInt_ARRAY __stringToFloat_ARRAY __stringToDouble_ARRAY __stringToComplex_ARRAY

		   __cast_num  ^U8 ^U16
		   stringToComplex moduloI

		   ;; TODO, FIXME: These I just haven't gotten to yet:
		   fftC ifftC ifftC2R
		   exptI 

		 lshiftI16 rshiftI16 logorI16 logandI16 logxorI16 
		 lshiftU16 rshiftU16 logorU16 logandU16 logxorU16 
		 lshiftI32 rshiftI32 logorI32 logandI32 logxorI32 

		   gnuplot_array gnuplot_array2d
		   
		   List:assoc List:assoc_update
		   hashrem hashset ;; pure versions
		   Array:map Array:fold
		   internString uninternString

		   exclusivePtr getPtr
		   marshal unmarshal
		   __type_unsafe_write __type_unsafe_read ;; It would be nice to implement these.
		   
		   HashTable:contains HashTable:get HashTable:set_BANG HashTable:rem_BANG 
		   HashTable:make HashTable:rem HashTable:set ;; pure versions
		   )
		 (map car generic-arith-primitives)
		 (map car meta-only-primitives)
		 (map car higher-order-primitives) ;; These were eliminated.
		 (map car wavescript-effectful-primitives) ;; These are handled by Block
		 (map car wavescript-stream-primitives)
		 )])
	   ;; Make some exceptions for things that are in Regiment but not WaveScript.
	   ;; Also exceptions for geneeric prims and other prims that have been desugared.
	   (filter (lambda (e) (not (memq (car e) exceptions)))
	     (append regiment-basic-primitives
		     wavescript-primitives))))
      )))




) ;; End Module


;;================================================================================

