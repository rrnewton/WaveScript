
;;;; .title WaveScript EmitMLton (SML)
;;;; .author Ryan Newton [2007.06.13]

(module emit-mlton mzscheme 
  (require  "../../../plt/common.ss"
	    "../../compiler_components/c_generator.ss" 
	    "../ocaml_bkend/shared-emit-ml.ss")
  (provide emit-mlton-wsquery )
  (chezprovide )  
  (chezimports shared-emit-ml 
	       (except helpers test-this these-tests))

;; MUTABLE
(define union-edges 'union-edges-uninit)

;; Experimenting with both of these:
(define int-module 'Int32)
;(define int-module 'Int)

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
(define (make-app rator rands) (list "(" rator " "(insert-between " " rands) ")"))

;; This is LAME, but having problems with "let val rec" syntax in MLton.
;; This produces the "f x = bod" text.
(define (make-fun-binding name formals funbody)
  (list " " (coerce-id name) " "
	(if (null? formals) "()"
	    (insert-between " " (map coerce-id formals)))
	" = "funbody))

;; Tuple version:
;; ...

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

#;	   (map (lambda (x)
		  (match x
		    [[,lhs ,rhs]     (list (coerce-id lhs) " = " rhs)]
		    ;; Type is a sexp or a string:
		    [[,lhs ,ty ,rhs] (list (coerce-id lhs) " :  "
					   (if (string? ty) ty
					       (Type ty))" = " rhs)]))
	     binds))
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


(define (make-prim-app f x*) (make-app f (list (apply make-tuple x*))))



; make-conditional
(define (ln . args)     (list args "\n"))
(define (lnfst . args)  (list "\n" args))
(define (lnboth . args) (list "\n" args "\n"))

;; If the type needs a specialized hashfun, returns its name,
;; otherwise returns #f.
;(define (HashType k v) )
;(define (SharedPtrType t))

;; This should give you an idea of the mapping between types:
(define (Type t)
  (match t
    [Float     "Real32.real"]
    [Double    "Real64.real"]
    
    [Bool    "bool"]
    [Int     (format "~a.int" int-module)]
    [Int16   "Int16.int"] ;; Not standard SML.

    [Complex  "Complex.complex"]
    [String   "string"]
    [(Ref ,[t]) `("(",t ") ref")]
    [(VQueue ,_) "unit"]

    [Timebase "SigSeg.timebase"] ;; This will change.

    [#() "unit "]

    [#(,[t*] ...) `("(",(insert-between " * " t*)")")]

    ;; Went back and forth on whether this should be a pointer:
    [(Sigseg ,t) 
     `(,(Type t) " sigseg")
#;     (let ([flatty (BigarrayType? t)])
       (if flatty
	   `("(",(Type t)", Bigarray.",flatty"_elt) sigseg_flat")
	   ....))]

    [(Array ,[t])  `("(",t") array")]
    [(List ,[t]) `("(",t") list")]
    ;[(HashTable ,kt ,vt) (SharedPtrType (HashType kt vt))]
    [,other (error 'emit-mlton:Type "Not handled yet.. ~s" other)]))


(define flush "TextIO.flushOut TextIO.stdOut")

(define (build-BASE type)  
  (if (equal? type #())      
      ;`(" baseSink x = print_endline (\"UNIT\"); flush stdout \n")
      `(" baseSink = ",(make-fun '("x") flush) "\n")
      `(" baseSink = ",(make-fun '("x") `("(print_endline (",(build-show type)" x); ",flush")"))
	"\n")
  ))

(define (build-show t)
  (match t

    [Int16 "Int16.toString"] ;; These are just represented as ints.

    ;; ERROR: FIXME:
    [(quote ,_) (make-fun '(_) "(\"POLYMORPHIC_OBJECT\")")]

    [String     (make-fun '("x") "x")]
    [Int    (format "~s.toString" int-module)]
    [Float  "Real32.toString"]
    [Double "Real64.toString"]
    [Bool   "Bool.toString"]
    [Complex "(fn {real,imag} => Real32.toString real ^ \"+\" ^ Real32.toString imag ^ \"i\")"]

    [(List ,[t]) (make-fun '("ls") (list "(\"[\" ^ concat_wsep \", \" (List.map "t" ls) ^ \"]\")"))]
    [(Array ,[t]) (make-fun '("a") (list "(\"[\" ^ concat_wsep \", \" (List.map "t" (arrayToList a)) ^ \"]\")"))]

    ;; Just print range:
    [(Sigseg ,t) 
     (let ([Int (format "~a" int-module)])
       (make-fun '("ss") 
	       (list 
		"(\"[\"^ "Int".toString ("  (DispatchOnArrayType 'start t)
		" ss) ^\", \"^ "Int".toString ("      (DispatchOnArrayType 'end t)
		" ss + 1) ^ \")\")")))]

    [#(,[t*] ...)
     (let ([flds (map Var (map unique-name (make-list (length t*) 'fld)))])
       (list 
	"(fn "(apply make-tuple flds)" =>\n"
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

    [(List   ,[t])  (list "(fn (l1, l2) => ListPair.all "t" (l1, l2))" )]
    [(Array  ,[t])  (list "(arrayEqual "t")")]
    [(Sigseg ,[t])  (list "(SigSeg.eq "t")")]

#|

    [#(,[t*] ...)
     (let ([flds (map Var (map unique-name (make-list (length t*) 'fld)))])
       (list 
	"(fn "(apply make-tuple flds)" =>\n"
	(indent 
	 (list "\"(\" ^"
	       (insert-between " \", \" ^"
			       (map (lambda (printer fld)
				      `("((",printer") ",fld") ^ \n")) 
				 t* flds)
			       )
	       " \")\"")
	 "  ")")"))]
  |#  
    
    [,else ;(make-fun (list (make-tuple "x" "y")) "(x = y)")
     (make-fun (list (make-tuple "x" "y")) "(x = y)")
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
    (define header1 (file->string (++ (REGIMENTD) "/src/generic/passes/mlton_bkend/scheduler.sml")))
    (define header2 (file->string (++ (REGIMENTD) "/src/generic/passes/mlton_bkend/prims.sml")));
    (define header3a (file->string (++ (REGIMENTD) "/src/generic/passes/mlton_bkend/sigseg.sig")))
    (define header3b (file->string (++ (REGIMENTD) "/src/generic/passes/mlton_bkend/sigseg.sml")))
    (define header4 (file->string (++ (REGIMENTD) "/src/generic/passes/mlton_bkend/data_reader.sml")))
    (define header5 (file->string (++ (REGIMENTD) "/src/generic/passes/mlton_bkend/foreign.sml")))

    (define complex1 (file->string (++ (REGIMENTD) "/src/generic/passes/mlton_bkend/Complex.sig")))
    (define complex2 (file->string (++ (REGIMENTD) "/src/generic/passes/mlton_bkend/Complex.sml")))

    (match prog
      [(,lang '(graph (const ,[ConstBind -> cb*] ...)
		      (sources ,[Source -> src* state1** init1*] ...)
		      (iterates ,[Iterate -> iter* state2**] ...)
		      (unionNs ,[Union -> union*] ...)
		      (sink ,base ,basetype)))
       
       ;; Just append this text together.
       (let ([result (list complex1 complex2 
		           header1 header2 header3a header3b header4 header5 "\n" 

			   ;; Block of constants first:
			   "\n(* First a block of constants *)\n\n"
			   (map (lambda (cb) (list "val " cb " ; \n")) cb*)

			   ;; Handle iterator state in the same way:
			   "\n(* Next the state for all iterates/sources. *)\n\n"
			   (map (lambda (iterstatebind)
				  (list "val " iterstatebind " ; \n"))
			     (apply append (append state1** state2**)))
			   
			   "\n(* Third, function bindings for iterates, sources, unions. *)\n\n"
			   " val "(build-BASE basetype)
			   
			   " fun ignored () = () \n" ;; starts off the block of mutually recursive defs
			   (map (lambda (x) (list "\n and " x "\n"))
			     ;; We reverse it because we wire FORWARD
			     (reverse (append  src*  iter* union*)))
			  
			   " \n\n"
			   "val _ = (\n"
			   "\n(*  Initialize the scheduler. *)\n"
			   (map (lambda (x) (list x ";\n")) init1*)

			   "\n\n(*  Then run it *)\n"			   
			   "runScheduler()\n"
			   "handle WSError str => \n"
			   "  (print (\"wserror: \" ^ str ^ \"\\n\");\n"
                           "   OS.Process.exit OS.Process.failure) \n"
			   " | WSEndOfFile => "
			   "  (TextIO.output (TextIO.stdErr, \"Reached end of file. \\n\");\n"
                           "   OS.Process.exit OS.Process.success) \n"			   

;			   "try runScheduler()\n"
;			   "with End_of_file -> Printf.printf \"Reached end of file.\n\";;\n"
;			   "\nPrintf.printf \"Query completed.\\n\";;\n"

			   ")\n"

			   )])
	 result)]
      [,other ;; Otherwise it's an invalid program.
       (error 'emit-mlton-wsquery "ERROR: bad top-level WS program: ~s" other)])))


; ======================================================================
;;; Helper functions for handling different program contexts:


;; The incoming values already have indices on them, this just needs
;; to pass them through.
(define (Union union)
  (match union
    [((name ,name) (output-type ,ty) (incoming ,up* ...) (outgoing ,down* ...))
     (let ([emitter (Emit down*)])
       ;(list " "(Var name)" = " (make-fun '("x") ((Emit down*) "x")) " \n")
       (make-fun-binding name '("x")((Emit down*) "x") )
       ;(list " "(Var name)" = " (make-fun '("x") ) " \n")
       )]))

        
(define (Var var)
  (ASSERT symbol? var)
  ;; This is the place to do any name mangling. 
  ;; We prefix with an underscore to avoid names beginning in capital letters.
  (if (regiment-primitive? var)
      (symbol->string var)
      (string-append "var_" (symbol->string var))))

;; Really the RHS could be any expression, it just does not have a stream type.
(define (ConstBind b)
  (match b

    [(,v ,t ,rhs)
     (list (Var v) " = " 
	   (Expr rhs 
		 (lambda (_) 
		   (error 'ConstBind 
			  "shouldn't have any 'emit's within a constant (non-stream) expression"))))]

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

(define Const
  (lambda (datum)
    (cond
     [(eq? datum 'BOTTOM) (wrap "wserror \"BOTTOM\"")] ;; Should probably generate an error.
     [(eq? datum 'UNIT) "()"]
     [(null? datum) "[]"]
     [(eq? datum #t) "true"]
     [(eq? datum #f) "false"]
     [(string? datum) (format "~s" datum)]
     [(flonum? datum)  (format "(~s)" datum)]
     [(cflonum? datum) (format "{real=~a, imag=~a }" 
			       (cfl-real-part datum)
			       (cfl-imag-part datum))]
     [(integer? datum)  (format "(~s)" datum)]

     [(eq? datum 'nulltimebase) "99999999"]

     ;[(eq? datum 'nulltimebase)  (wrap "WSNULLTIMEBASE")]     
     [(list? datum)
      (list "[" (insert-between "; " (map Const datum)) "]")]
     [(vector? datum)
      ;; UNFINISHED: Make this agnostic to vector representation: should do in another pass...
      #;
      (Expr 
       `(let ([,v 'notyp (Array:make )]))
       (lambda _ (error 'dummyemitter "should not be called")))

      ;; Array:null should have been handled elsewhere:
      (ASSERT (not (zero? (vector-length datum))))

      ;; TODO: array constants.
      (ASSERT #f)

      ]

     [else (error 'emit-mlton:Const "not an Mlton-compatible literal (currently): ~s" datum)])))


#|
      ;; Special Constants:
      [(assert-type ,t '())     (wrap (PolyConst '() t))]
      [nulltimebase             (Const name type 'nulltimebase)]      
|#




;; ================================================================================

;; Converts hertz to microseconds:
(define (rate->timestep freq)
  (when (zero? freq) (error 'rate->timestep "sampling rate of zero is not permitted"))
  (flonum->fixnum (* 1000000 (/ 1.0 freq))))

;; Returns: a binding snippet (incomplete), and initialization code.
(define (Source src)
    (define (E x) (Expr x 'noemits!))
  (match src
    [((name ,[Var -> v]) (output-type ,ty) (code ,app) (outgoing ,downstrm ...))
     (match app
       [(timer ',rate)
	(let ([r ;(Expr rate 'noemitsallowed!)
	       (number->string (rate->timestep rate))]
	      [t (Var (unique-name 'virttime))])
	  (values 	
	   ;; First, a function binding that drives the source.
	   (make-fun-binding v () 
	     (indent  
	      (make-seq
	       `(,t " := !",t" + ",r)
	       ((Emit downstrm) "()")
	       `("SE (!",t",",v")\n"))
	      "    "))	  

	   ;; Second, top level state bindings.
	   (list (make-bind `(,t (Ref Int) "ref 0")))
	   
	   ;; Third, initialization statement:
	   `("(* Seed the schedule with timer datasource: *)\n"
	     "schedule := SE(0,",v") :: !schedule\n"))
	  )]

;(__readFile file mode repeat rate skipbytes offset winsize types)

       [(__readFile ,[E -> file] ',mode ',repeats ',rate ',skipbytes ',offset ',winsize ',types)
	(cond
	 [(equal? mode "text") 
;	  (if (not (zero? repeats))
;	      (error 'emit-mlton "MLton text mode reader doesn't support replaying the datafile yet."))
	  (error 'emit-mlton "MLton text mode readFile not implemented yet.")
	  ]
	 
	 [(equal? mode "binary")
	  (let ([homogenous? (homogenous-sizes? types)]
		[tuptyp (if (= 1 (length types))
			    (car types)
			    (list->vector types ))])
	    (values 
	     ;; Builds a function from unit to an initial scheduler entry "SE" 
	     (make-fun-binding v '()
   	             (make-let `(["binreader"
				  ,(format "BinIO.vector -> int -> ~a" (Type tuptyp))
				  ,(indent (build-binary-reader types homogenous?) "    ")]
			         ["textreader" "33333"])
		   (list "    "
		   (if (> winsize 0) "dataFileWindowed" "dataFile")
		   (make-tuple file 
			       (list "\"" mode "\"")
			       (number->string repeats)
			       (number->string (rate->timestep rate)))
		   " \n"
		   (make-tuple "textreader" "binreader"
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
				 (make-tuple 
				  (DispatchOnArrayType 'Array:makeUNSAFE tuptyp)
				  (DispatchOnArrayType 'Array:set        tuptyp)
				  (DispatchOnArrayType 'toSigseg         tuptyp)))
			       ))
		       "")
		   "\n"
		   )))
	      ;; Second, top level bindings
	      ()
	      ;; Third, initialization statement:
	      `("schedule := ",v"() :: !schedule\n")))]
	 [else (error 'readFile "mode not handled yet in MLton backend: ~s" mode)]
	  )]
       
       ;[,other (values "UNKNOWNSRC\n" "UNKNOWNSRC\n")]
       
       )]))



(define (type->reader t) 
  (match t
    [Int    "read_int32"]
    [Int16  "read_int16"]
    [Float  "read_real32"]
    ))

(define (homogenous-sizes? types) (apply = (map type->width types)))

(define (build-binary-reader types homogenous?)
  (define widths (map type->width types))
  (make-fun '("vec" "ind")
  (list 
   ;"  let pos = ref ind in \n"
   (apply make-tuple
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


#;
;; These are the names of the Bigarray types.
(define (BigarrayType? t)
  (match t

;    [,_ #f] ;; DISABLE!

    [Int     "int"]
    [Int16   "int16_signed"]
    [Float   "float64"]
    [Double  "float64"]
    [Complex "complex64"]
    [,oth #f]
    ))

  
#;
(define (ConvertArrType t) 
  (or (BigarrayType? t)
      (error 'emit-mlton:ConvertArrType "can't make a Bigarray of this type: ~s" t)))

;; It is error prone to keep writing this:
(define (sigseg-prim? p)
  (memq p '(joinsegs subseg width toSigseg toArray timebase start end seg-get)))

;; Converts an operator based on the array element type.
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
				  (make-tuple "n" 
				    (match (make-mlton-zero-for-type elt)
				      [(quote ,c) (Const c)]
				      [,str       str]))))))]

    [(Array:length) "Array.length"]
    [(Array:set)    "Array.update"]
    [(Array:ref)    "Array.sub"]

    [(nullseg) "nullseg()"]
    ;; We just use the normal name conversion:
    [else (if (sigseg-prim? op)
	      (ASSERT (PrimName op))
	      (error 'DispatchOnArrayType "don't know how to dispatch this operator: ~s" op))]
    ))

(define make-mlton-zero-for-type 
  (lambda (t)
    (match t
      [Int   ''0]
      [Int16 "(Int16.fromInt 0)"]
      [Float ''0.0]
      [Double ''0.0]
      [Complex ''0.0+0.0i]
      [#(,[t*] ...) `(tuple ,t* ...)]
      [,oth (error 'make-mlton-zero-for-type "unhandled type: ~s" oth)])))




(define (Prim expr emitter)
  (define (myExpr x) (Expr x emitter))
  (match expr

    ;; Handle equality tests.  This is more laborious than in Caml.
    [(,eq (assert-type ,ty ,[myExpr -> x]) ,[myExpr -> y]) 
     (guard (memq eq '(wsequal? =)))
     (make-app (build-equality-test ty) (list (make-tuple x y)))]

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
    [(,prim (assert-type (Array ,elt) ,[myExpr -> first]) ,[myExpr -> rest] ...)
     (guard (memq prim '(Array:ref Array:set Array:length)))
     (make-prim-app (DispatchOnArrayType prim elt)
	       (cons first rest))]

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

    [(,op (assert-type ,ty ,[myExpr -> x]) ,[myExpr -> y])
     (guard (memq op '(< <= >= > max min)))
     (make-app (case ty
		 [(Int)    (format "~a.~s" int-module op)]
		 [(Int16)  (format "Int16.~s" op)]
		 [(Float)  (format "Real32.~s" op)]
		 [(Double) (format "Real64.~s" op)]
		 [else (error 'emit-mlton "unhandled type for comparison operator ~s: ~s" op ty)]
		 )
	       (list (make-tuple x y)))]

    ;; Safety net:
    [(,prim ,_ ...)     
     (guard (memq prim '(Array:make Array:makeUNSAFE Array:ref Array:set Array:length
  	                 < <= >= > max min
			 )))
     (error 'emit-mlton:Prim "missed this array prim: ~s" (cons prim _))]
 
    [(assert-type ,t ,[primapp]) primapp]
    [(,prim ,[myExpr -> rands] ...) (guard (regiment-primitive? prim))
     (make-prim-app 
      (cond
       [(PrimName prim) => (lambda (x) x)]
       [else (error 'emit-mlton:Prim "currently unhandled: ~s" prim)])
      rands)]
))



;;================================================================================
;; Import the rest of our functionality from the shared module.


;; This packages up the MLton specific functionality to pass back up to the parent module.
;; This is not complete, just what I happen to be using.
(define MLtonSpecific 
  (lambda args
    (apply
     (make-dispatcher (car args)
		      
        make-let 
        make-letrec
	make-tuple 
	make-fun
	make-for
	make-while
	make-fun-binding
	
	Var Prim Const 
	DispatchOnArrayType
	Type

	)
     (cdr args))))

;; Here we import bindings from the "superclass"
;; We pass upwards our "method table"
(define-values (Expr Iterate Emit make-bind) 
  (sharedEmitCases MLtonSpecific))




;;================================================================================

;; This just converts the name of the primitive, for those primitives
;; that map directly onto Mlton functions:
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
      
      [+I16 "( Int16.+)"]
      [-I16 "( Int16.-)"] 
      [*I16 "( Int16.* )"] 
      [/I16 "( Int16.quot )"]
      [^I16 "powInt16"]

      [+_ ,(format "(~s.+)" int-module)]  
      [-_ ,(format "(~s.-)" int-module)] 
      [*_ ,(format "(~s.*)" int-module)]
      [/_ ,(format "(~s.quot)" int-module)]
      [^_ powInt] ;; Defined in prims.sml

      [+. "( Real32.+ )"]
      [-. "( Real32.- )"] 
      [*. "( Real32.* )"] 
      [/. "( Real32./ )"]

      ;; UHH UNFORTUNATELY REAL32 != REAL
      [cos  Real32.Math.cos]
      [sin  Real32.Math.sin]
      [tan  Real32.Math.tan] 
      [acos Real32.Math.acos]
      [asin Real32.Math.asin]
      [atan Real32.Math.atan]

      [absI16 Int16.abs]
      [absI   (format "~s.abs" int-module)]
      [absF   Real32.abs]
      [absC   Complex.magnitude]

      [string-append "(String.^)"] 
      [List:append List.@]

;; TODO ==========================

      [^. "( Real32.Math.pow )"]

      [+: "Complex.+"]
      [-: "Complex.-"] 
      [*: "Complex.*"] 
      [/: "Complex./"]
      [^: "Complex.pow"]


;; SHARED ==========================

      [Mutable:ref   "ref"]
      [deref         "!"]

      [sqrtI "(fn x => (Int32.fromLarge (Real32.toLargeInt IEEEReal.TO_ZERO (Real32.Math.sqrt (Real32.fromLargeInt (Int32.toLarge x))))))"]
      [sqrtF Real32.Math.sqrt]
      [sqrtC Complex.sqrt]

      [realpart "(fn {real, imag} => real)"]
      [imagpart "(fn {real, imag} => imag)"]

      [cons ,(make-fun (list (make-tuple "x" "y")) "x::y")]
      [car List.hd]
      [cdr List.tl]
      [List:length  List.length]
      [List:reverse List.rev]
      [List:ref     List.nth]

      [int16ToInt     ,(compose (format "~a.fromLarge" int-module) "Int16.toLarge")]
      [int16ToFloat   ,(compose "Real32.fromInt" "Int16.toInt")]
      [int16ToDouble  ,(compose "Real64.fromInt" "Int16.toInt")]
      [int16ToComplex  ,(make-fun '("n") "({real= Real32.fromInt (Int16.toInt n), imag= Real32.fromInt 0})")]

      [intToInt16     ,(compose "Int16.fromLarge"     (format "~a.toLarge" int-module))]
      [intToFloat     ,(compose "Real32.fromLargeInt" (format "~a.toLarge" int-module))]
      [intToDouble    ,(compose "Real64.fromLargeInt" (format "~a.toLarge" int-module))]
      [intToComplex  ,(make-fun '("n") "({real= Real32.fromInt n, imag= Real32.fromInt 0})")]

      [floatToInt     ,(make-fun '("x") "Int32.fromLarge (Real32.toLargeInt IEEEReal.TO_ZERO x)")]
      [floatToInt16   ,(make-fun '("x") "Int16.fromInt   (Real32.toInt IEEEReal.TO_ZERO x)")]
      [floatToDouble  ,(make-fun '("x") "Real64.fromLarge IEEEReal.TO_NEAREST (Real32.toLarge x)")]
      [floatToComplex ,(make-fun '("n") "({real= n, imag= Real32.fromInt 0})")]

      [doubleToInt    ,(compose (format "~a.fromLarge" int-module) "(Real64.toLargeInt IEEEReal.TO_ZERO)")]
      [doubleToInt16  ,(compose "Int16.fromInt" "Real64.toInt")]
      [doubleToFloat  ,doubleToFloat]
      [doubleToComplex ,(make-fun '("n") (list "({real= "doubleToFloat" n, imag= Real32.fromInt 0})"))]

      [complexToInt16 "(fn {real,imag} => Int16.fromInt (Real32.toInt IEEEReal.TO_ZERO real))"]
      [complexToInt   "(fn {real,imag} => (Real32.toInt IEEEReal.TO_ZERO real))"]
      [complexToFloat "(fn {real,imag} => real)"]
      [complexToDouble "(fn {real,imag} => (Real64.fromLarge IEEEReal.TO_ZERO (Real32.toLarge real)))"]

      ;; Should return option type:
      [stringToInt    ("(fn s => case ",(format "~s.fromString" int-module)" s of SOME x => x)")]
      [stringToFloat  "(fn s => case Real32.fromString s of SOME x => x)"]
      [stringToDouble "(fn s => case Real64.fromString s of SOME x => x)"]
;      [stringToComplex "(fun s -> Scanf.sscanf \"%f+%fi\" (fun r i -> {Complex.re=r; Complex.im=i}))"]

      [roundF  ,(make-fun '("x") "Real32.fromInt (Real32.floor (x + 0.5))")]

      [start   ss_start]
      [end     ss_end]
      [seg-get ss_get]
      ))

    (lambda (sym) 
      (cond 
       [(memq sym sametable) (Var sym)]
       [(assq sym aliastable) => (lambda (x) (format "~a" (cadr x)))]
       [else #f])
      )))

;;================================================================================

  (define-testing these-tests
    `(
      [3 3]

      ;; This makes sure we can generate *something* for all the primitives.
      ,@(map
	 (match-lambda ([,prim ,argtypes ,rettype])
	   `[(,Prim '(,prim ,@(map (lambda (_) (unique-name 'x)) argtypes))
		    (lambda (_) #f)) unspecified]
	   )
	 ;; Quadratic:
	 (let ([exceptions 
		(append 
		 '(;; These are obsolete:
		   eq? locdiff nodeid sense even? odd? 
		   ;; These weren't really primitives:    
		   tuple tupref ref deref static statref __foreign foreign foreign_box foreign_source
		   ;; These were desugared or reduced to other primitives:
		   or and dataFile show-and-string-append 
		   ;; These were resolved into the w/namespace versions:
		   head tail map append fold
		   List:head List:tail 

		   ;; This are handled specially by DispatchOnArrayType.
		   ;; This is due to the dual-representation for arrays.
		   Array:toList Array:make Array:makeUNSAFE Array:get Array:ref Array:length
		   joinsegs subseg width toSigseg toArray timebase start end seg-get
		   
		   ;; These have a special syntax, requiring an assert-type or whatnot:
		   cons car cdr null? prim_window 
		   List:ref List:append List:reverse List:length List:make 
		   < <= >= > max min = 
		   		   
		   wsequal? print show seg-get toArray

		   stringToComplex 

		   ;; TODO, FIXME: These I just haven't gotten to yet:
		   fftC ifftC ifftC2R
		   ENSBoxAudio
		   List:assoc List:assoc_update
		   hashrem hashset ;; pure versions
		   Array:map Array:fold
		   internString uninternString

		   exclusivePtr getPtr
		   
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
      ))

(define-testing test-this (default-unit-tester "wavescript_emit-mlton.ss: generating WaveScript Mlton code." these-tests))
(define test-emit-mlton test-this)



) ;; End Module


;;================================================================================

