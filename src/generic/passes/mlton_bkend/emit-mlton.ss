
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

;======================================================================
;======================================================================

;;; These are the functions that differ from their caml countparts.

(define (coerce x) (if (symbol? x) (symbol->string x) x))

;; Curried version:
(define (make-fun formals body)
  (if (null? formals)
      (list "(fn () => "body")")
      (let loop ([formals (map coerce formals)])
	(if (null? formals)
	    body
	    (list "(fn "(car formals)" => "(loop (cdr formals)) ")")))))
(define (make-app rator rands) (list "(" rator " "(insert-between " " rands) ")"))

;; Tuple version:
;; ...

(define (make-for i st en bod)
  (list
   "(let val "i" = ref "st" in \n"
   " while !"i" <= "en" do\n"
   "   (let val "i" = !"i" in"bod"\n end; "i" := !"i" + 1) end)"))

(define (make-let binds body . extra)
  (list "(let val " extra
	(insert-between "\n val "
	   (map (match-lambda ([,lhs ,rhs])
		  (list (coerce lhs) " = " rhs))
	     binds))
	" in \n"
	(indent body "  ")
	"\n end)"))

(define (make-letrec binds body) (make-let binds body "rec "))

(define (with-fun-binding name formals funbody body)
  (list "(let fun " name
	(if (null? formals) "()"
	    (insert-between " " (map coerce formals)))
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
    [Int     "int"]
    [Int16   "Int16.int"] ;; Not standard SML.

;    [Complex  "Complex.t"]
    [String   "string"]
    [(Ref ,[t]) `("(",t ") ref")]
    [(VQueue ,_) "unit"]
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
      `(" baseSink = ",(make-fun '(x) flush) "\n")
      `(" baseSink = ",(make-fun '(x) `("(print_endline (",(build-show type)" x); ",flush")"))
	"\n")
  ))

(define (build-show t)
  (match t

    [Int16 "Int16.toString"] ;; These are just represented as ints.

    ;; ERROR: FIXME:
    [(quote ,_) (make-fun '(_) "(\"POLYMORPHIC_OBJECT\")")]

    [String     (make-fun '(x) "x")]
    [Int    "Int.toString"]
    [Float  "Real32.toString"]
    [Double "Real64.toString"]
    [Bool   "Bool.toString"]
;    [Complex (make-fun '(c) "(Real32.toString c.Complex.re ^ \"+\" ^ string_of_float c.Complex.im ^ \"i\")")]

    [(List ,[t]) (make-fun '(ls) (list "(\"[\" ^ concat_wsep \", \" (List.map "t" ls) ^ \"]\")"))]
    [(Array ,[t]) (make-fun '(a) (list "(\"[\" ^ concat_wsep \", \" (List.map "t" (arrayToList a)) ^ \"]\")"))]

    ;; Just print range:
    [(Sigseg ,t) 
     (make-fun '(ss) 
	       (list 
		"(\"[\"^ Int.toString ("  (DispatchOnArrayType 'start t)
		" ss) ^\", \"^ Int.toString ("      (DispatchOnArrayType 'end t)
		" ss + 1) ^ \")\")"))]

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

    [(List ,[t]) (list "(fn (l1, l2) => ListPair.all "t" (l1, l2))" )]
    [(Array ,[t]) (list "(arrayEqual "t")")]
    
#|
    ;; Just print range:
    [(Sigseg ,t) 
     (make-fun '(ss) 
	       (list 
		"(\"[\"^ Int.toString ("  (DispatchOnArrayType 'start t)
		" ss) ^\", \"^ Int.toString ("      (DispatchOnArrayType 'end t)
		" ss + 1) ^ \")\")"))]

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
    (define header3 (file->string (++ (REGIMENTD) "/src/generic/passes/mlton_bkend/sigseg.sml")))
    (define header4 (file->string (++ (REGIMENTD) "/src/generic/passes/mlton_bkend/data_reader.sml")))

    (match prog
      [(,lang '(graph (const ,[ConstBind -> cb*] ...)
		      (sources ,[Source -> src* init1*] ...)
		      (iterates ,[Iterate -> iter*] ...)
		      (unionNs ,[Union -> union*] ...)
		      (sink ,base ,basetype)))
       
       ;; Just append this text together.
       (let ([result (list header1 header2 header3 header4 "\n" 

			   ;; Block of constants first:
			   (map (lambda (cb) (list "val " cb " ; \n")) cb*)

			   " val "(build-BASE basetype)

			   ;; These return incomplete bindings that are stitched with "and":
			   (map (lambda (x) (list "\nval " x ";\n")) 
			     ;; We reverse it because we wire FORWARD
			     (reverse (append  src*  iter* union*)))

			   "; \n\n"
			   init1* "\n"
			   "runScheduler();\n"
;			   "try runScheduler()\n"
;			   "with End_of_file -> Printf.printf \"Reached end of file.\n\";;\n"
;			   "\nPrintf.printf \"Query completed.\\n\";;\n"
			   )])
	 result)]
      [,other ;; Otherwise it's an invalid program.
       (error 'emit-mlton-wsquery "ERROR: bad top-level WS program: ~s" other)])))


; ======================================================================
;;; Helper functions for handling different program contexts:

(define (Iterate iter)
  (match iter
    [(,name ,ty (let ([,lhs* ,ty* ,rhs*] ...)
	       (lambda (,x ,vq) (,ty1 ,ty2) ,bod))
	 ,up (,down* ...))
     ;(if (null? down*) (inspect (vector "huh? why null?" name up down* bod)))
     (let* ([emitter (Emit down*)])
       `(" (* WS type: input:",(format "~a" ty1)" vq:",(format "~a" ty2)" -> ",(format "~a" ty)" *)\n"
	 " ",(Var name)" = \n"
	 ;; First we bind the iterator state.
	 ,(make-let `([,(Var vq) "()"]
		      ,@(map (lambda (lhs rhs) 
			       (list (Var lhs) (Expr rhs emitter)))
			  lhs* rhs*))
		    ;; Then we bind the actual function:

		    ;; TODO: We should really just pull the iterator state to the top of the program.
		    ;; Then we don't need to have this internal letrec here:
		    
		    (list (with-fun-binding 
			   (Var name)
			   (list "("(Var x)" : "(Type ty1)")") (indent (Expr bod emitter) "    ")
			   (Var name))
			  "\n"
			  ))))]))

;; The incoming values already have indices on them, this just needs
;; to pass them through.
(define (Union union)
  (match union
    [(,name ,ty (,up* ...) (,down* ...))       
     (let ([emitter (Emit down*)])
       (list " "(Var name)" x = " ((Emit down*) "x") " \n"))]))

;; Generates code for an emit.  (curried)
;; .param down*   A list of sinks (names of functions) to emit to.
(define (Emit down*)
  ;(ASSERT (not (null? down*)))
  (lambda (expr)
    ;; Just call each of the sites with the argument.
    (make-let `([emitted ,expr])
      (apply make-seq
       (append 
	(map (lambda (down) 
	      (cond 
	       [(eq? down 'BASE) `("baseSink emitted")]
	       ;; If we're emitting *to* a unionN, we include the index tag.
	       [(pair? down)
		(ASSERT (fixnum? (car down)))
		(ASSERT (= (length down) 2))
		(list (Var (cadr down))
		      "("(Const (car down))", emitted )")]
	       [else `(,(Var down)" emitted")]))
	  down*)
	'("()")))
      )))

        
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
     [(cflonum? datum) (format "{Complex.re=~a; Complex.im=~a }" 
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
    [(,[Var -> v] ,ty ,app (,downstrm ...))
     (match app
       [(timer ',rate)
	(let ([r ;(Expr rate 'noemitsallowed!)
	       (number->string (rate->timestep rate))
	       ])
	  (values 	
	   `(" ",v" = "
	     ,(make-let '((t "ref 0"))
	        (with-fun-binding v ()
		   (indent  
		    (make-seq
		     `("t := !t+",r)
		     ((Emit downstrm) "()")
		     `("SE (!t,",v")\n"))
		    "    ")
		   v)))
	   `("(* Seed the schedule with timer datasource: *)\n"
	     "schedule := SE(0,",v") :: !schedule;\n"))
	  )]

       [(audioFile ,fn ,win ,rate)
	000000000000]

;(__readFile file mode repeat rate skipbytes offset winsize types)

       [(__readFile ,[E -> file] ',mode ',repeats ',rate ',skipbytes ',offset ',winsize ',types)
	(cond
	 [(equal? mode "text") 
;	  (if (not (zero? repeats))
;	      (error 'emit-mlton "MLton text mode reader doesn't support replaying the datafile yet."))
	  (error 'emit-mlton "MLton text mode readFile not implemented yet.")
	  ]
	 
	 [(equal? mode "binary")
	  (values (list 
		   ;; Builds a function from unit to an initial scheduler entry "SE" 
		   " "v" = "
		   (make-fun '()
		   (make-let `([binreader ,(indent (build-binary-reader types) "    ")]
			       [textreader "33333"])
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
		   (indent (make-fun '(x) ((Emit downstrm) "x")) "      ")
		   ;; Also an additional arguments for
		   ;; dataFileWindowed.  One that has the window size.
		   ;; And one that bundles up array create/set and
		   ;; toSigseg.
		   (if (> winsize 0)
		       (begin 
			 ;; This is necessary for now:
			 (ASSERT (= (length types) 1))
			 (list " "(number->string winsize)" "			       
			       (let ([tuptyp (if (= 1 (length types))
						 (car types)
						 (list->vector types ))])
				 (make-tuple 
				  (DispatchOnArrayType 'Array:makeUNSAFE tuptyp)
				  (DispatchOnArrayType 'Array:set        tuptyp)
				  (DispatchOnArrayType 'toSigseg         tuptyp)))
			       ))
		       "")
		   "\n"
			      )
		    
		    )))
		  `("schedule := ",v"() :: !schedule;;\n"))]
	 [else (error 'readFile "mode not handled yet in Caml backend: ~s" mode)]
	  )]
       
       ;[,other (values "UNKNOWNSRC\n" "UNKNOWNSRC\n")]
       
       )]))




(define (type->reader t) 
  (match t
    [Int    "read_int32"]
    [Int16  "read_int16"]
    ))


(define (build-binary-reader types)
  (define widths (map type->width types))
  (make-fun '(vec ind)
  (list 
   ;"  let pos = ref ind in \n"
   (apply make-tuple
     (mapi (lambda (i t)
	     (list (type->reader t)" vec (ind + "
		   (number->string (apply + (list-head widths i)))")")
	     )
	   types))
   "\n")))

(define (build-text-reader types)
  (match types
    [(Int)
     "8888888"
     ]))


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

(define (ConvertArrType t) 
  (or (BigarrayType? t)
      (error 'emit-mlton:ConvertArrType "can't make a Bigarray of this type: ~s" t)))

;; It is error prone to keep writing this:
(define (sigseg-prim? p)
  (memq p '(joinsegs subseg width toSigseg toArray timebase start end seg-get)))

;; Converts an operator based on the array element type.
(define (DispatchOnArrayType op elt)
  (case op
    [(Array:null) "[||]"]
    [(Array:make) "Array.array"]
    ;; Just makes a normal array with zeros:
    ;; Cut/paste from above:

    ;; Can't do anything smart with this right now:
    ;; Could use MONO Arrays... (Like we do in Caml)
    [(Array:makeUNSAFE)
     (lnboth (make-fun '(n) 
		       (make-app (DispatchOnArrayType 'Array:make elt)
				 (list 
				  (make-tuple "n" 
				    (match (make-mlton-zero-for-type elt)
				      [(quote ,c) (Const c)]
				      [,str       str]))))))]

    [(Array:length) "Array.length"]
    [(Array:set)    "Array.update"]
    [(Array:ref)    "Array.sub"]

    [(nullseg) "nullseg_flat"]
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




;;================================================================================

;; This just converts the name of the primitive, for those primitives
;; that map directly onto Mlton functions:
(define (PrimName sym)
  (define (compose . ls)
    (make-fun '(x)
       (let loop ([ls ls])
	 (if (null? ls)
	     "x"
	     (make-app (car ls) (list (loop (cdr ls))))))))
  (define sametable ;; Prims with the same name:
    '(
      joinsegs subseg width toSigseg toArray timebase
      cos sin tan acos asin atan max min
      not 
      
      fftR2C 
      m_invert 
      ;;wserror ;generic_hash 
      ))
  (define aliastable
    `(
      
      [+I16 "( Int16.+)"]
      [-I16 "( Int16.-)"] 
      [*I16 "( Int16.* )"] 
      [/I16 "( Int16.div )"]
      [^I16 "powInt16"]

      [+_ "(Int.+)"]  
      [-_ "(Int.-)"] 
      [*_ "( Int.* )"]
      [/_ "(Int.div)"]
      [^_ powInt] ;; Defined in prims.sml

      [+. "( Real32.+ )"]
      [-. "( Real32.- )"] 
      [*. "( Real32.* )"] 
      [/. "( Real32.div )"]

      [absI16 Int16.abs]
      [absI   Int.abs]
      [absF   Real.abs]
;      [absC   Complex.norm]

      [string-append "(String.^)"] 
      [List:append List.@]

;; TODO ==========================

;      [^. "( ** )"] 

;      [+: "Complex.add"]
;      [-: "Complex.sub"] 
;      [*: "Complex.mul"] 
;      [/: "Complex.div"]
;      [^: "Complex.pow"]

;;  NOT POLYMORPHIC IN SML!
;      [<      "(<)"]
;      [<=     "(<=)"]
;      [>      "(>)"]
;      [>=     "(>=)"]
;      [=        "(=)"] ;; NOTE! FIXME! should be =???



;; SHARED ==========================

;      [equal?        "(=)"] ;; NOTE! FIXME! should be =???
      [Mutable:ref   "ref"]
      [deref         "!"]

;      [sqrtI "(fun x -> (int_of_float (sqrt (float_of_int x))))"]
;      [sqrtF sqrt]
;      [sqrtC Complex.sqrt]

;      [realpart "(fun x -> x.Complex.re)"]
;      [imagpart "(fun x -> x.Complex.im)"]

      [cons (make-fun '(x y) "x::y")] 
      [car List.hd]
      [cdr List.tl]
      [List:length  List.length]
      [List:reverse List.rev]
      [List:ref     List.nth]

      [int16ToInt     Int16.fromInt]
      [int16ToFloat   ,(compose "Real32.fromInt" "Int16.toInt")]
      [int16ToDouble  ,(compose "Real64.fromInt" "Int16.toInt")]
      ;[int16ToComplex  "(fun n -> {Complex.re= float_of_int n; Complex.im= 0.})"]

      [intToInt16     Int16.toInt]
      [intToFloat     Real32.fromInt]
      [intToDouble    Real64.fromInt]
;      [intToComplex  "(fun n -> {Complex.re= float_of_int n; Complex.im= 0.})"]

      [floatToInt    Real32.toInt]
      [floatToInt16  ,(compose "Int16.fromInt" "Real32.toInt")]
      [floatToDouble ,(make-fun '(x) "Real64.fromlarge IEEEReal.TO_NEAREST (Real32.toLarge x)")]
;      [floatToComplex "(fun f -> {Complex.re= f; Complex.im= 0.})"]

      [doubleToInt    Real64.toint]
      [doubleToInt16  ,(compose "Int16.fromInt" "Real64.toInt")]
      [doubleToFloat  ,(make-fun '(x) "Real32.fromlarge IEEEReal.TO_NEAREST (Real64.toLarge x)")]
;      [doubleToComplex "(fun f -> {Complex.re= f; Complex.im= 0.})"]

;      [complexToInt16 "(fun c -> int_of_float c.Complex.re)"]
;      [complexToInt   "(fun c -> int_of_float c.Complex.re)"]
;      [complexToFloat "(fun c -> c.Complex.re)"]
;      [complexToDouble "(fun c -> c.Complex.re)"]

      [stringToInt    Int.fromString]
      [stringToFloat  Real.fromString]
      [stringToDouble Real64.fromString]
;      [stringToComplex "(fun s -> Scanf.sscanf \"%f+%fi\" (fun r i -> {Complex.re=r; Complex.im=i}))"]

      [roundF  (make-fun '(x) "(floor (x + 0.5))")]

      [start   ss_start]
      [end     ss_end]
      [seg-get ss_get]
      ))
  (cond 
   [(memq sym sametable) (Var sym)]
   [(assq sym aliastable) => (lambda (x) (format "~a" (cadr x)))]
   [else #f]))

(define (Prim expr emitter)
  (define (myExpr x) (Expr x emitter))
  (match expr

    ;; Handle equality tests.  This is more laborious than in Caml.
    [(,eq (assert-type ,ty ,[myExpr -> x]) ,[myExpr -> y]) 
     (guard (memq eq '(equal? =)))
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
    ;; Safety net:
    [(,prim ,_ ...)     
     (guard (memq prim '(Array:make Array:makeUNSAFE Array:ref Array:set Array:length)))     
     (error 'emit-mlton:Prim "missed this array prim: ~s" prim)]


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


(define-syntax make-dispatcher
  (syntax-rules (else)
    [(_ exp syms ...) 
     (let ([x exp])
       (case x [(syms) syms] ... 
	   [else (error 'make-dispatcher "unmatched: ~s" x)]))]))

;; This packages up the caml specific functionality to pass back up to the parent module.
;; This is not complete, just what I happen to be using.
(define MLtonSpecific 
  (lambda args
    (apply
     (make-dispatcher (car args)
		      
        make-let 
	make-tuple 
	make-fun
	make-for
	
	Var Prim Const 
	DispatchOnArrayType

	)
     (cdr args))))
      
(define Expr (protoExpr MLtonSpecific))








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
		   		   
		   equal? print show seg-get toArray

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

