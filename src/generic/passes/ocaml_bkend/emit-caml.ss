
;;;; .title WaveScript EmitCaml
;;;; .author Ryan Newton [2007.03.19]

;;;; This uses the generic C-generation libary (c_generator.ss) and
;;;; provides procedures for producing a OCaML-file following WaveScript
;;;; conventions.

;; [2007.03.20] The first query I ran (one iterate over timer, emits
;; constant) does 23 million samples a second.  That's not that
;; encouraging.
;;
;;   Well, then again... on "Passchain" (with 10 ops), my scheme
;; emulator gets ~200K a second.  In the paper we report Coral8 at
;; ~200K/sec, XStream at ~100K/sec.  "wscaml" currently gets 14
;; million/sec, or 140 times more than XStream!?

;; TODO: I should output explicit type annotations for sanity checking.

(module emit-caml mzscheme 
  (require  "../../../plt/common.ss"
	    "../../compiler_components/c_generator.ss" 
            
            "../ocaml_bkend/shared-emit-ml.ss")
  (provide emit-caml-wsquery test-emit-caml)
  (chezprovide )  
  (chezimports shared-emit-ml
	       (except helpers test-this these-tests))


;; MUTABLE
(define union-edges 'union-edges-uninit)

;;CHEZ ONLY
;(define type->width (let () (import wavescript_sim_library_push) type->width))
;(define types->width (let () (import wavescript_sim_library_push) types->width))
  
;======================================================================
;;                    <WaveScript CAML generation>
;======================================================================

;;; First some helpers to produce syntax for certain caml constructs:

(define (coerce-id x) (if (symbol? x) (Var x) x))

;; Curried version:
(define (make-fun formals body)
  (list "(fun " (if (null? formals) "()"
		    (insert-between " " (map coerce-id formals)))
	" -> " body ")"))
(define (make-app rator rands) (list "(" rator " "(insert-between " " rands) ")"))
(define (make-for i st en bod)
  `("(for ",i" = ",st" to ",en" do\n ",bod"\n done)"))
(define (make-while tst bod) `("(while ",tst" do\n ",bod"\ndone)"))

(define (make-let binds body)
  (list "(let "
	(insert-between "\n and "
	   (map (lambda (x)
		  (match x
		    [[,lhs ,rhs]     (list (coerce-id lhs) " = " rhs)]
		    ;; Type is a sexp or a string:
		    [[,lhs ,ty ,rhs] (list (coerce-id lhs) " :  "
					   (if (string? ty) ty
					       (Type ty))" = " rhs)])) 
	     binds))
	" in \n"
	(indent body "  ")
	")"))

;; A "binding" is the "kwd v = rhs" text
(define (make-fun-binding name formals funbody)
  (list (coerce-id name) " = " (make-fun formals funbody)))

; make-conditional
(define (ln . args) (list args "\n"))
(define (lnfst . args) (list "\n" args))
(define (lnboth . args) (list "\n" args "\n"))

;; If the type needs a specialized hashfun, returns its name,
;; otherwise returns #f.
;(define (HashType k v) )
;(define (SharedPtrType t))

;; This should give you an idea of the mapping between types:
(define (Type t)
  (match t
    [Bool    "bool"]
    [Int     "int"]
    [Int16   "int"]
    [Float    "float"]
    [Double   "float"]
    [Complex  "Complex.t"]
    [String   "string"]
    [(Ref ,[t]) `("(",t ") ref")]
    [(VQueue ,_) "unit"]
    [#() "unit "]

    [#(,[t*] ...) `("(",(insert-between " * " t*)")")]

    ;; Went back and forth on whether this should be a pointer:
    [(Sigseg ,t) 
     (let ([flatty (BigarrayType? t)])
       (if flatty
	   `("(",(Type t)", Bigarray.",flatty"_elt) sigseg_flat")
	   `(,(Type t) " sigseg")))]

    ;[(Stream ,[t]) `("WSBox*")]
    ;[(Array ,[t]) `(,t "[]")]
    [Timebase  "int"]

    [(Array ,t) 
     (let ([flatty (BigarrayType? t)])
       (if flatty
	   (list "("
		 (Type t)", "
		 flatty"_elt, "
		 " Bigarray.c_layout)"
		 "Bigarray.Array1.t")
	   `("(",(Type t)") array")))]

    [(List ,[t]) `("(",t") list")]
    ;[(HashTable ,kt ,vt) (SharedPtrType (HashType kt vt))]
    [,other (error 'emit-caml:Type "Not handled yet.. ~s" other)]))


;======================================================================

;; This is the only entry point to the file.  A complete query can
;; be transformed into a complete query file.
;;
;; .param prog  The wsquery to process.
(define emit-caml-wsquery
  (lambda (prog)
    ;; Lame, requires REGIMENTD:
    (define header1 (file->string (** (REGIMENTD) "/src/generic/passes/ocaml_bkend/scheduler.ml")))
    (define header2 (file->string (** (REGIMENTD) "/src/generic/passes/ocaml_bkend/sigseg.ml")))
    ;(define header2 (file->string (** (REGIMENTD) "/src/generic/passes/ocaml_bkend/sigseg_seglist.ml")))
    (define header3 (file->string (** (REGIMENTD) "/src/generic/passes/ocaml_bkend/prims.ml")))
    (define header4 (file->string (** (REGIMENTD) "/src/generic/passes/ocaml_bkend/data_reader.ml")))

    (match prog
      [(,lang '(graph (const ,[ConstBind -> cb*] ...)
		      (init  ,[Effect -> init*] ...)
		      (sources ,[Source -> src* init*] ...)
		      (iterates ,[Iterate -> iter* state**] ...)
		      (unions ,[Union -> union*] ...)
		      (sink ,base ,basetype)))
       
       ;; Just append this text together.
       (let ([result (list header1 header2 header3 header4 "\n" 

			   ;; Block of constants first:
			   "\n(* First a block of constants *)\n\n"
			   (map (lambda (cb) (list "let " cb " ;; \n")) cb*)

			   "\n(* Then initialize the global bindings: *)\n"
			   (indent (apply make-seq init*) "   ")";;\n\n"

			   ;; Handle iterator state in the same way:
			   "\n(* Next the state for all iterates. *)\n\n"
			   (map (lambda (iterstatebind)
				  (list "let " iterstatebind " ;; \n"))
			     (apply append state**))

			   "\n(* Third, function bindings for iterates, sources, unions. *)\n\n"
			   "let rec ignored = () \n" ;; Start off the let block.
			   ;; These return incomplete bindings that are stitched with "and":
			   (map (lambda (x) (list "\nand\n" x)) (append  src*  iter* union*))
			   " and "(build-BASE basetype)
			   ";; \n\n"
			   
			   " \n\n"
			   "\n(*  Initialize the scheduler. *)\n"
			   "begin " (map (lambda (init) (list init ";\n")) init*) "\n"

			   "\n(*  Then run it *)\n"
			   "try runScheduler()\n"
			   "with End_of_file -> Printf.printf \"Reached end of file.\n\";\n"
			   "\nPrintf.printf \"Query completed.\\n\""
			   "end "
			   ";;\n"
			   )])
	 #;
	 (string->file (text->string result) 
		       (** (REGIMENTD) "/src/generic/passes/ocaml_bkend/foo.ml"))
	 result)]
      [,other ;; Otherwise it's an invalid program.
       (error 'emit-caml-wsquery "ERROR: bad top-level WS program: ~s" other)])))


; ======================================================================
;;; Helper functions for handling different program contexts:

#;
(define (Iterate iter)
  (match iter
    [((name ,name) (output-type ,ty) 
      (code (let ([,lhs* ,ty* ,rhs*] ...)
	       (lambda (,x ,vq) (,ty1 ,ty2) ,bod)))
      (incoming ,up) 
      (outgoing ,down* ...))
     ;(if (null? down*) (inspect (vector "huh? why null?" name up down* bod)))
     (let* ([emitter (Emit down*)])
       `(" (* WS type: input:",(format "~a" ty1)" vq:",(format "~a" ty2)" -> ",(format "~a" ty)" *)\n"
	 " ",(Var name)" = \n"
	 "  let ",(Var vq)" = () in\n"
	 ,@(map (lambda (lhs ty rhs)
		  `("  let ",(Var lhs)" = ",(Expr rhs emitter)" in\n"))
	     lhs* ty* rhs*)
	 "  fun (",(Var x)" : ",(Type ty1)") -> \n"
	 ,(indent (Expr bod emitter) "    ")
	 "\n")       
       )]))

;; The incoming values already have indices on them, this just needs
;; to pass them through.
(define (Union union)
  (match union
    [(union (name ,name) (output-type ,ty) (incoming ,up* ...) (outgoing ,down* ...))
     (let ([emitter (Emit down*)])
       (list " "(Var name)" x = " ((Emit down*) "x") " \n"))]
    ;; Merge unimplemented...
    ))

#;
;; Generates code for an emit.  (curried)
;; .param down*   A list of sinks (names of functions) to emit to.
(define (Emit down*)
  ;(ASSERT (not (null? down*)))
  (lambda (expr)
    ;; Just call each of the sites with the argument.
    `("(let emitted = ",expr" in\n"
      ,@(map (lambda (down) 
	       (cond 
		[(eq? down 'BASE) `("baseSink emitted;\n")]
		;; If we're emitting *to* a unionN, we include the index tag.
		[(pair? down)
		 (ASSERT (fixnum? (car down)))
		 (ASSERT (= (length down) 2))
		 (list (Var (cadr down))
		       "("(Const (car down))", emitted );\n")]
		[else `(,(Var down)" emitted;\n")]))
	  down*)
      "  ())")))
        
(define (Var var)
  (ASSERT symbol? var)
  ;; This is the place to do any name mangling. 
  ;; We prefix with an underscore to avoid names beginning in capital letters.
  (if (real-primitive? var)
      (symbol->string var)
      (string-append "_" (symbol->string var))))

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
    (if (eq? datum 'BOTTOM) (inspect 'HUH???))
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
      (let ([constArr 
	     (list "[|" 
		   (insert-between "; " (map Const (vector->list datum)))
		   "|] \n")])
	(if (BigarrayType? (type-const (vector-ref datum 0)))
	    (list
	     "  (Bigarray.Array1.of_array "
	     " Bigarray."
	     (if (zero? (vector-length datum))
		 "int"
		 (ConvertArrType (type-const (vector-ref datum 0)))) ;; Kind
	     " Bigarray.c_layout \n" 
	     constArr ")")
	    ;; Otherwise this is all we need:
	    constArr))]

     [else (error 'emit-caml:Const "not an OCaml-compatible literal (currently): ~s" datum)])))

;; This handles one of the statements that are part of query initialization.
;; (Initializing global state.)
(define (Effect expr)
  (Expr expr 
	(lambda (_) 
	  (error 'Effect
		 "shouldn't have any 'emit's within an initialization expression"))))


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
	       (number->string (rate->timestep rate))
	       ])
	  (values 	
	   `(" ",v" = let t=ref 0 in \n"
	     "  fun () -> \n"
	     "    t:=!t+",r";\n"
	     ,(indent ((Emit downstrm) "()") "    ")";\n"
	     "    SE (!t,",v")\n" )
	   `("(* Seed the schedule with timer datasource: *)\n"
	     "schedule := SE(0,",v") :: !schedule"))
	  )]

       [(audioFile ,fn ,win ,rate)
	000000000000]

;(__readFile file mode repeat rate skipbytes offset winsize types)

       [(__readFile ,[E -> file] ',mode ',repeats ',rate ',skipbytes ',offset ',winsize ',types)
	(cond
	 [(equal? mode "text") 
	  (if (not (zero? repeats))
	      (error 'emit-caml "Caml text mode reader doesn't support replaying the datafile yet."))
	  (values
	   (list 
	    ;; Builds a function from unit to an initial scheduler entry "SE" 
	    " "v" = fun () -> \n"	    
	    " let timestamp = ref 0 in \n"
	    " let hndl = open_in "file" in \n"
	    " let rec f () = \n"
	    "  let line = input_line hndl in \n"
	    "  Scanf.sscanf line \""
	    (insert-between " "
	     ;; CAREFUL: THIS DOESN'T EXPLICITELY LOOK FOR NEWLINES:
             (map (lambda (ty)
		    (match ty
		      [Float  "%f"] 
		      [Double "%f"] 
		      [Int    "%d"]
		      [Int16  "%d"]
		      [String "%s"]					  
		      ))	       
	       types))
	    "\" \n"
	    (let ([names (map (lambda (_) 
				(symbol->string (unique-name "fld"))) 
			   types)])
	      (list 
	       "  (fun " (insert-between " " names) " -> \n   "
	         ;; Form a tuple of the results and send it downstream.
	         ((Emit downstrm) (apply make-tuple-code names))"); \n"))
	    ;; Now discard the rest of the line.
;	    "  let _ = input_line hndl in "
	    "   timestamp := !timestamp + "(number->string (rate->timestep rate))";"
	    "   SE (!timestamp, f) in \n"
	    "  SE (0, f) \n" 
	    "  \n"
	    )
	   ;; Initialization: schedule this datasource:
	   `("schedule := ",v"() :: !schedule"))]
	 


	 [(equal? mode "binary")
	  (values (list 
		   ;; Builds a function from unit to an initial scheduler entry "SE" 
		   " "v" = fun () -> \n"
		   "  let binreader = "(indent (build-binary-reader types) "    ")" \n"
		   "  and textreader = 33333 in \n"
		   "    "
		   (if (> winsize 0) "dataFileWindowed" "dataFile")
		   (make-tuple-code file 
			       (list "\"" mode "\"")
			       (number->string repeats)
			       (number->string (rate->timestep rate)))
		   " \n"
		   (make-tuple-code "textreader" "binreader"
			       (number->string (apply + (map type->width types)))
			       (number->string skipbytes)
			       (number->string offset))
		   " \n"
		   (indent (list "(fun x -> "((Emit downstrm) "x")")") "      ")
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
				 (make-tuple-code 
				  (DispatchOnArrayType 'Array:makeUNSAFE tuptyp)
				  (DispatchOnArrayType 'Array:set        tuptyp)
				  (DispatchOnArrayType 'toSigseg         tuptyp)))
			       ))
		       "")
		   "\n")
		  `("schedule := ",v"() :: !schedule"))]
	 [else (error 'readFile "mode not handled yet in Caml backend: ~s" mode)]
	  )]
       
       ;[,other (values "UNKNOWNSRC\n" "UNKNOWNSRC\n")]
       
       )]))

(define (build-BASE type)  
  (if (equal? type #())      
      ;`(" baseSink x = print_endline (\"UNIT\"); flush stdout \n")
      `(" baseSink x = flush stdout \n")
      `(" baseSink x = print_endline (",(build-show type)" x); flush stdout \n")
  ))


(define (type->reader t) 
  (match t
    [Int16  "(fun str ind -> read_int16 str ind)"]
    ;[Int  "(fun str ind -> read_int32 str ind)"]
    ))

(define (build-binary-reader types)
  (define widths (map type->width types))
  (list 
   "fun str ind -> \n"
   ;"  let pos = ref ind in \n"
   (apply make-tuple-code
     (mapi (lambda (i t)
	     (list (type->reader t)" str (ind + "
		   (number->string (apply + (list-head widths i)))")")
	     )
	   types))
   "\n"))

(define (build-text-reader types)
  (match types
    [(Int)
     "8888888"
     ]))

(define (build-show t)
  (match t
    ;; ERROR: FIXME:
    [(quote ,_) "(fun _ -> \"POLYMORPHIC_OBJECT\")"]

    [String "(fun x -> x)"]
    [Int   "string_of_int"]
    [Int16 "string_of_int"] ;; These are just represented as ints.
    [Float "string_of_float"]
    [Double "string_of_float"]
    [Bool "string_of_bool"]
    [Complex "(fun c -> string_of_float c.Complex.re ^ \"+\" ^ string_of_float c.Complex.im ^ \"i\")"]

    [(List ,[t]) (list "(fun ls -> \"[\" ^ String.concat \", \" (List.map "t" ls) ^ \"]\")")]
    [(Array ,[t]) (list "(fun a -> \"[\" ^ String.concat \", \" (List.map "t" (arrayToList a)) ^ \"]\")")]

    ;; Just print range:
    [(Sigseg ,t) (list 
     "(fun ss -> \"[\"^ string_of_int ("  (DispatchOnArrayType 'start t)
     " ss) ^\", \"^ string_of_int ("      (DispatchOnArrayType 'end t)
     " ss + 1) ^ \")\")")]

    [#(,[t*] ...)
     (let ([flds (map Var (map unique-name (make-list (length t*) 'fld)))])
       (list 
	"(fun "(apply make-tuple-code flds)" ->\n"
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
      (error 'emit-caml:ConvertArrType "can't make a Bigarray of this type: ~s" t)))

;; It is error prone to keep writing this:
(define (sigseg-prim? p)
  (memq p '(joinsegs subseg width toSigseg toArray timebase start end seg_get)))

;; Converts an operator based on the array element type.
(define (DispatchOnArrayType op elt)
  (let ([flatty (BigarrayType? elt)])
    (if flatty
	(case op
	  [(Array:null) `("(Bigarray.Array1.create Bigarray.",flatty" Bigarray.c_layout 0)")]
	  [(Array:set)     "Bigarray.Array1.set"]
	  [(Array:ref)     "Bigarray.Array1.get"]
	  [(Array:length)  "Bigarray.Array1.dim"]
	  ;; Depend on that inliner!
	  [(Array:make)  
	   (lnboth (make-fun '("n" "x") 
	     (lnfst (make-let `(["ar" ("(Bigarray.Array1.create Bigarray.",flatty" Bigarray.c_layout n)")])
			   "begin Bigarray.Array1.fill ar x; ar end"))))]
	  ;(make-begin (make-app 'Bigarray.Array1.fill '(ar x)) 'ar)

	  ;; TODO: This could actually be unsafe in the bigarray
	  ;; case... but we don't exploit that yet, we just use the safe version
	  [(Array:makeUNSAFE)
	   (lnboth (make-fun '("n") 
	     (make-app (DispatchOnArrayType 'Array:make elt)
		(list "n" 
		  (match (make-caml-zero-for-type elt)
		    [(quote ,c) (Const c)])))))]
	  
	  ;; This takes an extra arg for bigarrays:
	  [(nullseg) `("(nullseg_flat ",flatty")")]	  
	  ;; For the sigseg prims we just append "_flat" to the name:
	  [else 
	   (if (sigseg-prim? op)
	       (format "~a_flat" (ASSERT (PrimName op)))
	       (error 'DispatchOnArrayType "don't know how to dispatch this operator: ~s" op))])
	;; This is the native Caml array case:
	(case op
	  [(Array:null) "[||]"]
	  [(Array:make) "Array.make"]
	  ;; Just makes a normal array with zeros:
	  ;; Cut/paste from above:
	  [(Array:makeUNSAFE)
	   (lnboth (make-fun '("n") 
	     (make-app (DispatchOnArrayType 'Array:make elt)
		(list "n" 
		  (match (make-caml-zero-for-type elt)
		    [(quote ,c) (Const c)])))))]

	  [(Array:length) "Array.length"]
	  [(Array:set)    "Array.set"]
	  [(Array:ref)    "Array.get"]

	  [(nullseg) "nullseg_flat"]
	  ;; We just use the normal name conversion:
	  [else (if (sigseg-prim? op)
		    (ASSERT (PrimName op))		    
		    (error 'DispatchOnArrayType "don't know how to dispatch this operator: ~s" op))]
	  ))))

(define make-caml-zero-for-type 
  (lambda (t)
    (match t
      [Int   ''0]
      [Int16 ''0]
      [Float ''0.0]
      [Double ''0.0]
      [Complex ''0.0+0.0i]
      [#(,[t*] ...) `(tuple ,t* ...)]
      [,oth (error 'make-caml-zero-for-type "unhandled type: ~s" oth)])))

;;================================================================================

;; This just converts the name of the primitive, for those primitives
;; that map directly onto OCaml functions:
(define (PrimName sym)
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
    '([memoized_fftR2C fftR2C]

      [_+_ "(+)"]  
      [_-_ "(-)"] 
      [*_ "( * )"]
      [/_ "(/)"]
      [^_ powInt] ;; Defined in prims.ml

      [_+. "(+.)"]
      [_-. "(-.)"] 
      [*. "( *.)"] 
      [/. "(/.)"]
      [^. "( ** )"]

      [_+D "(+.)"]
      [_-D "(-.)"]
      [*D "( *.)"] 
      [/D "(/.)"]
      [^D "( ** )"]

      [_+: "Complex.add"]
      [_-: "Complex.sub"] 
      [*: "Complex.mul"] 
      [/: "Complex.div"]
      [^: "Complex.pow"]

      [_+I16 "(+)"]
      [_-I16 "(-)"] 
      [*I16 "( * )"] 
      [/I16 "(/)"]
      [^I16 "powInt"]

      [<      "(<)"]
      [<=     "(<=)"]
      [>      "(>)"]
      [>=     "(>=)"]
      [=        "(=)"] ;; NOTE! FIXME! should be =???
      [wsequal?   "(=)"] ;; NOTE! FIXME! should be =???
      [string-append "(^)"]      
      [Mutable:ref "ref"]
      [deref "!"]

      [absI16 abs]
      [absI   abs]
      [absF   abs_float]
      [absD   abs_float]
      [absC   Complex.norm]

      [sqrtI "(fun x -> (int_of_float (sqrt (float_of_int x))))"]
      [sqrtF sqrt]
      [sqrtC Complex.sqrt]

      [realpart "(fun x -> x.Complex.re)"]
      [imagpart "(fun x -> x.Complex.im)"]

      [cons "(fun x y -> x::y)"]
      [car List.hd]
      [cdr List.tl]
      [List:length List.length]
      [List:reverse List.rev]
      [List:ref List.nth]
      [List:append List.append]

      [int16ToInt    "(fun x -> x)"]
      [int16ToFloat  float_of_int]
      [int16ToDouble  float_of_int]
      [int16ToComplex  "(fun n -> {Complex.re= float_of_int n; Complex.im= 0.})"]

      [intToInt16    "(fun x -> x)"]
      [intToFloat    float_of_int]
      [intToDouble   float_of_int]
      [intToComplex  "(fun n -> {Complex.re= float_of_int n; Complex.im= 0.})"]

      [floatToInt    int_of_float]
      [floatToInt16  int_of_float]
      [floatToDouble  "(fun x -> x)"]
      [floatToComplex "(fun f -> {Complex.re= f; Complex.im= 0.})"]

      [doubleToInt    int_of_float]
      [doubleToInt16  int_of_float]
      [doubleToFloat  "(fun x -> x)"]
      [doubleToComplex "(fun f -> {Complex.re= f; Complex.im= 0.})"]

      [complexToInt16 "(fun c -> int_of_float c.Complex.re)"]
      [complexToInt   "(fun c -> int_of_float c.Complex.re)"]
      [complexToFloat "(fun c -> c.Complex.re)"]
      [complexToDouble "(fun c -> c.Complex.re)"]

      [makeComplex "(fun re im -> {Complex.re= re; Complex.im= im})"]

      [stringToInt int_of_string]
      [stringToFloat float_of_string]
      [stringToDouble float_of_string]
      [stringToComplex "(fun s -> Scanf.sscanf \"%f+%fi\" (fun r i -> {Complex.re=r; Complex.im=i}))"]

      [roundF "(fun x -> floor (x +. 0.5))"]

      [start   ss_start]
      [end     ss_end]
      [seg_get ss_get]
      ))
  (cond 
   [(memq sym sametable) (Var sym)]
   [(assq sym aliastable) => (lambda (x) (format "~a" (cadr x)))]
   [else #f]))

(define (Prim expr emitter)
  (define (myExpr x) (Expr x emitter))
  (match expr

    ;; Print is required to be pre-annotated with a type.
    ;; (We can no longer do recover-type.)
    [(print (assert-type ,t ,e))    
     `("(print_string ",(Prim `(show (assert-type ,t ,e)) emitter)")")]
    [(print ,_) (error 'emit-c:Effect "print should have a type-assertion around its argument: ~s" _)]
    [(show (assert-type ,t ,[myExpr -> e]))
     `("((",(build-show t)") ",e")")]
    [(wserror ,[myExpr -> s])
     ;; Should declare a special WSException or something:
     `("(raise (Failure ",s"))")]

    ;; This is annoying, but we use different sigseg prims based on the type of Array that they use.
    [(,prim (assert-type (,tc ,elt) ,first) ,rest ...)
     (guard (memq tc '(Array Sigseg)) (sigseg-prim? prim))
     (make-app (DispatchOnArrayType prim elt)
	       (map myExpr (cons first rest)))]
    ;; Safety net:
    [(,prim ,_ ...) (guard (sigseg-prim? prim))
     (error 'emit-caml:Prim "missed this sigseg prim: ~s" prim)]

    ;; Now do array prims in much the same way:
    [(assert-type (Array ,elt) (,prim ,[myExpr -> arg*] ...))
     (guard (memq prim '(Array:make Array:makeUNSAFE)))
     (make-app (DispatchOnArrayType prim elt) arg*)]
    [(,prim (assert-type (Array ,elt) ,[myExpr -> first]) ,[myExpr -> rest] ...)
     (guard (memq prim '(Array:ref Array:set Array:length)))
     (make-app (DispatchOnArrayType prim elt)
	       (cons first rest))]
    ;; Safety net:
    [(,prim ,_ ...)     
     (guard (memq prim '(Array:make Array:makeUNSAFE Array:ref Array:set Array:length)))     
     (error 'emit-caml:Prim "missed this array prim: ~s" prim)]


    [(assert-type ,t ,[primapp]) primapp]
    [(,prim ,[myExpr -> rands] ...) (guard (real-primitive? prim))
     (list "("(cond
	       [(PrimName prim) => (lambda (x) x)]
	       [else (error 'emit-caml:Prim "currently unhandled: ~s" prim)])
	   " "
	   (insert-between " " rands) ")\n")]
))



;;================================================================================
;; Import the rest of our functionality from the shared module.


;; This packages up the caml specific functionality to pass back up to the parent module.
;; This is not complete, just what I happen to be using.
(define CamlSpecific 
  (lambda args
    (apply
     (make-dispatcher (car args)
		      
        make-let 
	make-tuple-code 
	make-fun
	make-for
	make-while
	make-fun-binding
	
	Var Prim Const 
	DispatchOnArrayType
	Type

	)
     (cdr args))))
      

(define-values (Expr Iterate Emit make-bind)
  (sharedEmitCases CamlSpecific))






;======================================================================
;;; Bits of boilerplate.

;(define boilerplate_premain )
;(define (boilerplate_postmain return_name return_type) )

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
		   eq? locdiff nodeid getID sense even? odd? 
		   ;; These weren't really primitives:    
		   tuple tupref ref deref static statref __foreign foreign foreign_box foreign_source
		   ;; These were desugared or reduced to other primitives:
		   or and dataFile 
		   ;; These were resolved into the w/namespace versions:
		   head tail map append fold
		   List:head List:tail  List:is_null 

		   ptrToArray ptrIsNull

		   ;; This are handled specially by DispatchOnArrayType.
		   ;; This is due to the dual-representation for arrays.
		   Array:toList Array:make Array:makeUNSAFE Array:get Array:ref Array:length
		   joinsegs subseg width toSigseg toArray timebase start end seg_get
		   
		   ;; These have a special syntax, requiring an assert-type or whatnot:
		   cons car cdr null? prim_window 
		   List:ref List:append List:reverse List:length List:make 

		   __cast_num  
		   _+U16 *U16 _-U16 /U16 ^U16
		   _+I32 *I32 _-I32 /I32 ^I32
	   
		   wsequal? print show seg_get toArray __show_ARRAY __wserror_ARRAY __backtoSTR
		   __stringToInt_ARRAY __stringToFloat_ARRAY __stringToDouble_ARRAY __stringToComplex_ARRAY

		   ;; [2007.07.24] Because the caml backend isn't really supported, we're not doing Int64s:
		   int64ToInt16   int64ToInt    int64ToFloat int64ToDouble int64ToComplex
		   int16ToInt64 intToInt64 floatToInt64 doubleToInt64 complexToInt64
   		   _+I64 _-I64 *I64 /I64 ^I64 absI64 
		   randomI

		   ensBoxAudio ensBoxAudioF ensBoxAudioAll
		   moduloI

		 lshiftI16 rshiftI16 logorI16 logandI16 logxorI16 
		 lshiftU16 rshiftU16 logorU16 logandU16 logxorU16 
		 lshiftI32 rshiftI32 logorI32 logandI32 logxorI32 

		   ;; TODO, FIXME: These I just haven't gotten to yet:
		   fftC ifftC ifftC2R
		   exptI logD logF

		   List:assoc List:assoc_update
		   hashrem hashset ;; pure versions
		   Array:map Array:fold
		   internString uninternString

		   exclusivePtr getPtr
		   clock realtime
		   
		   HashTable:contains HashTable:get HashTable:set_BANG HashTable:rem_BANG 
		   HashTable:make HashTable:rem HashTable:set ;; pure versions


		   ;; [2007.08.30] NO FURTHER DEVELOPMENT ON CAML BACKEND, THEREFORE NOT ADDING PRIMS!
		   List:toArray ptrMakeNull gnuplot_array gnuplot_array2d 
		   String:length String:explode String:implode charToInt intToChar 
		   Secret:newTimebase

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

(define-testing test-this (default-unit-tester "wavescript_emit-caml.ss: generating WaveScript OCaML code." these-tests))
(define test-emit-caml test-this)


) ;; End Module


;;================================================================================

