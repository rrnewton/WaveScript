
;;;; .title WaveScript EmitMLton (SML)
;;;; .author Ryan Newton [2007.06.13]

(module emit-mlton mzscheme 
  (require  "../../../plt/common.ss"
	    "../../compiler_components/c_generator.ss" )
  (provide emit-mlton-wsquery )
  (chezprovide )  
  (chezimports (except helpers test-this these-tests))

;; MUTABLE
(define union-edges 'union-edges-uninit)

;======================================================================
;======================================================================

;;; These are the functions that differ from their caml countparts.

(define (coerce x) (if (symbol? x) (symbol->string x) x))
(define (make-fun formals body) 
  (list "(fn " (if (null? formals) "()"
		   (insert-between " " (map coerce formals)))
	" => \n" body ")"))
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

(define (make-begin . exprs) (list "(" (insert-between ";\n" exprs) ")"))


(define (make-tuple . args)  (list "(" (insert-between ", " args) ")"))
(define (make-app rator rands) (list "(" rator " "(insert-between " " rands) ")"))




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
    [Float     "real"]
    [Double    "real"]
    
    

    [Bool    "bool"]
    [Int     "int"]
    [Int16   "int"]

;    [Complex  "Complex.t"]
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
    ;; ERROR: FIXME:
    [(quote ,_) "(fun _ -> \"POLYMORPHIC_OBJECT\")"]

    [String (make-fun '(x) "x")]
    [Int   "Int.toString"]
    [Int16 "Int.toString"] ;; These are just represented as ints.
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
	"(fun "(apply make-tuple flds)" ->\n"
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
;    (define header2 (file->string (++ (REGIMENTD) "/src/generic/passes/mlton_bkend/sigseg.sml")))
    ;(define header2 (file->string (++ (REGIMENTD) "/src/generic/passes/mlton_bkend/sigseg_seglist.sml")))
;    (define header3 (file->string (++ (REGIMENTD) "/src/generic/passes/mlton_bkend/prims.sml")));
;    (define header4 (file->string (++ (REGIMENTD) "/src/generic/passes/mlton_bkend/data_reader.sml")))

    (match prog
      [(,lang '(graph (const ,[ConstBind -> cb*] ...)
		      (sources ,[Source -> src* init1*] ...)
		      (iterates ,[Iterate -> iter*] ...)
		      (unionNs ,[Union -> union*] ...)
		      (sink ,base ,basetype)))
       
       ;; Just append this text together.
       (let ([result (list header1 ; header2 header3 header4 "\n" 

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
      (apply make-begin 
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
		    (make-begin 
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
	  (if (not (zero? repeats))
	      (error 'emit-mlton "Caml text mode reader doesn't support replaying the datafile yet."))
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
	         ((Emit downstrm) (apply make-tuple names))"); \n"))
	    ;; Now discard the rest of the line.
;	    "  let _ = input_line hndl in "
	    "   timestamp := !timestamp + "(number->string (rate->timestep rate))";"
	    "   SE (!timestamp, f) in \n"
	    "  SE (0, f) \n" 
	    "  \n"
	    )
	   ;; Initialization: schedule this datasource:
	   `("schedule := ",v"() :: !schedule;;\n"))]
	 
	 [(equal? mode "binary")
	  (values (list 
		   ;; Builds a function from unit to an initial scheduler entry "SE" 
		   " "v" = fun () -> \n"
		   "  let binreader = "(indent (build-binary-reader types) "    ")" \n"
		   "  and textreader = 33333 in \n"
		   "    "
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
				 (make-tuple 
				  (DispatchOnArrayType 'Array:makeUNSAFE tuptyp)
				  (DispatchOnArrayType 'Array:set        tuptyp)
				  (DispatchOnArrayType 'toSigseg         tuptyp)))
			       ))
		       "")
		   "\n")
		  `("schedule := ",v"() :: !schedule;;\n"))]
	 [else (error 'readFile "mode not handled yet in Caml backend: ~s" mode)]
	  )]
       
       ;[,other (values "UNKNOWNSRC\n" "UNKNOWNSRC\n")]
       
       )]))




(define (type->reader t) 
  (match t
    [Int16  (make-fun '(str ind) (make-app "read_int16" '("str" "ind")))]
    ;[Int  "(fun str ind -> read_int32 str ind)"]
    ))

(define (build-binary-reader types)
  (define widths (map type->width types))
  (list 
   "fun str ind -> \n"
   ;"  let pos = ref ind in \n"
   (apply make-tuple
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
  (let ([flatty (BigarrayType? elt)])
    (if flatty
	(case op
	  [(Array:null) `("(Bigarray.Array1.create Bigarray.",flatty" Bigarray.c_layout 0)")]
	  [(Array:set)     "Bigarray.Array1.set"]
	  [(Array:ref)     "Bigarray.Array1.get"]
	  [(Array:length)  "Bigarray.Array1.dim"]
	  ;; Depend on that inliner!
	  [(Array:make)  
	   (lnboth (make-fun '(n x) 
	     (lnfst (make-let `([ar ("(Bigarray.Array1.create Bigarray.",flatty" Bigarray.c_layout n)")])
			   "begin Bigarray.Array1.fill ar x; ar end"))))]
	  ;(make-begin (make-app 'Bigarray.Array1.fill '(ar x)) 'ar)

	  ;; TODO: This could actually be unsafe in the bigarray
	  ;; case... but we don't exploit that yet, we just use the safe version
	  [(Array:makeUNSAFE)
	   (lnboth (make-fun '(n) 
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
	   (lnboth (make-fun '(n) 
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

; ======================================================================
;; Expressions.

;; .param exp      The expression to process.	
;; .param emitter  Function that generates the emit code, given an argument.
(define Expr ;(Expr tenv)
  (lambda (exp emitter)
    (match exp
      [,v (guard (symbol? v) (regiment-constant? v)) (Const v)]
      [,v (guard (symbol? v)) (Var v)]
      [',c (Const c)]

      [(assert-type (Sigseg ,elt) nullseg)    (DispatchOnArrayType 'nullseg elt)]
      [(assert-type (Array ,elt) Array:null)  (DispatchOnArrayType 'Array:null elt)]

      [(tuple ,[rands] ...)   (apply make-tuple rands)]
      [(tupref ,i ,len ,[v])
       (let ([pat 
	      (apply make-tuple
		     (append (make-list i "_") '("x")			
			     (make-list (- len i 1) "_")))])
	 (make-let `((,pat ,v)) "x"))]
      
      [(let ([,[Var -> v] ,ty ,[rhs]]) ,[bod])
       (make-let `((,v ,rhs)) bod)]
      [(begin ,[e*] ...)  (indent (apply make-begin e*) "  ")]
      [(emit ,vq ,[x]) (emitter x)]
      [(set! ,[Var -> v] ,[e])  `("(",v " := " ,e")")]
      [(if ,[t] ,[c] ,[a])   `("(if ",t"\nthen ",c"\nelse ",a")\n")]

      ;; This is a really lame hack for now... emulating "break":
      [(for (,i ,[st] ,[en]) ,[bod])
       `("(for ",(Var i)" = ",st" to ",en" do\n ",bod"\n done)")]
      ;[(break) "(broke := true)"]
      [(while ,[tst] ,[bod]) `("(while ",tst" do\n ",bod"\ndone)")]


      [(,prim ,rand* ...) (guard (regiment-primitive? prim))
       (Prim (cons prim rand*) emitter)]
      [(assert-type ,t (,prim ,rand* ...)) (guard (regiment-primitive? prim))
       (Prim `(assert-type ,t (,prim . ,rand*)) emitter)]

      [(assert-type ,t ,[x]) 
       ;(printf "MISC ASCRIPTION: ~s on ~s\n" t x)
       x]
      [,unmatched (error 'emit-mlton:Expr "unhandled form ~s" unmatched)]

)))


;;================================================================================

;; This just converts the name of the primitive, for those primitives
;; that map directly onto Mlton functions:
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
    '([+_ "(+)"]  
      [-_ "(-)"] 
      [*_ "( * )"]
      [/_ "(/)"]
      [^_ powInt] ;; Defined in prims.sml

      [+. "(+.)"]
      [-. "(-.)"] 
      [*. "( *.)"] 
      [/. "(/.)"]
      [^. "( ** )"]

      [+: "Complex.add"]
      [-: "Complex.sub"] 
      [*: "Complex.mul"] 
      [/: "Complex.div"]
      [^: "Complex.pow"]

      [+I16 "(+)"]
      [-I16 "(-)"] 
      [*I16 "( * )"] 
      [/I16 "(/)"]
      [^I16 "powInt"]

      [<      "(<)"]
      [<=     "(<=)"]
      [>      "(>)"]
      [>=     "(>=)"]
      [=        "(=)"] ;; NOTE! FIXME! should be =???
      [equal?   "(=)"] ;; NOTE! FIXME! should be =???
      [string-append "(^)"]      
      [Mutable:ref "ref"]
      [deref "!"]

      [absI16 abs]
      [absI   abs]
      [absF   abs_float]
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

      [stringToInt int_of_string]
      [stringToFloat float_of_string]
      [stringToDouble float_of_string]
      [stringToComplex "(fun s -> Scanf.sscanf \"%f+%fi\" (fun r i -> {Complex.re=r; Complex.im=i}))"]

      [roundF "(fun x -> floor (x + 0.5))"]

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
     (error 'emit-mlton:Prim "missed this sigseg prim: ~s" prim)]

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
     (error 'emit-mlton:Prim "missed this array prim: ~s" prim)]


    [(assert-type ,t ,[primapp]) primapp]
    [(,prim ,[myExpr -> rands] ...) (guard (regiment-primitive? prim))
     (list "("(cond
	       [(PrimName prim) => (lambda (x) x)]
	       [else (error 'emit-mlton:Prim "currently unhandled: ~s" prim)])
	   " "
	   (insert-between " " rands) ")\n")]
))




















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

