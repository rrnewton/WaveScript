
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

(module emit-caml mzscheme 
  (require  "../../../plt/common.ss"
	    "../../compiler_components/c_generator.ss" )
  (provide emit-caml-wsquery
	   explicit-stream-wiring)
  (chezprovide )  
  (chezimports (except helpers test-this these-tests))
  
;======================================================================
;;                    <WaveScript CAML generation>
;======================================================================

;; If the type needs a specialized hashfun, returns its name,
;; otherwise returns #f.
;(define (HashType k v) )
;(define (SharedPtrType t))

;; This should give you an idea of the mapping between types:
(define (Type t)
  (match t
    [Bool    "bool"]
    [Int     "int"]
    ;[Int16   "wsint16_t"]
    [Float    "float"]
    [Complex  "Complex.t"]
    [String   "string"]
    [(Ref ,[t]) `("(",t ") ref")]
    [(VQueue ,_) "unit"]
    [#() "unit "]
    ;; Went back and forth on whether this should be a pointer:
    [(Sigseg ,[t]) `("(",t") sigseg")]
    ;[(Stream ,[t]) `("WSBox*")]
    ;[(Array ,[t]) `(,t "[]")]
    [(Array ,[t])  `("(",t") array")]
    ;[Timebase  "int"]
    [(List ',_) `("cons<int>::ptr")]
    ;; Boosted cons cells:
    [(List ,[t]) `("cons< ",t" >::ptr")]
    ;[(HashTable ,kt ,vt) (SharedPtrType (HashType kt vt))]
    [,other (error 'emit-caml:Type "Not handled yet.. ~s" other)]))

;======================================================================

;; This is the only entry point to the file.  A complete query can
;; be transformed into a complete query file.
;;
;; .param prog  The wsquery to process.
(define emit-caml-wsquery
  (trace-lambda ECW (prog)
    ;; Lame, requires REGIMENTD:
    (define header1 (file->string (++ (REGIMENTD) "/src/generic/passes/ocaml_bkend/scheduler.ml")))
    (define header2 (file->string (++ (REGIMENTD) "/src/generic/passes/ocaml_bkend/sigseg.ml")))
    (define header3 (file->string (++ (REGIMENTD) "/src/generic/passes/ocaml_bkend/data_reader.ml")))

    (match prog
      [(,lang '(graph (const . ,c*)
		      (sources ,[Source -> src* init1] ...)
		      (iterates ,[Iterate -> iter*] ...)
		      (sink ,base)))
       ;; Just append this text together.
       (let ([result (list header1 header2 header3 "\n" 
			   "let rec ignored = () \n" ;; Start off the let block.
			   ;; These return incomplete bindings that are stitched with "and":
			   (map (lambda (x) (list "\nand\n" x)) (append c*  src*  iter*))
			   ";; \n\n"
			   init1 "\n"
			   "runScheduler();;\n"
			   "\nPrintf.printf \"Query completed.\\n\";;\n"
			   )])
	 (string->file (text->string result) 
		       (++ (REGIMENTD) "/src/generic/passes/ocaml_bkend/foo.ml"))
	 result	
	 )]
      [,other ;; Otherwise it's an invalid program.
       (error 'emit-caml-wsquery "ERROR: bad top-level WS program: ~s" other)])))



; ======================================================================
;;; Helper functions for handling different program contexts:

(define (Iterate iter)
  (match iter
    [(,name ,ty (let ([,lhs* ,ty* ,rhs*] ...)
	       (lambda (,x ,vq) (,ty1 ,ty2) ,bod))
	 ,up (,down* ...))
     (let* ([emitter (Emit down*)])
       `(" ",(Var name)" = \n"
	 "  let ",(Var vq)" = () in\n"
	 ,@(map (lambda (lhs ty rhs)
		  `("  let ",(Var lhs)" = ",(Expr rhs emitter)" in\n"))
	     lhs* ty* rhs*)
	 "  fun ",(Var x)" -> \n"
	 ,(indent (Expr bod emitter) "    ")
	 "\n")       
       )]))

(define (Emit down*)
  (lambda (expr)
    ;; Just call each of the sites with the argument.
    `("(let emitted = ",expr" in\n"
      ,@(map (lambda (down) 
	       (if (eq? down 'BASE)
		   `("baseSink emitted;\n")
		   `(,(Var down)" emitted;\n")))
	  down*)
      "  ())")))

        
(define (Var var)
  (ASSERT (symbol? var))
  ;; This is the place to do any name mangling.  I'm not currently doing any for WS.
  (symbol->string var))

(define Const
  (lambda (datum)
    (cond
     ;[(eq? datum 'BOTTOM) (wrap "0")] ;; Should probably generate an error.
     [(eq? datum 'UNIT) "()"]
     [(eq? datum #t) "true"]
     [(eq? datum #f) "false"]
     [(string? datum) (format "~s" datum)]
     [(flonum? datum)  (number->string datum)]
     #;
     [(cflonum? datum) (wrap (format "(wscomplex_t)(~a + ~afi)" 
				     (cfl-real-part datum)
				     (cfl-imag-part datum)))]
     [(integer? datum) (number->string datum)]

     ;[(eq? datum 'nulltimebase)  (wrap "WSNULLTIMEBASE")]     
#;
     [(vector? datum)
      (ASSERT name)
      (let ([contenttype (if (zero? (vector-length datum))
			     ;; Throw in a default:
			     'Int
			     (type-const (vector-ref datum 0)))])
	`(,type" ",name" = boost::shared_ptr< vector< ",(Type contenttype)
	       " > >(new vector< ",(Type contenttype)" >(",
	       (number->string (vector-length datum))"));\n" ;; MakeArray.
	       ,(mapi (lambda (i x) (Const `("(*",name")[",(number->string i)"]")
					   "" x))			   
		      (vector->list datum))))]

     [else (error 'emit-caml:Const "not an OCaml-compatible literal (currently): ~s" datum)])))




;; ================================================================================

;; Converts hertz to microseconds:
(define (rate->timestep freq)
  (when (zero? freq) (error 'rate->timestep "sampling rate of zero is not permitted"))
  (flonum->fixnum (* 1000000 (/ 1.0 freq))))

;; Returns: a binding snippet (incomplete), and initialization code.
(define (Source src)
  (match src
    [(,[Var -> v] ,ty ,app (,downstrm ...)) 
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
	     "schedule := SE(0,",v") :: !schedule;;\n"))
	  )]

       [(audioFile ,fn ,win ,rate)
	000000000000]

       [(__dataFile ,[file] ,[mode] ',rate ,[repeats] ,types)	
	(values (list 
		 " "v" = let reader = "(build-reader types)" in \n"
		 "  dataFile "file" "mode" "(number->string (rate->timestep rate))
		 "  "repeats" reader\n")
		`("schedule := SE(0,",v") :: !schedule;;\n"))]
       
       [,other (values "UNKNOWNSRC\n" "UNKNOWNSRC\n")]
       
       )]))

(define (build-reader types)
  (match types
    [(Int)
     ;; BROKEN BROKEN!!
     "fun str ind -> (Marshall.from_string str ind :: int)"
     ]))


; ======================================================================
;; Expressions.
	
(define Expr ;(Expr tenv)
  (lambda (exp emitter)
    (match exp
      [,v (guard (symbol? v)) (Var v)]
      [',c (Const c)]

      [(tuple ,[rands] ...)
       (list "(" (insert-between ", " rands) ")")]
      [(tupref ,i ,len ,[v])
       (let ([pat 
	      (insert-between ", "
		(append (make-list i "_") '("x")			
			(make-list (- len i 1) "_")))])
	 `("(let (",pat") = ",v" in x)\n"))]
      
      [(let ([,[Var -> v] ,ty ,[rhs]]) ,[bod])
       `("(let ",v" = ",rhs" in\n ",bod")")]
      [(begin ,[e*] ...)  `("begin\n" ,@(indent (insert-between ";\n" e*) "  ") "\nend")]
      [(emit ,vq ,[x]) (emitter x)]
      [(set! ,[Var -> v] ,[e])  `("(",v " := " ,e")")]
      [(if ,[t] ,[c] ,[a])   `("(if ",t"\nthen ",c"\nelse ",a")\n")]

      [(,prim ,rand* ...) (guard (regiment-primitive? prim))
       (Prim (cons prim rand*) emitter)]
      [(assert-type ,t (,prim ,rand* ...)) (guard (regiment-primitive? prim))
       (Prim `(assert-type ,t (,prim . ,rand*)) emitter)]

      [(assert-type ,t ,[x]) x]
      [,unmatched (error 'emit-caml:Expr "unhandled form ~s" unmatched)]

;;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#|
      ;; Special Constants:
      [(assert-type ,t nullseg) (wrap (PolyConst 'nullseg t))]
      [(assert-type ,t Array:null) (wrap (PolyConst 'Array:null t))]
      [(assert-type ,t '())     (wrap (PolyConst '() t))]
      [nulltimebase             (Const name type 'nulltimebase)]
      



|#


)))


;;================================================================================

(define (buildShow t)
  (match t
    [String "fun x -> x"]
    [Int   "string_of_int"]
    [Float "string_of_float"]
    [#(,[t*] ...)
     (let ([flds (map Var (map unique-name (make-list 'fld)))])
       (list 
	"fun ("(insert-between ", " flds)") ->\n"
	(indent (map (lambda (printer fld) 
		       `("((",printer") ",fld") ^ \n")) 
		  flds)
		"  ")))]
    ))

(define (Prim expr emitter)
  (define (myExpr x) (Expr x emitter))
  (define sametable 
    '(nullseg joinsegs subseg width start end toSigseg	      
      cos sin tan acos asin atan max min
      not 
      
      fft m_invert 
      ;;wserror ;generic_hash 
      ))
  (define aliastable
    '([+_ "(+)"]  
      [-_ "(-)"] 
      [*_ "(*)"]
      [/_ "(/)"]
      [+. "(+.)"]
      [-. "(-.)"] 
      [*. "(*.)"] 
      [/. "(/.)"]
     ;[+_ +] [-_ -] [*_ *] [/_ /]
      [< "(<)"]
      [<= "(<=)"]
      [> "(>)"]
      [>= "(>=)"]
      [wsequal "(==)"]
      [string-append "(^)"]      
      [Mutable:ref "ref"]
      [deref "!"]
      [absI abs]
      [absF abs_float]
      [sqrtF sqrt]

      ))
  (match expr

    ;; Print is required to be pre-annotated with a type.
    ;; (We can no longer do recover-type.)
    [(print (assert-type ,t ,e))    
     `("(print_string ",(Prim `(show (assert-type ,t ,e)) emitter)")")]
    [(print ,_) (error 'emit-c:Effect "print should have a type-assertion around its argument: ~s" _)]
    [(show (assert-type ,t ,[myExpr -> e]))
     `("((",(buildShow t)") ",e")")]
    [(,prim ,[myExpr -> rands] ...)
     (list "("(cond
	       [(memq prim sametable) (Var prim)]
	       [(assq prim aliastable) => (lambda (x) (format "~a" (cadr x)))]
	       [else (error 'emit-caml:Prim "currently unhandled: ~s" prim)])
	   " "
	   (insert-between " " rands) ")\n")]
  ;; TODO
#|
      [(roundF)                 "round"]

    ;[(^: ,[Simple -> x] ,[Simple -> y]) (wrap `("pow(",x", ",y")"))]
    ;[(^: ,[Simple -> x] ,[Simple -> y]) (wrap `("(wscomplex_t)pow((double)",x", (double)",y")"))]
    ;[(^: ,[Simple -> x] ,[Simple -> y]) (wrap `("(wsfloat_t)pow((double __complex__)",x", (double __complex__)",y")"))]
    ;; INEFFICIENT:
    [(^: ,[Simple -> x] ,[Simple -> y]) 
     (let ([tmp (Var (unique-name 'tmp))])
       `("complex<float> ",tmp" = pow((complex<float>)",x", (complex<float>)",y");\n"
       ,(wrap `("real(",tmp") + (imag(",tmp") * 1.0fi)"))
       ))]
    
    ;; TODO: tupref, exponentiation 
    [(,infix_prim ,[Simple -> left] ,[Simple -> right])
     (guard (memq infix_prim '(;+ - * /
			       +. -. *. /. 
				  +_ *_ -_ /_
				  +: *: -: /:
				  +I16 *I16 -I16 /I16
				  < > <= >= =
				  ^_ ^. ^: ^I16
				  )))
     (let ([cname (case infix_prim
		    [(=) "=="]
		    [(;+ * - / 
		      < > <= >=) infix_prim]
		    [(+. *. -. /.
			 +_ *_ -_ /_
			 +: *: -: /:
			 ^_ ^. 
			 ) ;; Chop off the extra character.
		     (substring (symbol->string infix_prim) 0 1)]
		    [(+I16 -I16 *I16 /I16 ^I16)
		     (substring (symbol->string infix_prim) 0 1)]
		    )])
       (wrap `("(" ,left ,(format " ~a " cname) ,right ")")))]

	;[(realpart ,[v]) `("(" ,v ".real)")]
	;[(imagpart ,[v]) `("(" ,v ".imag)")]
	[(imagpart ,[Simple -> v])   (wrap `("__imag__ " ,v))]
	[(realpart       ,[Simple -> v])   (wrap `("__real__ " ,v))]
	[(complexToFloat ,[Simple -> v])   (wrap `("__real__ " ,v))]
	[(complexToInt   ,[Simple -> v])   (wrap `("(wsint_t) __real__ " ,v))]
	[(complexToInt16 ,[Simple -> v])   (wrap `("(wsint16_t) __real__ " ,v))]

	[(absC ,[Simple -> c]) (wrap `("abs((complex<float>)",c")"))]

	[(intToInt16     ,[Simple -> e]) (wrap `("(wsint16_t)",e))]
	[(floatToInt16   ,[Simple -> e]) (wrap `("(wsint16_t)",e))]

	[(floatToInt   ,[Simple -> e]) (wrap `("(wsint_t)",e))]
	[(int16ToInt   ,[Simple -> e]) (wrap `("(wsint_t)",e))]
	
	[(intToFloat     ,[Simple -> e]) (wrap `("(wsfloat_t)",e))]
	[(int16ToFloat   ,[Simple -> e]) (wrap `("(wsfloat_t)",e))]

	;[(complexToInt16 ,e)   (wrap `("(wsint16_t)",(Prim `(complexToFloat ,e) #f "")))]
	;[(complexToInt ,e)     (wrap `("(wsint_t)"  ,(Prim `(complexToFloat ,e) #f "")))]
	;[(complexToFloat ,e)   (Prim `(realpart ,e) name type)]
	[(,ToComplex ,[Simple -> e])
	 (guard (memq ToComplex 
		      '(int16ToComplex intToComplex floatToComplex)))
    	 (wrap `("((wscomplex_t)(",e" + 0.0fi))"))]

	[(stringToInt ,[Simple -> e]) 
	 (let ([tmp (Var (unique-name 'tmp))])
	   `("wsint_t ",tmp";\n"
	     "sscanf(",e".c_str(), \"%d\", &",tmp");\n"
	     ,(wrap tmp)))]
	[(stringToFloat ,[Simple -> e]) 
	 (let ([tmp (Var (unique-name 'tmp))])
	   `("wsfloat_t ",tmp";\n"
	     "sscanf(",e".c_str(), \"%f\", &",tmp");\n"
	     ,(wrap tmp)))]
	[(stringToComplex ,[Simple -> e]) 
	 (let ([tmp1 (Var (unique-name 'tmp))]
	       [tmp2 (Var (unique-name 'tmp))])
	   `("wsfloat_t ",tmp1";\n"
	     "wsfloat_t ",tmp2";\n"
	     ;"printf(\"STRING %s\\n\", ",e".c_str());\n"
	     "sscanf(",e".c_str(), \"%f+%fi\", &",tmp1", &",tmp2");\n"
	     ,(wrap `(,tmp1"+(",tmp2"*1.0fi)"))))]

	[(show (assert-type ,t ,[Simple -> e])) (wrap (EmitShow e t))]
	[(show ,_) (error 'emit-c:Value "show should have a type-assertion around its argument: ~s" _)]

	[(toArray (assert-type (Sigseg ,t) ,sigseg))
	 (let ([tmp (Var (unique-name 'tmp))]
	       [tmp2 (Var (unique-name 'tmp))]
	       [len (Var (unique-name 'len))]
	       [ss (Simple sigseg)]
	       [tt (Type t)])
	   `("boost::shared_ptr< vector<",tt"> >",tmp"(new vector<",tt">(",ss".length()));\n"
	     "int len = ",ss".length();\n"
	     "for(int i=0; i<len; i++) {\n"
	     "  ",(Prim `(seg-get (assert-type (Sigseg ,t) ,sigseg) i) tmp2 tt)
	     "  (*",tmp")[i] = ",tmp2";\n"
	     "}\n"
	     ,(wrap tmp)
	     ))]

	[(wserror ,[Simple -> str])
	 ;; Don't do anything with the return value.
	 `(,(if name `(,type" ",name";\n") "")
	   "WSPrim::wserror(",str");\n")]

	;; This is inefficient.  Only want to call getDirect once!
	;; Can't trust the C-compiler to know it's effect free and do CSE.
	[(seg-get (assert-type (Sigseg ,[Type -> ty]) ,[Simple -> seg]) ,[Simple -> ind])
	 ;`("(" ,seg ".getDirect())[" ,ind  "]")
	 (wrap `("(*((",ty"*)(*(" ,seg ".index_i(" ,ind  ")))))"))]
	[(seg-get ,foo ...)
	 (error 'emit-c:Value "seg-get without or with badtype annotation: ~s" 
		`(seg-get ,@foo))]
	[(timebase ,[Simple -> seg]) (wrap `("(" ,seg ".getTimebase())"))]
	
	;; Need to use type environment to find out what alpha is.
	;; We store the length in the first element.
	[(newarr ,[Simple -> int] ,alpha)
	 ;(recover-type )
	 "newarr_UNFINISHED"]
	
	;[(Array:ref ,[arr] ,[ind]) `(,arr "[" ,ind "]")]
	[(Array:ref ,[Simple -> arr] ,[Simple -> ind]) (wrap `("(*",arr ")[" ,ind "]"))]
	[(Array:make ,[Simple -> n] ,[Simple -> x])   (wrap `("makeArray(",n", ",x")"))]
	;; This version just doesn't initialize:
	[(assert-type (Array ,[Type -> ty]) (Array:makeUNSAFE ,[Simple -> n]))
	 (wrap `("boost::shared_ptr< vector< ",ty
		 " > >(new vector< ",ty" >(",n "))"))]

	[(Array:length ,[Simple -> arr])                   (wrap `("(wsint_t)(",arr"->size())"))]

	[(Array:set ,x ...)
	 (error 'emitC:Value "Array:set in Value context: ~s" `(Array:set ,x ...))]
	[(begin ,stmts ...)
	 (error 'emitC:Value "begin in Value context: ~s" `(begin ,stmts ...))]

	;; Later we'll clean it up so contexts are normalized:
	;[(set! ,[Var -> v] ,[(Value tenv) -> rhs]) `(,v " = " ,rhs ";\n")]

       	;; ----------------------------------------
	;; Lists:
	;; These primitives are tricky because of the template magic:

	[(assert-type (List ,[Type -> ty]) (cons ,[Simple -> a] ,[Simple -> b]))
	 (wrap `("cons< ",ty" >::ptr(new cons< ",ty" >(",a", (cons< ",ty" >::ptr)",b"))"))]
	[(car ,[Simple -> ls]) (wrap `("(",ls")->car"))]
	[(cdr ,[Simple -> ls]) (wrap `("(",ls")->cdr"))]
	[(assert-type (List ,t) (List:reverse ,[Simple -> ls]))
	 (wrap `("cons<",(Type t)">::reverse(",ls")"))]
	[(assert-type (List ,[Type -> ty]) (List:append ,[Simple -> ls1] ,[Simple -> ls2]))
	 (wrap `("cons<",ty">::append(",ls1", ",ls2")"))]
	[(List:ref (assert-type (List ,t) ,[Simple -> ls]) ,[Simple -> ind])
	 (wrap `("cons<",(Type t)">::ref(",ls", ",ind")"))]
	[(List:length (assert-type (List ,t) ,[Simple -> ls]))
	 (wrap `("cons<",(Type t)">::length(",ls")"))]
	[(List:make ,[Simple -> n] (assert-type ,t ,[Simple -> init]))
	 (wrap `("cons<",(Type t)">::make(",n", ",init")"))]
	;; TODO: nulls will be fixed up when remove-complex-opera is working properly.

;; Don't have types for nulls yet:
;	[(null_list ,[Type -> ty]) `("cons< "ty" >::ptr((cons< "ty" >)0)")]

	;; Safety net:
	[(,lp . ,_) (guard (memq lp '(cons car cdr append reverse toArray
					   List:ref List:length makeList ))) 
	 (error 'emit-C:Value "bad list prim: ~s" `(,lp . ,_))
	 ]

	;; ----------------------------------------
	;; Hash tables:

	;; We should have the proper type assertion on there after flattening the program.
	;; (Remove-complex-opera*)
	[(assert-type (HashTable ,k ,v) (hashtable ,[Simple -> n]))
	 (let ([hashtype (HashType k v)]
	       ;[eqfun ]
	       [k (Type k)]
	       [v (Type v)])
	   (wrap `(,(SharedPtrType hashtype)"(new ",hashtype"(",n"))")))]
	[(hashtable ,_) (error 'emitC:Value "hashtable not wrapped in proper assert-type: ~s"
			       `(hashtable ,_))]
	[(hashget ,[Simple -> ht] ,[Simple -> key])      (wrap `("(*",ht ")[",key"]"))]
	;; TEMP, HACK: NEED TO FIGURE OUT HOW TO CHECK FOR MEMBERSHIP OF A KEY!
	[(hashcontains ,[Simple -> ht] ,[Simple -> key]) (wrap `("(*",ht ")[",key"]"))]

	;; Generate equality comparison:
	[(equal? (assert-type ,t ,[Simple -> a]) ,[Simple -> b])
	 (let ([simple (wrap `("wsequal(",a", ",b")"))])
	   (match t
	     [Int          simple]
	     [Float        simple]
	     [String       simple]
	     ;; This is effectively physical equality:
	     ;; Requires that they have the same parents.
	     ;; Won't read the contents of two different Sigsegs...
	     ;; FIXME: Should consider fixing this.
	     [(Sigseg ,t)  simple]
	     
	     [(List ,t)    simple]
	     ;[(List ,[Type -> t]) `("cons<",t">::lsEqual(NULL_LIST, ",a", ",b")")]
	     
	     ;; We have generated a comparison op for each struct.
	     ;; UNFINISHED:
	   ;[(Struct ,name) `("eq",name"(",a", ",b")")]
	   [,_ (error 'emitC "no equality yet for type: ~s" t)])
	   )	 
	 ]
	
	;; Other prims fall through to here:
	[(,other ,[Simple -> rand*] ...)
	 (wrap `(,(SimplePrim other) "(" ,(insert-between ", " rand*) ")"))
	 ;`(,(SimplePrim prim) "(" ,(insert-between ", " rand*) ")")
	 ]
|#

	)
  )







































; ======================================================================
;; Statements.

#;

;; Blocks can be either in effect position (body of an iterate) or in
;; value position (the arms of an if).  Only in value position will
;; there be a name/type provided to store the resulting value.
;;
;; .param name : #f or Text representing the name of the variable in which 
;;               to store the resulting value
;; .param type : #f or Text representing the type to go with the name
;; .param b : the block of code in sexp form
(define (Block tenv)
  (lambda (name type b)
    (define (wrap x) (if name 
			 (list type " " name " = " x ";\n")
			 (list x ";\n")))
    (match b 

      
      [(for (,i ,[Simple -> st] ,[Simple -> en]) ,bod) (ASSERT not name)
       (let ([istr (Var i)])	   
	 (block `("for (int ",istr" = ",st"; ",istr" <= ",en"; ",istr"++)")
		((Block (tenv-extend tenv (list i) '(Int))) #f #f bod)))]
      [(break) (ASSERT not name) "break;\n"]

      ;; Must distinguish expression from statement context.
      [(if ,[Simple -> test] ,conseq ,altern)
       `(,(if name `(,type" ",name";\n") "")
	 "if (" ,test ") {\n"
	 ,(indent ((Block tenv) name "" conseq) "  ")
	 "} else {\n"
	 ,(indent ((Block tenv) name "" altern) "  ")
	 "}\n")]
      
      ;; HACK: cast to output type. FIXME FIXME
      [(emit ,vqueue ,[Simple -> val])
       (ASSERT not name)
       (match (recover-type vqueue tenv)
	 [(VQueue ,ty)  `("emit((",(Type ty)")" ,val ");\n")])]

      ;; This just does nothing in the c++ backend:
      [(gnuplot_array ,a) ""]

      [(,containerset! ,[Simple -> container] ,[Simple -> ind] ,[Simple -> val])
       (guard (memq containerset! '(Array:set hashset_BANG)))
       (ASSERT not name)
       `("(*",container ")[" ,ind "] = " ,val ";\n")]

      ;; Can't normalize-context this because of it's forall a.a return type:
      [(wserror ,str)
       (list (Prim `(wserror ,str) #f #f) ";\n")]

      [,oth (error 'emitC:Block "unhandled: ~s" oth)]
      )))



















#;

    ;; This processes an expression along the stream-processing "spine".
    ;; .param name   A string naming the variable that stores the current result.
    ;; .param type   Type of current result.
    ;; .param x      The query construct to process.
    ;; .returns 3 values: A new expression, a set of declarations, wsq declarations
(trace-define (Query name type x tenv)
    ;; Coercion:
    (if (symbol? name) (set! name (symbol->string name)))
  (trace-match Q x
    
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#|
	;; A constant, make global:
	[,e (guard (not (distributed-type? typ)))
	    (values ""
		    ((Value tenv) name (Type typ) e)
;		    `(("\n" ,(Type typ) ;(Type (recover-type e tenv))
;		       " ",name " = " ,((Value tenv) e) ";\n"))
                    '())]
	;; An alias:
	[,e (guard (symbol? e))
	    ;; UH, not an expression:
	    (values `(,(Type typ)" " ,name " = " ,(symbol->string e) ";\n")
                    ()
                    ())]
	
	[(audio ,[Simple -> channel] ,[Simple -> size] ,[Simple -> skip] ,[Simple -> rate])
	 ;; HMM, size seems to be FIXED:  FIXME	  
	 ;; (const char *path, int offset, int skip, double sample_rate, uint64_t cpuspeed)
	 (values 
	  ;; Rate was hardcoded at 24000*100
	  `("WSBox* ",name" =  new Rewindow<float>(",size", ",size");\n" 
	    "{ RawFileSource* tmp = new RawFileSource(\"/tmp/100.raw\", " ,channel ", 4, ",rate" * 50);\n"
	    "  ",name"->connect(tmp); }\n"
	    )
	  '()
          `())]

	;; TEMP: HACK!  Currently it just treats it as a marmot file.  THIS IS NOT RIGHT.
	[(audioFile ,[Simple -> file] ,[Simple -> size] ,[Simple -> overlap] ,[Simple -> rate])
	 ;; HMM, size seems to be FIXED:  FIXME	  
	 ;; (const char *path, int offset, int skip, double sample_rate, uint64_t cpuspeed)
	 (define new-class-name (Var (unique-name 'AudioFileSource)))
	 (values 
	  `("WSBox* ",name";\n"
	    "{ int size = ",size";\n"
	    "  ",name" =  new Rewindow<float>(size, size - ",overlap ");\n" 
	    "  RawFileSource* tmp = new RawFileSource(",file".c_str(), 0, 4, ",rate" * 50);\n"
	    "  ",name"->connect(tmp); }\n"
	    )
	  ;; TOFINISH: make new class:
	  '()
          `(("source \"",name"\" \"",new-class-name"\" \"query.so\" \n")))]

	;; Produces an instance of a generic dataFile reader.
	[(assert-type (Stream (Struct ,structname))
		      (__dataFile ,[Simple -> file] ,[Simple -> mode]
				  ,[Simple -> rate]
				  ,[Simple -> repeats] 
				  ;,[myExpr -> types]
				  ,_ignored
				  ))
	 (let* (
		[classname (symbol->string (unique-name 'WSDataFileSource))]
		[types (map cadr (cdr (ASSERT (assq structname struct-defs))))]
		[numstrings (length (filter (lambda (s) (eq? s 'String)) types))]
		[maintext 
		 (list
		  (block (list "class " classname " : public WSSource")
		 (list "public:\n"
	       (block (list classname "(wsstring_t path, wsstring_t mode, wsint_t repeats)")
		 `("_f = fopen(path.c_str(), binarymode ? \"rb\" : \"r\");\n"
		   "binarymode = (mode == string(\"binary\"));\n"
		   "if (_f == NULL) {\n"
		   "  chatter(LOG_CRIT, \"Unable to open data file %s: %m\", path.c_str());\n"
		   "  abort();\n"
		   "}\n"
		   "Launch();\n")
		 )
	       "\n  DEFINE_SOURCE_TYPE(struct "(symbol->string structname)");\n"
	       "\nprivate:\n"
	       "  FILE* _f;\n"
	       "  bool binarymode;\n"
 	       (block "void *run_thread()"
	       (list		
		(block "while (!Shutdown())"
		  `("struct ",(symbol->string structname)" tup;\n"
		    "// Cap of a 100 on length of read strings:\n"
		    ,(map (lambda (i) (format "char str~a[100];\n" i))
		          (iota 1 numstrings))
		    "int status;\n"
		    ,(block "if (!binarymode)"
			   `("status = fscanf(_f, \""
			     ,(insert-between " "
				 (map (lambda (ty)
					(match ty
					  [Float  "%f"] ;; Single precision floats
					  [Int    "%d"]
					  [Int16  "%hd"]
					  [String "%s"]
					  ))
				   types))
			     "\", "
			     ,(insert-between ", "
				(let loop ([n 1]
					   [flds (map symbol->string
						   (list-head standard-struct-field-names (length types)))]
					   [types types])
				  (if (null? types) '()
				      (match (car types)
					[,s (guard '(memq s '(Int Int16 Float)))
					 (cons `("&(tup.",(car flds)")") (loop n (cdr flds) (cdr types)))]
					[String (cons (format "str~a" n) (loop (add1 n) (cdr flds) (cdr types)))]
					))))
			     ");\n"))
		    ,(block "else"
			    ;; The binary format of tuples matches that in the file:
			    `("status = fread(&tup,sizeof(",(symbol->string structname)"),1,_f);\n"))

		    ;; Now with that nasty scanf finished we still
		    ;; have to put the strings into the right fields:
		    ,(map (lambda (n fld ty)
			    (if (eq? ty 'String)
				(format "tup.~a = str~a;\n" fld n)
				'()))
		       (iota 1 (length types))
		       (list-head standard-struct-field-names (length types))
		       types)

		    ,(block `("if (status != (binarymode ? 1 : ",(number->string (length types))"))")
		      '("chatter(LOG_WARNING, \"dataFile EOF encountered (%d).\", status);\n"
			"WSSched::stop();\n"
			"return NULL;\n"))
		    ;"t.time = (uint64_t)(time*1000000);\n"
		    "source_emit(tup);\n"
		    ))
		"return NULL;")
	       ))) ";")])
	   (DEBUGASSERT text? maintext)
	 ;; This is the code that actually builds a dataFile reader object:
	 (values 
	  `("WSSource* ",name" = new ",classname"(",file", ",mode", ",repeats");\n"
	    ;; Literal array:
	    ;;"{ ",(insert-between ", " (map symbol->string types)) " });\n"
	    )
	  (list maintext)
          `()))]

	;; [2006.11.18] This is for readng pipeline data currently.
	[(doubleFile ,[Simple -> file] ,[Simple -> size] ,[Simple -> overlap])
	 ;; CODE DUPLICATION:
	 (values 
	  `("WSBox* ",name";\n"
	    "{ size = ",size";\n"
	    "  ",name" =  new Rewindow<float>(size, size - ",overlap ");\n" 
	    "  RawFileSource* tmp = new PipeFileSource(\"",file"\", size, WSSched::findcpuspeed());\n"
	    "  ",name"->connect(tmp); }\n"
	    )
	  '()
          `()
          )]

	
	;; UNOPTIMIZED: should combine with the downstream iterate.
	;; Wire these all to our iterate.
	[(assert-type (Stream (Struct ,tupname)) (unionN ,inputs ...))
	 (ASSERT (not (null? inputs)))
	 (ASSERT (andmap symbol? inputs))
	 ]

	;; UNOPTIMIZED: should combine with the downstream iterate.
	;; Wire these all to our iterate.
	[(timer ,[Simple -> period])
	 (values 
	  `(" WSSource* ",name" = new WSBuiltins::Timer(",period");\n"  )
	  '()
	  '() ;;TODO, FIXME: wsq decls
	  )]

	[(assert-type ,t ,[q1 q2 q3]) (values q1 q2 q3)]

|#
	
	[,other (error 'wsquery->text:Query "unmatched query construct: ~s" other)]
	))















;================================================================================
;;; Other helpers:

  ;; Value types.
  (define (immediate-type? t)
    (match t
      [,nt (guard (memq nt num-types)) #t]
      [Char #t]
      [(Struct ,name) #t]

      ;; FIXME: Not sure about this:
      ;[(List ,t) ]

      [,_ #f]
      ))


#;
    (define PolyConst 
      (lambda (datum ty)
	(match (vector datum ty)
	  (DEBUGASSERT (not (polymorphic-type? ty)))
	  [#(() (List ,t)) 	   
					;"NULL_LIST"
					;`("(cons<",(Type t)">::ptr)NULL_LIST")
					;`("(cons<",(Type t)">::null_ls)")
	   `("boost::shared_ptr< cons< ",(Type t)" > >((cons< ",(Type t)" >*) 0)")
	   ]
	  [#(nullseg ,t) "WSNULLSEG"]
					;[#(Array:null (Array ,t)) `("boost::shared_ptr< vector< ",(Type t)" > >(new ",(Type t)"[0])")]
	  [#(Array:null (Array ,t)) `("boost::shared_ptr< vector< ",(Type t)" > >(new vector< ",(Type t)" >(0))")]	  
	  )))
#;
    (define Simple
      (lambda (x)
	(match x 
          [(tuple) "((wsunit_t)0)"]
	  [(assert-type ,t '())  (wrap (PolyConst '() t))]
	  ['() (error 'Simple "null list without type annotation")]
	  [(quote ,c) (Const #f #f c)]
	  [,v (guard (symbol? v)) (Var v)]
	  [(assert-type ,_ ,[x]) x]
	  [,else (error 'Simple "not simple expression: ~s" x)])))


;================================================================================
;; Primitive calls:

#;
(define (Prim expr name type)
  (define (wrap x) (list type " " name " = " x ";\n"))
  ;; This is for primitives that correspond exactly to exactly one C call.
  (define (SimplePrim var)
    (define (fromlib v) (format "WSPrim::~a" v))
    (define (mangle v) (mangle-name (symbol->string v)))
    ;; Handle special cases here.
    (case var
      [(not)         
       "!" ;(mangle 'wsnot)
       ] ; The name "not" makes g++ unhappy.
      ;; These are the same as their C++ names:
      [(cos sin tan acos asin atan max min) 
       (symbol->string var)]
      [(absF absI absI16)       "abs"]
      [(roundF)                 "round"]
      [(sqrtI sqrtF)            "sqrt"]
      [(sqrtC)                  "csqrt"]
      ;; This is the "default"; find it in WSPrim:: class
      [(m_invert string-append 
	width start end joinsegs subseg toSigseg
	;wserror ;generic_hash 
	fft
	)
       (fromlib (mangle var))]
      [else (error 'emitC:Prim "primitive not specifically handled: ~s" var)]
      ))

  (match expr
    ;; First we handle "open coded" primitives and special cases:

    [(Mutable:ref ,[Simple -> x]) (wrap x)]
    [(deref ,[Simple -> x]) (wrap x)]

    ;[(^: ,[Simple -> x] ,[Simple -> y]) (wrap `("pow(",x", ",y")"))]
    ;[(^: ,[Simple -> x] ,[Simple -> y]) (wrap `("(wscomplex_t)pow((double)",x", (double)",y")"))]
    ;[(^: ,[Simple -> x] ,[Simple -> y]) (wrap `("(wsfloat_t)pow((double __complex__)",x", (double __complex__)",y")"))]
    ;; INEFFICIENT:
    [(^: ,[Simple -> x] ,[Simple -> y]) 
     (let ([tmp (Var (unique-name 'tmp))])
       `("complex<float> ",tmp" = pow((complex<float>)",x", (complex<float>)",y");\n"
       ,(wrap `("real(",tmp") + (imag(",tmp") * 1.0fi)"))
       ))]
    
    ;; TODO: tupref, exponentiation 
    [(,infix_prim ,[Simple -> left] ,[Simple -> right])
     (guard (memq infix_prim '(;+ - * /
			       +. -. *. /. 
				  +_ *_ -_ /_
				  +: *: -: /:
				  +I16 *I16 -I16 /I16
				  < > <= >= =
				  ^_ ^. ^: ^I16
				  )))
     (let ([cname (case infix_prim
		    [(=) "=="]
		    [(;+ * - / 
		      < > <= >=) infix_prim]
		    [(+. *. -. /.
			 +_ *_ -_ /_
			 +: *: -: /:
			 ^_ ^. 
			 ) ;; Chop off the extra character.
		     (substring (symbol->string infix_prim) 0 1)]
		    [(+I16 -I16 *I16 /I16 ^I16)
		     (substring (symbol->string infix_prim) 0 1)]
		    )])
       (wrap `("(" ,left ,(format " ~a " cname) ,right ")")))]

	;[(realpart ,[v]) `("(" ,v ".real)")]
	;[(imagpart ,[v]) `("(" ,v ".imag)")]
	[(imagpart ,[Simple -> v])   (wrap `("__imag__ " ,v))]
	[(realpart       ,[Simple -> v])   (wrap `("__real__ " ,v))]
	[(complexToFloat ,[Simple -> v])   (wrap `("__real__ " ,v))]
	[(complexToInt   ,[Simple -> v])   (wrap `("(wsint_t) __real__ " ,v))]
	[(complexToInt16 ,[Simple -> v])   (wrap `("(wsint16_t) __real__ " ,v))]

	[(absC ,[Simple -> c]) (wrap `("abs((complex<float>)",c")"))]

	[(intToInt16     ,[Simple -> e]) (wrap `("(wsint16_t)",e))]
	[(floatToInt16   ,[Simple -> e]) (wrap `("(wsint16_t)",e))]

	[(floatToInt   ,[Simple -> e]) (wrap `("(wsint_t)",e))]
	[(int16ToInt   ,[Simple -> e]) (wrap `("(wsint_t)",e))]
	
	[(intToFloat     ,[Simple -> e]) (wrap `("(wsfloat_t)",e))]
	[(int16ToFloat   ,[Simple -> e]) (wrap `("(wsfloat_t)",e))]

	;[(complexToInt16 ,e)   (wrap `("(wsint16_t)",(Prim `(complexToFloat ,e) #f "")))]
	;[(complexToInt ,e)     (wrap `("(wsint_t)"  ,(Prim `(complexToFloat ,e) #f "")))]
	;[(complexToFloat ,e)   (Prim `(realpart ,e) name type)]
	[(,ToComplex ,[Simple -> e])
	 (guard (memq ToComplex 
		      '(int16ToComplex intToComplex floatToComplex)))
    	 (wrap `("((wscomplex_t)(",e" + 0.0fi))"))]

	[(stringToInt ,[Simple -> e]) 
	 (let ([tmp (Var (unique-name 'tmp))])
	   `("wsint_t ",tmp";\n"
	     "sscanf(",e".c_str(), \"%d\", &",tmp");\n"
	     ,(wrap tmp)))]
	[(stringToFloat ,[Simple -> e]) 
	 (let ([tmp (Var (unique-name 'tmp))])
	   `("wsfloat_t ",tmp";\n"
	     "sscanf(",e".c_str(), \"%f\", &",tmp");\n"
	     ,(wrap tmp)))]
	[(stringToComplex ,[Simple -> e]) 
	 (let ([tmp1 (Var (unique-name 'tmp))]
	       [tmp2 (Var (unique-name 'tmp))])
	   `("wsfloat_t ",tmp1";\n"
	     "wsfloat_t ",tmp2";\n"
	     ;"printf(\"STRING %s\\n\", ",e".c_str());\n"
	     "sscanf(",e".c_str(), \"%f+%fi\", &",tmp1", &",tmp2");\n"
	     ,(wrap `(,tmp1"+(",tmp2"*1.0fi)"))))]

	[(show (assert-type ,t ,[Simple -> e])) (wrap (EmitShow e t))]
	[(show ,_) (error 'emit-c:Value "show should have a type-assertion around its argument: ~s" _)]

	[(toArray (assert-type (Sigseg ,t) ,sigseg))
	 (let ([tmp (Var (unique-name 'tmp))]
	       [tmp2 (Var (unique-name 'tmp))]
	       [len (Var (unique-name 'len))]
	       [ss (Simple sigseg)]
	       [tt (Type t)])
	   `("boost::shared_ptr< vector<",tt"> >",tmp"(new vector<",tt">(",ss".length()));\n"
	     "int len = ",ss".length();\n"
	     "for(int i=0; i<len; i++) {\n"
	     "  ",(Prim `(seg-get (assert-type (Sigseg ,t) ,sigseg) i) tmp2 tt)
	     "  (*",tmp")[i] = ",tmp2";\n"
	     "}\n"
	     ,(wrap tmp)
	     ))]

	[(wserror ,[Simple -> str])
	 ;; Don't do anything with the return value.
	 `(,(if name `(,type" ",name";\n") "")
	   "WSPrim::wserror(",str");\n")]

	;; This is inefficient.  Only want to call getDirect once!
	;; Can't trust the C-compiler to know it's effect free and do CSE.
	[(seg-get (assert-type (Sigseg ,[Type -> ty]) ,[Simple -> seg]) ,[Simple -> ind])
	 ;`("(" ,seg ".getDirect())[" ,ind  "]")
	 (wrap `("(*((",ty"*)(*(" ,seg ".index_i(" ,ind  ")))))"))]
	[(seg-get ,foo ...)
	 (error 'emit-c:Value "seg-get without or with badtype annotation: ~s" 
		`(seg-get ,@foo))]
	[(timebase ,[Simple -> seg]) (wrap `("(" ,seg ".getTimebase())"))]
	
	;; Need to use type environment to find out what alpha is.
	;; We store the length in the first element.
	[(newarr ,[Simple -> int] ,alpha)
	 ;(recover-type )
	 "newarr_UNFINISHED"]
	
	;[(Array:ref ,[arr] ,[ind]) `(,arr "[" ,ind "]")]
	[(Array:ref ,[Simple -> arr] ,[Simple -> ind]) (wrap `("(*",arr ")[" ,ind "]"))]
	[(Array:make ,[Simple -> n] ,[Simple -> x])   (wrap `("makeArray(",n", ",x")"))]
	;; This version just doesn't initialize:
	[(assert-type (Array ,[Type -> ty]) (Array:makeUNSAFE ,[Simple -> n]))
	 (wrap `("boost::shared_ptr< vector< ",ty
		 " > >(new vector< ",ty" >(",n "))"))]

	[(Array:length ,[Simple -> arr])                   (wrap `("(wsint_t)(",arr"->size())"))]

	[(Array:set ,x ...)
	 (error 'emitC:Value "Array:set in Value context: ~s" `(Array:set ,x ...))]
	[(begin ,stmts ...)
	 (error 'emitC:Value "begin in Value context: ~s" `(begin ,stmts ...))]

	;; Later we'll clean it up so contexts are normalized:
	;[(set! ,[Var -> v] ,[(Value tenv) -> rhs]) `(,v " = " ,rhs ";\n")]

       	;; ----------------------------------------
	;; Lists:
	;; These primitives are tricky because of the template magic:

	[(assert-type (List ,[Type -> ty]) (cons ,[Simple -> a] ,[Simple -> b]))
	 (wrap `("cons< ",ty" >::ptr(new cons< ",ty" >(",a", (cons< ",ty" >::ptr)",b"))"))]
	[(car ,[Simple -> ls]) (wrap `("(",ls")->car"))]
	[(cdr ,[Simple -> ls]) (wrap `("(",ls")->cdr"))]
	[(assert-type (List ,t) (List:reverse ,[Simple -> ls]))
	 (wrap `("cons<",(Type t)">::reverse(",ls")"))]
	[(assert-type (List ,[Type -> ty]) (List:append ,[Simple -> ls1] ,[Simple -> ls2]))
	 (wrap `("cons<",ty">::append(",ls1", ",ls2")"))]
	[(List:ref (assert-type (List ,t) ,[Simple -> ls]) ,[Simple -> ind])
	 (wrap `("cons<",(Type t)">::ref(",ls", ",ind")"))]
	[(List:length (assert-type (List ,t) ,[Simple -> ls]))
	 (wrap `("cons<",(Type t)">::length(",ls")"))]
	[(List:make ,[Simple -> n] (assert-type ,t ,[Simple -> init]))
	 (wrap `("cons<",(Type t)">::make(",n", ",init")"))]
	;; TODO: nulls will be fixed up when remove-complex-opera is working properly.

;; Don't have types for nulls yet:
;	[(null_list ,[Type -> ty]) `("cons< "ty" >::ptr((cons< "ty" >)0)")]

	;; Safety net:
	[(,lp . ,_) (guard (memq lp '(cons car cdr append reverse toArray
					   List:ref List:length makeList ))) 
	 (error 'emit-C:Value "bad list prim: ~s" `(,lp . ,_))
	 ]

	;; ----------------------------------------
	;; Hash tables:

	;; We should have the proper type assertion on there after flattening the program.
	;; (Remove-complex-opera*)
	[(assert-type (HashTable ,k ,v) (hashtable ,[Simple -> n]))
	 (let ([hashtype (HashType k v)]
	       ;[eqfun ]
	       [k (Type k)]
	       [v (Type v)])
	   (wrap `(,(SharedPtrType hashtype)"(new ",hashtype"(",n"))")))]
	[(hashtable ,_) (error 'emitC:Value "hashtable not wrapped in proper assert-type: ~s"
			       `(hashtable ,_))]
	[(hashget ,[Simple -> ht] ,[Simple -> key])      (wrap `("(*",ht ")[",key"]"))]
	;; TEMP, HACK: NEED TO FIGURE OUT HOW TO CHECK FOR MEMBERSHIP OF A KEY!
	[(hashcontains ,[Simple -> ht] ,[Simple -> key]) (wrap `("(*",ht ")[",key"]"))]

	;; Generate equality comparison:
	[(equal? (assert-type ,t ,[Simple -> a]) ,[Simple -> b])
	 (let ([simple (wrap `("wsequal(",a", ",b")"))])
	   (match t
	     [Int          simple]
	     [Float        simple]
	     [String       simple]
	     ;; This is effectively physical equality:
	     ;; Requires that they have the same parents.
	     ;; Won't read the contents of two different Sigsegs...
	     ;; FIXME: Should consider fixing this.
	     [(Sigseg ,t)  simple]
	     
	     [(List ,t)    simple]
	     ;[(List ,[Type -> t]) `("cons<",t">::lsEqual(NULL_LIST, ",a", ",b")")]
	     
	     ;; We have generated a comparison op for each struct.
	     ;; UNFINISHED:
	   ;[(Struct ,name) `("eq",name"(",a", ",b")")]
	   [,_ (error 'emitC "no equality yet for type: ~s" t)])
	   )	 
	 ]
	
	;; Other prims fall through to here:
	[(,other ,[Simple -> rand*] ...)
	 (wrap `(,(SimplePrim other) "(" ,(insert-between ", " rand*) ")"))
	 ;`(,(SimplePrim prim) "(" ,(insert-between ", " rand*) ")")
	 ])
  )




;======================================================================
;;; Bits of boilerplate.

;(define boilerplate_premain )
;(define (boilerplate_postmain return_name return_type) )

;;================================================================================

  (define-testing these-tests
    `(
      [3 3]

#;
      ;; This makes sure we can generate *something* for all the primitives.
      ,@(map
	 (match-lambda ([,prim ,argtypes ,rettype])
	   `[(,Prim '(,prim ,@(map (lambda (_) (unique-name 'x)) argtypes)) "foo" "") unspecified]
	   )
	 ;; Quadratic:
	 (let ([exceptions 
		(append 
		 '(;; These are obsolete:
		   eq? locdiff nodeid sense even? odd? 
		   ;; These weren't really primitives:    
		   tuple tupref ref deref
		   ;; These were desugared or reduced to other primitives:
		   or and dataFile show-and-string-append Array:toList
		   ;; These were resolved into the w/namespace versions:
		   head tail map append fold
		   List:head List:tail 
		   
		   ;; These have a special syntax, requiring an assert-type or whatnot:
		   cons car cdr null? hashtable prim_window 
		   List:ref List:append List:reverse List:length List:make 
		   Array:makeUNSAFE
		   
		   equal? print show seg-get toArray

		   ;; TODO, FIXME: These I just haven't gotten to yet:
		   ENSBoxAudio
		   List:assoc List:assoc_update
		   hashrem hashset ;; pure versions
		   Array:map Array:fold
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
(define test-wavescript_emit-caml test-this)





;; This pass makes the forward-links explicit in the stream graph.
;; 
;; The output of this pass is no longer an expression in the original sense.
;; It's four things:
;;  1) A set of constant bindings (which may, in the future, include functions).
;;  2) A set of Sources: [v ty (prim const ...) (downstream-links ...)]
;;  3) A set of Iterates: [v ty fundef upstream (downstream-links ...)]
;;  4) A designated name of a source/iterate that returns to BASE<-
(define-pass explicit-stream-wiring 
    (define (Expr x aliases)
      (define (dealias v)
	(let ([entry (assq v aliases)])
	  (if entry (dealias (cadr entry)) v)))
      (unless (null? aliases) (printf "ALIASES: ~s\n" aliases))
      (match x
	;; Operators:
	[(let ([,v ,ty (iterate ,f ,[dealias -> sig])]) ,[bod])
	 (cons `[,sig -> ,v ,ty ,f] bod)]
	;; Sources:
	[(let ([,v (Stream ,ty) (,prim ,rands* ...)]) ,[bod])
	 (guard (assq prim wavescript-stream-primitives))
	 (cons `[-> ,v (Stream ,ty) (,prim . ,rands*)] bod)]
	;; Alias:
	[(let ([,v1 (Stream ,ty) ,v2]) ,bod) (guard symbol? v2)
	 (Expr bod (cons (list v1 v2) aliases))]
	;; Constants:
	[(let ([,v ,ty ,rhs]) ,[bod])
	 (ASSERT (lambda (t) (not (deep-assq 'Stream t))) ty)
	 (cons `[CONST ,v ,ty ,rhs] bod)]
	;; Sink:
	[,v (guard (symbol? v)) `([BASE ,(dealias v)])]
	[,oth (error 'explicit-stream-wiring "unmatched query construct: ~s" oth)]
	))
  (define (decl->upstream d)
    (match d
      [(-> . ,_)            #f]
      [(,src -> ,dest . ,_) src]
      [(BASE  ,src)         src]
      [(CONST ,_)           #f]))
  (define (decl->name d) 
    (match d
      [(-> ,v . ,_)         v]
      [(,src -> ,dest . ,_) dest]
      [(BASE  ,src)         #f]
      [(CONST ,_)           #f]))
  (define (gather-refs v decls)
    (match decls
      [() ()]
      [((-> . ,_) . ,[rest])     rest]
      [((,src -> ,dest . ,_) . ,[rest]) 
       (guard (eq? v src))
       (cons dest rest)]
      [((BASE ,src) . ,[rest]) (guard (eq? v src))  (cons 'BASE rest)]
      [(,_ . ,[rest]) rest]))
  [Program 
   (lambda (p _)
     (match p
       [(,lang '(program ,e ,t))
	(let ([decls (Expr e '())])
	  (define c*
	    (map cdr(filter (lambda (x) (eq? (car x) 'CONST)) decls)))
	  ;; Here we gather all the forward wirings. Quadratic:
	  (define src*
	    (apply append 
		   (map (lambda (d)
			  (match d
			    [(-> ,v ,ty ,app) `((,v ,ty ,app ,(gather-refs v decls)))]
			    [,_ ()]))
		     decls)))
	  (define iter*
	    (apply append 
		   (map (lambda (d)
			  (match d
			    [(,src -> ,v ,ty ,f) 
			     `((,v ,ty ,f ,src ,(gather-refs v decls)))]
			    [,_ ()]))
		     decls)))
	  (define base (cadr (assq 'BASE decls)))
	  `(explicit-stream-wiring-language
	    '(graph (const . ,c*)
		    (sources . ,src*)
		    (iterates . ,iter*)
		    (sink ,base))))]))]
  )


) ;; End Module


;;================================================================================

