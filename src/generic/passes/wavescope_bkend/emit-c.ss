

;;;; .title WaveScript EmitC
;;;; .author Ryan Newton

;;;; This uses the generic C-generation libary (c_generator.ss) and
;;;; provides procedures for producing a C-file following WaveScript
;;;; conventions, that is, a ".wsc" file.

;;;; A note on type conversions:
;;;; These are roughly the correspondences:
;;;;   Stream 'a -> WSBox* 
;;;;   Sigseg t -> Sigseg<t>
;;;; 


(module emit-c mzscheme 
  (require  "../../../plt/common.ss"
	    (all-except "nominalize-types.ss" test-this these-tests)
	    "../../compiler_components/c_generator.ss" )
  (provide ;WSBox wscode->text
	   wsquery->text
	   
	   testme	   testme2	   testme0
	   test-this  test-wavescript_emit-c)
  (chezprovide )  
  (chezimports (except helpers test-this these-tests)
	       )
  
;======================================================================
;;                       <WaveScript C generation>
;======================================================================

;; Should I use this sort of name mangling?  The numbers from unique
;; names will keep things unique...  Currently no, we just enforce
;; C-names from the start for WaveScript.
#|
(define acceptable_chars 
  '(#\_ 
    #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z 
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
;; String -> String
(define (mangle-name str)
  (list->string (filter (lambda (c) (memq c acceptable_chars))
		  (string->list str))))
|#


;; This insures no recursion in a given letrec.  That is, it makes
;; sure it's not really a letrec.
(define (no-recursion! binds)
  ;; TODO FINISHME!!! FIXME!
  #t)


;; If the type needs a specialized hashfun, returns its name,
;; otherwise returns #f.
(define (HashType k v)
  (define hashfun
    (match k
      [,s (guard (symbol? s) (memq s '(Int Float))) #f]
      [String "boost::hash<string>"]
      [(Struct ,name)	`("hash",(symbol->string name))]
      [,_ (error 'emitC:make-hashfun "don't know how to hash type: ~s" k)]
      ))
  `("hash_map< ",(Type k)", ",(Type v),(if hashfun `(", ",hashfun) '())" >"))

(define (SharedPtrType t) `("boost::shared_ptr< ",t" >"))

;; This should give you an idea of the mapping between types:
(define (Type t)
  (match t
    [Int     "wsint_t"]
    [Float   "wsfloat_t"]
    [Complex "wscomplex_t"]

    [String "wsstring_t"] ;; Not boosted.

    ;; Went back and forth on whether this should be a pointer:
    [(Sigseg ,[t]) `("RawSeg")]
    [(Signal ,[t]) `("WSBox*")]

    [(Array ,[t]) `(,t "[]")]
    [(Struct ,name) (symbol->string name)]
    
    ;; HACK HACK FIXME:
    ;; This is for null lists.
    [(List ',_) `("cons<int>::ptr")]
    ;; Boosted cons cells:
    [(List ,[t]) `("cons< ",t" >::ptr")]

    [(Hashset ',_ ,__) (error 'emitC "hash table with typevar" )]
    [(Hashset ,_ ',__) (error 'emitC "hash table with typevar" )]
					;[(HashTable ,[kt] ,[vt]) `("boost::shared_ptr< hash_map< ",kt", ",vt" > >")]
    [(HashTable ,kt ,vt) (SharedPtrType (HashType kt vt))]
    
    [,other (error 'emit-c:Type "Not handled yet.. ~s" other)]))

;======================================================================

;; This is the only entry point to the file.  A complete query can
;; be transformed into a complete query file.
;; .param prog  The wsquery to process.
;; .param mode (Optional) 'static or 'dynamic, indicating whether to
;;             produce a main() function, or code for a .so & .wsq.
;;             Default is 'static.
(define wsquery->text
  (lambda (prog . mode)
    (define static-linkage 
      (match mode 
	[()        #t]	
	[(static)  #t]
	[(dynamic) #f]
	[,else (error 'wsquery->text "bad additional arguments: ~s" mode)]))
    (match prog
      [(,lang (quote (program ,expr (struct-defs . ,struct-defs) ,typ)))
       
    ;; This processes an expression along the stream-processing "spine".
    ;; .param name   A string naming the variable that stores the current result.
    ;; .param type   Type of current result.
    ;; .param x      The query construct to process.
    ;; .returns A new expression and a set of declarations.
    (define (Query name typ x tenv)
	(define myExpr (Expr tenv))
      ;; Coercion:
      (if (symbol? name) (set! name (symbol->string name)))
      (match x

	;; A constant, make global:
	[,e (guard (not (distributed-type? typ))) 
	    (values ""
		    `(("\n" ,(Type typ) ;(Type (recover-type e tenv))
		       " ",name " = " ,(myExpr e) ";\n")))]

	;; An alias:
	[,e (guard (symbol? e))
	    ;; UH, not an expression:
	    (values `(,(Type typ)" " ,name " = " ,(symbol->string e) ";\n") ())]

	;; Forbidding recursion for now (even though this says 'letrec').
	[(,letsym ,binds ,bod) (guard (memq letsym '(let letrec)))
	 ;(ASSERT (symbol? body))
	 (ASSERT (no-recursion! binds))
	 (mvlet ([(stmts decls)
		  (match binds 
		    [([,lhs* ,ty* ,rhs*] ...)
		     (let ([newenv (tenv-extend tenv lhs* ty*)])
		       (mvlet ([(bodstmts boddecls) (Query name typ bod newenv)])
		       (let loop ([lhs* (map Var lhs*)] [ty* ty*] [rhs* rhs*]
				  [stmtacc '()] [declacc '()])
			 ;; Should really use the Text ADT here:
			 (if (null? lhs*)
			     ;; Now we do the body.
			     (values (list (reverse! stmtacc) bodstmts)
				     (append (reverse! declacc) boddecls))
			     (mvlet ([(stmt decl) (Query (car lhs*) (car ty*) (car rhs*) newenv)])
			       (loop (cdr lhs*) (cdr ty*) (cdr rhs*)
				     (cons stmt stmtacc) (cons decl declacc)))))))]
		    [,other (error 'wsquery->text "Bad letrec binds: ~s" other)])])

	   (values stmts decls))]
		       
	[(assert-type (Signal (Sigseg ,[Type -> ty])) 
		      (window ,sig ,[myExpr -> size]))
	 (ASSERT symbol? sig)
	 (values `("WSBox* ",name" = new WSBuiltins::Window(",size", sizeof(",ty"));\n"
		   ,name"->connect(",(symbol->string sig)");\n")
		 '())]

	[(audio ,[myExpr -> channel] ,[myExpr -> size] ,[myExpr -> skip])
	 ;; HMM, size seems to be FIXED:  FIXME	  
	 ;; (const char *path, int offset, int skip, double sample_rate, uint64_t cpuspeed)
	 (values 
	  `("WSBox* ",name" =  new Rewindow<float>(",size", ",size");\n" 
	    "{ RawFileSource* tmp = new RawFileSource(\"/tmp/100.raw\", " ,channel ", 4, 24000*100);\n"
	    "  ",name"->connect(tmp); }\n"
	    )
	  '())
	 ]

	;; TEMP: HACK!  Currently it just treats it as a marmot file.  THIS IS NOT RIGHT.
	[(audioFile ,[myExpr -> file] ,[myExpr -> size] ,[myExpr -> overlap])
	 ;; HMM, size seems to be FIXED:  FIXME	  
	 ;; (const char *path, int offset, int skip, double sample_rate, uint64_t cpuspeed)
	 (values 
	  `("WSBox* ",name";\n"
	    "{ int size = ",size";\n"
	    "  ",name" =  new Rewindow<float>(size, size - ",overlap ");\n" 
	    "  RawFileSource* tmp = new RawFileSource(\"/tmp/100.raw\", 0, 4, 24000*100);\n"
	    "  ",name"->connect(tmp); }\n"
	    )
	  '())]

	;; Produces an instance of a generic dataFile reader.
	[(assert-type (Signal (Struct ,structname))
		      (__dataFile ,[myExpr -> file] ,[myExpr -> mode]
				  ,[myExpr -> repeats] ;,[myExpr -> types]
				  ,_ignored
				  ))
	 (let* ([classname (symbol->string (unique-name 'WSDataFileSource))]
		[types (map cadr (cdr (ASSERT (assq structname struct-defs))))]
		[numstrings (length (filter (lambda (s) (eq? s 'String)) types))]
		[maintext 
		 (list
		  (block (list "class " classname " : public WSSource")
		 (list "public:\n"
	       (block (list classname "(wsstring_t path, wsstring_t mode, wsint_t repeats)")
		 `("_f = fopen(path.c_str(), \"r\");\n"
		   "if (_f == NULL) {\n"
		   "  chatter(LOG_CRIT, \"Unable to open data file %s: %m\", path.c_str());\n"
		   "  abort();\n"
		   "}\n"
		   "Launch();\n")
		)
	       "\n  DEFINE_SOURCE_TYPE(struct "(symbol->string structname)");\n"
	       "\nprivate:\n"
	       "  FILE* _f;\n"
 	       (block "void *run_thread()"
	       (list		
		(block "while (!Shutdown())"
		  `("struct ",(symbol->string structname)" tup;\n"
		    "// Cap of a 100 on length of read strings:\n"
		    ,(map (lambda (i) (format "char str~a[100];\n" i))
		          (iota 1 numstrings))
		    "int status = fscanf(_f, \""
		    ,(insert-between " "
		      (map (lambda (ty)
			     (match ty
                               [Float  "%f"] ;; Single precision floats
                               [Int    "%d"]
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
			      [Float  (cons `("&(tup.",(car flds)")") (loop n (cdr flds) (cdr types)))]
			      [Int    (cons `("&(tup.",(car flds)")") (loop n (cdr flds) (cdr types)))]
			      [String (cons (format "str~a" n)        (loop (add1 n) (cdr flds) (cdr types)))]
			      )
			    )
			))
		    ");\n"
		    ;; Now with that nasty scanf finished we still
		    ;; have to put the strings into the right fields:
		    ,(map (lambda (n fld ty)
			    (if (eq? ty 'String)
				(format "tup.~a = str~a;\n" fld n)
				'()))
		       (iota 1 (length types))
		       (list-head standard-struct-field-names (length types))
		       types)

		    ,(block `("if (status != ",(number->string (length types))")")
		      '("chatter(LOG_WARNING, \"dataFile EOF encountered (status=%d).\", status);\n"
			"WSSched::stop();\n"
			"return NULL;\n"))
		    ;"t.time = (uint64_t)(time*1000000);\n"
		    "source_emit(tup);\n"
		    ))
		"return NULL;")
	       ))) ";")])
	   (DEBUGASSERT text? maintext)
	 (values 
	  `("WSSource* ",name" = new ",classname"(",file", ",mode", ",repeats");\n"
	    ;; Literal array:
	    ;;"{ ",(insert-between ", " (map symbol->string types)) " });\n"
	    )
	  (list maintext)))]

	;; [2006.11.18] This is for readng pipeline data currently.
	[(doubleFile ,[myExpr -> file] ,[myExpr -> size] ,[myExpr -> overlap])
	 ;; CODE DUPLICATION:
	 `("WSBox* ",name";\n"
	   "{ size = ",size";\n"
	   "  ",name" =  new Rewindow<float>(size, size - ",overlap ");\n" 
	   "  RawFileSource* tmp = new PipeFileSource(\"",file"\", size, WSSched::findcpuspeed());\n"
	   "  ",name"->connect(tmp); }\n"
	   )
	 ]

	[(iterate ,let-or-lambda ,sig)
	 ;; Program better have been flattened!!:
	 (ASSERT (symbol? sig))	  
	 (let* ([parent sig] ;(if (symbol? sig) sig (unique-name 'sig))]
		[class_name `("Iter_" ,name)]
		;; First we produce a line of text to construct the box:
		[ourstmts `(  "WSBox* ",name" = new ",class_name "(" ");\n" 
			      ,name"->connect(" ,(Var parent) ");\n")]
		;; Then we produce the declaration for the box itself:
		[ourdecls 
		 (mvlet ([(iterator+vars stateinit) (wscode->text let-or-lambda name tenv)])
		   (list (WSBox class_name 
				(match typ
				  [(Signal ,t) (Type t)]
				  [,other (error 'emitC:Query "expected iterate to have signal output type! ~s" other)])
				;; Constructor:
				(block `(,class_name "()")  stateinit)
				;; This produces a function declaration for iterate:				
				iterator+vars)))]) 
	   ;(if (symbol? sig) 
	       (values ourstmts ourdecls)
	   )]

	
	;; This is purely hackish... should use the zip library function.	
	[(zip2 ,s1 ,s2)
	 (ASSERT symbol? s1)
	 (ASSERT symbol? s2)
	 (let* ([t1 (recover-type s1 tenv)]
		[t2 (recover-type s2 tenv)]
		[ty (format " ~a, ~a " (Type t1) (Type t2))])
	   `(" Zip2<",ty"> ",name" = Zip2<",ty">();"
	     " ",name".connect(",(symbol->string s1)"); "
	     " ",name".connect(",(symbol->string s2)"); " )	   
	   )
					;  /* zip2 */
					;  Zip2<SigSeg<float>,float> z=Zip2<SigSeg<float>,float>();
					;  z.connect(&rw1);
					;  z.connect(&ms);	 
	 ]


	;; This WON'T be implemented.  Doesn't really make sense.
	[(unionList ,structtype ,ls)	 
	 (error 'emit-C "unionList not implemented yet: ~s" 
		`(unionList ,structtype ,ls))
	 ]

	;; UNOPTIMIZED: should combine with the downstream iterate.
	;; Wire these all to our iterate.
	[(unionN ,inputs ...)
	 (ASSERT (not (null? inputs)))
	 (ASSERT (andmap symbol? inputs))
	 (let ([ty (recover-type (car inputs) tenv)])	   
	   `(" WSBuiltins::UnionN<",ty"> ",name" = WSBuiltins::UnionN<",ty">();"
	     ,(map (lambda (in)
		     `(" ",name".connect(",(symbol->string in)"); "))
		inputs)
	     ))]	
	
	[,other (error 'wsquery->text:Query "unmatched query construct: ~s" other)]
	)) ;; End Query

; ======================================================================
;;; Helper functions for handling different program contexts:
        
    (define (Var var)
      (ASSERT (symbol? var))
      ;; This is the place to do any name mangling.  I'm not currently doing any for WS.
      (symbol->string var))
    (define (PrimName var)
      ;; Handle special cases here.
      (define (special v)
	(case v
	  [(not) 'wsnot] ; The name "not" makes g++ unhappy.
	  [else v]))
      (format "WSPrim::~a" (mangle-name (symbol->string (special var)))))
      ;(symbol->string var))
    (define (FunName var)
      (format "WSFunLib::~a" var))
      ;(symbol->string var))

    (define Const
      (case-lambda
	[(datum) 
	 ;; Should also make sure it's 32 bit or whatnot:
	 (cond
	  [(eq? datum #t) "TRUE"]
	  [(eq? datum #f) "FALSE"]       
	  [(string? datum) (format "string(~s)" datum)]
	  [(or (integer? datum) (flonum? datum))  (number->string datum)]
	  [else (error 'emitC:Expr "not a C-compatible literal: ~s" datum)])]
	[(datum ty)
	 (match (vector datum ty)
	   [#(() (List ,t)) 	   
	    "NULL_LIST"
	    ;`("(cons<",(Type t)">::ptr)NULL_LIST")
	    ;`("(cons<",(Type t)">::null)")
	    ]
	   [#(nullseg ,t) "WSNULLSEG"]
	   [#(nullarr ,t) "WSNULL"]
	   )]))

; ======================================================================
;; Statements.

    (define (Stmt tenv)
      (lambda (st)
	(define myExpr (Expr tenv))
	(define result 
	  (match st

	;; These immediates generate no code in Stmt position.
	[,v (guard (symbol? v)) ""]
	[(quote ,c) ""]


	;; Must distinguish expression from statement context.
	[(if ,[myExpr -> test] ,[conseq] ,[altern])
	 `("if (" ,test ") {\n"
	   ,(indent conseq "  ")
	   "} else {\n"
	   ,(indent altern "  ")
	   "}\n")]

	;; Not allowed in expression position currently:
	[(let ([,[Var -> v] ,[Type -> t] ,[myExpr -> rhs]]) ,[body])
	 `(,t " " ,v " = " ,rhs ";\n" ,body)]

	;; TEMP:
	[(,letsym () ,body)  (guard (memq letsym '(let letrec)))
;	 (inspect `(HMM ,body))
;	 `("toplevel = " ,(Var body))]
	 ((Stmt tenv) body)]

	;; No recursion!
	[(letrec ,binds ,body)
	 (ASSERT (no-recursion! binds))
	 ((Stmt tenv) `(let (,(car binds))
			 (letrec ,(cdr binds) ,body)))]
	[(let ,binds ,body)
	 ((Stmt tenv) `(let (,(car binds))
			 (let ,(cdr binds) ,body)))]
	
;	[(emit ,vqueue (assert-type ,ty ,[myExpr -> val]))
;	 `("emit((",(Type ty)")" ,val ");\n")]

	;; HACK: cast to output type. FIXME FIXME
	[(emit ,vqueue ,[myExpr -> val])
	 (match (recover-type vqueue tenv)
	   [(VQueue ,ty)
	    `("emit((",(Type ty)")" ,val ");\n")
	    ])]

	;; Print is required to be pre-annotated with a type.
	;; (We can no longer do recover-type.)
	[(print (assert-type ,t ,e))
	 (EmitPrint (myExpr e) t)]
	[(print ,_) (error 'emit-c:Stmt "print should have a type-assertion around its argument: ~s" _)]

	;; This begin is already *in* Stmt context, don't switch back to Expr for its last:
	[(begin ,[stmts] ...) stmts]

	[(,containerset! ,[myExpr -> container] ,[myExpr -> ind] ,[myExpr -> val])
	 (guard (memq containerset! '(arr-set! hashset_BANG)))
	 `("(*",container ")[" ,ind "] = " ,val ";\n")]

	[(set! ,[Var -> v] ,[myExpr -> e])
	 `(,v " = " ,e ";\n")]       

	[(for (,i ,[myExpr -> st] ,[myExpr -> en]) ,bod)
	 (let ([istr (Var i)])	   
	   (block `("for (int ",istr" = ",st"; ",istr" <= ",en"; ",istr"++)")
		  ((Stmt (tenv-extend tenv (list i) '(Int))) bod)))]
	[(break) "break;\n"]

;	[___VIRTQUEUE___ ""] ;; [2006.11.24] Should this still be here?

	;; Otherwise it's just an expression.
	;; TEMP: HACK: Need to normalize contexts.
	;; TODO: Not all expressions make valid statements.
	[,[myExpr -> exp] 
	 (ASSERT (compose not procedure?) exp)
	 `(,exp ";\n")]
	[,unmatched (error 'emitC:Stmt "unhandled form ~s" unmatched)]
	))
	(DEBUGASSERT text? result)
	result
	))

; ======================================================================
;; Expressions.
	
    (define (Expr tenv)
      (lambda (exp)
	(match exp

	  ;; Special Constants:
	  [(assert-type ,t nullseg) (Const 'nullseg t)]
	  [(assert-type ,t nullarr) (Const 'nullarr t)]
	  [(assert-type ,t '())     (Const '() t)]
	  ;[,c (guard (constant? c)) (Const c)]
	  [(quote ,datum) (Const datum)]

	  [,v (guard (symbol? v)) (Var v)]	

	  [(if ,[test] ,[conseq] ,[altern])
	   `("(",test " ? " ,conseq " : " ,altern")")]
	  
	; ============================================================
	;; Here we handle "open coded" primitives:

	;; TODO: tupref
	[(,infix_prim ,[left] ,[right])
	 (guard (memq infix_prim '(+ - * / < > <= >= =
				     +. -. *. /. )))
	 (let ([cname (case infix_prim
			[(=) "=="]
			[(+ * - / < > <= >=) infix_prim]
			[(+. *. -. /.) ;; Chop off the period:
			 (substring (symbol->string infix_prim) 0 1)])])
	   `("(" ,left ,(format " ~a " cname) ,right ")"))]

	;[(realpart ,[v]) `("(" ,v ".real)")]
	;[(imagpart ,[v]) `("(" ,v ".imag)")]
	[(realpart ,[v]) `("__real__ " ,v)]
	[(imagpart ,[v]) `("__imag__ " ,v)]

	[(int_to_float ,[e]) `("(wsfloat_t)",e)]


	[(show (assert-type ,t ,e))
	 (EmitShow ((Expr tenv) e) t)]
	[(show ,_) (error 'emit-c:Stmt "show should have a type-assertion around its argument: ~s" _)]

	;; This is inefficient.  Only want to call getDirect once!
	;; Can't trust the C-compiler to know it's effect free and do CSE.
	[(seg-get (assert-type (Sigseg ,[Type -> ty]) ,[seg]) ,[ind])
	 ;`("(" ,seg ".getDirect())[" ,ind  "]")
	 `("(*((",ty"*)(*(" ,seg ".index_i(" ,ind  ")))))")]
	[(seg-get ,foo ...)
	 (error 'emit-c "seg-get without type annotation: ~s" 
		`(seg-get ,@foo))]
	
	;; Need to use type environment to find out what alpha is.
	;; We store the length in the first element.
	[(newarr ,[int] ,alpha)
	 ;(recover-type )
	 "newarr_UNFINISHED"]
	
	[(arr-get ,[arr] ,[ind]) `(,arr "[" ,ind "]")]
	[(length ,arr) "array_length_UNFINISHED"]

	[(arr-set! ,x ...)
	 (error 'emitC:Expr "arr-set! in expression context: ~s" `(arr-set! ,x ...))]
	[(begin ,stmts ...)
	 (error 'emitC:Expr "begin in expression context: ~s" `(begin ,stmts ...))]

	;; Later we'll clean it up so contexts are normalized:
	;[(set! ,[Var -> v] ,[(Expr tenv) -> rhs]) `(,v " = " ,rhs ";\n")]

	;; Forming tuples.
	[(make-struct ,name ,[arg*] ...)
	 `(,(symbol->string name)"(",(insert-between ", " arg*)")")]
	;; Referencing tuples.
	[(struct-ref ,[x] ,fld)
	 `("(",x "." ,(symbol->string fld)")")]
       
	;; ----------------------------------------
	;; Lists:
	[(assert-type (List ,[Type -> ty]) (cons ,[a] ,[b]))
	 `("cons< ",ty" >::ptr(new cons< ",ty" >(",a", (cons< ",ty" >::ptr)",b"))")]
	[(car ,[ls]) `("(",ls")->car")]
	[(cdr ,[ls]) `("(",ls")->cdr")]
	;; TODO: nulls will be fixed up when remove-complex-opera is working properly.

;; Don't have types for nulls yet:
;	[(null_list ,[Type -> ty]) `("cons< "ty" >::ptr((cons< "ty" >)0)")]
	[(,lp . ,_) (guard (memq lp '(cons car cdr))) ;; Safety net.
	 (error 'emit-C:Expr "bad list prim: ~s" `(,lp . ,_))
	 ]

	;; ----------------------------------------
	;; Hash tables:

	;; We should have the proper type assertion on there after flattening the program.
	;; (Remove-complex-opera*)
	[(assert-type (HashTable ,k ,v) (hashtable ,[n]))
	 (let ([hashtype (HashType k v)]
	       ;[eqfun ]
	       [k (Type k)]
	       [v (Type v)])
	   `(,(SharedPtrType hashtype)"(new ",hashtype"(",n"))"))]
	[(hashtable ,_) (error 'emitC:Expr "hashtable not wrapped in proper assert-type: ~s"
			       `(hashtable ,_))]
	[(hashget ,[ht] ,[key]) `("(*",ht ")[",key"]")]
	;; TEMP, HACK: NEED TO FIGURE OUT HOW TO CHECK FOR MEMBERSHIP OF A KEY!
	[(hashcontains ,[ht] ,[key]) `("(*",ht ")[",key"]")]

	[(tupref . ,_) (error 'emit-c:Expr "tuprefs should have been eliminated: ~s" `(tupref . ,_))]
	[(tuple . ,_) (error 'emit-c:Expr "tuple should have been eliminated: ~s" `(tuple . ,_))]

	;; TODO: Could make this into a cast statement for a sanity check??
	[(assert-type ,t ,[e]) e]

	;; Generate equality comparison:
	[(equal? (assert-type ,t ,[a]) ,[b])
	 (let ([simple `(,a" == ",b)])
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
	
	; ============================================================
	;; Other prims fall through:
	[(,prim ,[rand*] ...)
	 (guard (regiment-primitive? prim))
	 `(,(PrimName prim) "(" ,(insert-between ", " rand*) ")")]
	[(app ,rator ,[rand*] ...)
	 (ASSERT (symbol? rator))				       
	 `(,(FunName rator) "(" ,@(insert-between ", " rand*) ")")]
	[,unmatched (error 'emitC:Expr "unhandled form ~s" unmatched)])
	))



;; This implements our polymorphic print/show functions.
;; It prints something or other for any type.
(define (Emit-Print/Show-Helper e typ printf stream)
  (match typ
    [Int            (printf "%d" e)]
    [Float          (printf "%f" e)]
    [String         (printf "%s" `(,e".c_str()"))]
    [(Sigseg ,t)    (stream `("SigSeg<",(Type t)">(",e")"))]
    [(Struct ,name) (printf "%s" `("show_",(symbol->string name)"(",e").c_str()"))]
    [,other (printf "<object of type %s>" (format "\"~a\"" typ))]))

;; This emits code for a print.
(define (EmitPrint e typ)
  (Emit-Print/Show-Helper 
   e typ
   (lambda (s e) `("printf(\"",s"\", ",e");\n"))
   (lambda (e)   `("cout << " ,e ";\n"))))
                         
;; This emits code for a show.
(define (EmitShow e typ)
  (Emit-Print/Show-Helper 
   e typ
   (lambda (s e) `("WSPrim::show_helper(sprintf(global_show_buffer, \"",s"\", ",e"))"))
   (lambda (e)   `("WSPrim::show_helper2(global_show_stream << " ,e ")"))))

;; This produces a struct definition as well as a printer function for the struct.
(define (StructDef entry)
     (match entry
       [(,(symbol->string -> name) (,[symbol->string -> fld*] ,typ*) ...)
	(let ([tmpargs (map (lambda (_) (symbol->string (unique-name 'tmp))) fld*)]
	      [ctype* (map Type typ*)])
	  `(,(block `("struct ",name)
		    ;; Fields:
		    `([,ctype* " " ,fld* ";\n"] ...
		      
		      ;; Constructors, first nullary:
		      (,name"() {}\n")
		      ;; Full constructor:
		      ,(if (null? fld*) ""
			   `((,name"(",(insert-between 
				      ", " `([,ctype* " ",tmpargs] ...)
				      )")") " :\n"					   
			    ,@(insert-between ", \n"
			      (map (lambda (fld arg)
				     `("  ",fld "(" ,arg ")"))
				fld* tmpargs)) " {}\n"
				))))
	    ";\n\n"
            
            ;; This produces a printing function.  We could save space
            ;; by not generating this for structure types that are
            ;; never printed.  (Dead code elimination.)
	    "string show_" ,name "(",name" rec) {\n" 
	    "  ostringstream oss(ostringstream::out);"
	    "  oss << \"(\";\n"
	    ,(insert-between "  oss << \", \";\n"
			     (map (lambda (fld typ)
				    `("  oss << "
				      ,(EmitShow (format "rec.~a" fld) typ)
				      ";\n"))
			       fld* typ*))
	    "  oss << \")\";\n"
	    "  return oss.str();"
	    "}\n\n"
            ))]))
;; TODO: Add these:
; struct hashtest {
;   size_t operator()(thistest tup) const 
;   {
;     return myhash((unsigned char*)tup, sizeof(struct test));   
;   }
; };
; struct eqtest {
;   bool operator()(thistest tup1, thistest tup2) {
;     return (tup1->x == tup2->x && 
; 	    tup1->y == tup2->y );
;   }
; };



  ;============================================================
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

;; This converts data from the form it's on "in the wire" (queues
;; between nodes) into the form that's expected for the body of the
;; iterate.
(define (naturalize inname outname type)
  (define tstr (Type type))
  `("/* Naturalize input type to meet expectations of the iterate-body. */\n"
    ,(match type
    ;; The input is passed into the body as a reference.
    ;; All other sigseg variables in the body are by value.
    [(Sigseg ,t) 
     `("/* Sigseg input.  This is a bit of a trick, uses ref rather than value: */\n"
       ,tstr "& " ,outname " = *(",tstr"*) ",inname";\n")]
    ;; Immediates are passed by value.
    [,imm (guard (immediate-type? imm))
	  `(,tstr " " ,outname " = *((",tstr"*) ",inname");\n")]
    
    ;; Currently let's just not let you pass sigsegs in lists!
    [(List ,t)
     `(,tstr " " ,outname " = *((",tstr"*) ",inname");\n")]
	  
    ;; TODO ARRAYS:    
    [,other (error 'naturalize "Can't naturalize as box input type: ~s" other)]
  )))


(define (make-output-printer typ)
  (match typ
    [(Signal ,typ)
     (let ([T (Type typ)])
       `("\n\n"
	 ,(block "class PrintQueryOutput : public WSBox"
	 `("public:\n"
	   "PrintQueryOutput(const char *name) : WSBox(\"PrintQueryOutput\") {}\n\n"
	   "private:\n"
	   "DEFINE_NO_OUTPUT_TYPE;\n\n"
	   ,(block "bool iterate(uint32_t port, void *input)"
		   `(,T " *element = (",T" *)input;\n"

			,(match typ
			   [(Struct ,name)
			    (guard (null? (cdr (ASSERT (assq name struct-defs)))))
			    ;; If it's a unit return type, we don't print anything:
			    '()]
			   [,else 
			    `("if(WSOUTPUT_PREFIX) printf(\"WSOUT: \");\n"
			      ,(EmitPrint "(*element)" typ) ";\n"
			      "printf(\"\\n\");\n")])

; [2007.01.22] Don't need to do this, it happens automatically:
;			"delete element;\n"  
			"return false;\n"
			))))
	 ";\n\n"))]))

;; Takes the *inside* of an iterate box and turns it to C text.
(define wscode->text
    (let () 
    ;; Entry point
      (lambda (exp name tenv)
	(match exp 
	  [(lambda (,input ,vq) (,T ,vqT) ,bod)
	   (let ([body ((Stmt (tenv-extend tenv `(,input ,vq) `(,T ,vqT))) bod)]
		 [typ (Type T)])
	     (values `(,(format "/* WaveScript input type: ~s */\n" T)
		       ,(block 
			 ;"bool iterate(WSQueue *inputQueue)"
			 "bool iterate(uint32_t portnum, void* datum)"
			 `(;"printf(\"Execute iterate for ",name"\\n\");\n"
			   ;"void *input = inputQueue->dequeue();\n"
			   ;,typ " " ,(Var input) " = *((",typ"*)input);\n"
			   ,(naturalize "datum" (Var input) T)
			   
			   ,@body
			   
			   ;; This is a quirky feature.  The bool returns
			   ;; indicates whether the scheduler should NOT
			   ;; reschedule this box further (even if there is
			   ;; input left in its queue.)  For all the iterate
			   ;; operators, this will be FALSE.
			   "return FALSE;\n"
			   )))
		     '()))]

	;; Iterator state:
	[(,letsym ([,lhs* ,ty* ,rhs*] ...) ,bod) (guard (memq letsym '(let letrec)))
	 (let* ([newenv (tenv-extend tenv lhs* ty*)]
		[myExpr (Expr newenv)]
		[lhs* (map Var lhs*)]
		[rhs* (map myExpr rhs*)])
	   ;; KNow do the body with the new tenv:
	   (mvlet ([(body inits) (wscode->text bod name newenv)])
	     (let ([decls (map (lambda (l t orig) `(,t " " ,l ,(format "; // WS type: ~s\n" orig)))
			    lhs* (map Type ty*) ty*)]
		   [inits2 (map (lambda (l r) `(,l " = " ,r ";\n")) lhs* rhs*)]
		   )
	       (values `(,decls "\n" ,body) `(,inits ,inits2))))
	   )]
	
	[,other (error 'wscode->text "Cannot process: ~s" other)]
	))
    ))

  (define (build-main body)
    `(,boilerplate_premain
      ;;"// " ,(Type typ) " toplevel;\n"
      ,(indent body "  ")
      ,(boilerplate_postmain (Var 'toplevel) typ)))

  ;============================================================
  ;;; Set unit tests

  ;============================================================
  ;; Main body:
  ;; Here we stitch together the file out of its composit bits.
    (mvlet ([(body funs) (Query "toplevel" typ expr (empty-tenv))])
      `(,(file->string (++ (REGIMENTD) "/src/linked_lib/WSHeader.hpp"))
	,(file->string (++ (REGIMENTD) "/src/linked_lib/WSPrim.cpp"))
	"\n/* These structs represent tuples in the WS program. */\n"
	,(map StructDef struct-defs)
	,funs
	,(make-output-printer typ)
	
	,@(if static-linkage
	      (build-main body)
	      '())
	))]

  [,other ;; Otherwise it's an invalid program.
   (warning 'wsquery->text "ERROR: bad top-level WS program: ~s" other)
   (inspect other)
   (error 'wsquery->text "")]))
) ; End wsquery->text



;======================================================================
;;; Bits of boilerplate.

;(define boilerplate_headers "")

(define boilerplate_premain "

int main(int argc, char ** argv)
{
  /* set global variable(s) */
  if (misc_parse_out_switch(&argc, argv, \"no_prefix\", 0))
    WSOUTPUT_PREFIX = FALSE;
  else WSOUTPUT_PREFIX = TRUE;

  /* initialize subsystems */ 
  WSInit(&argc, argv);

  /* declare variable to hold final result */
  //WSBox* toplevel;

  /* begin constructing operator graph */
")

(define (boilerplate_postmain return_name return_type)   
  (printf "Generating code for returning stream of type ~s\n" return_type)
  `("
  /* dump output of query -- WaveScript type = ",(format "~s" return_type)" */
  PrintQueryOutput out = PrintQueryOutput(\"WSOUT\");
  out.connect(",return_name");

  /* now, run */
  WSRun();

  return 0;
}

"))

;; Boilerplate for producing a WSBox class:
(define (WSBox name outtype constructor body)
  `(,(block (wrap `("\nclass " ,name " : public WSBox"))
	    `("public:\n"
	      "WS_DEFINE_OUTPUT_TYPE(" ,outtype ");\n\n"
	      ,constructor
	      "\nprivate:\n"
	      ,body)) ";\n"))



;;================================================================================

(define (testme)
  (define str 
    (text->string (wsquery->text
		   '(base-language
  '(program
     (letrec ([s1 (Signal (Sigseg Complex)) (audio 1 4096 0)]
              [s2 (Signal (Sigseg Complex)) (iterate
                                              (lambda (w ___VIRTQUEUE___)
                                                ((Sigseg Complex) (VQueue
								   (Sigseg
								    Complex)))
						(begin
                                                    (emit
                                                      ___VIRTQUEUE___
                                                      w)
                                                    ___VIRTQUEUE___))
                                              s1)])
       s2)
     (Signal (Sigseg Complex)))))))
  (display str)
  (string->file str (string-append (getenv "HOME") "/WaveScope/code/v1/Ryan2.cpp")))


(define (testme0)
  (define str 
    (text->string (wsquery->text
		   '(base-language
  '(program
     (letrec ([s1 (Signal (Sigseg Complex)) (audio 1 4096 0)])
       s1)
     (Signal (Sigseg Complex))))
		   )))
  (display str)
  (string->file str (string-append (getenv "HOME") "/WaveScope/code/v1/Ryan2.cpp")))
  

(define (testme2)
  (define str 
    (text->string (wsquery->text
  '(base-language
  '(program
     (letrec ([s1 (Signal (Sigseg Complex)) (audio 0 1024 0)]
              [s2 (Signal (Array Complex)) (iterate
                                             (lambda (w ___VIRTQUEUE___)
                                               ((Sigseg Complex) (VQueue
								  (Array
								   Complex)))
					       (begin
						 (emit
						  ___VIRTQUEUE___
						  (fft (fft (to_array
							     w))))
						 ___VIRTQUEUE___))
                                             s1)]
              [s3 (Signal Float) (iterate
				  (lambda (arr0 ___VIRTQUEUE___)
                                     ((Array Complex) (VQueue  Float))
                                       (begin
                                         (letrec ([x Int 3])
                                           (letrec ([arr (Array Complex) (fft (fft arr0))])
                                             (if (> (realpart
                                                      (arr-get arr 100))
                                                    224192.0)
                                                 (begin
                                                   (emit
                                                     ___VIRTQUEUE___
                                                     0.0)
                                                   (emit
                                                     ___VIRTQUEUE___
                                                     (imagpart
                                                       (arr-get arr 100))))
                                                 (tuple))))
                                         ___VIRTQUEUE___))
                                   s2)])
       s3)
     (Signal Float))))))
  
  (display str)
  (string->file str (string-append (getenv "HOME") "/WaveScope/code/v1/Ryan2.cpp"))
  )

;; Disabled because this code can't get to wscode->text right now.
  (define these-tests
    `(
#;
      [(mvlet ([(txt _) (,wscode->text '(lambda (x) (Int) (begin '35)) "noname" (empty-tenv))])
	 (,text->string txt))
       
       ;; Requires helpers.ss
       ;; Not very tight:
       ;; Doesn't generate any code for this now.
       ,(lambda (s) (not (substring? "35" s)))]
					;     ,(lambda (s) (substring? "35" s))]

#;      
      [(mvlet ([(txt _) (,wscode->text '(lambda (x) (Int) (+ '1 (if '#t '35 '36))) "noname" (empty-tenv))])
	 (,text->string txt))
					;"TRUE ? 35 : 36"]
       ,(let ([substring? substring?])
	  (lambda (s) (substring? "1 + (TRUE ? 35 : 36)" s)))]
      ))

(define test-this (default-unit-tester "wavescript_emit-c.ss: generating WaveScript C code." these-tests))
(define test-wavescript_emit-c test-this)

) ;; End Module

