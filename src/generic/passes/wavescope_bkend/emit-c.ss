

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
    [Bool    "wsbool_t"]
    [Int     "wsint_t"]
    [Int16   "wsint16_t"]
    [Float   "wsfloat_t"]
    [Complex "wscomplex_t"]

    [String "wsstring_t"] ;; Not boosted.

    [(VQueue ,_) "void*"]

    ;; Went back and forth on whether this should be a pointer:
    [(Sigseg ,[t]) `("RawSeg")]
    [(Stream ,[t]) `("WSBox*")]

    ;[(Array ,[t]) `(,t "[]")]
    [(Array ,[t])  `("boost::shared_ptr< vector< ",t" > >")]
    [(Struct ,name) (symbol->string name)]

    [#() "wsunit_t"]
    
    [Timebase  "int"]

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
;; .returns text of a .cpp query file (if in static linkage mode)
;;;.returns OR vector containing text of .cpp file + .wsq query descriptor (dynamic linkage)
(define wsquery->text
  (lambda (prog . mode)

    ;; I use mutation below to accumulate a list of the connections between boxes, this allows us to produce a WSQ file:
    (define query-descriptor '())
    
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
    ;; .returns 3 values: A new expression, a set of declarations, wsq declarations
    (define (Query name typ x tenv)
      ;; Coercion:
      (if (symbol? name) (set! name (symbol->string name)))
      (match x

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

	;; Only do let for now.
#;	[(,letsym ,binds ,bod) (guard (memq letsym '(let )))
	 ;(ASSERT (symbol? body))
	 ;(ASSERT (no-recursion! binds))
         (match binds 
           [([,lhs* ,ty* ,rhs*] ...)
            (let ([newenv (tenv-extend tenv lhs* ty*)])
              (let-values ([(bodstmts boddecls wsqdecls) (Query name typ bod newenv)])
                (let loop ([lhs* (map Var lhs*)] [ty* ty*] [rhs* rhs*]
                                                 [stmtacc '()] 
                                                 [declacc '()] 
                                                 [wsqacc '()])
                  ;; Should really use the Text ADT here:
                  (if (null? lhs*)
                      ;; Now we do the body.
                      (values (list (reverse! stmtacc) bodstmts)
                              (append (reverse! declacc) boddecls)
                              (append (reverse! wsqacc) wsqdecls))
                      (mvlet ([(stmt decl wsq) (Query (car lhs*) (car ty*) (car rhs*) newenv)])
                             (loop (cdr lhs*) (cdr ty*) (cdr rhs*)
                                   (cons stmt stmtacc) (cons decl declacc) (cons wsq wsqacc)))))))]
           [,other (error 'wsquery->text "Bad letrec binds: ~s" other)])]

	[(let ([,lhs ,ty ,rhs]) ,bod)
	   (let ([newenv (tenv-extend tenv (list lhs) (list ty))])
              (let-values ([(bodstmts boddecls wsqdecls) (Query name typ bod newenv)]
			   [(stmt decl wsq) (Query lhs ty rhs tenv)])
		(values (append stmt bodstmts)
			(append decl boddecls)
			(append wsq wsqdecls))))]

	[(iterate ,let-or-lambda ,sig)
	 ;; Program better have been flattened!!:
	 (ASSERT (symbol? sig))	  
	 (let* ([parent (Var sig)]
		[class_name `("Iter_" ,name)]
		;; First we produce a line of text to construct the box:
		[ourstmts `(  "WSBox* ",name" = new ",class_name "(" ");\n" 
			      ,name"->connect(" ,parent ");\n")]
		;; Then we produce the declaration for the box itself:
		[ourdecls 
		 (mvlet ([(iterator+vars stateinit) (wscode->text let-or-lambda name tenv)])
		   (list (WSBox class_name 
				(match typ
				  [(Stream ,t) (Type t)]
				  [,other (error 'emitC:Query "expected iterate to have signal output type! ~s" other)])
				;; Constructor:
				(block `(,class_name "()")  stateinit)
				;; This produces a function declaration for iterate:				
				iterator+vars)))]) 
	   ;(if (symbol? sig) 
	   (values ourstmts ourdecls
                   `(("op \"",name"\" \"",class_name"\" \"query.so\"\n") 
                     ("connect \"" ,parent"\" \"",name"\"\n"))
                   ))]

		       
	[(assert-type (Stream (Sigseg ,[Type -> ty])) 
		      (prim_window ,sig ,[Simple -> size]))
	 (define new-class-name (Var (unique-name 'Window)))
	 (ASSERT symbol? sig)
	 (values `("WSBox* ",name" = new WSBuiltins::Window(",size", sizeof(",ty"));\n"
		   ,name"->connect(",(symbol->string sig)");\n")
		 (if static-linkage
		     '()
		     ;; Otherwise we need to make a wrapper that parameterizes this window:
		     `("class ",new-class-name" : public WSBuiltins::Window {\n"
		       "  public:\n"
		       "  ",new-class-name"() : Window(",size", sizeof(",ty")) {}\n"
		       "}\n\n")
		     )
                 `(("op \"",name"\" \"",new-class-name"\" \"query.so\"\n")
		   ("connect \"",(symbol->string sig)"\" \"",name"\"\n")))]
	
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

	
	;; This is purely hackish... should use the zip library function.	
#;
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


	;; UNOPTIMIZED: should combine with the downstream iterate.
	;; Wire these all to our iterate.
	[(assert-type (Stream (Struct ,tupname)) (unionN ,inputs ...))
	 (ASSERT (not (null? inputs)))
	 (ASSERT (andmap symbol? inputs))
	 
	 (let ([ty (Type (match (recover-type (car inputs) tenv)
			   [(Stream ,t) t]))]
	       [classname (format "UnionN~a" (unique-name ""))])
	   (values 
	    `(" WSBox* ",name" = new ",classname"();"
	      ;; Order is critical here:
	     ,(map (lambda (in)
		     `(" ",name"->connect(",(symbol->string in)"); "))
		inputs)
	     )
	    ;; Decls:
	    `("
  /* This takes any number of connections, they must be connected in order. */
  class ",classname" : public WSBox{ 
   
    private:
    WS_DEFINE_OUTPUT_TYPE(",(symbol->string tupname)");
    
    bool iterate(uint32_t port, void *item)
    {
      emit(",(symbol->string tupname)"((wsint_t)port, *(",ty"*)item));      
      return true;
    }
  };
")
            ;; wsq decls:
            `()))]


	;; UNOPTIMIZED: should combine with the downstream iterate.
	;; Wire these all to our iterate.
	[(timer ,[Simple -> period])
	 (values 
	  `(" WSSource* ",name" = new WSBuiltins::Timer(",period");\n"  )
	  '()
	  '() ;;TODO, FIXME: wsq decls
	  )]

	
	  [,other (error 'wsquery->text:Query "unmatched query construct: ~s" other)]
	)) ;; End Query

; ======================================================================
;;; Helper functions for handling different program contexts:
        
    (define (Var var)
      (ASSERT (symbol? var))
      ;; This is the place to do any name mangling.  I'm not currently doing any for WS.
      (symbol->string var))

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
	  [(flonum? datum)  (format "(wsfloat_t)~a" datum)]
	  [(integer? datum) (format "(wsint_t)~a" datum)]
	  [(eq? datum 'nulltimebase)  "WSNULLTIMEBASE"]
	  [(cflonum? datum) (format "(wscomplex_t)complex<float>(~a, ~a)" 
				    (cfl-real-part datum)
				    (cfl-imag-part datum))]
	  [else (error 'emitC:Const "not a C-compatible literal: ~s" datum)])]
	[(datum ty)
	 (match (vector datum ty)
	   (DEBUGASSERT (not (polymorphic-type? ty)))
	   [#(() (List ,t)) 	   
	    ;"NULL_LIST"
	    ;`("(cons<",(Type t)">::ptr)NULL_LIST")
	    ;`("(cons<",(Type t)">::null_ls)")
	    `("boost::shared_ptr< cons< ",(Type t)" > >((cons< ",(Type t)" >*) 0)")
	    ]
	   [#(nullseg ,t) "WSNULLSEG"]
	   ;[#(nullarr (Array ,t)) `("boost::shared_ptr< vector< ",(Type t)" > >(new ",(Type t)"[0])")]
	   [#(nullarr (Array ,t)) `("boost::shared_ptr< vector< ",(Type t)" > >(new vector< ",(Type t)" >(0))")]	  
	   )]))

    (define Simple
      (lambda (x)
	(match x 
	  [(quote ,c) (Const c)]
	  [,v (guard (symbol? v)) (Var v)]
	  [(assert-type ,_ ,[x]) x]
	  [,else (error 'Simple "not simple expression: ~s" x)])))

; ======================================================================
;; Statements.


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
      [,v (guard (symbol? v)) (if name (wrap (Var v)) "")]
      [(quote ,c)             (if name (wrap (Const c)) "")]

      [(let ([,v ,ty ,rhs]) ,bod)
       (list
	((Value tenv) (symbol->string v) (Type ty) rhs)
	((Block (tenv-extend tenv (list v) (list ty))) name type bod))]
      [(begin ,e1 . ,e*)
       (list
	((Block tenv) #f #f e1)
	((Block tenv) name type `(begin . ,e*)))]
      ;; Both void value:
      [(begin) ""]
      [(tuple) ""]

      ;; Not using return statements right now:
#;
      [(return ,[Simple -> e]) (ASSERT not name)
       `("return ",e";\n")]

      [(set! ,[Var -> v] ,[Simple -> e]) (ASSERT not name)
       `(,v " = " ,e ";\n")]
      
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

      ;; Print is required to be pre-annotated with a type.
      ;; (We can no longer do recover-type.)
      [(print (assert-type ,t ,[Simple -> e]))
       (ASSERT not name)
       (EmitPrint e t)]
      [(print ,_) (error 'emit-c:Effect "print should have a type-assertion around its argument: ~s" _)]

      ;; This just does nothing in the c++ backend:
      [(gnuplot_array ,a) ""]

      [(,containerset! ,[Simple -> container] ,[Simple -> ind] ,[Simple -> val])
       (guard (memq containerset! '(arr-set! hashset_BANG)))
       (ASSERT not name)
       `("(*",container ")[" ,ind "] = " ,val ";\n")]

      [,oth (error 'Block "unhandled: ~s" oth)]
      )))

; ======================================================================
;; Expressions.
	
    (define (Value tenv)
      (lambda (name type exp)
	(define (wrap x) (list type " " name " = " x ";\n"))
	(ASSERT name)
	(match exp

	  ;; Special Constants:
	  [(assert-type ,t nullseg) (wrap (Const 'nullseg t))]
	  [(assert-type ,t nullarr) (wrap (Const 'nullarr t))]
	  [(assert-type ,t '())     (wrap (Const '() t))]

	  [nulltimebase             (wrap (Const 'nulltimebase))]

	  [(quote ,vec) (guard (vector? vec))
	   (let ([contenttype (if (zero? (vector-length vec))
				  ;; Throw in a default:
				  'Int
				  (type-const (vector-ref vec 0)))])	    
	     `(,type" ",name"(new vector< ",(Type contenttype)" >(",
		    (number->string (vector-length vec))"));\n" ;; MakeArray.
		    ,(mapi (lambda (i x) `("(*",name")[",(number->string i)"] = ",(Const x)";\n"))
			   (vector->list vec))))]

	  ;[,c (guard (constant? c)) (Const c)]
	  [(quote ,datum)           (wrap (Const datum))]

	  [,v (guard (symbol? v))
	      (ASSERT (not (regiment-primitive? v)))
	      (wrap (Var v))]

	  ;[(if ,[Simple -> test] ,conseq ,altern)
	   ;`("(",test " ? " ,conseq " : " ,altern")") 	   ]

	  [(if ,[Simple -> test] ,conseq ,altern)
	   `(,type" ",name";\n"
	     "if (" ,test ") {\n"
	     ,(indent ((Block tenv) name "" conseq) "  ")
	     "} else {\n"
	     ,(indent ((Block tenv) name "" altern) "  ")
	     "}\n")]
	  
	  ;; This is needed just for the RHS's of iterator state variables.
	  [(let ([,v ,ty ,rhs]) ,bod)
	   (list
	    ((Value tenv) (symbol->string v) (Type ty) rhs)
	    ((Value (tenv-extend tenv (list v) (list ty))) name type bod))]

	[(tuple) (wrap "((wsunit_t)0)")]
	;; Forming tuples.
	[(make-struct ,name ,[Simple -> arg*] ...)
	 (wrap `(,(symbol->string name)"(",(insert-between ", " arg*)")"))]
	;; Referencing tuples.
	[(struct-ref ,[Simple -> x] ,fld)
	 (wrap `("(",x "." ,(symbol->string fld)")"))]

	; ============================================================
	[(,prim ,rand* ...) (guard (regiment-primitive? prim))
	 (Prim (cons prim rand*) name type)]
	[(assert-type ,t (,prim ,rand* ...)) (guard (regiment-primitive? prim))
	 (Prim `(assert-type ,t (,prim . ,rand*)) name type)]

	; ============================================================

	[(tupref . ,_) (error 'emit-c:Value "tuprefs should have been eliminated: ~s" `(tupref . ,_))]
	[(tuple . ,_) (error 'emit-c:Value "tuple should have been eliminated: ~s" `(tuple . ,_))]

	;; TODO: Could make this into a cast statement for a sanity check??
	[(assert-type ,t ,[e]) e]

	#;
	[(app ,rator ,[Simple -> rand*] ...)
	 (ASSERT (symbol? rator))				       
	 `(,(FunName rator) "(" ,@(insert-between ", " rand*) ")")]
	[,unmatched (error 'emitC:Value "unhandled form ~s" unmatched)])
	))



;================================================================================
;; Primitive calls:

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
      [(cos sin tan)     (symbol->string var)]
      [(absF absI absI16)       "abs"]
;      [(absC)                   "cabs"]
      [(sqrtI sqrtF sqrtC)      "sqrt"]
      [(max)                    "max"]
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
    ;; First we handle "open coded" primitives:
    
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
			 ^_ ^. ^: 
			 ) ;; Chop off the extra character.
		     (substring (symbol->string infix_prim) 0 1)]
		    [(+I16 -I16 *I16 /I16 ^I16)
		     (substring (symbol->string infix_prim) 0 1)]
		    )])
       (wrap `("(" ,left ,(format " ~a " cname) ,right ")")))]

	;[(realpart ,[v]) `("(" ,v ".real)")]
	;[(imagpart ,[v]) `("(" ,v ".imag)")]
	[(realpart ,[Simple -> v])   (wrap `("__real__ " ,v))]
	[(imagpart ,[Simple -> v])   (wrap `("__imag__ " ,v))]

	[(absC ,[Simple -> c]) (wrap `("abs((complex<float>)",c")"))]
	
	[(intToFloat ,[Simple -> e]) (wrap `("(wsfloat_t)",e))]
	[(floatToInt ,[Simple -> e]) (wrap `("(wsint_t)",e))]

	[(int16ToInt ,[Simple -> e]) (wrap `("(wsint_t)",e))]
	[(int16ToFloat ,[Simple -> e]) (wrap `("(wsfloat_t)",e))]

	[(show (assert-type ,t ,[Simple -> e])) (wrap (EmitShow e t))]
	[(show ,_) (error 'emit-c:Value "show should have a type-assertion around its argument: ~s" _)]

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
	 (error 'emit-c:Value "seg-get without type annotation: ~s" 
		`(seg-get ,@foo))]
	[(timebase ,[Simple -> seg]) (wrap `("(" ,seg ".getTimebase())"))]
	
	;; Need to use type environment to find out what alpha is.
	;; We store the length in the first element.
	[(newarr ,[Simple -> int] ,alpha)
	 ;(recover-type )
	 "newarr_UNFINISHED"]
	
	;[(arr-get ,[arr] ,[ind]) `(,arr "[" ,ind "]")]
	[(arr-get ,[Simple -> arr] ,[Simple -> ind]) (wrap `("(*",arr ")[" ,ind "]"))]
	[(makeArray ,[Simple -> n] ,[Simple -> x])   (wrap `("makeArray(",n", ",x")"))]
	
	[(length ,[Simple -> arr])                   (wrap `("(wsint_t)(",arr"->size())"))]

	[(arr-set! ,x ...)
	 (error 'emitC:Value "arr-set! in Value context: ~s" `(arr-set! ,x ...))]
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
	[(assert-type (List ,t) (reverse ,[Simple -> ls]))
	 (wrap `("cons<",(Type t)">::reverse(",ls")"))]
	[(assert-type (List ,[Type -> ty]) (append ,[Simple -> ls1] ,[Simple -> ls2]))
	 (wrap `("cons<",ty">::append(",ls1", ",ls2")"))]
	[(listRef (assert-type (List ,t) ,[Simple -> ls]) ,[Simple -> ind])
	 (wrap `("cons<",(Type t)">::listRef(",ls", ",ind")"))]
	[(listLength (assert-type (List ,t) ,[Simple -> ls]))
	 (wrap `("cons<",(Type t)">::listLength(",ls")"))]
	[(makeList ,[Simple -> n] (assert-type ,t ,[Simple -> init]))
	 (wrap `("cons<",(Type t)">::makeList(",n", ",init")"))]
	;; TODO: nulls will be fixed up when remove-complex-opera is working properly.

;; Don't have types for nulls yet:
;	[(null_list ,[Type -> ty]) `("cons< "ty" >::ptr((cons< "ty" >)0)")]
	[(,lp . ,_) (guard (memq lp '(cons car cdr append reverse listRef listLength makeList))) ;; Safety net.
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

;================================================================================

;; This implements our polymorphic print/show functions.
;; It prints something or other for any type.
(define (Emit-Print/Show-Helper e typ printf stream)
  (match typ
    [Bool           (printf "%s" (format "(~a ? \"true\" : \"false\")" e))]
    [Int            (printf "%d" e)]
    [Int16          (printf "%hd" e)]
    [Float          (printf "%f" e)]
    [Complex        (stream `("complex<float>(",e")"))]
    [String         (printf "%s" `(,e".c_str()"))]
    ;[(List ,t)      (stream e)]
    ;[(List ,t)      (stream (cast-type-for-printing `(List ,t) e))]
    [(Sigseg ,t)    (stream `("SigSeg<",(Type t)">(",e")"))]
    [(Struct ,name) (printf "%s" `("show_",(symbol->string name)"(",e").c_str()"))]
    [,other (printf "<object of type %s>" (format "\"~a\"" typ))]))

#;
;; NOTE: duplicated code from the "Type" function.
(define (cast-type-for-printing ty x)
  (match ty
    [(Sigseg ,[Type -> t]) (format "SigSeg<~a>" t)]

    [Bool       x]
    [Int        x]
    [Float      x]
    [String     x]
    [(Struct ,name) (Type ty)]

    [(Array ,t) `("boost::shared_ptr< vector< ",(cast-type-for-printing t)" > >")]
    
    [(List ,t) `("cons< ",(cast-type-for-printing t)" >::ptr")]

    ;[(HashTable ,kt ,vt) (SharedPtrType (HashType kt vt))]
    
    [,other (error 'cast-type-for-printing "Not handled yet.. ~s" other)]))


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


            ;; This produces an equality function. 
	    "bool wsequal(const ",name"& x, const ",name"& y) {\n" 
	    ,(if (null? fld*)
		"return TRUE;\n"
		`(" return "
		  ,(insert-between " && "
				   (map (lambda (fld)
					  `(" wsequal(x.",fld", y.",fld") "))
				     fld*))
		  ";\n"))
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
    [,imm (guard (or (immediate-type? imm) (equal? imm '#())))
	  `(,tstr " " ,outname " = *((",tstr"*) ",inname");\n")]
    
    ;; Currently let's just not let you pass sigsegs in lists!
    [(List ,t)
     `(,tstr " " ,outname " = *((",tstr"*) ",inname");\n")]

    [(Array ,t)
     `(,tstr " " ,outname " = *((",tstr"*) ",inname");\n")
     ]    
	  
    ;; TODO ARRAYS:    
    [,other (error 'naturalize "Can't naturalize as box input type: ~s" other)]
  )))


(define (make-output-printer typ)
  (match typ
    [(Stream ,typ)
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
	   (let ([body ((Block (tenv-extend tenv `(,input ,vq) `(,T ,vqT))) #f #f bod)]
		 [typ (Type T)])
	     (values `(,(format "/* WaveScript input type: ~s */\n" T)
		       ,(block 
			 ;"bool iterate(WSQueue *inputQueue)"
			 "bool iterate(uint32_t portnum, void* datum)"
			 `(;"printf(\"Execute iterate for ",name"\\n\");\n"
			   ;"void *input = inputQueue->dequeue();\n"
			   ;,typ " " ,(Var input) " = *((",typ"*)input);\n"
			   ,(naturalize "datum" (Var input) T)
			   ;; Declare the VQueue... this is basically dead code:
			   ,(Type vqT)" ",(Var vq)";\n"
			   
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
		[myValue (Value newenv)]
		[lhs* (map Var lhs*)]
		[newty* (map Type ty*)]
		[inits2 (map myValue lhs* (map (lambda _ "") newty*) rhs*)])
	   ;; KNow do the body with the new tenv:
	   (mvlet ([(body inits) (wscode->text bod name newenv)])
	     (let ([decls (map (lambda (l t orig) `(,t " " ,l ,(format "; // WS type: ~s\n" orig)))
			    lhs* newty* ty*)]
		   ;[inits2 (map (lambda (l r) `(,l " = " ,r ";\n")) lhs* rhs*)]
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

  #;
  (define (build-wsq name query sofile)
    (match query
	[(iterate ,_ ,s) 
         (let ([boxname (format "Iter_~a" name)])
           `(("op \"",boxname"\" \"",sofile"\"\n") 
             ("connect \"" ,(Var s)"\" \"",boxname"\"\n")))]
      	[(assert-type (Stream (Struct ,structname))
		      (__dataFile ,file ,mode ,rate ,repeat ,types))
         `(("source "))]
      	[,oth (error 'wscode->text:build-wsq "Cannot process: ~s" oth)]
    ))
  ;============================================================
  ;;; Set unit tests

  ;============================================================
  ;; Main body:
  ;; Here we stitch together the file out of its composit bits.
  (let-values ([(body funs wsq) (Query "toplevel" typ expr (empty-tenv))])
    (define header
      (list (file->string (++ (REGIMENTD) "/src/linked_lib/WSHeader.hpp"))
            (file->string (++ (REGIMENTD) "/src/linked_lib/WSPrim.cpp"))
            "\n/* These structs represent tuples in the WS program. */\n"
            (map StructDef struct-defs)
            funs
            (make-output-printer typ)))
    
    (newline)(display (text->string wsq))(newline)(newline)
    ;(break)


    (if static-linkage 
        (snoc (build-main body) header)
        (vector header wsq))
    )
#;    
  (if static-linkage querycode
      (vector querycode (build-wsq "toplevel" expr "query.so")))
    ]

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
     (letrec ([s1 (Stream (Sigseg Complex)) (audio 1 4096 0)]
              [s2 (Stream (Sigseg Complex)) (iterate
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
     (Stream (Sigseg Complex)))))))
  (display str)
  (string->file str (string-append (getenv "HOME") "/WaveScope/code/v1/Ryan2.cpp")))


(define (testme0)
  (define str 
    (text->string (wsquery->text
		   '(base-language
  '(program
     (letrec ([s1 (Stream (Sigseg Complex)) (audio 1 4096 0)])
       s1)
     (Stream (Sigseg Complex))))
		   )))
  (display str)
  (string->file str (string-append (getenv "HOME") "/WaveScope/code/v1/Ryan2.cpp")))
  

(define (testme2)
  (define str 
    (text->string (wsquery->text
  '(base-language
  '(program
     (letrec ([s1 (Stream (Sigseg Complex)) (audio 0 1024 0)]
              [s2 (Stream (Array Complex)) (iterate
                                             (lambda (w ___VIRTQUEUE___)
                                               ((Sigseg Complex) (VQueue
								  (Array
								   Complex)))
					       (begin
						 (emit
						  ___VIRTQUEUE___
						  (fft (fft (toArray
							     w))))
						 ___VIRTQUEUE___))
                                             s1)]
              [s3 (Stream Float) (iterate
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

