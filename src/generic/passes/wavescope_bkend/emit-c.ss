
;;;; TODO: Refactor this file to process the more sensible graph representation used by emit-mlton.

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
	    (all-except (lib "list.ss") sort sort! filter)
	    (all-except "nominalize-types.ss" test-this these-tests)
	    "convert-sums-to-tuples.ss"
	    "../../compiler_components/c_generator.ss" )
  (provide ;WSBox wscode->text
	   wsquery->text
	   
	   ;testme	   testme2	   testme0
	   test-this  test-wavescript_emit-c)
  (chezprovide )  
  (chezimports (except helpers test-this these-tests)
	       )
  
;======================================================================
;;                       <WaveScript C generation>
;======================================================================

;; MUTABLE BINDINGS:
;; Typical problem -- I don't want to thread these through all relevant functions.
;; These are mutated during the compilation of the program.

;(define include-files 'include-files-uninit)
;(define link-files 'link-files-uninit)
(define include-files ())
(define link-files    ())

(define (add-include! fn)
  (unless (member fn include-files)
    (set! include-files (cons fn include-files))))
(define (add-link! fn)
  (unless (member fn link-files)
    (set! link-files (cons fn link-files))))

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

;================================================================================
;;; Abstract syntax producing functions.

;;; [2007.05.28] Trying to raise the level of abstraction a bit for producing syntax.

(define make-decl 
  (case-lambda
    [(type name)     `(,type" ",name";\n")]
    [(type name val) `(,type" ",name" = ",val";\n")]
    [(type name val flag) 
     (case flag
       [(reference) `(,type"& ",name" = ",val";\n")]
       [(pointer)   `(,type"* ",name" = ",val";\n")]
       [else (make-decl type name val)])]))

(define (make-for i st en bod)
  (block (list "for ("i" = "st"; "i" < "en"; "i"++)")
	 bod))

(define (make-while test bod)  (block (list "while ("test")") bod))

(define sym2str symbol->string)

;================================================================================

;; If the type needs a specialized hashfun, returns its name,
;; otherwise returns #f.
(define (HashType k v)
  (define hashfun
    (match k
      [,s (guard (symbol? s) (memq s '(Int Float))) #f]
      [String "boost::hash<string>"]
      [(Struct ,name)	`("hash",(sym2str name))]
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
    [Int64   "wsint64_t"]
    [Double  "wsdouble_t"]
    [Float   "wsfloat_t"]
    [Complex "wscomplex_t"]

    [Char   "wschar_t"]
    [String "wsstring_t"] ;; Not boosted.
    
    [(Ref ,[t]) t]  ;; These disappar.
    [(VQueue ,_) "void*"]

    ;; Went back and forth on whether this should be a pointer:
    [(Sigseg ,[t]) `("SigSeg< ",t" >")]
    [(Stream ,[t]) `("WSBox*")]

    ;[(Array ,[t])  `("wsarray_t< ",t" >")]
    [(Array ,[t])  `("boost::intrusive_ptr< WSArrayStruct< ",t" > >")]

    [(Struct ,name) (sym2str name)]

    [#() "wsunit_t"]
    
    [Timebase  "int"]
    ;[Pointer   "void*"]
    [(Pointer ,cty)   cty]

    ;; HACK HACK FIXME:
    ;; This is for null lists.
    ;[(List ',_) `("wslist_t<int>")]
    [(List ',_) `("boost::shared_ptr< cons<int> >")]
    ;; Boosted cons cells:
    ;[(List ,[t]) `("wslist_t< ",t" >")]
    [(List ,[t]) `("boost::shared_ptr< cons< ",t" > >")]

    [(Hashset ',_ ,__) (error 'emitC "hash table with typevar" )]
    [(Hashset ,_ ',__) (error 'emitC "hash table with typevar" )]
					;[(HashTable ,[kt] ,[vt]) `("boost::shared_ptr< hash_map< ",kt", ",vt" > >")]
    [(HashTable ,kt ,vt) (SharedPtrType (HashType kt vt))]

    ;; Variables of this type might occur (for foreign entries) but they'd better not be referenced!
    [(,arg* ... -> ,result) "void*"]
    
    [(Union ,name) `("struct ",(sym2str name))]
    
#;
    [(Union (,[sym2str -> v*] ,[t*]) ...) 
     (list "union {" (map (lambda (v t) (list t" "v"; ")) v* t*) "}")]
    
    [,other (error 'emit-c:Type "Not handled yet.. ~s" other)]))

(define (ToForeignType ty txt)  
  (match ty
    ;; For these types we just put in a cast:
    [,t (guard (scalar-type? t))
	`("((",(Type t)")" ,txt ")")]
    [String `("(",txt".c_str())")]
    ;[Pointer txt]
    [(Pointer ,cty) `("(",cty")(",txt")")]
    [(Array ,elt) (guard (scalar-type? elt)) `("(",(Type elt)"*)(",txt"->data)")]
    [,oth (error 'emit-c:ToForeignType
		 "cannot currently map this type onto equivalent C-type: ~s" oth)]
    )
  )

(define (FromForeignType ty txt)
  (match ty
    ;; For these types we just put in a cast:
    [,t (guard (memq t '(Int Float))) 
	`("((",(Type t)")" ,txt ")")]
    [#() txt]
    [String `("string(",txt")")]
    ;[Pointer txt]
    ;[(Pointer ,_) `("(size_t)(",txt")")]
    [(Pointer ,_) `("(void*)(",txt")")]
    [,oth (error 'emit-c:FromForeignTypeConvert 
		 "cannot currently map this type onto equivalent C-type: ~s" oth)]
    ))


;======================================================================

;; This is the only entry point to the file.  A complete query can
;; be transformed into a complete query file.
;; .param prog      The wsquery to process.
;; .param scheduler Symbol indicating the WaveScope scheduler to be used; one of: default-scheduler FIXME
;; .param mode      (Optional) 'static or 'dynamic, indicating whether to
;;                  produce a main() function, or code for a .so & .wsq.
;;                  Default is 'static.
;; .returns text of a .cpp query file (if in static linkage mode)
;;;.returns OR vector containing text of .cpp file + .wsq query descriptor (dynamic linkage)
;;
;; FIXME: MIC: changed to ignore timer sources; WSDataFileSource classes are not written to support these,
;;        and would likely only be slowed down
;;
(define wsquery->text
  (lambda (prog scheduler . mode)

    ;; I use mutation below to accumulate a list of the connections between boxes, this allows us to produce a WSQ file:
    (define query-descriptor '())
    
    ; FIXME: restructure this; there may be more named params. in the future
    (define static-linkage 
      (match mode 
	[()        #t]	
	[(static)  #t]
	[(dynamic) #f]
	[,else (error 'wsquery->text "bad additional arguments: ~s" mode)]))
    (match prog
      [(,lang (quote (program ,expr (struct-defs ,struct-defs ...) ,meta* ... ,typ)))
       
       (define uniondefs (cdr (ASSERT (assq 'union-types meta*))))

       
    ;; This processes an expression along the stream-processing "spine".
    ;; .param name   A string naming the variable that stores the current result.
    ;; .param type   Type of current result.
    ;; .param x      The query construct to process.
    ;; .returns 3 values: A new expression, a set of declarations, wsq declarations
    (define (Query name typ x tenv)
      ;; Coercion:
      (if (symbol? name) (set! name (sym2str name)))
      (match x

	[(__foreign . ,_) (values (ForeignEntry name (cons '__foreign _)) () ())]

	;; Anything else that's not of stream type, make global:
	[,e (guard (not (distributed-type? typ)))
	    ;; Here we put the binding at the top level but initialize in the MAIN function:
	    (values ((Value tenv) name "" e) ;; No type passed.  Mutate rather than bind.
		    (list (Type typ)" "name";\n")
;		    `(("\n" ,(Type typ) ;(Type (recover-type e tenv))
;		       " ",name " = " ,((Value tenv) e) ";\n"))
                    '())]




	;; An alias:
	[,e (guard (symbol? e))
	    ;; UH, not an expression:
	    (values `(,(Type typ)" " ,name " = " ,(sym2str e) ";\n")
                    ()
                    ())]

	;; Only do let for now.
#;
	[(,letsym ,binds ,bod) (guard (memq letsym '(let )))
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


	;; [2007.07.20] Now allowing arbitrary initialization code.
	;; And therefore, we handle begin's here:
#;
	[(begin ,[(lambda (e) ((Block tenv) #f #f e)) -> e*] ... ,[q])
	 (inspect "GOINGOINGINIGINGINIG")
	 (values ?????????)]

	[(let ([,lhs ,ty ,rhs]) ,bod)
	   (let ([newenv (tenv-extend tenv (list lhs) (list ty))])
              (let-values ([(stmt decl wsq)              (Query lhs ty rhs tenv)])	
	      (let-values ([(bodstmts boddecls wsqdecls) (Query name typ bod newenv)])
		  (ASSERT list? bodstmts)
		  (values (cons stmt bodstmts)
			  (cons decl boddecls)
			  (cons wsq wsqdecls)))))]

	[(iterate ,annot ,let-or-lambda ,sig)
	 ;; Program better have been flattened!!:
	 (ASSERT (symbol? sig))	  
	 (let* ([parent (Var sig)]
		[class_name `("Iter_" ,name)]

		;; First we produce a few lines of text to construct and connect the box:
		[ourstmts (case scheduler
                  [(default-scheduler train-scheduler depth-first)
                   `("WSBox* ",name" = new ",class_name "(" ");\n"
                     ,name"->connect(",parent");\n")]
                  [(corefit-scheduler-ex corefit-scheduler-df)
                   `("WSBox* ",name" = new ",class_name "(" ");\n"
                     "query.addOp(",name");\n"
                     ,name"->setCPU(0);\n" ; FIXME: use params. file
                     "query.connectOps(",parent", ",name");\n\n")])]

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


        ; FIXME: for now, just ignore the data rates
        [(data-rate ,r ,e) (Query name typ e tenv)]

		       
	[(assert-type (Stream (Sigseg ,[Type -> ty])) 
		      (prim_window ,sig ,[Simple -> size]))
	 (define new-class-name (Var (unique-name 'Window)))
	 (ASSERT symbol? sig)
	 (values `("WSBox* ",name" = new WSBuiltins::Window(",size", sizeof(",ty"));\n"
		   ,name"->connect(",(sym2str sig)");\n")
		 (if static-linkage
		     '()
		     ;; Otherwise we need to make a wrapper that parameterizes this window:
		     `("class ",new-class-name" : public WSBuiltins::Window {\n"
		       "  public:\n"
		       "  ",new-class-name"() : Window(",size", sizeof(",ty")) {}\n"
		       "}\n\n")
		     )
                 `(("op \"",name"\" \"",new-class-name"\" \"query.so\"\n")
		   ("connect \"",(sym2str sig)"\" \"",name"\"\n")))]


	 ;(ENSBoxAudio      (Int Int Int Int) (Stream (Sigseg Int16)))
	[(ensBoxAudioF ',[number->string -> ch])
;	 (ASSERT zero? overlap)
	 (add-include! "<ENSBox.hpp>")
;	 (add-link!  "libensbox_ws.so")
	 (values `("WSBox* ",name" = (WSBox*) new ENSBoxSource<wsfloat_t>(",ch");\n") () ())]
	[(ensBoxAudio ',[number->string -> ch])
;	 (ASSERT zero? overlap)
	 (add-include! "<ENSBox.hpp>")
;	 (add-link!  "libensbox_ws.so")
	 ;(values `("ENSBoxSource<wsint16_t> ",name" = ENSBoxSource<wsint16_t>(",ch");") () ())
	 (values `("WSBox* ",name" = (WSBox*) new ENSBoxSource<wsint16_t>(",ch");\n") () ())
	 ]

	;; Produces an instance of a generic dataFile reader.
	;; TODO: It's not necessary to generate code for both text & binary in the same run:
	[(__readFile ,annot
                ,[Simple -> file] 
                ,source
                ,[Simple -> mode] 
                ,[Simple -> repeats] 
                ',skipbytes 
                ,[Simple -> offset] 
                ',winsize 
                ',types_ignored)
	 (ASSERT symbol? source)
	 (let-match ([(Stream ,thetype) typ])
	   (let* (
		  ;; Do we output a struct of values or just a single value:
		  [structoutput? (match thetype 
				   [(Struct ,name) name] 
				   [(Sigseg (Struct ,name)) name] 
				   [,else #f])]
		  [tuptype (match thetype
			     [(Struct ,structname) `("struct ",(sym2str structname))]
			     [(Sigseg ,[t]) t]
			     [,other (ASSERT (not (eq? other 'String)))(Type other)])]
		  [classname (sym2str (unique-name 'WSDataFileReader))]
		  [types (if structoutput?
			     (map cadr (cdr (ASSERT (assq structoutput? struct-defs))))
			     (match thetype
			       [(Sigseg ,t) (list t)]
			       [,oth        (list oth)]))]
		  [numstrings (length (filter (lambda (s) (eq? s 'String)) types))]

		  [textmodereader
		   `(,(if (> winsize 0) "// ERROR //" "") ;; For now we don't handle windowed reading.
		     "status = fscanf(_f, \""
			     ,(insert-between " "
				 (map (lambda (ty)
					(match ty
					  [Float  "%f"] ;; Single precision floats
					  [Double "%f"] ;; Double precision floats
					  [Int    "%d"]
					  [Int64  "%lld"]
					  [Int16  "%hd"]
					  [String "%s"]					  
					  ))
				   types))
			     "\", "
			     ,(if structoutput?
				  (insert-between ", "
				    (let loop ([n 1]
					       [flds (map sym2str
						       (list-head standard-struct-field-names (length types)))]
					       [types types])
				      (if (null? types) '()
					  (match (car types)
					    ;; These are the types that can be filled by scanf directly:
					    [,s (guard (memq s '(Int Int16 Int64 Float Double)))
						(cons `("&(tup.",(car flds)")") (loop n (cdr flds) (cdr types)))]
					    ;; TODO: Complex numbers are not handled!
					    [String (cons (format "str~a" n) (loop (add1 n) (cdr flds) (cdr types)))]
					    ))))
				  ;; Otherwise the output is just a single value:
				  "&tup"
				  #;
				  (if (> winsize 0)
				      "tup" ;; It's an array already.
				      "&tup")
				  )
			     ");\n")]


		[maintext ;; A big class definition.
		 (list
		  (block (list "class " classname " : public WSBox")
		   (list "public:\n"
		   ;; First the constructor:
		   (block 
		    (list classname "(wsstring_t path, wsstring_t mode, wsint_t repeats)")
		    ;(list classname "(wsstring_t path, WSSource *ignored_source, wsstring_t mode)")
		    `("_f = fopen(path.c_str(), binarymode ? \"rb\" : \"r\");\n"
		      "binarymode = (mode == string(\"binary\"));\n"
		      "if (_f == NULL) {\n"
		      "  chatter(LOG_CRIT, \"Unable to open data file %s: %m\", path.c_str());\n"
		      "  abort();\n"
		      "}\n"
		      ;;"Launch();\n"
		      "fseek(_f, ",offset", SEEK_SET);\n"
		      ,(if (> winsize 0) "sampnum = 0;\n" "")
		      )
		    )
		   ;; "\n  DEFINE_SOURCE_TYPE("(if (> winsize 0) "RawSeg" tuptype)");\n"
		   ;"DEFINE_OUTPUT_TYPE("(if (> winsize 0) "RawSeg" tuptype)");\n"
         "DEFINE_OUTPUT_TYPE("(if (> winsize 0) (format "SigSeg< ~a >" tuptype) tuptype)");\n"

	          ;; Then some private state:
		   "\nprivate:\n"
		   "  FILE* _f;\n"
		   "  bool binarymode;\n"
		   (if (> winsize 0) "  int sampnum;\n" "")

		  ;; Then the main method that drives it:
		  (block "bool iterate(uint32_t port, void *item)"
		    (list		
		     ;(if (> winsize 0) "int sampnum = 0;\n" "")
		     ;"fseek(_f, "offset", SEEK_SET);\n"
		     (list ;block "while (!Shutdown())"
			    `(,(if (> winsize 0)
                    `("SigSeg< ",tuptype" > storage(Unitless, sampnum, ",(number->string winsize)", DataSeg);\n"
                      ,tuptype"* buf = storage.getDirect(0, ",(number->string winsize)");\n")
                    
                    ; FIXME: remove this
                    #;
                    `("RawSeg storage(sampnum, ",(number->string winsize)
                      ", DataSeg, 0, sizeof(",tuptype"), Unitless, true);\n"
                      "Byte* buf;\n"
                      "storage.getDirect(0, ",(number->string winsize)", buf);")

				   `(,tuptype" tup;\n"))
			      
			      ,(map (lambda (i) 
				      (list 
				       "// Nasty hack to handle string fields separately, since we're not using C strings:\n"
				       "// Cap of a 100 on length of strings read in:\n"
				       (format "char str~a[100];\n" i)))
				 (iota 1 numstrings))
			      "int status = 0;\n"
			      ,(block "if (!binarymode)"
				      ;; Text mode is more complicated:
				      textmodereader)
			      ,(block "else"			    
				      `("// The binary format of tuples matches that in the file:\n"
					,(let ([readcmd 
                       (lambda (n dest)
                         `("status += fread((void*)",dest
                           ",sizeof(",tuptype"), ",(number->string n)",_f);\n"))])
					   (if (> winsize 0)
					       (if (> skipbytes 0)
                          ;; Have to interleave reading and skipping forward:
                          (block `("for (wsint_t i=0; i<",(number->string winsize)"; i++)")
                                 (list (readcmd 1 `("(buf+i)"))
                                       "fseek(_f, "(number->string skipbytes)", SEEK_CUR);\n"))
                          ;; Otherwise can do one read:
                          (readcmd winsize "buf"))
					       (readcmd 1 "&tup")))
					))
			      
			      ;; Now with that nasty scanf finished we still
			      ;; have to put the strings into the right fields:
			      ,(map (lambda (n fld ty)
				      (if (eq? ty 'String)
					  (format "tup.~a = str~a;\n" fld n)
					  '()))
				 (iota 1 (length types))
				 (list-head standard-struct-field-names (length types))
				 types)
			      
			      ,(block `("if (status != ",(number->string (max 1 winsize))
					" * (binarymode ? 1 : ",(number->string (length types))"))")
				      '("chatter(LOG_WARNING, \"dataFile EOF encountered (%d).\", status);\n"
					"WSSched::stop();\n"
					"return true;\n"))
					;"t.time = (uint64_t)(time*1000000);\n"
			      ,(if (> winsize 0)

                    `("storage.release(buf);\n"
                      "emit(storage);\n"
                      "storage = SigSeg< ",tuptype" >(Unitless, sampnum, ",(number->string winsize)", DataSeg);\n"
                      "sampnum += ",(number->string winsize)";\n")

                    #;
                    `("storage.release(buf);\n"
                                        ;"source_emit(storage);\n"
                      "emit(storage);\n"
                      "storage = RawSeg(sampnum, ",(number->string winsize)
                      ", DataSeg, 0, sizeof(",tuptype"), Unitless, true);\n"
                      "sampnum += ",(number->string winsize)";\n")
                                        ;"source_emit(tup);\n"

                    "emit(tup);\n")
			      ))
		     "return true;")
		    ))) ";")])

	   (DEBUGASSERT text? maintext)
	 ;; This is the code that actually builds a dataFile reader object:
	 (values
     (case scheduler
       [(default-scheduler train-scheduler depth-first)
        `("WSBox* ",name" = new ",classname"(",file", ",mode", ",repeats");\n"
          " ",name"->connect(",(sym2str source)");\n")]
       [(corefit-scheduler-ex corefit-scheduler-df)
        `("WSBox* ",name" = new ",classname"(",file", ",mode", ",repeats");\n"
          "query.addOp(",name");\n"
          ,name"->setCPU(0);\n"
          "query.connectOps(",(sym2str source)", ",name");\n\n")])
	  (list maintext)
          `())))]
	
	;; This is purely hackish... should use the zip library function.	
#;
	[(zip2 ,s1 ,s2)
	 (ASSERT symbol? s1)
	 (ASSERT symbol? s2)
	 (let* ([t1 (recover-type s1 tenv)]
		[t2 (recover-type s2 tenv)]
		[ty (format " ~a, ~a " (Type t1) (Type t2))])
	   `(" Zip2<",ty"> ",name" = Zip2<",ty">();"
	     " ",name".connect(",(sym2str s1)"); "
	     " ",name".connect(",(sym2str s2)"); " )	   
	   )
					;  /* zip2 */
					;  Zip2<SigSeg<float>,float> z=Zip2<SigSeg<float>,float>();
					;  z.connect(&rw1);
					;  z.connect(&ms);	 
	 ]

	;; UNOPTIMIZED: should combine with the downstream iterate.
	;; Wire these all to our iterate.
	[(assert-type (Stream (Struct ,tupname)) (unionN ,annot ,inputs ...))
	 (ASSERT (not (null? inputs)))
	 (ASSERT (andmap symbol? inputs))
	 
	 (let ([ty (Type (match (recover-type (car inputs) tenv)
			   [(Stream ,t) t]))]
	       [classname (format "UnionN~a" (unique-name ""))])
	   (values

       (case scheduler
         [(default-scheduler train-scheduler depth-first)
          `(" WSBox* ",name" = new ",classname"();"
            ;; Order is critical here:
            ,(map (lambda (in) `(" ",name"->connect(",(sym2str in)"); ")) inputs))]
         [(corefit-scheduler-ex corefit-scheduler-df)
          `(" WSBox* ",name" = new ",classname"();\n"
            "query.addOp(",name");\n"
            ,name"->setCPU(0);\n" ; FIXME: use params. file
            ;; Order is critical here:
            ,(map (lambda (in) `("query.connectOps(",(sym2str in)", ",name");\n")) inputs))])

	    ;; Decls:
	    `("
  /* This takes any number of connections, they must be connected in order. */
  class ",classname" : public WSBox{ 
   
    private:
    DEFINE_OUTPUT_TYPE(",(sym2str tupname)");
    
    bool iterate(uint32_t port, void *item)
    {
      emit(",(sym2str tupname)"((wsint_t)port, *(",ty"*)item));      
      return true;
    }
  };
")
            ;; wsq decls:
            `()))]


	;; UNOPTIMIZED: Should combine with downstream iterate.
	[(_merge ,annot ,left ,right)
	 (ASSERT symbol? left)
	 (ASSERT symbol? right)
	 
	 (let ([ty (Type (match (recover-type left tenv)
			   [(Stream ,t) t]))]
	       [classname (format "Merge~a" (unique-name ""))])
	   (values
       (case scheduler
         [(default-scheduler train-scheduler depth-first)
          `(" WSBox* ",name" = new ",classname"();"
            (" ",name"->connect(",(sym2str left)"); ")
            (" ",name"->connect(",(sym2str right)"); "))]
         [(corefit-scheduler-ex corefit-scheduler-df)
          `("WSBox* ",name" = new ",classname"();\n"
            "query.addOp(",name");\n"
            ,name"->setCPU(0);\n"
            "query.connectOps(",(sym2str left)",",name");\n"
            "query.connectOps(",(sym2str right)",",name");\n\n")])

	    ;; Decls:
	    `("
  /* This takes any number of connections, they must be connected in order. */
  class ",classname" : public WSBox{ 
   
  private:
    DEFINE_OUTPUT_TYPE(",ty");

    
    bool iterate(uint32_t port, void *item)
    {
      emit(*(",ty"*)item);
      return true;
    }
  };
")
            `()))]



	;; UNOPTIMIZED: should combine with the downstream iterate.
	;; Wire these all to our iterate.
	[(timer ,annot ,[Simple -> period])
	 ;; HACK, we use the "setBatchSize" version for timing queries if "-t" was given.
	 ;; This only makes sense if the query has a SINGLE timer as its data source.
	 (define classname (if (wsint-time-query) "MagicPullTimer" "Timer"))
	 (values 
	  (case scheduler
	    [(default-scheduler train-scheduler depth-first)
	     `("WSSource * ",name" = new WSBuiltins::",classname"(",period");\n")]
	    [(corefit-scheduler-ex corefit-scheduler-df)
	     `("WSSource* ",name" = new WSBuiltins::",classname"(",period");\n"
	       "query.addOp(",name");\n"
	       ,name"->setCPU(0);\n\n")])
	  '()
	  '() ;;TODO, FIXME: wsq decls
	  )]

	[(assert-type ,t ,[q1 q2 q3]) (values q1 q2 q3)]

	;; These just do NOTHING for now:
	[(gnuplot_sigseg_stream   ,[a b c]) (values a b c)]
	[(gnuplot_sigseg_stream2d ,[a b c]) (values a b c)]
	[(gnuplot_array_stream    ,[a b c]) (values a b c)]
	[(gnuplot_array_stream2d  ,[a b c]) (values a b c)]

	[(spawnprocess ,[Simple -> cmd] ,[a b c])
	 ;; FIXME: TEMPTOGGGLE FOR NOW WE DO NOTHING
	 (values a b c)]

	[,other (error 'wsquery->text:Query "unmatched query construct: ~s" other)]
	)) ;; End Query


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
      [(deref ,v) (guard (symbol? v)) (if name (wrap (Var v)) "")]

      [(quote ,c)             (if name ((Value tenv) name type `',c) "")]
      [(tuple)                (if name ((Value tenv) name type '(tuple)) "")]
      
      [(let ([,v ,ty ,rhs]) ,bod)
       (list
	((Value tenv) (sym2str v) (Type ty) rhs)
	((Block (tenv-extend tenv (list v) (list ty))) name type bod))]

      [(begin ,[e]) e]
      [(begin ,e1 . ,e*)
       (list
	((Block tenv) #f #f e1)
	((Block tenv) name type `(begin . ,e*)))]
      ;; Both void value:
      [(begin) (if name (wrap "0") "")]

      [(set! ,[Var -> v] ,[Simple -> e]) (ASSERT not name)
       `(,v " = " ,e ";\n")]
      
      [(for (,i ,[Simple -> st] ,[Simple -> en]) ,bod) (ASSERT not name)
       (let ([istr (Var i)])	   
	 (block `("for (wsint_t ",istr" = ",st"; ",istr" <= ",en"; ",istr"++)")
		((Block (tenv-extend tenv (list i) '(Int))) #f #f bod)))]
      [(while ,tst ,[bod]) (ASSERT not name)
       ;; Just because of the way our code generator is set up, we generate this ugly code:
       (block `("while (1)") 
	      (list 
	       ((Block tenv) "grosshack" "wsbool_t" tst)
	       "if (grosshack) {\n"
	       (indent bod "  ")
	       "} else break; \n"
	       ))]

      ;; [2007.10.12] DUPLICATED!!!! FIXME NEED TO REFACTOR:
      [(wscase ,[Simple -> x] ((,tag* . ,tc*) (lambda (,v*) (,ty*) ,bod*)) ...)
       (list (if name (make-decl type name) "")
	     (block `("switch (",x".tag)")
		    (map (lambda (tc v ty bod)
			   (define TC (sym2str tc))
			   `("case ",TC": {\n"
			     ;; Simply bind a reference to the struct:
			     ,(make-decl (Type ty) (sym2str v) (list x ".payload." TC) 'reference)
			     ,(indent ((Block (tenv-extend tenv (list v) (list ty))) name "" bod) "  ")
			     "  }  break;\n"))
		      tc* v* ty* bod*)
		    ))]

      ;; Deprecated:
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
      ;[(gnuplot_array ,a) ""]

      [(hashset_BANG ,[Simple -> container] ,[Simple -> ind] ,[Simple -> val])
       (ASSERT not name)
       `("(*",container ")[" ,ind "] = " ,val ";\n")]

      [(Array:set (assert-type (Array ,[Type -> ty]) ,[Simple -> arr]) ,[Simple -> ind] ,[Simple -> val])
       `("((",ty" *)",arr "->data)[" ,ind "] = " ,val ";\n")]

      ;; Can't normalize-context this because of it's forall a.a return type:
      [(wserror ,str)
       (list (Prim `(wserror ,str) #f #f) ";\n")]
      [(foreign-app . ,x) (wrap (ForeignApp x))]
      [,oth (error 'emitC:Block "unhandled: ~s" oth)]
      )))

; ======================================================================
;; Expressions.
	
    (define (Value tenv)
      (lambda (name type exp)
	(define (wrap x) (list type " " name " = " x ";\n"))
	(ASSERT name)
	(match exp

	  ;; Special Constants:
	  [(assert-type ,t nullseg)    (wrap (PolyConst 'nullseg t))]
	  [(assert-type ,t Array:null) (wrap (PolyConst 'Array:null t))]
	  [(assert-type ,t '())        (wrap (PolyConst '() t))]
;	  ['()                         (wrap (PolyConst '() t))]

	  [nulltimebase                (Const name type 'nulltimebase)]
	  
	  [,missed (guard (member missed '(nullseg Array:null '())))
		   (error 'emitC:Value "a polymorphic constant didn't have a type ascription: ~s" missed)]

	  ;[,c (guard (simple-constant? c)) (Const c)]
	  [(quote ,datum)           (Const name type datum)]
	  [(tuple)                  (wrap (Simple '(tuple)))]

	  ;; [2007.07.31] TEMP: Need to refactor this whole pass to use explicit-stream-wiring
	  [(begin ,[(lambda (e) ((Block tenv) #f #f e)) -> stmt*] ... ,[val])
	   (list stmt* val)]

	  [,v (guard (symbol? v))
	      (ASSERT (compose not regiment-primitive?) v)
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


	  [(cast-variant-to-parent ,[sym2str -> tc] ,[Type -> ty] 
				   (assert-type ,[Type -> variant-ty] ,e))
	   (let ([newvar (sym2str (unique-name 'sumbld))])
	     ;; First build the variant itself:
	     (list ((Value tenv) newvar variant-ty e)
		   ;; Build the parent container:
		   (make-decl ty name)
		   ;; Next store the result within the parent union type.
		   name".payload."tc" = "newvar";\n"
		   ;; And store the tag:
		   name".tag = "tc";\n"
		   ))]

	  [(wscase ,[Simple -> x] ((,tag* . ,tc*) (lambda (,v*) (,ty*) ,bod*)) ...)
	   (ASSERT name) (ASSERT type)
	   (list (make-decl type name)
		 (block `("switch (",x".tag)")
			(map (lambda (tc v ty bod)
			       (define TC (sym2str tc))
			       `("case ",TC": {\n"
				 ;; Simply bind a reference to the struct:
				 ,(make-decl (Type ty) (sym2str v) (list x ".payload." TC) 'reference)
				 ,(indent ((Block (tenv-extend tenv (list v) (list ty))) name "" bod) "  ")
				 "  }  break;\n"))
			  tc* v* ty* bod*)
			))]

#;
;; WON'T WORK!!
	  [(cast-variant-to-parent ,ty ,[e])
	   (let ([ty (match ty [(Struct ,name) (Type ty)])])
	     ;; Hmm: does this copy it?
	     `("(*((",ty"*)&(",e")))"))]

	  ;; This is needed just for the RHS's of iterator state variables.
	  [(let ([,v ,ty ,rhs]) ,bod)
	   (list
	    ((Value tenv) (sym2str v) (Type ty) rhs)
	    ((Value (tenv-extend tenv (list v) (list ty))) name type bod))]

	;; Forming tuples.
	[(make-struct ,name ,[Simple -> arg*] ...)
	 (wrap `(,(sym2str name)"(",(insert-between ", " arg*)")"))]
	;; Referencing tuples.
	[(struct-ref ,[Simple -> x] ,fld)
	 (wrap `("(",x "." ,(sym2str fld)")"))]

	; ============================================================
	[(,prim ,rand* ...) (guard (regiment-primitive? prim))
	 (Prim (cons prim rand*) name type)]
	[(assert-type ,t (,prim ,rand* ...)) (guard (regiment-primitive? prim))
	 ;(printf "\nANNOTATED PRIM: ~s\n" prim)
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

	[(foreign-app . ,x) (wrap (ForeignApp x))]
	[,unmatched (error 'emitC:Value "unhandled form ~s" unmatched)])
	))

;; Generate code for a "foreign" application.  Not really foreign since we're hosted in C++
(define ForeignApp
  (lambda (ls)
    (match ls 
      [(',realname (assert-type ,type ,rator) ,[Simple -> rand*] ...)
       (ASSERT (symbol? rator))
       (match type
	 [(,argty* ... -> ,result)
	  (FromForeignType result 
			   `(,realname "(" ,@(insert-between ", " 
							     (map ToForeignType argty* rand*)) ")"))
	  ])])))

;================================================================================


(define (UnionDef def)
  (match def
    [((,[sym2str -> name]) (,[sym2str -> tc*] ,[Type -> ty*]) ...)
     (let ([tagty (Type tag-type)])
       (list (block `("struct ",name)
	      (list
	       tagty" tag;\n"
	       (block "union"
		      (map (lambda (tc ty) (list ty " " tc ";\n"))
			tc* ty*)
		      )
	       " payload;\n"))
	     ";\n"
	     (block (list "enum "name"_enum")
		    (list (insert-between ", " tc*) "\n"))
        ";\n"
        "uint32_t getTotalByteSize(const struct "name" &e) {\n"
        "  return sizeof(e.tag) + sizeof(e.payload);\n"
        "}\n"))
     ]))

;; This produces a struct definition as well as a printer function for the struct.
(define (StructDef entry)
     ;(printf "StructDef entry is: ~n") (pretty-print entry) (printf "~n")
     (match entry
       [(,(sym2str -> name) (,[sym2str -> fld*] ,typ*) ...)
	(let ([tmpargs (map (lambda (_) (sym2str (unique-name 'tmp))) fld*)]
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
				))
            ;; Corefit byte accounting:
            ;,(if (or (eq? scheduler 'corefit-scheduler-ex) (eq? scheduler 'corefit-scheduler-df))
            ;     "uint32_t getTotalByteSize() const { return sizeof(*this); }\n"
            ;     "")
            ))
	    ";\n"

            ;; size provider, for corefit
            ;; FIXME: deal with variable sizes!
            ,(if (or (eq? scheduler 'corefit-scheduler-ex) (eq? scheduler 'corefit-scheduler-df))
                 (let* ([make-term-str (lambda (fld) (format "getTotalByteSize(e.~a)" fld))]
                        [sum-string 
                         (foldr (lambda (term sum) (format "~a + ~a" (make-term-str term) sum))
                                (make-term-str (car fld*)) (cdr fld*))])
                   (string-append
                    "uint32_t getTotalByteSize(const struct "name" &e) {\n"
                    "  return "sum-string";\n"
                    "}\n"))
                 "")
            
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


;; [2007.11.01] THIS CAN HOPEFULLY BE DISABLED WITH generate-comparison-code ACTIVATED:

            ;; This produces an equality function. 
	    "bool wsequal(const ",name"& x, const ",name"& y) {\n" 
	    ,(if (null? fld*)
		"return TRUE;\n"
		`(" return "
		  ,(insert-between " && "
				   (map (lambda (fld ty)
					  ;`(" wsequal(x.",fld", y.",fld") ")
					  (generate-wsequal-code ty `("x.",fld) `("y.",fld) (lambda (x) x))
					  )
				     fld* typ*))
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
  (define default `(,tstr " " ,outname " = *((",tstr"*) ",inname");\n"))
  `("/* Naturalize input type to meet expectations of the iterate-body. */\n"
    ,(match type
    ;; The input is passed into the body as a reference.
    ;; All other sigseg variables in the body are by value.
    [(Sigseg ,t) 
     `("/* Sigseg input.  This is a bit of a trick, uses ref rather than value: */\n"
       ,tstr "& " ,outname " = *(",tstr"*) ",inname";\n")]
    ;; Immediates are passed by value.
    [,imm (guard (or (immediate-type? imm) 
		     (equal? imm '#())
		     (eq? imm 'String)))
	  default]
    
    [(Union ,tyname)  default] ;; double check these
    [(Struct ,tyname) default] ;; double check these

    ;; Currently let's just not let you pass sigsegs in lists!
    [(List ,t)
     `(,tstr " " ,outname " = *((",tstr"*) ",inname");\n")]

    [(Array ,t)
     `(,tstr " " ,outname " = *((",tstr"*) ",inname");\n")
     ]    
    
	  
    ;; TODO ARRAYS:    
    [,other (error 'naturalize "Can't naturalize as box input type: ~s" other)]
  )))


;; Print the output from the entire query.
(define (make-output-printer typ)
  (match typ
    [(Stream ,typ)
     (let ([T (Type typ)])
       `("\n\n"
	 ,(block "class PrintQueryOutput : public WSBox"
	 `("public:\n"
	   "PrintQueryOutput(const char *name, int maxTuples) "
	   " : WSBox(\"PrintQueryOutput\"), maxTuples(maxTuples), curTuples(0) "
	   " {}\n\n"
	   "private:\n"
	   "  int maxTuples;\n"
	   "  int curTuples;\n"
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

			" curTuples++;\n"
			;" if (curTuples == maxTuples) { printf(\"Tuple limit hit.  Stopping query.\\n\"); WSSched::stop();} \n"
			" if (curTuples == maxTuples) { "
			"   chatter(LOG_WARNING, \"Tuple limit hit.  Stopping query.\"); "
			;"   WSSched::stopnow(); } \n"
			"   exit(0); } \n" ;; Trying this instead.
			

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
			   ;,(Type vqT)" ",(Var vq)" = 0;\n"
			   ;; Hardcoding this as void*:
			   "void* ",(Var vq)" = (void*)0;\n"
			   
			   ,body
			   
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
    `(,(boilerplate_premain scheduler)
      ;;"// " ,(Type typ) " toplevel;\n"
      ,(indent body "  ")
      ,(boilerplate_postmain scheduler (Var 'toplevel) typ)))

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
  ;; Main body:
  ;; Here we stitch together the file out of its composite bits.

  (ASSERT (or (eq? scheduler 'default-scheduler)
	      (eq? scheduler 'depth-first) ;; Old scheduler
              (eq? scheduler 'train-scheduler) ;; Old scheduler
              (eq? scheduler 'corefit-scheduler-ex)
              (eq? scheduler 'corefit-scheduler-df)))

  (fluid-let ([include-files ()]
	      [link-files    ()])
    (let-values ([(body funs wsq) (Query "toplevel" typ expr (empty-tenv))])
    ;; This is a lame hack to distinguish system libraries.
    (trace-define (extract-lib-name fn)
      (let ([ext (extract-file-extension fn)]
	    [sansext (remove-file-extension fn)])
	(let-values ([(base _) (split-before (curry string=? "so") (string-split fn #\.))])
	  (let ([base (apply string-append (insert-between "." base))])
	    ;; Take the shorter one:
	    (set! base (if (< (string-length base) (string-length sansext)) base sansext))
	    (and (or (equal? ext "so") (equal? ext "dylib")
		     (substring? ".so." fn))
		 (> (string-length base) 3)
		 (equal? "lib" (substring base 0 3))
		 (substring base 3 (string-length base))
		 )))))
    (define header
      (list "//WSLIBDEPS: "
	    (map (lambda (fn) 
		   (let ([lib (extract-lib-name fn)])
		     (if lib (list " -l" lib) fn)))
	      link-files)
	    "\n"
            (file->string (++ (REGIMENTD) "/src/linked_lib/WSHeader.hpp"))
	    (file->string (++ (REGIMENTD) "/src/linked_lib/WSTypedefs.hpp"))
	    
	    ;; After the types are declared we can bring in the user includes:
	    "\n\n" (map (lambda (fn) `("#include ",fn "\n")) 
		     (reverse include-files)) "\n\n"

            (file->string (++ (REGIMENTD) "/src/linked_lib/WSPrim.cpp"))
            "\n/* These structs represent tuples in the WS program. */\n"
            (map StructDef struct-defs)
            (map UnionDef uniondefs)
            funs
            (make-output-printer typ)))
    
    (unless (regiment-quiet)
      (printf "WSQ connection graph:\n")
      (newline)(display (text->string wsq))(newline)(newline))
    ;(break)

    (if static-linkage 
        (snoc (build-main body) header)
        (vector header wsq))
    ))
#;    
  (if static-linkage querycode
      (vector querycode (build-wsq "toplevel" expr "query.so")))
    ]

  [,other ;; Otherwise it's an invalid program.
   (warning 'wsquery->text "ERROR: bad top-level WS program: ~s" other)
   (inspect other)
   (error 'wsquery->text "")]))



) ; End wsquery->text


;;; THESE ALL LIVE OUTSIDE wsquery->text:
; ======================================================================
;;; Helper functions for handling different program contexts:
        
    (define (Var var)
      (ASSERT (symbol? var))
      ;; This is the place to do any name mangling.  I'm not currently doing any for WS.
      (sym2str var))

      ;(sym2str var))
    (define (FunName var)
      (format "WSFunLib::~a" var))
      ;(sym2str var))


    (define Const
      (lambda (name type datum)
         (define (wrap x) (if name 
                              (list type " " name " = " x ";\n")
                              x))
	 ;; Should also make sure it's 32 bit or whatnot:
	 (cond
          [(eq? datum 'BOTTOM) (wrap "0")] ;; Should probably generate an error.
          [(eq? datum 'UNIT) (wrap (Simple '(tuple)))]
	  [(eq? datum #t) (wrap "TRUE")]
	  [(eq? datum #f) (wrap "FALSE")]       
	  [(string? datum) (wrap (format "string(~s)" datum))]

          ;; FIXME THIS WON'T HANDLE NON-PRINTING CHARACTERS YET!!
          [(char? datum) (wrap (format "'~a'" datum))]

          ;; Hacked this to handle NAN (not in a pretty way).
	  [(flonum? datum) 
            ;(printf "GOT FLOAT: ~a ~a \n" datum (or (eq? datum +nan.0) (eq? datum -nan.0)))
            (wrap (format "(wsfloat_t)~a" 
			  (if (not (= datum datum)) ;(or (eq? datum +nan.0) (eq? datum -nan.0))
			      "(0.0/0.0)" ;(inspect/continue datum);"(0.0/0.0)"
			      datum)					 
			  ))]
	  [(cflonum? datum) (wrap (format "(wscomplex_t)(~a + ~afi)" 
				    (cfl-real-part datum)
				    (cfl-imag-part datum)))]
	  [(eq? datum 'nulltimebase)  (wrap "WSNULLTIMEBASE")]
	  [(integer? datum) (wrap (format "(wsint_t)~a" datum))]
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

	  [else (error 'emitC:Const "not a C-compatible literal: ~s" datum)])))

    (define PolyConst 
      (lambda (datum ty)
	(match (vector datum ty)
	  (DEBUGASSERT (not (polymorphic-type? ty)))
	  [#(() (List ,t)) 	   
					;"NULL_LIST"
					;`("(cons<",(Type t)">::ptr)NULL_LIST")
					;`("(cons<",(Type t)">::null_ls)")
	   ;`("boost::shared_ptr< cons< ",(Type t)" > >((cons< ",(Type t)" >*) 0)")
      `("boost::shared_ptr< cons< ",(Type t)" > >((cons< ",(Type t)" >*) 0)")
	   ]
	  [#(nullseg ,t) "WSNULLSEG"]
          [#(Array:null (Array ,t))
            ;; A boosted null pointer.
	   `("boost::intrusive_ptr< WSArrayStruct< ",(Type t)" > >((WSArrayStruct< ",(Type t)" >*) 0)")
            ;`("wsarray_t(0)")
          ]
	  )))


    ;; TODO, Finish this:
    (define (make-zero-for-type ty) 
     (match ty
      [Int "0"] [Int16 "0"] [Int64 "0"]
      [Float "0.0"] [Double "0.0"]
      [Complex "0.0fi"]
      [Bool "0"]
;      [String "\"\""]
      [(Struct ,name) (format "~a()" name)]
      [(List ,elt)  (PolyConst '() `(List ,elt))]
      [(Array ,elt) (PolyConst 'Array:null `(Array ,elt))]
      [(Sigseg ,elt) (PolyConst 'nullseg ty)]))

    (define Simple
      (lambda (x)
	(match x 
          [(tuple) "((wsunit_t)0)"]
	  [(assert-type ,t '())  (wrap (PolyConst '() t))]
	  ['() (error 'Simple "null list without type annotation")]
	  [(quote ,c) (Const #f #f c)]
          [nulltimebase (Const #f #f 'nulltimebase)]

          [(deref ,var) (ASSERT (not (regiment-primitive? var))) (Var var)]
          [,v (guard (symbol? v)) (ASSERT (not (regiment-primitive? v))) (Var v)]
	  [(assert-type ,_ ,[x]) x]
	  [,else (error 'Simple "not simple expression: ~s" x)])))

;================================================================================

(define (ForeignEntry name form)
  (match form
    [(__foreign ',cname ',files ',ty)
     (match ty
       [(,argty* ... -> ,retty)	
	(let ([add-file!
	       (lambda (file)
		 ;; Add to global list of includes if it's not already there.
		 (let ([ext (extract-file-extension file)])
		   (cond
		    [(member ext '("c" "cpp" "h" "hpp"))
		     (add-include! (list "\"" file "\""))]
		    [(or (member ext '("so" "a" "o"))
			 ;; A hack:
			 (substring? ".so." file))
		     ;; Note: If you try to load a pre-compiled object, you must also provide a header!
		     (add-link! file)]
		    [else (error 'emit-c:foreign "cannot load C extension from this type of file: ~s" file)]))
		 )])
	  (for-each add-file! files)
	  ;;(list "\n#define "name" "cname"\n")

	  ;; We bind the user's name it to a null value. Then, later,
	  ;; we short-circuit the applications to refer directly to
	  ;; the cname.
	  (let ([ty (Type ty)]) `(,ty " ",name" = (",ty")0;\n"))
	  )])]
    [(__foreign . ,_)
     (error 'emit-c:ForeignEntry "unhandled foreign form: ~s" `(__foreign ,@_))]
    [,_ (error 'emit-c:ForeignEntry "non foreign form: ~s" form)]))

;================================================================================
;; Primitive calls:

;; This returns an block of code putting the result of the prim call in "name"
;; It takes the primapp expression, and a name to store the return value.
(define (Prim expr name type)
  (define (wrap x) (list type " " name " = " x ";\n"))
  ;; This is for primitives that correspond exactly to exactly one C call.
  (define (SimplePrim var)
    (define (fromlib v) (format "WSPrim::~a" v))
    (define (mangle v) (mangle-name (sym2str v)))
    ;; Handle special cases here.
    (case var
      [(not)         
       "!" ;(mangle 'wsnot)
       ] ; The name "not" makes g++ unhappy.
      ;; These are the same as their C++ names:
      [(cos sin tan acos asin atan max min) 
       (sym2str var)]
      [(absF absD absI absI16 absI64) "abs"]
      [(roundF)                 "round"]
      [(sqrtI sqrtF)            "sqrt"]
      [(sqrtC)                  (fromlib "csqrt")]
      
      [(toArray)                (fromlib "toArray")]
      
      ;; These use GSL and require appropriate includes.
      [(m_invert)
       (add-include! "<gsl/gsl_linalg.h>")
       (add-include! "<gsl/gsl_matrix.h>")
       ;; This is a bit inflexible.
       ;; Now the C++ query MUST be compiled on the same system that it's generated.  
       ;; Alternatively, we could inline the whole file right here.
       (add-include! (list "\"" (REGIMENTD)
		     "/src/linked_lib/GSL_wrappers.cpp\""))
       (add-link! "libm.so")
       (add-link! "libgsl.so")
       (add-link! "libgslcblas.so")
       (mangle var)]

      ;; These use FFTW
      [(fftR2C ifftC2R fftC ifftC)
       (add-include! "<fftw3.h>")
       (add-include! (list "\"" (REGIMENTD) 
			   "/src/linked_lib/FFTW_wrappers.cpp\""))
       (add-link! "libfftw3f.so")
       (mangle var)]

      ;; Currently the memoized version is not implemented for C++.
      [(memoized_fftR2C) (SimplePrim 'fftR2C)]

      ;; This is the "default"; find it in WSPrim:: class
      [(string-append 
        width start end joinsegs subseg toSigseg
        String:implode
        ;wserror ;generic_hash        
        )
       (fromlib (mangle var))]      

      [else (error 'emitC:Prim "primitive not specifically handled: ~s" var)]
      ))

  (match expr
    ;; First we handle "open coded" primitives and special cases:

    [(clock) (wrap "((double)clock() / 1000.0)")]

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
       (wrap `("(" ,left ,(format " ~a " cname) ,right ")")))]

	;[(realpart ,[v]) `("(" ,v ".real)")]
	;[(imagpart ,[v]) `("(" ,v ".imag)")]
	[(imagpart ,[Simple -> v])   (wrap `("__imag__ (" ,v ")"))]

	;; Wait... do we need to cast to double here?
	[(,extractreal ,[Simple -> v]) 
	 (guard (memq extractreal '(realpart complexToFloat complexToDouble)))
	 (wrap `("__real__ (" ,v ")"))]
	[(complexToInt   ,[Simple -> v])   (wrap `("wsint_t((int)__real__ (" ,v "))"))]
	[(complexToInt16 ,[Simple -> v])   (wrap `("wsint16_t((int16_t)__real__ (" ,v "))"))]
	[(complexToInt64 ,[Simple -> v])   (wrap `("wsint64_t((int64_t)__real__ (" ,v "))"))]

;; Makes gcc 3.4.3 barf:
;;	[(absC ,[Simple -> c]) (wrap `("abs((complex<float>)",c")"))]
	[(absC ,[Simple -> c]) (wrap `("WSPrim::CNorm(",c")"))]

	[(intToChar ,[Simple -> e]) (wrap `("(wschar_t)",e))]
	[(,toint     ,[Simple -> e]) 
	 (guard (memq toint '(int16ToInt int64ToInt floatToInt doubleToInt charToInt)))
	 (wrap `("(wsint_t)",e))]
	[(,toint16     ,[Simple -> e]) 
	 (guard (memq toint16 '(intToInt16 int64ToInt16 floatToInt16 doubleToInt16)))
	 (wrap `("(wsint16_t)",e))]
	[(,toint64    ,[Simple -> e]) 
	 (guard (memq toint64 '(int16ToInt64 intToInt64 floatToInt64 doubleToInt64)))
	 (wrap `("(wsint64_t)",e))]
	[(,tofloat  ,[Simple -> e]) 
	 (guard (memq tofloat '(intToFloat int16ToFloat int64ToFloat doubleToFloat)))
	 (wrap `("(wsfloat_t)",e))]
	[(,todouble  ,[Simple -> e]) 
	 (guard (memq todouble '(intToDouble int16ToDouble int64ToDouble floatToDouble)))
	 (wrap `("(wsdouble_t)",e))]

	;[(complexToInt16 ,e)   (wrap `("(wsint16_t)",(Prim `(complexToFloat ,e) #f "")))]
	;[(complexToInt ,e)     (wrap `("(wsint_t)"  ,(Prim `(complexToFloat ,e) #f "")))]
	;[(complexToFloat ,e)   (Prim `(realpart ,e) name type)]
	[(,ToComplex ,[Simple -> e])
	 (guard (memq ToComplex 
		      '(int16ToComplex int64ToComplex intToComplex floatToComplex doubleToComplex)))
    	 (wrap `("wscomplex_t(",e" + 0.0fi)"))]
	[(makeComplex ,[Simple -> re] ,[Simple -> im])
	 (wrap `("((wscomplex_t)(",re" + (",im" * 1.0fi)))"))]

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
	[(stringToDouble ,[Simple -> e]) 
	 (let ([tmp (Var (unique-name 'tmp))])
	   `("wsdouble_t ",tmp";\n"
	     "sscanf(",e".c_str(), \"%lf\", &",tmp");\n"
	     ,(wrap tmp)))]

	[(stringToComplex ,[Simple -> e]) 
	 (let ([tmp1 (Var (unique-name 'tmp))]
	       [tmp2 (Var (unique-name 'tmp))])
	   `("wsfloat_t ",tmp1";\n"
	     "wsfloat_t ",tmp2";\n"
	     ;"printf(\"STRING %s\\n\", ",e".c_str());\n"
	     "sscanf(",e".c_str(), \"%f+%fi\", &",tmp1", &",tmp2");\n"
	     ,(wrap `(,tmp1"+(",tmp2"*1.0fi)"))))]

#;
	[(,stringTo ,[Simple -> e]) 
	 (guard (memq stringTo '(stringToInt stringToFloat stringToComplex)))	
	 (let ([tmp (Var (unique-name 'tmp))]
	       [printfflag (case stringTo
			     [(stringToInt)     "%d"]
			     [(stringToFloat)   "%f"]
			     [(stringToComplex) "%f+%fi"])]
	       [type (case stringTo
		       [(stringToInt)     "wsint_t"]
		       [(stringToFloat)   "wsfloat_t"]
		       [(stringToComplex) "wscomplex_t"])])
	   `("wsint_t ",tmp";\n"
	     "sscanf(",e".c_str(), \"%d\", &",tmp");\n"
	     ,(wrap tmp)))]

	[(show (assert-type ,t ,[Simple -> e])) (wrap (EmitShow e t))]
	[(show ,_) (error 'emit-c:Value "show should have a type-assertion around its argument: ~s" _)]

	;; FIXME  FIXME FIXME FIXME FIXME FIXME FIXME FIXME 
	;; THIS HAS THE WRONG COMPLEXITY!!  It does a seg-get every time.
#;
	[(toArray (assert-type (Sigseg ,t) ,sigseg))
	 (let ([tmp (Var (unique-name 'tmp))]
	       [tmp2 (Var (unique-name 'tmp))]
	       [len (Var (unique-name 'len))]
	       [ss (Simple sigseg)]
	       [tt (Type t)])
	   `("int ",len" = ",ss".length();\n"
	     ;,type" ",name" = makeArrayUnsafe(",len", sizeof(",tt"));\n"
	     ,type" ",name" = makeArrayUnsafe(",len", (",tt")(",(make-zero-for-type t)"));\n"
	     ;,type" ",name" = makeArrayUnsafe(",len");\n"
	     "for(int i=0; i<",len"; i++) {\n"
	     "  ",(Prim `(seg-get (assert-type (Sigseg ,t) ,sigseg) i) tmp2 tt)
	     "  ((",tt" *)",name"->data)[i] = ",tmp2";\n"
	     "}\n"
	     ))]
	[(toSigseg (assert-type (Array ,[Type -> ty]) ,[Simple -> arr]) ,[Simple -> startsamp] ,[Simple -> timebase])
	 ;; type should be "RawSeg"
	 ;; FIXME: CURRENTLY DOESNT WORK FOR NULLARRAY!
    `("SigSeg< ",ty" > ",name"(Unitless, (SeqNo)",startsamp", ",arr"->len, DataSeg);
       {
         ",ty" *direct = ",name".getDirect(0, ",arr"->len);
         memcpy((void*)direct, ",arr"->data, sizeof(",ty") * ",arr"->len);
         ",name".release(direct); // FIXME: what should this be?
       }\n"
      )

    #;
	 `("
     RawSeg ",name"((SeqNo)",startsamp", ",arr"->len, DataSeg, 0, sizeof(",ty"), Unitless, true);
     { 
       Byte* direct;
       ",name".getDirect(0, ",arr"->len, direct);
       memcpy(direct, ",arr"->data, sizeof(",ty") * ",arr"->len);
       ",name".releaseAll();
     }\n")]

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
	[;(assert-type ,[Type -> ty] (Array:ref ,[Simple -> arr] ,[Simple -> ind]))
	 (Array:ref (assert-type (Array ,[Type -> ty]) ,[Simple -> arr]) ,[Simple -> ind])
	 (wrap `("((",ty" *)",arr "->data)[" ,ind "]"))]
	[(Array:make ,[Simple -> n] ,[Simple -> x])   (wrap `("makeArray(",n", ",x")"))]
	;; This version just doesn't initialize:

	[(Array:length ,[Simple -> arr])                  
	 ;; The boosted pointer could be null.  Maybe we should actually allocate something for the nulls.  Sigh.
	 (wrap `("(wsint_t)(",arr".get() ? ",arr"->len : 0)"))]
		
	[(assert-type (Array ,ty) (Array:makeUNSAFE ,[Simple -> n]))
	 (wrap `("makeArrayUnsafe(",n", (",(Type ty)")(",(make-zero-for-type ty)"));\n"
		 ;"makeArrayUnsafe(",n");\n"
		 ))]
#;
	[(assert-type (Array ,[Type -> ty]) (Array:makeUNSAFE ,[Simple -> n]))
	 ;; This is redundant with the body of makeArray
	 (let ([arr (Var (unique-name 'tmp))]
	       [len (Var (unique-name 'len))])
	   `(
	     "WSArrayStruct* ",arr" = new WSArrayStruct;\n"
	     ,arr"->len = (int)",n";\n"
	     ,arr"->data = malloc(sizeof(",ty") * (int)",n");\n"
	     ,arr"->rc = 0;\n"
	     ,(wrap `("wsarray_t(",arr")"))))]

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
	 (wrap `("cons< ",ty" >::ptr(new cons< ",ty" >(",a", (cons< ",ty" >::ptr)(",b")))"))]
	[(car ,[Simple -> ls]) (wrap `("(",ls")->car"))]
	[(cdr ,[Simple -> ls]) (wrap `("(",ls")->cdr"))]
	[(assert-type (List ,t) (List:reverse ,[Simple -> ls]))
	 (wrap `("cons< ",(Type t)" >::reverse(",ls")"))]
	[(assert-type (List ,[Type -> ty]) (List:append ,[Simple -> ls1] ,[Simple -> ls2]))
	 (wrap `("cons< ",ty" >::append(",ls1", ",ls2")"))]
	[(List:ref (assert-type (List ,t) ,[Simple -> ls]) ,[Simple -> ind])
	 (wrap `("cons< ",(Type t)" >::ref(",ls", ",ind")"))]
	[(List:length (assert-type (List ,t) ,[Simple -> ls]))
	 (wrap `("cons< ",(Type t)" >::length(",ls")"))]
	[(List:make ,[Simple -> n] (assert-type ,t ,[Simple -> init]))
	 (wrap `("cons< ",(Type t)" >::make(",n", ",init")"))]
	;; TODO: nulls will be fixed up when remove-complex-opera is working properly.

;; Don't have types for nulls yet:
;	[(null_list ,[Type -> ty]) `("cons< "ty" >::ptr((cons< "ty" >)0)")]

	;; Safety net:
	[(,lp . ,_) (guard (memq lp '(cons car cdr append reverse List:toArray
					   List:ref List:length makeList ))) 
	 (error 'emit-C:Value "bad list prim: ~s" `(,lp . ,_))
	 ]

	;; ----------------------------------------
	;; Hash tables:

	;; We should have the proper type assertion on there after flattening the program.
	;; (Remove-complex-opera*)
	[(assert-type (HashTable ,k ,v) (HashTable:make ,[Simple -> n]))
	 (let ([hashtype (HashType k v)]
	       ;[eqfun ]
	       [k (Type k)]
	       [v (Type v)])
	   (wrap `(,(SharedPtrType hashtype)"(new ",hashtype"(",n"))")))]
	[(HashTable:make ,_) (error 'emitC:Value "hashtable not wrapped in proper assert-type: ~s"
				    `(HashTable:make ,_))]
	[(HashTable:get ,[Simple -> ht] ,[Simple -> key])      (wrap `("(*",ht ")[",key"]"))]
	;; TEMP, HACK: NEED TO FIGURE OUT HOW TO CHECK FOR MEMBERSHIP OF A KEY!
	[(HashTable:contains ,[Simple -> ht] ,[Simple -> key]) (wrap `("(*",ht ")[",key"]"))]
	
	[(__foreign . ,_) (ForeignEntry name (cons '__foreign _))]

	;; Generate equality comparison:
	[(wsequal? (assert-type ,ty ,e1) ,[Simple -> b])	
	 (let* ([a (Simple `(assert-type ,ty ,e1))])
	   (generate-wsequal-code ty a b wrap))]

	;; If we have an extra assert-type... just ignore it.
	[(assert-type ,ty ,[e]) e]

	;; Other prims fall through to here:
	[(,other ,[Simple -> rand*] ...)
	 (wrap `(,(SimplePrim other) "(" ,(insert-between ", " rand*) ")"))
	 ;`(,(SimplePrim prim) "(" ,(insert-between ", " rand*) ")")
	 ])
  )


(define (generate-wsequal-code ty a b wrap)
  (define (simple) (wrap `("wsequal(",a", ",b")")))
  (match ty
    [Int          (simple)]
    [Int16        (simple)]
    [Int64        (simple)]
    [Float        (simple)]
    [Complex      (simple)] ;; does this work?
    [String       (simple)]
    [Char         (simple)]
    [Bool         (simple)]

    [Timebase         (simple)]
    ;; This is effectively physical equality:
    ;; Requires that they have the same parents.
    ;; Won't read the contents of two different Sigsegs...
    ;; FIXME: Should consider fixing this.
    [(Sigseg ,elt)  (simple)]
    ;; Could do this c++-ishly... but yuck.
    [(Array ,elt)   (simple)
     ;(error 'emit-C:Prim "shouldn't run into wsequal? on an array type")
     ]
    
    [(List ,elt)    (simple)]
					;[(List ,[Type -> t]) `("cons<",t">::lsEqual(NULL_LIST, ",a", ",b")")]

    ;; We have generated a comparison op for each struct.
    ;; UNFINISHED:
					;[(Struct ,name) `("eq",name"(",a", ",b")")]
    [(Struct ,name) (simple)]
    [,_ (error 'emitC "no equality yet for type: ~s" ty)]))


;; This implements our polymorphic print/show functions.
;; It prints something or other for any type.
;;
;; [2007.10.28] FIXME: Most of this should really be done in a previous pass.
;; Only basic scalar and string printing should be handled here.
(define (Emit-Print/Show-Helper e typ printf stream)
  (match typ
    [Bool           (printf "%s" (format "(~a ? \"true\" : \"false\")" e))]
    [Int            (printf "%d" (format "~a" e))]
    [Int16          (printf "%hd" (format "~a" e))]
    [Int64          (printf "%lld" (format "~a" e))]
    [Float          (printf "%f" (format "~a" e))]
    [Double         (printf "%lf" (format "~a" e))]

    [(Pointer ,_)   (printf "%p" e)]

    ;;[Complex        (stream `("complex<float>(",e")"))]
    [Complex        (printf "%f+%fi" `("__real__ (" ,e "), __imag__ (",e")"))]
    ;[Complex        (stream "\"<Cannot currently print complex>\"" )]
    ;[Complex        (stream `("complex<float>(__real__ ",e", __imag__ ",e")"))]

    ;[String         (printf "%s" `(,e".c_str()"))]
    [String         (stream e)]
    ;[(List ,t)      (stream e)]
    ;[(List ,t)      (stream (cast-type-for-printing `(List ,t) e))]

#;
    [(List ,t)
     (let* ([var (Var (unique-name 'ptr))]
	    [subprinter (Emit-Print/Show-Helper (list var"->car") t printf stream)])
       (list (stream "\"[\"")
	     (make-decl (Type `(List ,t)) var e)
	     (make-while (list "! IS_NULL("var")")
			 (list subprinter 
			       (stream "\" \"")
			       (list var" = "var"->cdr;\n")))
	     (stream "\"]\"")))]
    [(Array ,ty)
     (let* ([varsym (unique-name 'arrtmp)] [var (Var varsym)]
	    [len (Var (unique-name 'len))]
	    [ind (Var (unique-name 'i))])
       (list (stream "\"#[ \"")
	     (make-decl (Type `(Array ,ty)) var e)
	     (format "int ~a;\n" ind)
	     ;(format "int ~a;\n" len)
	     (Prim `(Array:length ,varsym) len (Type 'Int))
	     (make-for ind "0" len
		       (list 
			"if ("ind">0) " (printf ", %s" "\"\"") ;(stream "\", \"")
			;; This can be an unsafe access if we want (no null check), but it doesn't matter much:
			(Emit-Print/Show-Helper (list "("var"->data)["ind"]")
						ty printf stream)
			;(stream "\" \"")
			))
	     (stream "\"]\"")))]

    ;[(Sigseg ,t)    (stream `("SigSeg<",(Type t)">(",e")"))]
    [(Sigseg ,t)    (stream `(,e))]
    [(Struct ,name) (printf "%s" `("show_",(sym2str name)"(",e").c_str()"))]

    [Pointer (printf "%p" `("(void*)",e))]
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
   (lambda (s e) `("WSPrim::show_helper(sprintf(global_show_buffer, \"",s"\", ",e")); \n"))
   (lambda (e)   `("WSPrim::show_helper2(global_show_stream << " ,e "); \n"))))



;======================================================================
;;; Bits of boilerplate.

;(define boilerplate_headers "")

; char *numberelements = misc_parse_out_option(&argc, argv, "n", 0);

(define (boilerplate_premain scheduler)
  (case scheduler
    [(default-scheduler train-scheduler depth-first)
     ;; FIXME: Yuck, pretty bad code duplication:
     "

int main(int argc, char ** argv)
{
  /* max. number of tuples to run for all sources */
  int maxTuples;


  /* set global variable(s) */
  if (misc_parse_out_switch(&argc, argv, \"no_prefix\", 0))
    WSOUTPUT_PREFIX = FALSE;
  else WSOUTPUT_PREFIX = TRUE;

  if (0 != misc_parse_option_as_int(&argc, argv, \"num_tuples\", 'n', &maxTuples)) {
    maxTuples = -1;
  }

  /* initialize subsystems */ 
  WSInit(&argc, argv);

  /* declare variable to hold final result */
  //WSBox* toplevel;

  /* begin constructing operator graph */
"]
    [(corefit-scheduler-ex corefit-scheduler-df)
     "

int main(int argc, char ** argv)
{
  /* max. number of tuples to run for all sources */
  int maxTuples;


  /* set global variable(s) */
  if (misc_parse_out_switch(&argc, argv, \"no_prefix\", 0))
    WSOUTPUT_PREFIX = FALSE;
  else WSOUTPUT_PREFIX = TRUE;

  if (0 != misc_parse_option_as_int(&argc, argv, \"num_tuples\", 'n', &maxTuples)) {
    maxTuples = -1;
  }

  /* initialize subsystems */ 
  WSInit(&argc, argv);

  /* for corefit */
  WSQuery query;

  /* declare variable to hold final result */
  //WSBox* toplevel;

  /* begin constructing operator graph */
"]))


(define (boilerplate_postmain scheduler return_name return_type)   
  (unless (regiment-quiet) (printf "Generating code for returning stream of type ~s\n" return_type))
  (case scheduler
    [(default-scheduler train-scheduler depth-first)
     `("
  /* dump output of query -- WaveScript type = ",(format "~s" return_type)" */
  PrintQueryOutput out = PrintQueryOutput(\"WSOUT\", maxTuples);
  out.connect(",return_name");

  /* now, run */
  WSRun();

  return 0;
}
"
  )]
    [(corefit-scheduler-ex corefit-scheduler-df) ; FIXME: use params. file for setCPU() below
     `("
  /* dump output of query -- WaveScript type = ",(format "~s" return_type)" */
  PrintQueryOutput out = PrintQueryOutput(\"WSOUT\", maxTuples);
  query.addOp(&out);
  out.setCPU(0);
  query.connectOps(",return_name", &out);
  
  /* now, run */
  query.connectAll();
  WSSched::addQuery(&query);
  WSRun();

  return 0;
}
"
  )]))


;; Boilerplate for producing a WSBox class:
(define (WSBox name outtype constructor body)
  `(,(block (wrap `("\nclass " ,name " : public WSBox"))
	    `("public:\n"
         "DEFINE_OUTPUT_TYPE(",outtype");\n\n"
	      ,constructor
	      "\nprivate:\n"
	      ,body)) ";\n"))


;;================================================================================

;; Disabled because this code can't get to wscode->text right now.
  (define-testing these-tests
    `(
      [3 3]

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
		   tuple tupref ref deref static statref __foreign foreign foreign_box foreign_source
		   ;; These were desugared or reduced to other primitives:
		   or and dataFile Array:toList
		   ;; These were resolved into the w/namespace versions:
		   head tail map append fold
		   List:head List:tail 

		   
		   ;; These have a special syntax, requiring an assert-type or whatnot:
		   cons car cdr null? HashTable:make prim_window 
		   List:ref List:append List:reverse List:length List:make 
		   Array:makeUNSAFE Array:ref Array:set
		   
		   realtime

		   wsequal? print show seg-get toArray 

		   ;; TODO, FIXME: These I just haven't gotten to yet:
		   ensBoxAudioAll
		   List:toArray ptrToArray ptrIsNull ptrMakeNull
		   gnuplot_array gnuplot_array2d
		   String:length String:explode String:implode
		   intToChar charToInt Secret:newTimebase

		   List:assoc List:assoc_update
		   HashTable:rem HashTable:set ;; pure versions
		   Array:map Array:fold
		   ifftC2R fftC ifftC
		   internString uninternString

		   exclusivePtr getPtr
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

(define-testing test-this (default-unit-tester "wavescript_emit-c.ss: generating WaveScript C code." these-tests))
(define test-wavescript_emit-c test-this)

) ;; End Module


;;================================================================================




#|


(define (testme)
  (define str 
    (text->string (wsquery->text
		   '(base-language
  '(program
     (letrec ([s1 (Stream (Sigseg Complex)) (audio 1 4096 0)]
              [s2 (Stream (Sigseg Complex)) (iterate
                                              ()
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
     (Stream (Sigseg Complex))))
         'default-scheduler)))
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
         'default-scheduler
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
                                             ()
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
                                  ()
				  (lambda (arr0 ___VIRTQUEUE___)
                                     ((Array Complex) (VQueue  Float))
                                       (begin
                                         (letrec ([x Int 3])
                                           (letrec ([arr (Array Complex) (fft (fft arr0))])
                                             (if (> (realpart
                                                      (Array:ref arr 100))
                                                    224192.0)
                                                 (begin
                                                   (emit
                                                     ___VIRTQUEUE___
                                                     0.0)
                                                   (emit
                                                     ___VIRTQUEUE___
                                                     (imagpart
                                                       (Array:ref arr 100))))
                                                 (tuple))))
                                         ___VIRTQUEUE___))
                                   s2)])
       s3)
     (Signal Float)))
  'default-scheduler)))
  
  (display str)
  (string->file str (string-append (getenv "HOME") "/WaveScope/code/v1/Ryan2.cpp"))
  )

|#
