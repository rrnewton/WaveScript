#!r6rs

;;;; [2008.05.21] Factoring out sublclasses of the emitC2 class into
;;;; their own files.  This file implements the TinyOS backend.

;;;; .author Ryan Newton

(library (ws passes wavescope_bkend emit-java)
  (export <java>
	  <javaME>
	  )
  (import (rnrs) (except (rnrs r5rs) force delay)
	  (rnrs mutable-strings)
	  (ws compat compat)
	  (ws common)
	  (ws passes wavescope_bkend insert-refcounts)
	  (ws passes wavescope_bkend emit-c)
	  (ws compiler_components c_generator)
	  (ws util bos_oop) 
	  (ws compiler_components c_generator)
	  (ws passes wavescope_bkend emit-c2)
	  ;(ws util rn-match) ;; TEMPTOGGLE
	  )
  
(define-class <java> (<emitC2>) (wserror-acc import-acc))

(define sym2str symbol->string)

(__spec initialise <java> (self prog)
  ;;(slot-set! self 'include-files (list (** "\"" (REGIMENTD) "/src/linked_lib/wsc2.h\"")))
  (slot-set! self 'include-files '()) ;; Alas, can't reuse this right now.
  (slot-set! self 'import-acc '())
  (slot-set! self 'wserror-acc " 
 static void wserror(String msg) {
    //throw new Exception(\"wserror: \"+msg);
    System.out.println(\"wserror: \"+msg);
    System.exit(1);
 }

  void BASE(char x) {
    outputcount++;
    if (outputcount == wsjava_tuplimit) System.exit(0);
  }

"))

;; Java GC replaces our memory management.
(define __build-free-fun-table!
  (specialise! build-free-fun-table! <java> 
     (lambda (next self heap-types)
       (make-lines ""))))

(define (upcase-first-letter str)
  (define copy (string-copy str))
  (string-set! copy 0 (char-upcase (string-ref copy 0)))
  copy)

(define ___Type 
  (specialise! Type <java>
    (lambda (next self ty)
      (match ty
	[Bool   "boolean"]
	[String "String"]
	[Int16 "short"]
	[Int32 "int"]
	[Int64 "long"]

	;; TEMP HACK:
	;[Uint16 "short"]
	;[Uint16 "int/*fakeuint*/"]
	[Uint16 (error 'Type "<java> can't emulate unsigned ints yet")]
	[Int    "int"]

	;; TEMP HACK:
	[(Array Char) "String"]
	[(Array ,elt) (list (Type self elt) "[]")]	
	[(List ,elt) "Cons"]
	[(Struct ,name) (list (upcase-first-letter (sym2str name) ))] ;; Value type.
	[,else (next)]
	))))

(__specreplace Emit <java> (self down* ty)
  (lambda (expr)
    (ASSERT simple-expr? expr)
    (let ([element (Simple self expr)])
      (make-lines (map (lambda (down) (list (Var self down) "(" (list element) ");\n"))
		    down*)))))

(define ___Const
  (specialise! Const <java>
    (lambda (next self datum wrap)
      (cond
	[(boolean? datum) (wrap (if datum "true" "false"))]
	[else (next)]))))

(define ___Value
  (specialise! Value <java>
    (lambda (next self)      
      (lambda (xp kont)
	(define Simp (lambda (x) (Simple self x)))
	(match xp
	  [',vec (guard (vector? vec))
		 (ASSERT (vector-andmap char? vec))
		 ;; Strip out any null characters??
		 (let* ([ls (vector->list vec)]
			[ls2 ;; Hack, a null terminator on the end is redundant for a string const:
			 (if (fx= 0 (char->integer (vector-ref vec (sub1 (vector-length vec)))))
			     (rdc ls) 
			     ls)]
			[str (list->string ls2)])
		   (kont (format "~s" str)))]

	  [(make-struct ,name ,[Simp -> arg*] ...)
	   (kont `("new ",(upcase-first-letter (sym2str name))"(",(insert-between ", " arg*)")"))]

	  [,oth ((next) xp kont)])))))

;; These helpers box and unbox scalars into their corresponding reference types:
(define (box-scalar ty obj)
  (match ty
    [Int   (list "new Integer("obj")")]
    [Int32 (list "new Integer("obj")")]
    [Int16 (list "new Short("obj")")]
    [Int64 (list "new Long("obj")")]
    [Float  (list "new Float("obj")")]
    [Double (list "new Double("obj")")]
    [Bool   (list "new Boolean("obj")")]
    [Complex (error 'box-scalar "Complex numbers not handled in java yet.")]
    [,unsigned (guard (memq unsigned '(Uint16 Uint32 Uint64)))
	       (error 'emitC2:Value "cannot handle unsigned types in java yet")]
    [,else (ASSERT (not (scalar-type? ty)))
	   ;; Otherwise it's already a reference type
	   obj]))
(define (unbox-scalar ty obj)
  (match ty
    [Int   (list "(((Integer)"obj").intValue())")]
    [Int32 (list "(((Integer)"obj").intValue())")]
    [Int16 (list "(((Short)"obj").shortValue())")]
    [Int64 (list "(((Long)"obj").longValue())")]
    [Float  (list "(((Float)"obj").floatValue())")]
    [Double (list "(((Double)"obj").doubleValue())")]
    [Bool   (list "(((Boolean)"obj").booleanValue())")]
    [Complex (error 'unbox-scalar "Complex numbers not handled in java yet.")]
    [,unsigned (guard (memq unsigned '(Uint16 Uint32 Uint64)))
	       (error 'emitC2:Value "cannot handle unsigned types in java yet")]
    [,else (ASSERT (not (scalar-type? ty)))
	   obj]))
(define (cast tytxt objtxt) (list "(("tytxt")"objtxt")"))

(define ___PrimApp
  (specialise! PrimApp <java>
    (lambda (next self app kont mayberetty)
      (define (Simp x)  (Simple self x))
      (match app

	[(show ,[(TyAndSimple self) -> ty obj])
	 (define wrapped (box-scalar ty obj))
	 (kont (list wrapped ".toString() "))]	
	[(string-append ,[Simp -> left] ,[Simp -> right])
	 (kont (list left " + " right))]

	[(cons ,[Simp -> a] ,[Simp -> b])
	 (match mayberetty
	   [(List ,elt)
	    ;(kont (list "new Cons<"(Type self elt)">("a", "b")"))
	    (kont (list "new Cons("(box-scalar elt a)", "b")"))
	    ])]
	[(List:is_null ,[Simp -> pr]) (kont `("(",pr" == null)"))]
	[(car (assert-type (List ,ty) ,[Simp -> x]))
	 ;(kont (cast (Type self ty) (unbox-scalar ty (list x ".car"))))
	 (kont (if (scalar-type? ty)
		   (unbox-scalar ty (list x ".car"))
		   (cast (Type self ty) (list x ".car"))))
	 ;(kont (list x ".car"))
	 ]
	[(cdr ,[Simp -> x]) (kont (list x ".cdr"))]

       [(wsequal? ,[(TyAndSimple self) -> ty left] ,[Simp -> right])
	(if (scalar-type? ty)	    
	    (kont `("(",left" == ",right")"))
	    ;; Horrible hack to get around javac's conservative nature:
	    (if (equal? left "null")
		(kont `("(",right" == null)"))
		(kont `("((",left" == null && null == ",right") || (",left" != null && ",left".equals(",right")))")))	    
	    )]

       [(Array:length ,arr)
	(define _arr (Simp arr))
	(if (equal? (peel-annotations arr) 'Array:null) ;; Hack for Java's benefit.
	    (kont "0 /* arr len of null */")
	    (kont `("(",_arr" == null ? 0 : ",_arr".length)")))]

#;    
    [(,abs ,[Simp -> a]) (guard (memq abs '(absI16 absI64 absI absF absD absC)))
     (kont `("Math.abs(",a")"))]
    [(absI16 ,[Simp -> a]) (kont `("(",(Type self 'Int16)")Math.abs(",a")"))]
    [(absI32 ,[Simp -> a]) (kont `("(",(Type self 'Int32)")Math.abs(",a")"))]
    [(absI   ,[Simp -> a]) (kont `("(",(Type self 'Int  )")Math.abs(",a")"))]
    [(absF   ,[Simp -> a]) (kont `("(",(Type self 'Float)")Math.abs(",a")"))]
    [(absD   ,[Simp -> a]) (kont `("(",(Type self 'Double)")Math.abs(",a")"))]

    [(^. ,[Simp -> a] ,[Simp -> b]) (kont `("(float)Math.pow(",a", ",b")"))]
    [(^D ,[Simp -> a] ,[Simp -> b]) (kont `("Math.pow(",a", ",b")"))]

    [(sqrtF ,[Simp -> a]) (kont `("(float)Math.sqrt(",a")"))]
    [(sqrtD ,[Simp -> a]) (kont `("Math.sqrt(",a")"))]

    [(cos ,[Simp -> a]) (kont `("(float)Math.cos(",a")"))]
    
    [(logF ,[Simp -> a]) (kont `("(float)Math.log(",a")"))]
    

       #;
       ;; Handle exponents:
       [(,infix_prim ,[Simp -> left] ,[Simp -> right])	
	(guard (assq infix_prim infix-arith-prims)
	       (equal? "^" (substring (sym2str infix_prim) 0 1)))
	
	]

       ;; FIXME DUPLICATED CODE:
       [(,infix_prim ,[Simp -> left] ,[Simp -> right])	
	(guard (assq infix_prim infix-arith-prims))
	(if (equal? "^" (substring (sym2str infix_prim) 0 1))
	    (kont (list "pow("left", "right")")) ;; Actually this doesn't work for integers
	    (let ()
	      (define valid-outputs '("+" "-" "/" "*" "<" ">" "==" "<=" ">=")) ; "^"
	      (define result
		(case infix_prim
		  [(=) "=="]
		  [(< > <= >=) (sym2str infix_prim)]
		  [else 
		   (match (string->list (sym2str infix_prim))	  
		     [(#\_ ,op ,suffix ...) (list->string (list op))]
		     [(,op ,suffix ...)     (list->string (list op))])]))
	      (ASSERT (member result valid-outputs))	     
	      (kont (list "("(Type self (get-primitive-return-type infix_prim))")(" left " " result " " right ")"))))]

       [,else (next)]))))

(define ___Simple
  (specialise! Simple <java>
    (lambda (next self expr)
      (match expr
	[(assert-type ,ty Array:null) "null"]
	[(assert-type ,ty '())        "null"]
	[,else (next)]))))

(define ___Effect 
  (specialise! Effect <java>
    (lambda (next self)
      (define (Simp x)  (Simple self x))
      (lambda (xp)
	(match xp
	  [(print ,[(TyAndSimple self) -> ty x])
	   (make-lines `("outstrm.print(",x");\n"))]
	  [(wserror ,[Simp -> str]) (make-lines (list "wserror("str");\n"))]
	  [,oth ((next) oth)])))))


;; Silly javac:
(__specreplace DummyInit <java> (self ty)
   ;;(printf " scalar? ~s ~s\n" ty (if (or (scalar-type? ty) (equal? ty #())) 'TRUE '#f))
   (match ty
     [#() ""]
     [(Ref ,[t]) t]
     [,ty (guard (scalar-type? ty)) ""]
     [,ty " = null";(list (format " = null /* ~a " ty) (Type self ty) " */")       
      ]))


(__specreplace BuildOutputFiles <java> (self includes freefundefs state ops init driver)  
  ;; We can store this in a separate file at some point:
  (define header (list "
  void parseOptions(int argc, String[] argv) {}

  private int wsjava_tuplimit = 10;
  private int outputcount = 0;

"(slot-ref self 'wserror-acc)"

  // Assuming there will only be one query, thus this is static:
  static private PrintStream outstrm;
  public WSQuery(OutputStream out) { outstrm = new PrintStream(out); }

  // Can't use generics because I don't think Jave ME supports java 5.
  public class Cons {
    public Object car;
    public Cons cdr;
    public Cons(Object head, Cons rest) {
      car = head;
      cdr = rest;
    }
  }

/*
  public class Cons<T> {
    public T car;
    public Cons<T> cdr;
    public Cons(T head, Cons<T> rest) {
      car = head;
      cdr = rest;
    }
  }
*/

  public static void main(String[] argv) {
    WSQuery theQuery = new WSQuery(System.out);
    try {
      theQuery.main(argv.length, argv);
    } catch (Exception e) {
  	wserror(\"!mainfail!\"+e.toString());
    }
  }

"))
  (define bod 
    (text->string
     (list includes 
	   "import java.io.*;\n"
	   (slot-ref self 'import-acc)
	   "\n\n"
	   (block "public class WSQuery"	   
		  (list 
		   header
		   ;"private static int ARRLEN(Object[] arr) { return arr.length; }\n"
		   
		   ;"void setOut(OutputStream out) { outstrm = new PrintStream(out); }\n"
		   (insert-between "\n"
				   (list 
				    (map (curry StructDef self) (slot-ref self 'struct-defs))
				    state
				    ops 
				    init driver)))))))
  (ASSERT (equal? (lines-text freefundefs) ""))
  (vector
   ;; We return an association list of files to write.
   (list (list "WSQuery.java"  (text->string bod)))

   ;; We also return a post-file-write thunk to execute:
   void))

(__specreplace StructDef <java> (self entry)
	       (match entry
       [(,name (,[sym2str -> fld*] ,typ*) ...)
	(let ([tmpargs (map (lambda (_) (sym2str (unique-name 'tmp))) fld*)]
	      [ctype* (map (curry Type self) typ*)]
	      [_name (upcase-first-letter (sym2str name))]
	      [_fld* (map (lambda (x) (string-append "_" x)) fld*)])
	  `(,(block `("class ",_name)
		    (list
		     (block (list "public "_name"("(insert-between ", " 
				  (map (lambda (ty var) (list ty" "var))
				    ctype* _fld*))")")
			    (map (lambda (fld)
				   (list fld" = _"fld";\n")) fld*))
		     (map (lambda (ctype fld) `["public " ,ctype " " ,fld ";\n"])
		       ctype* fld*)))
	    ";\n"
            ))]))

;(__specreplace incr-local-refcount <java> (self ty ptr) (make-lines ""))
;(__specreplace decr-local-refcount <java> (self ty ptr) (make-lines ""))
;(__specreplace incr-heap-refcount <java> (self ty ptr) (make-lines ""))
;(__specreplace decr-heap-refcount <java> (self ty ptr) (make-lines ""))
(__specreplace gen-incr-code <java> (self ty ptr msg) (make-lines ""))
(__specreplace gen-decr-code <java> (self ty ptr)     (make-lines ""))
(__specreplace gen-free-code <java> (self ty ptr)     (make-lines ""))

(__specreplace array-constructor-codegen <java> (self len init ty kont)
  (define tmp (unique-name "tmparr"))
  (match ty
    [(Array ,elt)
     ;; Handle nested arrays:
     (define nesting 0) ;; Hack, mutated on next line:
     (define basetype (match elt [(Array ,[x]) (set! nesting (add1 nesting) )x] [,y y]))
     (define newstmt (list "new "(Type self basetype)"["len"]" (make-list nesting "[]")))
     (if (not init)
	 (kont newstmt)
	 (let* ([tmp (unique-name "tmp")]
		[_tmp (Var self tmp)]
		;; If it's not a scalar type should we cast to object?
		[_init (if (scalar-type? elt) (Simple self init)
			   (cast "Object" (Simple self init)))]
		[_ty (Type self ty)]
		[_elt (Type self elt)])
	   (append-lines 
	    ;(list tmp ty `(assert-type ,ty (Array:makeUNSAFE ,len))))
	    (make-lines (list 
			  _ty" "_tmp" = "newstmt";\n"
			  "// This doesn't work in older JVMs \n"
			  "// java.util.Arrays.fill("_tmp", 0,"len" - 1, "_init");\n"
			  ;"java.util.Arrays.fill("_tmp", "_init");\n"
			  "for(int i=0; i<"len"; i++) "_tmp"[i] = ("_elt")"_init";\n"
			  
			  ))
	    (kont _tmp))))]))


;;================================================================================

(define-class <javaME> (<java>) ())


;; Changing wserror to print to the phone:
(__spec initialise <javaME> (self prog)
 (slot-set! self 'include-files '())
 (slot-set! self 'import-acc
	    (list "// For the 'Form' class:\n"
		  "import javax.microedition.lcdui.*;\n"))
 (slot-set! self 'wserror-acc (list " 
 static void wserror(String msg) {
    form.append(new StringItem(null, \"!wserror!\" + msg));
    outstrm.print(\"!wserror!\" + msg);
 }

 static private Form form;
 public void setForm(Form f) { form = f; }

  void BASE(char x) {
    "(print-w-time "BASE ")"
    outputcount++;
    if (outputcount == wsjava_tuplimit) System.exit(0);
  }

")))

(define ____Effect 
  (specialise! Effect <javaME>
    (lambda (next self)
      (define (Simp x)  (Simple self x))
      (lambda (xp)
	(match xp
	  [(print ,[(TyAndSimple self) -> ty x])
	   (make-lines `("outstrm.print(",x");\n"
			 "// For now let's print to the phone also:\n"
			 "form.append(new StringItem(null, \"\" + ",x"));\n"
			 ))]
	  [(wserror ,[Simp -> str]) (make-lines (list "wserror("str");\n"))]
	  [,oth ((next) oth)])))))

;; Replace just the thunk portion of the <java> config:
(define ____BuildOutputFiles
  (specialise! BuildOutputFiles <javaME> 
    (lambda (next . rest)
      (match (next)
	[#(,alist ,thunk)
	 (vector alist
	    (lambda () 
	      ;; Copy the stub to the current directory:
	      ;; (Fixme, should use cross-platform scheme routines for this)
	      (system "cp -fpr $REGIMENTD/src/linked_lib/javaME_stub ./")
	      (system "mv WSQuery.java ./javaME_stub/src/")))]))))

(define ____PrimApp
  (specialise! PrimApp <javaME>
    (lambda (next self app kont mayberetty)
      (define (Simp x)  (Simple self x))
      (match app
	;; JavaME is missing some math ops:
	[(^. ,[Simp -> a] ,[Simp -> b]) (kont `("(float)Float11.pow(",a", ",b")"))]
	[(^D ,[Simp -> a] ,[Simp -> b]) (kont `("Float11.pow(",a", ",b")"))]
	[(logF ,[Simp -> a]) (kont `("(float)Float11.log(",a")"))]
	
	[,else (next)]))))

;; ================================================================================
;; [2008.04.12] When I'm not in a hurry these should go in their own class, <javaME-timed> 

(define (print-w-time prefix)
  (list 
   "outstrm.print(\"("prefix"\"); "
   "outstrm.print(System.currentTimeMillis()); "
   "outstrm.print(\")\\n\");\n"))

(define ___IterStartHook
  (specialise! IterStartHook <javaME>  
    (lambda (next self name arg argty)
    ;(LoadPrintf self)
    (list (next)	  
	  ;;(format "printf(\"(Start ~a %u %u)\\n\", overflow_count, call Cntr.get());\n" name)
	  ;;"outstrm.print(\"(Start "(sym2str name)" 0 \"+ System.currentTimeMillis() + \")\\n\");\n"
	  (print-w-time (list "Start "(sym2str name)" "))
	  ))))
(define ___IterEndHook
  (specialise! IterEndHook <javaME>
    (lambda (next self name arg argty) 
    ;(LoadPrintf self)
    (list (next)
	  ;;(format "printf(\"(End   ~a %u %u)\\n\", overflow_count, call Cntr.get());\n" name)
	  ;;"outstrm.print(\"(End "(sym2str name)" 0 \"+ System.currentTimeMillis() + \")\\n\");\n"	 
	  (print-w-time (list "End "(sym2str name)" "))
	  ))))

;; Because we follow tuples on emits, we need to time differently.
;; [2008.04.12] Scratching this... just using the next "Start" instead of "Emit".
;; That means blaming the function call on the parent operator...
#;
(define __Emit 
  (specialise! Emit <javaME> 
    (lambda (next self down* ty)
      (lambda (expr)
	(append-lines ;(make-lines "outstrm.print(\"(Emit \"+ System.currentTimeMillis() + \")\\n\");\n")	 
	 (make-lines (print-w-time "Emit "))
	 ((next) expr)
	 (make-lines (print-w-time "Ret "))
	 ;(make-lines "outstrm.print(\"(Ret  \"+ System.currentTimeMillis() + \")\\n\");\n")
	 )))))

;; Wrap timers around the whole Source call:
(define ___Source
  (specialise! Source <javaME>
  (lambda (next self xp)
    (define-values (nm code state rate init) (next))

    (values nm 
	    (append-lines (make-lines (print-w-time "StartTraverse "))
			  code
			  (make-lines (print-w-time "EndTraverse ")))
	    state rate init))))


) ;; End module

