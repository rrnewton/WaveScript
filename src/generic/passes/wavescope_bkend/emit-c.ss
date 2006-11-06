

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


(module wavescript_emit-c mzscheme 
  (require "c_generator.ss" "helpers.ss")
  (provide ;WSBox wscode->text
	   wsquery->text
	   
	   testme	   testme2	   testme0

	   test-this  test-wavescript_emit-c)
  (chezprovide )  
  (chezimports (except helpers test-this these-tests))
  
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

;======================================================================

;; This is the only entry point to the file.  A complete query can
;; be transformed into a complete query file.
(define wsquery->text
  (lambda (prog)
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
	    (values `( ,name " = " ,(symbol->string e) ";\n") ())]

	;; Forbidding recursion for now (even though this says 'letrec').
	[(letrec ,binds ,bod)
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
	  ;; CODE DUPLICATION:
	  `("WSBox* ",name" =  new Rewindow<float>(",size", ",size" - ",overlap ");\n" 
	    "{ RawFileSource* tmp = new RawFileSource(\"/tmp/100.raw\", 0, 4, 24000*100);\n"
	    "  ",name"->connect(tmp); }\n"
	    )
	  '())]

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
	
	[,other (error 'wsquery->text:Query "unmatched query construct: ~s" other)]
	)) ;; End Query

; ======================================================================
;;; Helper functions for handling different program contexts:
        
    (define (Var var)
      (ASSERT (symbol? var))
      ;; This is the place to do any name mangling.  I'm not currently doing any for WS.
      (symbol->string var))
    (define (PrimName var)
      (format "WSPrim::~a" (mangle-name (symbol->string var))))
      ;(symbol->string var))
    (define (FunName var)
      (format "WSFunLib::~a" var))
      ;(symbol->string var))

    (define (Stmt tenv)
      (lambda (st)
	(define myExpr (Expr tenv))
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

	;; This is a boilerplate that we can now remove:
	[(letrec ([,v ,t (virtqueue)] ,rest ...) ,body)
	 (let ([newenv (tenv-extend tenv (list v) (list t))])
	   ((Stmt newenv) `(letrec ,rest ,body)))]

	;; TEMP:
	[(letrec () ,body) 
;	 (inspect `(HMM ,body))
;	 `("toplevel = " ,(Var body))]
	 ((Stmt tenv) body)]

	;; No recursion!
	[(letrec ,binds ,body)
	 (ASSERT (no-recursion! binds))
	 ((Stmt tenv) `(let (,(car binds))
			 (letrec ,(cdr binds) ,body)))]

	[(emit ,vqueue ,[myExpr -> val])
	 `("emit(" ,val ");\n")]

	;; Print is required to be pre-annotated with a type.
	;; (We can no longer do recover-type.)
	[(print (assert-type ,t ,e))
	 ;(EmitPrint (myExpr e) (recover-type e tenv))
	 (EmitPrint (myExpr e) t)]
	[(print ,_) (error 'emit-c:Stmt "print should have a type-assertion around its argument: ~s" _)]

	;; This begin is already *in* Stmt context, don't switch back to Expr for its last:
	[(begin ,[stmts] ...) stmts]

	[(arr-set! ,[myExpr -> arr] ,[myExpr -> ind] ,[myExpr -> val])
	 `(,arr "[" ,ind "] = " ,val ";\n")]

	[(set! ,[Var -> v] ,[myExpr -> e])
	 `(,v " = " ,e ";\n")]
	
	[(for (,i ,[myExpr -> st] ,[myExpr -> en]) ,bod)
	 (let ([istr (Var i)])	   
	   (block `("for (int ",istr" = ",st"; ",istr" <= ",en"; ",istr"++)")
		  ((Stmt (tenv-extend tenv (list i) '(Int))) bod)))]
	[(break) "break;\n"]

	;; This becomes nothing:
	[___VIRTQUEUE___ ""]

	;; Otherwise it's just an expression.
	;; TEMP: HACK: Need to normalize contexts.
	;; TODO: Not all expressions make valid statements.
	[,[myExpr -> exp] `(,exp ";\n")]
	[,unmatched (error 'emitC:Stmt "unhandled form ~s" unmatched)]
	)))
	
    (define (Expr tenv)
      (lambda (exp)
	(match exp

         ;; Special Constants:
;	[nullseg "WSPrim::nullseg"]
;	[nullseg "WSNULL"]
	[nullseg "WSNULLSEG"]
	[nullarr "WSNULL"]

	[,c (guard (constant? c)) (Expr `(quote ,c))]
	[(quote ,datum)
	 ;; Should also make sure it's 32 bit or whatnot:
	 (cond
	  [(eq? datum #t) "TRUE"]
	  [(eq? datum #f) "FALSE"]	  
	  [(or (integer? datum) (flonum? datum))  (number->string datum)]
	  [(string? datum) (format "~s" datum)]
	  [else (error 'emitC:Expr "not a C-compatible literal: ~s" datum)])]
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

	[(realpart ,[v]) `("(" ,v ".real)")]
	[(imagpart ,[v]) `("(" ,v ".imag)")]

	;; TYPE??
	;[(show ,e) (EmitShow [(Expr tenv) e] (recover-type e tenv))]

	;; This is inefficient.  Only want to call getDirect once!
	;; Can't trust the C-compiler to know it's effect free and do CSE.
	[(seg-get ,[seg] ,[ind])	 
	 `("(" ,seg ".getDirect())[" ,ind  "]")]
	
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

    (define (Type t)
      (match t
	[Int "wsint_t"]
	[Float "wsfloat_t"]
	[,simple (guard (memq simple '(Complex Float)))
		 (list->string (map char-downcase (string->list (symbol->string simple))))]
	[,v (guard (symbol? v)) (symbol->string v)]
	;; Went back and forth on whether this should be a pointer:
	[(Sigseg ,[t]) `("RawSeg")]
;	[(Sigseg ,[t]) `("SigSeg<" ,t ">")]
;	[(Signal ,[t]) `("Signal<" ,t ">*")]

	[(Array ,[t]) `(,t "[]")]
	[(Struct ,name) (symbol->string name)]
	
	;[,other (format "~a" other)]
	[,other (error 'emitC:Type "Not handled yet.. ~s" other)]))

;; TEMP: HACK: THIS NEEDS TO RETURN A STRING:
;; This implements our polymorphic show function.
;; It prints something or other for any type.
(define (EmitPrint e typ)
  (match typ
    [Int `("printf(\"%d\", ",e");\n")]
    [Float `("printf(\"%f\", ",e");\n")]
    [String `("printf(",e");\n")]
    ;; TEMP: Have to wrap the sigseg to get the << method.
    ;; This should be fixed in the C++.
    [(Sigseg ,t) `("cout << SigSeg<",(Type t)">(",e");;\n")]

    ;; Make a print routine for this struct.
    [(Struct ,name)
     (match (assq name struct-defs)
       [(,name [,fld* ,typ*] ...)
	`("{ " ,(symbol->string name) " tmp = ",e";\n"
	  "  cout << \"{\";\n"
	  ,(insert-between "  cout << \"; \";\n"
			   (map (lambda (fld typ)
				  (EmitPrint (format "tmp.~a" fld) typ)
				  )
			     fld* typ*
			     ))
	  "  cout << \"}\"; }\n")
	])]
    
    [,other `("printf(\"<object of type ",(format "~a" typ)">\");\n")]
    ))

   (define (StructDef entry)
     (match entry
       [(,(symbol->string -> name) (,[symbol->string -> fld*] ,[Type -> typ*]) ...)
	(let ([tmpargs (map (lambda (_) (symbol->string (unique-name 'tmp))) fld*)])
	  `(,(block `("struct ",name)
		    `([,typ* " " ,fld* ";\n"] ...
		      ,(block `(,name"(",(insert-between 
					  ", " `([,typ* " ",tmpargs] ...)
					  )")")
			      (map (lambda (fld arg)
				     `(,fld " = " ,arg ";\n"))
				fld* tmpargs)
			      )))
	    ";\n\n"))]))

  ;============================================================
  ;;; Other helpers:

  ;; Value types.
  (define (immediate-type? t)
    (match t
      [,nt (guard (memq nt num-types)) #t]
      [Char #t]
      [(Struct ,name) #t]
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
			;"cout << _name << \": \" << *element << endl;\n"
			"printf(\"WSOUT: \");\n"
			,(EmitPrint "(*element)" typ) ";\n"
			"printf(\"\\n\");\n"
			"delete element;\n"  
			"return false;\n"
			))))
	 ";\n\n"))]))

;; Takes the *inside* of an iterate box and turns it to C text.
(define wscode->text
    (let () 
    ;; Entry point
      (lambda (exp name tenv)
	(match exp 
	  [(lambda (,input) (,T) ,bod)	  	   
	   (let ([body ((Stmt (tenv-extend tenv `(,input) `(,T))) bod)]
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
	[(letrec ([,lhs* ,ty* ,rhs*] ...) ,bod)
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

  ;============================================================
  ;;; Set unit tests

  ;============================================================
  ;; Main body:
  ;; Here we stitch together the file out of its composit bits.
    (mvlet ([(body funs) (Query "toplevel" typ expr (empty-tenv))])
      `(,(file->string (++ (REGIMENTD) "/src/linked_lib/WSHeader.hpp"))
	,(file->string (++ (REGIMENTD) "/src/linked_lib/WSPrim.cpp"))
	"/* These structs represent tuples in the WS program. */\n"
	,(map StructDef struct-defs)
	,funs 
	,(make-output-printer typ) 
	
	,boilerplate_premain
					;"// " ,(Type typ) " toplevel;\n"
	,(indent body "  ")
	,(boilerplate_postmain (Var 'toplevel) typ)
	"}\n\n"
	))]
  [,other ;; Otherwise it's an invalid program.
   (warning 'wsquery->text "ERROR: bad top-level WS program: ~s" other)
   (inspect other)
   (error 'wsquery->text "")])))



;======================================================================
;;; Bits of boilerplate.

;(define boilerplate_headers "")

(define boilerplate_premain "

int main(int argc, char ** argv)
{
  /* initialize subsystems */ 
  WSInit(&argc, argv);

  /* declare variable to hold final result */
  WSBox* toplevel;

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
                                              (lambda (w)
                                                ((Sigseg Complex))
                                                (letrec ([___VIRTQUEUE___ (VQueue
                                                                            (Sigseg
                                                                              Complex)) (virtqueue)])
                                                  (begin
                                                    (emit
                                                      ___VIRTQUEUE___
                                                      w)
                                                    ___VIRTQUEUE___)))
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
                                             (lambda (w)
                                               ((Sigseg Complex))
                                               (letrec ([___VIRTQUEUE___ (VQueue
                                                                           (Array
                                                                             Complex)) (virtqueue)])
                                                 (begin
                                                   (emit
                                                     ___VIRTQUEUE___
                                                     (fft (fft (to_array
                                                                 w))))
                                                   ___VIRTQUEUE___)))
                                             s1)]
              [s3 (Signal Float) (iterate
                                   (lambda (arr0)
                                     ((Array Complex))
                                     (letrec ([___VIRTQUEUE___ (VQueue
                                                                 Float) (virtqueue)])
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
                                         ___VIRTQUEUE___)))
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

