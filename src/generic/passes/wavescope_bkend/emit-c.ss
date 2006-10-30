
;;;; .title WaveScript EmitC
;;;; .author Ryan Newton

;;;; This uses the generic C-generation libary (c_generator.ss) and
;;;; provides procedures for producing a C-file following WaveScript
;;;; conventions, that is, a ".wsc" file.

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
  (let ()
    ;; .param name   A string naming the variable that stores the current result.
    ;; .param type   Type of current result.
    ;; .param x      The query construct to process.
    ;; .returns A new expression and a set of declarations.
    (define (Query name typ x)
      ;; Coercion:
      (if (symbol? name) (set! name (symbol->string name)))
      (match x
	;; Forbidding recursion for now (even though this says 'letrec').
	[(letrec ,binds ,body)
	 (ASSERT (symbol? body))
	 (ASSERT (no-recursion! binds))
	 (match binds [([,[Var -> lhs*] ,ty* ,rhs*] ...)
		       (let loop ([lhs* lhs*] [ty* ty*] [rhs* rhs*]
				  [stmtacc '()] [declacc '()])
			 ;; Should really use the Text ADT here:
			 (if (null? lhs*)
			     (values (reverse! stmtacc) (reverse! declacc))
			     (mvlet ([(stmt decl) (Query (car lhs*) (car ty*) (car rhs*))])
			       (loop (cdr lhs*) (cdr ty*) (cdr rhs*)
				     (cons stmt stmtacc) (cons decl declacc)))))]
		       
;		       `((,ty*" ",lhs*" = ",(map Query lhs* rhs*)";\n") ...
;			 ,(Var body))]
		[,other (error 'wsquery->text "Bad letrec binds: ~s" other)])]
		       
	[(audio ,[Expr -> channel] ,[Expr -> size] ,[Expr -> skip])
	 ;; HMM, size seems to be FIXED:  FIXME	  
	 ;; (const char *path, int offset, int skip, double sample_rate, uint64_t cpuspeed)
	 (values `("RawFileSource " ,name " = RawFileSource(\"/tmp/100.raw\", " ,channel ", 4, 24000*100);\n")
		 '())
	 ]
	
	[(iterate ,let-or-lambda ,sig)
	 ;; Program better have been flattened!!:
	 (ASSERT (symbol? sig))
	  
	 (let ([class_name `("Iter_" ,name)])
	   ;; First we produce a line of text to construct the box:
	   (values `(  ,class_name" ",name" = ",class_name "(" ");\n" 
			      ,name".connect(&" ,(Var sig) ");\n")
		   ;; Then we produce the declaration for the box itself:
		   (list (WSBox class_name 
				(match typ
				  [(Signal ,t) (Type t)]
				  [,other (error 'Query "expected iterate to have signal output type! ~s" other)])
				;; This produces a function declaration for iterate:				
				(wscode->text let-or-lambda name))))
	   )]
	
	[,other (error 'wsquery->text:Query "unmatched query construct: ~s" other)]
	))
    
    (lambda (prog)
      (match prog
	[(,lang (quote (program (letrec ,binds ,body) ,struct-defs ,typ)))
	 (mvlet ([(return-name) body]
		 [(body funs) (Query "toplevel" typ `(letrec ,binds ,body))])
	   (ASSERT (symbol? return-name))

	   `(,boilerplate_headers 
	     ,(file->string (++ (REGIMENTD) "/src/linked_lib/WSPrim.cpp"))
	     ,funs
	     ,boilerplate_premain
	     ;"// " ,(Type typ) " toplevel;\n"
	     ,(indent body "  ")
	     ,(boilerplate_postmain (Var return-name) typ)
	     "}\n\n"
	     )
	   )]
	[,other 
	 (warning 'wsquery->text "ERROR: bad top-level WS program: ~s" other)
	 (inspect other)
	 (error 'wsquery->text "")]))))

;; Takes the *inside* of an iterate box and turns it to C text.
(define wscode->text
  (let () 
    ;; Entry point
    (lambda (exp name)
      (match exp 
	[(lambda (,[Var -> input]) (,[Type -> typ]) ,[Stmt -> body])
	 (block "bool iterate(WSQueue *inputQueue)"
		`("printf(\"Execute iterate for ",name"\\n\");"
                  "void *input = inputQueue->dequeue();\n"
		  ,typ " " ,input " = *((",typ"*)input);\n"
		  ,@body

		  ;; This is a quirky feature.  The bool returns
		  ;; indicates whether the scheduler should NOT
		  ;; reschedule this box further (even if there is
		  ;; input left in its queue.)  For all the iterate
		  ;; operators, this will be FALSE.
		  "return FALSE;\n"
		))]
	[,other (error 'wscode->text "Cannot process: ~s" other)]
	))
    ))


;======================================================================
;;; Bits of boilerplate.

(define boilerplate_headers 
"
#include <WaveScope.h>
#include <Heartbeat.hpp>
#include <PrintBox.hpp>
#include <RawFileSource.hpp>
#include <AsciiFileSink.hpp>
#include <Boxes.hpp>

#include <stdio.h>

#define TRUE 1
#define FALSE 0

")

(define boilerplate_premain "

int main(int argc, char ** argv)
{
  /* initialize subsystems */ 
  WSInit(&argc, argv);

")


(define (boilerplate_postmain return_name return_type) 
  (printf "Generating code for returning stream of type ~s\n" return_type)
  `(
,@(if (equal? return_type '(Signal (Sigseg Float))) `("
  /* dump output to file */
  AsciiFileSink<float> fs = AsciiFileSink<float>(\"/tmp/wavescript_query.out\");
  fs.connect(&",return_name");

")      '())
"
  /* now, run */
  WSRun();

  return 0;
"))

;; Boilerplate for producing a WSBox class:
(define (WSBox name outtype body)
  `(,(block (wrap `("class " ,name " : public WSBox"))
	    `("public:\n"
	      "DEFINE_OUTPUT_TYPE(" ,outtype ");\n"
	      "\nprivate:\n"
	      ,body)) ";\n"))

; ======================================================================
;;; Helper functions for handling different program contexts:
        
    (define (Var var)
      (ASSERT (symbol? var))
      ;; This is the place to do any name mangling.  I'm not currently doing any for WS.
      (symbol->string var))
    (define (PrimName var)
      (format "WSPrim::~a" var))
      ;(symbol->string var))
    (define (FunName var)
      (format "WSFunLib::~a" var))
      ;(symbol->string var))

    (define (Stmt st)
      (match st
	;; Must distinguish expression from statement context.
	[(if ,[Expr -> test] ,[conseq] ,[altern])
	 `("if (" ,test ") {\n"
	   ,(indent conseq "  ")
	   "} else {\n"
	   ,(indent altern "  ")
	   "}\n")]
	;; Not allowed in expression position currently:
	[(let ([,[Var -> v] ,[Type -> t] ,[rhs]]) ,[body])
	 `(,t " " ,v ";\n" ,body)]

	;; This is a boilerplate that we can now remove:
	[(letrec ([,v ,_ (virtqueue)] ,rest ...) ,body)
	 (Stmt `(letrec ,rest ,body))]

	;; TEMP:
	[(letrec () ,body) 
;	 (inspect `(HMM ,body))
;	 `("toplevel = " ,(Var body))]
	 (Stmt body)]

	;; No recursion!
	[(letrec ,binds ,body)
	 (ASSERT (no-recursion! binds))
	 (Stmt `(let (,(car binds))
		  (letrec ,(cdr binds) ,body)))]

	[(emit ,vqueue ,[Expr -> val])
	 `("emit(" ,val ");\n")]
	[(begin ,[stmts] ...) stmts]

	[(arr-set! ,[Expr -> arr] ,[Expr -> ind] ,[Expr -> val])
	 `(,arr "[" ,ind "] = " ,val ";\n")]

	;; This becomes nothing:
	[___VIRTQUEUE___ ""]

	;; Otherwise it's just an expression.
	;; TODO: Not all expressions make valid statements.
	[,[Expr -> exp] `(,exp ";\n")]
	))
	
    (define (Expr exp)
      (match exp
	[,c (guard (constant? c)) (Expr `(quote ,c))]
	[(quote ,datum)
	 ;; Should also make sure it's 32 bit or whatnot:
	 (cond
	  [(eq? datum #t) "TRUE"]
	  [(eq? datum #f) "FALSE"]	  
	  [(or (integer? datum) (flonum? datum))  (number->string datum)]
	  [else (error 'wscode->text "not a C-compatible literal: ~s" datum)])]
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

	;; This is inefficient.  Only want to call getDirect once!
	;; Can't trust the C-compiler to know it's effect free and do CSE.
	[(seg-get ,[seg] ,[ind])	 
	 `("(" ,seg ".getDirect())[" ,ind  "]")]
	
	;; Need to use type environment to find out what alpha is.
	[(newarr ,[int] ,[alpha]) "newarr_UNFINISHED"]

	[(arr-get ,[arr] ,[ind]) `(,arr "[" ,ind "]")]
	[(length ,arr) "array_length_UNFINISHED"]
	[(arr-set! ,x ...)
	 (error 'wscode->text "arr-set! in expression context: ~s" `(arr-set! ,x ...))]
	[(begin ,stmts ...)
	 (error 'wscode->text "begin in expression context: ~s" `(begin ,stmts ...))]

	;; Forming tuples.
	[(tuple ,[arg*] ...)
	 ;; Here we need to produce a new typedef for the struct type.
	 FINISH-TUPLES
	 ]

	; ============================================================
	;; Other prims fall through:
	[(,prim ,[rand*] ...)
	 (guard (regiment-primitive? prim))
	 `(,(PrimName prim) "(" ,(insert-between ", " rand*) ")")]
	[(app ,rator ,[rand*] ...)
	 (ASSERT (symbol? rator))				       
	 `(,(FunName rator) "(" ,@(insert-between ", " rand*) ")")]
	[,unmatched (error 'Expr "unhandled form ~s" unmatched)]))

    (define (Type t)
      (match t
	[Int "int"]
	[,simple (guard (memq simple '(Complex Float)))
		 (list->string (map char-downcase (string->list (symbol->string simple))))]
	[,v (guard (symbol? v)) (symbol->string v)]
	;; Went back and forth on whether this should be a pointer:
	[(Sigseg ,[t]) `("SigSeg<" ,t ">")]
	[(Signal ,[t]) `("Signal<" ,t ">")]
	[(Array ,[t]) `(,t "[]")]
	;[,other (format "~a" other)]
	[,other (error 'Type "Not handled yet.. ~s" other)]))

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


(define these-tests
  `(
    [(,text->string (,wscode->text '(lambda (x) (Int) '35) "noname"))
     ;; Requires helpers.ss
     ;; Not very tight:
     ,(lambda (s) (substring? "35" s))]
    [(,text->string (,wscode->text '(lambda (x) (Int) (+ '1 (if '#t '35 '36))) "noname"))
     ;"TRUE ? 35 : 36"]
     ,(let ([substring? substring?])
	(lambda (s) (substring? "1 + (TRUE ? 35 : 36)" s)))]
    ))

(define test-this (default-unit-tester "wavescript_emit-c.ss: generating WaveScript C code." these-tests))
(define test-wavescript_emit-c test-this)

) ;; End Module

