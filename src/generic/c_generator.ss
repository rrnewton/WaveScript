
;;;; .title c_generator.ss
;;;; .author Ryan Newton

;;;; I already have pass32_emit-nesc.ss, but I'm looking to make a more
;;;; generically useful C-generation library.  This should be very simple.

;;;; The representation used here is that of a list of strings.  This
;;;; list is manipulated extensively, but the strings should only be
;;;; appended once -- at the end.

;;;; For added flexibility these procedures can take straight strings
;;;; and wrap them as lists.  That is:
;;;;  type Text = String | List of String

(module c_generator mzscheme   
  (require )  
  (provide test-cgenerator

	   wrap text->string append-text 
	   text->lines 

	   ;; TEMP
	   ;graft! 
	   testtext

	   indent block WSBox 
	   wscode->text wsquery->text

	   testme	   testme2
	   testme0

	   )   
  (chezprovide )  
  (chezimports ) ;regiment_helpers

;;======================================================================
;;; "Text" ADT implementation:
;;; A more efficient implementation would probably make line structure explicit.
(begin 

  ;; This takes a Text or a string and returns a Text.
  (define (wrap x)
  ;; If it's a string, then we just wrap it.
  (or (and (string? x) (list x))
      (IFDEBUG 
       ;; We do this more defensively in debug mode
       (and (list? x)
	    (let loop ([x x])
	      (or (string? x) 
		  (null? x)
		  (and (pair? x) (loop (car x)) (loop (cdr x)))
		  (error 'wrap "can't make into Text: ~s" x)
		  ))
	    x)
       ;; In nondebug we just assume it's the right kind of thing:
       (and (pair? x)  x))
      (error 'wrap "can't make into Text: ~s" x)))

  (define append-text append)

  ;; Internal helper function for text->lines below.
  ;; Works on reversed lists, so it looks for incomplete lines at the head of lines2.
  ;; Currently this does string-appending, but it could just preserve the list structure.
  (define (graft! lines1 lines2)
      (define (last-cell pr)
	(if (null? (cdr pr)) pr
	    (last-cell (cdr pr))))
      (define my-append string-append)
      ;(define my-append list)
      (cond 
       [(null? lines1) lines2]
       [(null? lines2) lines1]
       [else (case (length (car lines2))
	       ;; Complete line (with "\n")
	       [(2) (set-cdr! (last-cell lines1) lines2)]
	       ;; Incomplete line.
	       [(1) 
		(let ([cell (last-cell lines1)])
		  (set-cdr! cell (cdr lines2))
		  (set-car! cell (cons (my-append (caar lines2) (caar cell)) (cdar cell))))]
	       [else (error 'text->lines "internal error, bad line: ~s" (car cell))])
	     lines1]))

;; Takes a Text and returns a list of Text.
;; NOTE: ADDS A TRAILING NEWLINE.
;; Should be its own fixed point.
(define (text->lines text)
  (let ([text (wrap text)])
    ;; Tail recursive
    (let loop ([text text] [acc '()])
      (cond
       [(null? text)  (reverse! acc)]
       [(string? (car text))
	(loop (cdr text)
	      (graft! 
	       (let* ([ls (reverse! (string-split (car text) #\newline))]
		      [terminated (map (lambda (s) (list s "\n")) (cdr ls))])
		 (if (equal? (car ls) "")
		     terminated
		     ;; Don't put a newline on the last one:	       
		     (cons (list (car ls)) terminated)))
	       acc))]
       [(null? (car text)) (loop (cdr text) acc)]
       [(pair? (car text))
	(loop (cdr text)
	      (graft! (reverse! (text->lines (car text))) acc))]
       [else (error 'text->lines "Bad 'Text' object: ~s" (car text))]))))

(define (text->string txt)
  (define out (open-output-string))
  (let loop ([txt txt])
    (cond
     [(null? txt) (void)]
     [(string? txt) (display txt out)]
     [(not (pair? txt)) (error 'text->string "bad Text: ~s" txt)]
     ;[(null? (car txt)) (loop (cdr txt))]
     [else (loop (car txt))
	   (loop (cdr txt))]))
  (get-output-string out))

) ;; End Text implementation
;; ======================================================================

;======================================================================
;;; Simple STRING implementation of "Text"
#;
(begin 
  (define append-text string-append)
  (define (text->lines s) (string-split s "\n"))
  (define (text->string s) s)
  (define (wrap txt)    
    (if (string? txt) 
	txt
	(let ([out (open-output-string)])
	(let loop ([txt txt])
	  (cond
	   [(null? txt) (void)]
	   [(string? txt) (display txt out)]
	   [else (loop (car txt))
		 (loop (cdr txt))]))
	(get-output-string out))))
  (define graft! #f)
)
;======================================================================

;; This takes "Text" and adds indentation.
(define (indent text str)
  (map (lambda (line) (cons str (wrap line)))
    (text->lines text)))

(define (block header contents . footer)
  `(,header " {\n"
    ,@(indent contents "  ")
    "} " ,@footer "\n"
    ))

;======================================================================
;;                       <WaveScript C generation>
;======================================================================

;; Should I use this sort of name mangling?  The numbers from unique
;; names will keep things unique...
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

(define (no-recursion! binds)
  ;; TODO FINISHME!!! FIXME!
  #t)

;======================================================================
;;; wscode->text and wsquery->text are the main entrypoints:

;; This takes an entire "wiring diagram" and makes C code.
(define wsquery->text
  (let ()
    ;; .param name   A string naming the variable that stores the current result.
    ;; .param type   Type of current result.
    ;; .param x      The query construct to process.
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
	 (values `("RawFileSource ",name 
		   " = RawFileSource(\"/tmp/archive/4/marmots/meadow1vxp.all_8_100_973449798.903759_0.raw\", \n"
		   "                       " ,channel ", 4, 48000*30, WSSched::findcpuspeed());\n")
		 '())]
	
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
	[(,lang (quote (program (letrec ,binds ,body) ,typ)))
	 (mvlet ([(return-name) body]
		 [(body funs) (Query "toplevel" typ `(letrec ,binds ,body))])
	   (ASSERT (symbol? return-name))

	   `(,boilerplate_headers 
	     ,funs
	     ,boilerplate_premain
	     ;"// " ,(Type typ) " toplevel;\n"
	     ,(indent body "  ")
	     ,(boilerplate_postmain (Var return-name) typ)
	     "}\n\n"
	     )
	   )]
	[,other (error 'wsquery->text "bad top level WS program: ~s" other)]))))

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

(define boilerplate_headers "
#include <WaveScope.h>
#include <Heartbeat.hpp>
#include <PrintBox.hpp>
#include <RawFileSource.hpp>
#include <AsciiFileSink.hpp>
#include <Boxes.hpp>

#include <stdio.h>

#define TRUE 1
#define FALSE 0

class WSPrim {

  static const vector<complex> fft( const vector<complex> input) {
    printf(\"FFT.\\n\");
//     int i;
//     complex *fft_buf = new complex[input.length()];
//     for (i=0; i<input->length(); i++)
//       fft_buf[i] = input[i];

//     float *fft_flt = (float *)fft_buf;    

//     /* copy input over to output buffer */
//     float *cbuf = casted->getDirect();
//     memmove(fft_flt, cbuf, sizeof(float)*input->length());

//     /* do the fft */
//     realft(fft_flt-1, casted->length(), +1);    

//     /* copy back over to an STL vec */
//     vector<complex> output = new vector<complex>(input->length());
//     for (i=0; i<input->length(); i++)
//       output[i] = [i];

//     return output;
    return input;
  }

};

")

(define boilerplate_premain "

int main(int argc, char ** argv)
{
  /* initialize subsystems */
  MiscLogInit(&argc, argv);
  TimebaseMgrInit(&argc, argv);

")

(define (boilerplate_postmain return_name return_type) `(
,@(if (equal? return_type '(Signal (Sigseg Float)))
      `("
  /* dump output specgram to file */
  AsciiFileSink<float> fs = AsciiFileSink<float>(\"/tmp/specgram.out\");
  fs.connect(&",return_name");
") '()) "

  /* now, run */
  WSSource::StartThreads();
  WSSched::run();

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
	[(if ,[test] ,[conseq] ,[altern])
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
	
	;; TODO: tupref
	[(,infix_prim ,[left] ,[right])
	 (guard (memq infix_prim '(+ - * / < > <= >= ==				     
				     +. -. *. /. )))
	 (let ([cname (case infix_prim
			[(+ * - / < > <= >= ==) infix_prim]
			[(+. *. -. /.) ;; Chop off the period:
			 (substring (symbol->string infix_prim) 0 1)])])
	   `("(" ,left ,(format " ~a " cname) ,right ")"))]

	;; This is inefficient.  Only want to call getDirect once!
	;; Can't trust the C-compiler to know it's effect free and do CSE.
	[(seg-get ,[seg] ,[ind])	 
	 `("(" ,seg "->getDirect())[" ,ind  "]")]
	
	;; Need to use type environment to find out what alpha is.
	[(newarr ,[int] ,[alpha]) "newarr_UNFINISHED"]

	[(arr-get ,[arr] ,[ind]) `(,arr "[" ,ind "]")]
	[(length ,arr) "array_length_UNFINISHED"]
	[(arr-set! ,x ...)
	 (error 'wscode->text "arr-set! in expression context: ~s" `(arr-set! ,x ...))]
	[(begin ,stmts ...)
	 (error 'wscode->text "begin in expression context: ~s" `(begin ,stmts ...))]

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
	[Integer "int"]
	[,simple (guard (memq simple '(Complex Float)))
		 (list->string (map char-downcase (string->list (symbol->string simple))))]
	[,v (guard (symbol? v)) (symbol->string v)]
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
                                         (letrec ([x Integer 3])
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

#;

(wscode->text
 '(lambda (w)
    (letrec ((___VIRTQUEUE___ (virtqueue)))
      (begin
	(emit ___VIRTQUEUE___ (app fft (app fft (app to_array w))))
	___VIRTQUEUE___))))

#;
(display (text->string (wscode->text
  '(lambda (w)
     ((Sigseg Complex))
     (letrec ([___VIRTQUEUE___ (VQueue
				(Array
				 Complex)) (virtqueue)])
       (begin
	 (emit
	  ___VIRTQUEUE___
	  (fft (fft (to_array
		     w))))
	 ___VIRTQUEUE___))))))
  

(define testtext
  '("foo();\n"
    "bar();\n"))

(define these-tests
  `(
    [(,wrap "foo")   ("foo")]
    [(,wrap '("foo" "bar")) ("foo" "bar")]
    
    ,@(IFDEBUG `([(,wrap '("foo" "bar" 39)) error])
	       '())    

    [(apply ,graft! (map list-copy '((("a" "\n") ("b" "\n")) (("x" "\n") ("y" "\n")))))
     (("a" "\n") ("b" "\n") ("x" "\n") ("y" "\n"))]
    ["graft! with an incomplete line."
     (apply ,graft! (map list-copy '((("a" "\n") ("b" "\n")) (("x") ("y" "\n")))))
     (("a" "\n") ("xb" "\n") ("y" "\n"))]

    [(text->lines '( "one\ntwo" "too\nthree"))
     (("one" "\n") ("twotoo" "\n") ("three"))]
    [(text->lines (text->lines '("one\ntwo" "too\nthree")))
     (("one" "\n") ("twotoo" "\n") ("three"))]
    [(text->lines '((("one" "\n") ("two")) (("too" "\n") ("three"))))
     (("one" "\n") ("twotoo" "\n") ("three"))]

    ;; This one is overly strict.  text->lines needn't actually flatten to this extent.
    [(text->lines '((("one" "\n") ("two")) (("too" "\n") (("three" ((("f\nour\n"))))))))
     (("one" #0="\n") ("twotoo" #0#) ("threef" #0#) ("our" "\n"))]

    [(,text->string "foo") "foo"]
    [(,text->string '("foo" "bar")) "foobar"]
    
    [(text->string (wscode->text '(lambda (x) (Integer) '35) "noname"))
     ;; Requires helpers.ss
     ;; Not very tight:
     ,(lambda (s) (substring? "35" s))]
    [(text->string (wscode->text '(lambda (x) (Integer) (+ '1 (if '#t '35 '36))) "noname"))
     ;"TRUE ? 35 : 36"]
     ,(lambda (s) (substring? "1 + (TRUE ? 35 : 36)" s))]

    ))
(define test-this (default-unit-tester "c_generator.ss: generating C code." these-tests))
(define test-cgenerator test-this)

) ;; End module