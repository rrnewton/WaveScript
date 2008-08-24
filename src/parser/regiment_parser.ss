;; An interactive calculator inspired by the calculator example in the bison manual.

;; TODO:

;; *) Handle complex constants.
;; *) Handle arr[3][4]

(module regiment_parser mzscheme  
  ;; Import the parser and lexer generators.
  (require (lib "yacc.ss" "parser-tools")
	   (lib "lex.ss" "parser-tools")
	   (prefix : (lib "lex-sre.ss" "parser-tools"))
	   
	   "iu-match.ss"
	   ;"../generic/util/helpers.ss"
	   ;;"plt/prim_defs.ss"
	   )
  (provide (all-defined))

;; RESERVED name!
;; This is lame, I should do my emit hack later in the compiler... [2006.07.25]
;; Here in the parser we don't have access to unique-name generation:
(define VIRTQUEUE '___VIRTQUEUE___)
  
(define-empty-tokens op-tokens 
   (LeftParen RightParen
    LeftBrace RightBrace
    LeftAngleBrk RightAngleBrk 
    LeftSqrBrk RightSqrBrk
    HashLeftSqrBrk

    ::: $
    += -= *= := -> = == != >= <= < > <-
    + - * / ^ 
    g+ g- g* g/ g^ 
    +_ -_ *_ /_ ^_ 
    +. -. *. /. ^. 
    +: -: *: /: ^: 
    :: ++  COLON
    AND OR NEG HASH 
    APP SEMI COMMA DOT MAGICAPPLYSEP DOTBRK DOTSTREAM BAR BANG
    ; Keywords :
    fun for while to emit return include deep_iterate iterate state in if then else true false break let 
    namespace using AS typedef uniontype static case
;    foreign foreign_box foreign_source 
    typecase returncase

    ;; Fake tokens:
    EXPIF STMTIF ONEARMEDIF
    ;SLASHSLASH NEWLINE 
    EOF ))
(define-tokens value-tokens (NUM DOUBLE VAR DOTVARS TYPEVAR NUMVAR CHAR STRING))

(define-lex-abbrevs (lower-letter (:/ "a" "z"))
                    (upper-letter (:/ #\A #\Z))
  ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
                    (digit (:/ "0" "9"))
		    
		    ;; This can include colons for namespace specifiers.
		    ;; Currently namespaces must be more than one letter!!
                    (variable (:seq (:or lower-letter upper-letter "_")
                                    (:* (:or lower-letter upper-letter "_" digit
					     ;; Can have a single semi-colon inbetween the reasonable characters:
					     (:seq (:or lower-letter upper-letter "_" digit)
						   ":"
						   (:or lower-letter upper-letter "_" digit))
					     )))))

(define (unescape-chars str)
  (read (open-input-string (string-append "\"" str "\""))))

(define ws-lex
  (lexer-src-pos
   [(eof) 'EOF]
   ;; Ignore all whitespace:
   [(:or #\tab #\space #\newline #\return) (return-without-pos (ws-lex input-port))]
   ;; Throw out the rest of the line:
   ;["//" 'SLASHSLASH]
   
   ;; WARNING: These recursive calls produce NESTED position tokens... only the inner one is valid!!
   ["//" (begin (read-line input-port) (return-without-pos (ws-lex input-port)))]
   ["/*" (begin (read-balanced "/*" "*/" input-port 1) (return-without-pos (ws-lex input-port)))]
;   ["(*" (begin (read-balanced "(*" "*)" input-port 1) (return-without-pos (ws-lex input-port)))]
  
   ;; Since (token-=) returns '=, just return the symbol directly
   [(:or "::" "++" 
	 "->" "<-" ":::" "$"
	 ":=" "+=" "*=" "-="
	 "<=" "<" ">" ">=" "==" "!="
	 "=" "+" "-" "*" "/" "^" 
	 "g=" "g+" "g-" "g*" "g/" "g^" 
         "+_" "-_" "*_" "/_" "^_"
         "+." "-." "*." "/." "^."
         "+:" "-:" "*:" "/:" "^:"
	 )
    (string->symbol lexeme)]
   ;; Keywords: 
   [(:or "fun" "for" "while" "break" "to" "emit" "return" "include" "deep_iterate" "iterate" 
	 "state"  "in" "if" "then" "else" "true" "false" "let" 
	 "namespace" "using" "static" "uniontype" "case" "typecase" "returncase" 
;	 "foreign" "foreign_box" "foreign_source"
	 )
    (string->symbol lexeme)]
   ["as" 'AS]
   ["type" 'typedef]
   
   [(:seq "'" (:or (:seq "\\" any-char) (:- any-char "'")) "'")
    (token-CHAR (string-ref lexeme 1))]
   [(:seq "'" (:+ lower-letter)) (token-TYPEVAR (string->symbol (substring lexeme 1 (string-length lexeme))))]
   ;; This allows strings to contain escaped characters.
   [(:seq "\"" (:* (:or (:- (:- any-char "\"")  "\\") ;; Safe characters
			(:seq "\\" any-char)))        ;; Escape sequences
	  "\"")
    (token-STRING (unescape-chars (substring lexeme 1 (sub1 (string-length lexeme)))))]

   ;["#"  'HASH]
   [(:seq "#" (:+ lower-letter)) (token-NUMVAR (string->symbol (substring lexeme 1 (string-length lexeme))))]
   ["&&" 'AND] ["||" 'OR]
   
   ;; Delimiters:
   [";" 'SEMI]  ["," 'COMMA] ;["'" 'QUOTE]
   ["|" 'BAR] ["!" 'BANG]
   [":" 'COLON] ;; A single colon.
   ["(" 'LeftParen]  [")" 'RightParen]
   ["{" 'LeftBrace]  ["}" 'RightBrace]
   ["[" 'LeftSqrBrk] ["]" 'RightSqrBrk]
   ["<" 'LeftAngleBrk] [">" 'RightAngleBrk]
   ["#[" 'HashLeftSqrBrk]

;   ["sin" (token-FNCT sin)]

   ;; Variables:
   [variable (token-VAR (string->symbol lexeme))]

   ;; Dot/stream-projection
   [".(" 'DOTSTREAM]

   ;; Dot-syntax:
;   [(:seq (:+ (:seq variable ".")) variable)  (token-DOTVARS (map string->symbol (string-split lexeme #\.)))]
   ["." 'DOT]
   ["`" 'MAGICAPPLYSEP] 

   ;; Parsing Numbers:
   [(:seq ;(:or "-" "")
	  (:+ digit)) (token-NUM (string->number lexeme))]
   ;; Floating point:
#;
   [(:seq ;(:or "-" "") 
	  (:: (:+ digit) #\. (:* digit)))
    (token-NUM (string->number lexeme))]
   ;; Floating point exponential (scientific) notation:
   [(:seq ;(:or "-" "") 
          (:: (:+ digit) #\. (:* digit))
	  (:or "" (:seq "e" (:or "-" "+" "") (:+ digit)))
	  (:or "" "l" "L" "f" "F"))
    (let ([lastchar (string-ref lexeme (sub1 (string-length lexeme)))])
      (cond
	 ;; By default floating point numbers are currently considered floats.
	 ;; Should change this...
	 [(or (eq? lastchar #\f) (eq? lastchar #\F))
	  (token-NUM 
	   (string->number 
	    (substring lexeme 0 (sub1 (string-length lexeme)))))]
	 [(or (eq? lastchar #\l) (eq? lastchar #\L))
	  (token-DOUBLE 
	   (string->number	    
	    (substring lexeme 0 (sub1 (string-length lexeme)))))]
	 [else (token-NUM (string->number lexeme))]
	 )
      )
    
    ]

   ;; FIXME: Doesn't support exponential notation:
   ;; Complex:
   [(:seq ;(:or "-" "") 
	  (:or (:+ digit) (:: (:+ digit) #\. (:* digit))) "+"
	  (:or (:+ digit) (:: (:+ digit) #\. (:* digit))) "i")
    ;; All complex nums are inexact currently:
    (token-NUM (exact->inexact (string->number lexeme)))]
   
   ))

(define (read-balanced startstr endstr port balance)
  (define start (string->list startstr))
  (define end (string->list endstr))
  (define len (length start))
  (unless (= len (length end))
    (error read-balanced "start and end must be the same length"))
  (unless (zero? balance)
    (let loop ([peek (let ([ls '()])
                       (do ([i 0 (add1 i)])
                         ((= i len) (reverse ls))
                         (set! ls (cons (read-char port) ls))))]
               [balance balance])
      (define (scroll) (reverse (cons (read-char port)
                                      (reverse (cdr peek)))))
      ;(printf "PEEK: ~s  start/end ~s ~s\n" peek startstr endstr)
      (cond
        [(zero? balance) (void)]
        [(equal? peek start) (loop (scroll) (add1 balance))]
        [(equal? peek end)   (loop (scroll) (sub1 balance))]
        [else                (loop (scroll) balance)]))))

(define (format-pos pos)
  (if (position-line pos)
      (format "line ~a:~a" (position-line pos) (position-col pos))
      (format "char ~a" (position-offset pos))))
      
;; Can get rid of this now that I'm using match...
(define make-begin
  (lambda  (ls)
    (let ((flattened 
           (let loop ([ls ls])
             (cond [(null? ls) '()]
                   [(and (pair? (car ls)) (eq? 'begin (caar ls)))
                    (append (loop (car ls)) (loop (cdr ls)))]
                   [else (cons (car ls) (loop (cdr ls)))]))))
      (cond [(null? flattened) '(tuple)] ;; Unit.
            [(null? (cdr flattened)) (car flattened)]
            [else (cons 'begin flattened)]))))

(define consify
  (lambda (ls)
    (if (null? ls) ''()
	`(cons ,(car ls) ,(consify (cdr ls))))))

(define (lower-case? s)
  (char-lower-case? 
   (string-ref 
    (symbol->string s) 0)))

;; Put in a list just to get drscheme's print-graph to treat it properly!
(define thefile (list "FILE_NOT_SET!?"))

(define source-position-tracking (make-parameter #t))

;; This wraps the source position information in the way the Regiment compiler expects.
(define (wrap pos end x)
  (if (source-position-tracking)
      `(src-pos #(,thefile ,(position-offset pos) ,(position-line pos) ,(position-col pos)
			   ,(position-offset end) ,(position-line end) ,(position-col end))
		,x)
      x))

(define (unwrap x)
  (if (source-position-tracking)   
      (if (eq? (car x) 'src-pos) (caddr x)
	  (error 'parser:unwrap "expected src-pos form"))
      ;(match x [(src-pos ,p ,e) e])
      x))

(define (ws-parse . args)
  (if (file-exists? "_parser.log")
      (delete-file "_parser.log"))
  (let ()

    (define (tag-double num)
      ;`(assert-type Double ',num) 
      `(assert-type Double (cast_num ',num))
      )

   ;; Returns a function that takes a lexer thunk producing a token on each invocation:a
    (define theparser
      (parser
    
   (src-pos)

   ;(suppress) ;; IMPORTANT!!! SUPPRESSES CONFLICT INFO! COMMENT OUT WHEN DEBUGGING PARSER!

   (start start)
   (end EOF)
   
   (tokens value-tokens op-tokens)
   (error (lambda (a b c start end) 
            (printf "PARSE ERROR: after ~a token, ~a~a.\n" 
                    (if a "valid" "invalid") b 
                    (if c (format " carrying value ~s" c) ""))
            (printf "  Located between ~a and ~a.\n"
                    (format-pos start) (format-pos end))))
   ;; Precedence:
   (precs 
    
          (left emit return)

	  ;; These have weak precedence:
          (right = := += -= *= -> )
	  (right AND OR ) 

	  ;(right)
	  (right then else )

    	  (nonassoc SEMI)
	  ;;(nonassoc ONEARMEDIF)
	  (nonassoc EXPIF if STMTIF)

	  (right ++ ::: $)
          (left < > <= >= == !=)

	  ;; Multiplication takes precedence over addition:
          (left - + g- g+ -_ +_ +. -.  +: -:)
          (left * / g* g/ *_ /_ *. /.  *: /:)

;          (left LeftSqrBrk)
;          (left DOTBRK) 
;	  (right BAR)
	  ;(right BANG) ;; This should be pretty strong but weaker than DOT.
          (left NEG APP DOT MAGICAPPLYSEP COMMA       )
          (right ^ g^ ^_ ^. ^:  BANG)

    	  (nonassoc in)
	  )


   (debug "_parser.log")

   (grammar
    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           ;[(error start) $2]
           [(decls) (cons 'program $1)])

    ;; This adds source position information!!
    (exp [(exp-raw) (wrap $1-start-pos $1-end-pos $1)])
    (selfterminated [(selfterminated-raw) (wrap $1-start-pos $1-end-pos $1)])

    ;; This is an expression which must, however, be a variable.
    ;; Yet it can still keep a source location.
    ;; [2007.07.13] This caused a reduce-reduce conflict so I had to do it manually.
;    (VarExp [(VAR) (wrap $1-start-pos $1-end-pos $1)])
;    (VarExp [(VAR) $1])

    (type
	  ;[(LeftParen type COMMA typeargs -> type RightParen) `(,$2 ,@$4 -> ,$6)]
          [(LeftParen type COMMA typeargs RightParen -> type) `(,$2 ,@$4 -> ,$7)]
	  ;; This is a special case: zero argument function:
	  [(LeftParen RightParen -> type) `( -> ,$4)]

	  [(type -> type) `(,$1 -> ,$3)]
     
          ;[(typeargs -> type) `(,@$1 -> ,$3)]
          
	  ;[(type COMMA type -> type) `(,$1 ,$3 -> ,$5)]
	  ;[(type COMMA typeargs -> type) `(,$1 ,@$3 -> ,$5)]
	  ;[(type -> type)  `(,$1 -> ,$3)]
	  ; Ambiguous: a, b -> c, d -> e

	  ;; Strings can be types... kinda... this only works for "Pointer" types.
	  [(STRING) $1]

          ;; A type constructor, make it right-associative.
;          [(VAR type) (prec APP) (list $1 $2)]
;          [(VAR VAR) (prec APP) (list $1 $2)]
;          [(VAR LeftParen typeargs RightParen) (prec APP) (cons $1 $3)]
;          [(LeftParen VAR type type* RightParen) (prec APP) (list* $2 $3 $4)]

          [(VAR type) (prec APP) (list $1 $2)]
          [(VAR LeftParen type COMMA typeargs RightParen) (prec APP) (list* $1 $3 $5)]

	  [(typevar) $1]

          ;; Tuple types:
	  [(LeftParen RightParen) (vector)]
          ;; No one-tuples!!
	  [(LeftParen type RightParen) $2]

          [(LeftParen type * typetuple RightParen) (apply vector (cons $2 $4))]
	  
	  ;[(type * typetuple) (list->vector $1)]
	  ;[(type * type * type) (vector $1 $3 $5)]
	  ;[(type * type) (vector $1 $3)]
	  ;[(typetuple) (list->vector $1)]
          )

    (typevar 
	  ;; Hmm: can't make up my mind whether we should single-quote typevars:
          [(VAR) (if (lower-case? $1) `(quote ,$1) $1)]
          [(TYPEVAR) `(quote ,$1)]
          ;[(HASH TYPEVAR) `(NUM ,$2)]
	  [(NUMVAR) `(NUM ,$1)])
    (typevars [() ()] [(typevar typevars) (cons $1 $2)])

    (typetuple [(type) (list $1)]
	       [(type * typetuple) (cons $1 $3)])
    (typeargs [(type) (list $1)]
              [(type COMMA typeargs) (prec COMMA) (cons $1 $3)]
              )
;     (typevar [(VAR) (if (lower-case? $1) `(quote ,$1) $1)]
; 	     [(TYPEVAR) `(quote ,$1)]
; 	     [(NUMVAR) `(NUM ,$1)])
;     (typevars+ [(typevar) (list $1)]
; 	       [(typevar COMMA typevars+) (prec COMMA) (cons $1 $3)]
; 	       )

    (type* [() ()]  [(type type*) (cons $1 $2)])
    (unioncases [(VAR type*)                `((,$1 ,@$2))]
		[(VAR type* BAR unioncases) `((,$1 ,@$2) . ,$4)])
    
    (decls ;; Top level variable binding
           ;; It's unfortunate that this duplicates much within the "stmt" production.

           [(typedef VAR = type SEMI maybedecls)         `((typedef ,$2 () ,$4) ,@$6)]
           [(typedef VAR typevar = type SEMI maybedecls) `((typedef ,$2 (,$3) ,$5) ,@$7)]
	   ;; This takes multiple type arguments within parentheses!!
	   ;; ACK inconsistent with uniontypes!!!
           [(typedef VAR LeftParen typeargs RightParen = type SEMI maybedecls) `((typedef ,$2 ,$4 ,$7) ,@$9)]

	   ;; TAGGED UNION:
	   ;; Haven't decided whether one or both of these terms is required:
	   ;; Only one typevar for now:
	   [(uniontype VAR = unioncases SEMI maybedecls)         `((uniondef (,$2) . ,$4) . ,$6)]
	   [(uniontype VAR typevars = unioncases SEMI maybedecls) `((uniondef (,$2 ,@$3) . ,$5) . ,$7)]

           [(VAR :: type SEMI maybedecls) `((:: ,$1 ,$3) ,@$5)]
           [(VAR :: type = exp optionalsemi maybedecls) `((define ,$1 (assert-type ,$3 ,$5)) ,@$7)]
           [(VAR = exp optionalsemi maybedecls) `((define ,$1 ,$3) ,@$5)]
           [(let pattern = exp optionalsemi maybedecls) `((define ,$2 ,$4) ,@$6)]
           [(let pattern :: type = exp optionalsemi maybedecls) `((define ,$2 (assert-type ,$4 ,$6)) . ,$8)]

           [(let VAR AS pattern = exp optionalsemi maybedecls) `((define-as ,$2 ,(vector->list $4) ,$6) . ,$8)]
           [(VAR AS pattern = exp optionalsemi maybedecls)     `((define-as ,$1 ,(vector->list $3) ,$5) . ,$7)]
	   
	   [(include exp optionalsemi maybedecls)  `((include ,(unwrap $2)) . ,$4)]
	   ;; [2007.03.19] Now including this at the decl level also:
	   [(using VAR SEMI maybedecls)   `((using ,$2) . ,$4)]

	   [(namespace VAR LeftBrace maybedecls RightBrace optionalsemi maybedecls) `((namespace ,$2 ,@$4) ,@$7)]

           ;; Returning streams to the base station or other "ports"
           [(VAR <- exp optionalsemi maybedecls) `((<- ,$1 ,$3) ,@$5)]
           
           ;; Fundef is shorthand for a variable binding to a function.
           [(fundef optionalsemi maybedecls) (cons $1 $3)]
           )
    (maybedecls [() '()] [(decls) $1])

    (fundef [(fun VAR LeftParen formals RightParen exp) ;LeftBrace stmts RightBrace) 
             `(define ,$2 (lambda ,$4 ,$6))]
            )
   (formals [() '()]
	    [(formals+) $1]
	    )
   (formals+ [(pattern_or_ascript) (list $1)]
	     [(pattern COMMA formals) (cons $1 $3)])

   (pattern_or_ascript [(pattern) $1]
		       [(pattern :: type) `(assert-type ,$3 ,$1)])

   ;; [2006.09.01] For now patterns are just tuples.
   (pattern [(VAR) $1]
	    ;; A type constructor:
	    [(VAR LeftParen RightParen)      `(data-constructor ,$1)]
	    [(VAR LeftParen pat+ RightParen) `(data-constructor ,$1 ,@$3)]
	    [(LeftParen RightParen) #()]
	    [(LeftParen pattern COMMA pat+ RightParen) `#(,$2 ,@$4)]
	    )
   (pat+ [(pattern) (list $1)]
	 [(pattern COMMA pat+) (cons $1 $3)])

   ;; Single statements.  These make sense outside of the context of other statements
   (stmt [(exp) $1]
	 ;; [2006.09.03] Demoting these to statements:
	 ;; Making semi-colon optional after for loop.

	 ;[(break) '(break)]
	 ;[(emit exp )   `(emit ,VIRTQUEUE ,$2)]

	 ;; [(return exp SEMI stmts) (cons `(return ,$2) $4)]

	 ;; One-armed statement conditional:
	 ;; Return unit:
	 ;[(if exp then stmt) (prec ONEARMEDIF) `(if ,$2 ,$4 (tuple))]
	 
	 ;; Statement conditional:
	 ;[(if exp then stmt else stmt)  `(if ,$2 ,$4 ,$6)]
	 ;; LAME: Allowing optional semi-colon:
	 ;[(if exp then stmt SEMI else stmt) (prec STMTIF) `(if ,$2 ,$4 ,$7)]
	 	
	 [(selfterminated) $1]
	 )
   ;; Blocks of statements.
   ;; Lots of redundancy with top level 'decls'
   (stmts  [() '()]
	   ;; LAME: We require let only when you're going to use a pattern.
           [(let pattern = exp SEMI stmts) `((letrec ([,$2 ,$4]) ,(make-begin $6)))]
           [(VAR = exp SEMI stmts) `((letrec ([,$1 ,$3]) ,(make-begin $5)))]

           [(let VAR AS pattern = exp SEMI stmts) `((let-as (,$2 ,(vector->list $4) ,$6) ,(make-begin $8)))]
           [(VAR AS pattern = exp SEMI stmts) `((let-as (,$1 ,(vector->list $3) ,$5) ,(make-begin $7)))]

           [(VAR :: type = exp SEMI stmts) `((letrec ([,$1 (assert-type ,$3 ,$5)]) ,(make-begin $7)))]
           [(let pattern :: type = exp SEMI stmts) `((letrec ([,$2 (assert-type ,$4 ,$6)]) ,(make-begin $8)))]

	   [(using VAR SEMI stmts) `((using ,$2 ,(make-begin $4)))]

	   [(fundef morestmts)
	    (let ([bind1 (match $1 [(define ,bind ...) bind])])
	      (match $2
		[((letrec ,morebinds ,bod) . ,rest)
		 (unless (null? rest)
		   (error 'parser "implementation error in fundef case"))
		 `((letrec ,(cons bind1 morebinds) ,bod) . ,rest)]
		[,oth `((letrec (,bind1) ,(make-begin $2)))]))]
	   
	   ;; This avoids conflicts:
	   [(selfterminated stmt morestmts) (cons $1 (cons $2 $3))]
           [(stmt morestmts) (cons $1 $2)]
           )
   (optionalsemi [() 'OPTIONALSEMI] [(SEMI) 'OPTIONALSEMI])
   (morestmts [() '()] [(SEMI stmts) $2])

   ;; These have a syntax that allows us to know where they terminate and optionally omit SEMI:
   ;; This is a hack....
   (selfterminated-raw 
    [(for VAR = exp to exp LeftBrace stmts RightBrace) `(for (,$2 ,$4 ,$6) ,(make-begin $8))]
    [(while exp LeftBrace stmts RightBrace) `(while ,$2 ,(make-begin $4))]
    )

    ;; Kinda redundant, used only for state {} blocks.
    (binds [() ()]
           [(bind) (list $1)]
	   ;; Special stream-binding syntax:
;           [(VAR < formals+ > = exp) (list 
;				      (list $1 $3)
;				      )]

           [(bind SEMI binds) (cons $1 $3)])
    ;; Adding 'static' bindings (only allowed on iterator state)
    (bind [(VAR = exp)         (list $1 $3)]
	  [(VAR :: type = exp) (list $1 `(assert-type ,$3 ,$5))]
	  [(static VAR = exp)         (list $2 `(static ,$4))]
	  [(static VAR :: type = exp) (list $2 `(static (assert-type ,$4 ,$6)))])

    (iter [(iterate) 'iterate]
          [(deep_iterate) 'deep-iterate])
    ;; [2007.03.25] Making parens optional:
    (iterbinder 
     [(pattern_or_ascript in exp)                      (list $1 $3)]
     [(LeftParen pattern_or_ascript in exp RightParen) (list $2 $4)])

    (tuple 
     [(LeftParen RightParen)  `(tuple)]
     [(LeftParen exp COMMA expls+ RightParen) `(tuple ,$2 ,@$4)]

     ;; Records:
     ;; These are not implemented yet, but just wanted to put them in
     ;; the parser so I know they don't cause future conflicts:
     [(LeftParen recordbinds+ RightParen) `(record ,@$2)]
     [(LeftParen exp BAR recordbinds+ RightParen) `(record-update ,$2 ,@$4)]
     ;; Alternatively, I kind of like this syntax:
     [(LeftBrace BAR recordbinds+ RightBrace) `(record ,@$3)]
     [(LeftBrace exp BAR recordbinds+ RightBrace) `(record-update ,$2 ,@$4)]

     
     )
    (recordbinds+ [(VAR = exp) (list (list $1 $3))]
		  [(VAR = exp COMMA recordbinds+) (cons (list $1 $3) $5)])


    ;; Hack to enable my syntax for sigseg refs!!
    ;; We separate out lists from other sy
    (exp-raw 
	  ;; List constants:
          [(LeftSqrBrk expls RightSqrBrk) (consify $2)]
	  [(notlist) $1])
    (expls [() '()]
	   [(expls+) $1])
    (expls+ [(exp) (list $1)]
	    [(exp COMMA expls) (cons $1 $3)])

    ;(innernotlist [(notlist) $1])    

    (matchcases [() '()] 
		[(pattern COLON exp matchcases) (cons (list $1 $3) $4)]
		[(pattern COLON exp BAR matchcases) (cons (list $1 $3) $5)]
		)
;    (typecases  [() '()] [(type    COLON exp typecases)     (cons (list $1 $3) $4)])

    ;; Helper for array navigation.
    (arrayNav ;[() (lambda (x) x)]
	      [(LeftSqrBrk notlist RightSqrBrk)  (lambda (x) `(Array:ref ,x ,$2))]
	      ;[(LeftSqrBrk notlist RightSqrBrk arrayNav)  (lambda (x) ($4 `(Array:ref ,x ,$2)))]
	      [(arrayNav LeftSqrBrk notlist RightSqrBrk)  (lambda (x) `(Array:ref ,($1 x) ,$3))]
	      )
    
    (assignment
                  
         [(VAR := exp) `(set! ,$1 ,$3)]

	 ;; This is a verbose way to specify it, but I run into trouble if I try with just one production:
         [(VAR LeftSqrBrk notlist RightSqrBrk := exp)  `(Array:set ,(wrap $1-start-pos $1-end-pos $1) ,$3 ,$6)]         
         [(VAR arrayNav LeftSqrBrk notlist RightSqrBrk := exp)  
	  `(Array:set ,($2 (wrap $1-start-pos $1-end-pos $1)) ,$4 ,$7)]

	 ;; Shorthands:
         [(VAR += exp) `(set! ,$1 (+ ,$1 ,$3))]
         [(VAR *= exp) `(set! ,$1 (* ,$1 ,$3))]
         [(VAR -= exp) `(set! ,$1 (- ,$1 ,$3))]

	 ;; Now this is REALLY verbose.  And these add one shift/reduce conflict each.
         [(VAR LeftSqrBrk notlist RightSqrBrk += exp)  
	  (let ([var (wrap $1-start-pos $1-end-pos $1)])
	    `(Array:set ,var ,$3 (+ (Array:ref ,var ,$3) ,$6)))]
         [(VAR arrayNav LeftSqrBrk notlist RightSqrBrk += exp)
	  (let ([var (wrap $1-start-pos $1-end-pos $1)])
	    `(Array:set ,($2 var) ,$4 (+ (Array:ref ,($2 var) ,$4) ,$7)))]

	 ;; This case bumped us from 65->67 shift/reduce conflicts.
         [(VAR LeftSqrBrk notlist RightSqrBrk -= exp)  
	  (let ([var (wrap $1-start-pos $1-end-pos $1)])
	    `(Array:set ,var ,$3 (- (Array:ref ,var ,$3) ,$6)))]
         [(VAR arrayNav LeftSqrBrk notlist RightSqrBrk -= exp)
	  (let ([var (wrap $1-start-pos $1-end-pos $1)])
	    `(Array:set ,($2 var) ,$4 (- (Array:ref ,($2 var) ,$4) ,$7)))]

         [(VAR LeftSqrBrk notlist RightSqrBrk *= exp)  
	  (let ([var (wrap $1-start-pos $1-end-pos $1)])
	    `(Array:set ,var ,$3 (* (Array:ref ,var ,$3) ,$6)))]
         [(VAR arrayNav LeftSqrBrk notlist RightSqrBrk *= exp)
	  (let ([var (wrap $1-start-pos $1-end-pos $1)])
	    `(Array:set ,($2 var) ,$4 (* (Array:ref ,($2 var) ,$4) ,$7)))]
     )

    (notlist
       ;; TEMPTOGGLE: Do we wish to treat complex numbers as constants, or calls to makeComplex? (for ikarus)
     ;[(NUM) `',$1]
     ;; [2008.05.29] Activating for now:
     [(NUM) (if (real? $1) `',$1 `(app makeComplex
				       ',(exact->inexact (real-part $1)) 
				       ',(exact->inexact (imag-part $1))))]
     ;; FIXME: Need a better direct syntax for double constants.
     [(DOUBLE) (tag-double $1)]

;         [(VarExp)  $1]
	 [(VAR) $1]
         [(CHAR) $1]
         [(STRING) $1]
         [(true) ''#t] 
	 [(false) ''#f]	 

	  ;; Array constants, added [2008.03.26]
	  ;; HACK: for now this parses as a conversion from the list form!!
	  ;; Metaprogram evaluation should make it more efficient:
	  [(HashLeftSqrBrk expls RightSqrBrk) `(app List:toArray ,(consify $2))]

#;
         [(DOTVARS) 
           (let loop ([ls (cdr $1)] [acc (car $1)])
             (if (null? ls) acc
                 (loop (cdr ls)
                       `(app ,(car ls) ,acc))))]
         [(tuple) $1]
	 
	 ;; Deconstructing sum types with pattern matching:
	 [(case exp LeftBrace matchcases RightBrace) `(wscase ,$2 . ,$4)]

;	 [(typecase exp LeftBrace typecases RightBrace) `(typecase ,$2 ,$4)]
	 ;; Haven't decided how to do this yet.  Can typecase on the *return* type:
;	 [(returncase LeftBrace typecases RightBrace) `(returncase ,$3)]


	 ;; Importing foreign functions (better be in assert-type)
	 ;[(foreign STRING in STRING)  `(foreign ,$2 ,$4)]	 
	 ;; One extra shift/reduce
;	 [(foreign exp in exp)  `(foreign ,$2 ,$4)]

         
;         [(VAR DOT DOT LeftSqrBrk NUM RightSqrBrk) (prec DOTBRK) `(seg_get ,$4 ,$1)]
         [(VAR LeftSqrBrk LeftSqrBrk exp RightSqrBrk RightSqrBrk) `(app seg_get ,(wrap $1-start-pos $1-end-pos $1) ,$4)]
;         [(VAR BAR exp BAR)  `(seg_get ,$1 ,$3)]

	 ;; Alternate syntax for "dot syntax".  Might switch to this
	 ;; to free up period for future record syntax.
	 [(exp MAGICAPPLYSEP VAR LeftParen expls RightParen) `(app ,(wrap $3-start-pos $3-end-pos $3) ,$1 . ,$5)]
         [(exp MAGICAPPLYSEP VAR) `(app ,(wrap $3-start-pos $3-end-pos $3) ,$1)]

	 ;; Another application syntax... Haskell's '$' operator.
	 ;; Don't want to pollute the language too much, but I find
	 ;; this useful.  I'll avoid using it in the standard library.
	 [(exp $ exp) `(app ,$1 ,$3)] ;; Note, every binop adds a shift/reduce conflict currently¢

	 ;; Extended dot syntax, adds three more shift-reduce conflicts;
	 [(exp DOT VAR LeftParen expls RightParen) `(app ,(wrap $3-start-pos $3-end-pos $3) ,$1 . ,$5)]
	 ;; Basic dot syntax:
	 ;; Can only use this to call named functions, that is symbols:
         [(exp DOT VAR) `(app ,(wrap $3-start-pos $3-end-pos $3) ,$1)]

	 ;y . (foo(x)) . if then else . 
	 ;((if then else) ((foo(x)) (y)))

	 ;; Special stream-projection dot syntax.
	 ;[(exp DOTSTREAM expls+ >) `(dot-project ,$1 ,$3)]
	 ;; For now unwrap the src-pos info on these:
	 [(exp DOTSTREAM expls+ RightParen) `(dot-project ,(map unwrap $3) ,(unwrap $1))]

	 [(assignment) $1]

	 ;; Normal application.  Operators that are simple are straightforward:
         [(VAR LeftParen expls RightParen) `(app ,(wrap $1-start-pos $1-end-pos $1) ,@$3)]
	 ;; This is a keyword, but it's also a prim name:
         [(static LeftParen expls RightParen) `(app static ,@$3)]

	 ;; Other operators must be wrapped in parens:
         [(LeftParen exp RightParen LeftParen expls RightParen) `(app ,$2 ,@$5)]

	 ;; It would be nice to have a convenient "cast" syntax.
	 ;;[(LeftParen type RightParen exp) `(assert-type ,$2 (cast_num ,$4))]
	 ;[(type BANG exp) `(assert-type ,$1 (cast_num ,$3))]
	 ;[(LeftParen type RightParen BANG exp) `(assert-type ,$2 (cast_num ,$5))]
	 ;[(type BANG exp) `(assert-type ,$1 (cast_num ,$3))]
	 [(VAR BANG exp) `(assert-type ,$1 (cast_num ,$3))]

	 ;; Array references/assignments:
         [(VAR LeftSqrBrk notlist RightSqrBrk) (prec APP) `(Array:ref ,(wrap $1-start-pos $1-end-pos $1) ,$3)]

         [(VAR arrayNav LeftSqrBrk notlist RightSqrBrk) (prec APP) 
	  `(Array:ref ,($2 (wrap $1-start-pos $1-end-pos $1)) ,$4)]

         [(LeftParen exp RightParen LeftSqrBrk notlist RightSqrBrk) `(Array:ref ,$2 ,$5)]
         
         ;; Expression with user type annotation:
;         [(LeftParen exp : type RightParen) $1]
         

         ;; Begin/end:
         [(LeftBrace stmts RightBrace) (make-begin $2)]
	         
         ;; Anonymous functions:
         [(fun LeftParen formals RightParen exp) (prec else) `(lambda ,$3 ,$5)]

         ;; Iterators:
         [(iter iterbinder LeftBrace stmts RightBrace) 
              `(,$1 (annotations) (lambda (,(car $2) ,VIRTQUEUE) ,(make-begin (append $4 (list VIRTQUEUE)))) ,(cadr $2))]
         [(iter iterbinder LeftBrace state LeftBrace binds RightBrace stmts RightBrace)
          `(,$1 (annotations) (letrec ,$6 (lambda (,(car $2) ,VIRTQUEUE) ,(make-begin (append $8 (list VIRTQUEUE))))) ,(cadr $2))]

	 ;; Now with type annotation:
#;
         [(iter LeftParen pattern :: type in exp RightParen LeftBrace stmts RightBrace) 
              `(,$1 (annotations) (lambda (,$3 ,VIRTQUEUE) ,(make-begin (append $10 (list VIRTQUEUE)))) 
		    (assert-type (Stream ,$5) ,$7))]
#;
         [(iter LeftParen pattern :: type in exp RightParen LeftBrace state LeftBrace binds RightBrace stmts RightBrace)
          `(,$1 (annotations) (letrec ,$12 (lambda (,$3 ,VIRTQUEUE)
			       ,(make-begin (append $14 (list VIRTQUEUE)))))
		(assert-type (Stream ,$5) ,$7))]
#;         
	 ;; Considering moving to no-parens for iterate (for consistency):
	 [(iter pattern in exp LeftBrace stmts RightBrace)
	  `(,$1 (annotations) (lambda (,$2 ,VIRTQUEUE) ,(make-begin (append $6 (list VIRTQUEUE)))) ,$4)]
	 

	 ;; Expression conditional:
;	 [(if exp then exp else exp) (prec EXPIF) `(if ,$2 ,$4 ,$6)]
;	 [(if exp then exp) (prec ONEARMEDIF) `(if ,$2 ,$4)]

	 ;; SUPERHACK: I've INLINED the "stmt" grammar here:

	 ;; Hmm.. this does constrain the types too much:
	 [(if exp then exp)  `(if ,$2 ,$4 (tuple))]
	 [(if exp then selfterminated)  `(if ,$2 ,$4 (tuple))]

	 [(if exp then exp else exp)  `(if ,$2 ,$4 ,$6)]
	 [(if exp then selfterminated else selfterminated) `(if ,$2 ,$4 ,$6)]
	 ;[(if exp then exp SEMI else exp)  `(if ,$2 ,$4 ,$7)]
	 ;[(if exp then selfterminated SEMI else selfterminated) (prec EXPIF) `(if ,$2 ,$4 ,$7)]

	 [(break) '(break)]
	 [(emit exp) `(emit ,VIRTQUEUE ,$2)] ;; 1 shift reduce conflict
         ;; Causes 5 shift-reduce conflicts:
         [(return exp) $2] ;; No, only 1 now...

	 ;; HACK: This allows STMTs!!!
	 ;[(if exp then stmt else stmt) (prec EXPIF) `(if ,$2 ,$4 ,$6)]
	 ;; LAME: Allowing optional semi-colon:
;	 [(if exp then stmt SEMI else stmt)  `(if ,$2 ,$4 ,$7)]
	 
         
         ;[(map LeftParen VAR in exp RightParen LeftBrace stmts RightBrace) `(map (,$3 in ,$5) ,$8)]

         
;         [(iterate LeftParen VAR in exp RightParen exp) 
              ;`(iterate (lambda (,$3) ,$8) ,$5)]
;              `(iterate (,$3 in ,$5) ,@$8)         
;         [(iter) $1]
;;         [(map LeftParen VAR in exp RightParen exp) `(map ($3, in ,$5) ,$$7)]


;         [(exp binop exp) (prec +) `(,$2 ,$1 ,$3)]
         
         ;; Using binary prims as values: (without eta-expanding!)
         [(LeftParen binop RightParen) $2]         
	 ;; Haskell's "sections".
         [(LeftParen binop exp RightParen) `(lambda (x) (app ,$2 x ,$3))]
         [(LeftParen exp binop RightParen) `(lambda (x) (app ,$3 ,$2 x))]

	 ;; Negative numbers.
	 [(- NUM)    (prec NEG) (- $2)]
	 [(- DOUBLE) (prec NEG) (tag-double (- $2))]

         [(exp AND exp) `(ws:and ,$1 ,$3)]
         [(exp OR exp)  `(ws:or  ,$1 ,$3)]

         [(exp ++ exp) `(string-append (show ,$1) (show ,$3))]
         [(exp ::: exp) `(cons ,$1 ,$3)]

         [(exp + exp) `(app + ,$1 ,$3)]
         [(exp - exp) `(app - ,$1 ,$3)]
         [(exp * exp) `(app * ,$1 ,$3)]
         [(exp / exp) `(app / ,$1 ,$3)]
         [(exp ^ exp) `(app ^ ,$1 ,$3)]

         [(exp g+ exp) `(g+ ,$1 ,$3)]
         [(exp g- exp) `(g- ,$1 ,$3)]
         [(exp g* exp) `(g* ,$1 ,$3)]
         [(exp g/ exp) `(g/ ,$1 ,$3)]
         [(exp g^ exp) `(g^ ,$1 ,$3)]

         [(exp +_ exp) `(_+_ ,$1 ,$3)]
         [(exp -_ exp) `(_-_ ,$1 ,$3)]
         [(exp *_ exp) `(*_ ,$1 ,$3)]
         [(exp /_ exp) `(/_ ,$1 ,$3)]
         [(exp ^_ exp) `(^_ ,$1 ,$3)]
         
         [(exp +. exp) `(_+. ,$1 ,$3)]
         [(exp -. exp) `(_-. ,$1 ,$3)]
         [(exp *. exp) `(*. ,$1 ,$3)]
         [(exp /. exp) `(/. ,$1 ,$3)]
         [(exp ^. exp) `(^. ,$1 ,$3)]

         [(exp +: exp) `(_+: ,$1 ,$3)]
         [(exp -: exp) `(_-: ,$1 ,$3)]
         [(exp *: exp) `(*: ,$1 ,$3)]
         [(exp /: exp) `(/: ,$1 ,$3)]
         [(exp ^: exp) `(^: ,$1 ,$3)]

         [(exp < exp) `(< ,$1 ,$3)]
         [(exp > exp) `(> ,$1 ,$3)]
         [(exp <= exp) `(<= ,$1 ,$3)]
         [(exp >= exp) `(>= ,$1 ,$3)]

         ;[(exp === exp) `(eq? ,$1 ,$3)] ; Do we want to do this?
         [(exp == exp) `(wsequal? ,$1 ,$3)]
         [(exp != exp) `(not (wsequal? ,$1 ,$3))]
         
         ;; Parentheses for precedence:
         [(LeftParen exp RightParen) $2]
         [(LeftParen exp :: type RightParen)
	  `(assert-type ,$4 ,$2)]

         )
    
    (binop [(+) '+]
           [(-) '-]
           [(*) '*]
           [(/) '/]           
           [(^) '^]

	   [(g+) 'g+]
           [(g-) 'g-]
           [(g*) 'g*]
           [(g/) 'g/]           
           [(g^) 'g^]

	   [(+_) '_+_]
           [(-_) '_-_]
           [(*_) '*_]
           [(/_) '/_]           
           [(^_) '^_]

           [(+.) '_+.]
           [(-.) '_-.]
           [(*.) '*.]
           [(/.) '/.]
           [(^.) '^.]

           [(+:) '_+:]
           [(-:) '_-:]
           [(*:) '*:]
           [(/:) '/:]
           [(^:) '^:]

	   [(:::) 'cons]
	   [(++) '(lambda (x y) (string-append (show x) (show y)))]
           
           [(<) '<]
           [(>) '>]
           [(<=) '<=]
           [(>=) '>=]
           [(==) 'wsequal?]
           [(!=) '(lambda (x y) (not (= x y)))]
           ))))
    ;; Invoke the parser:
    (apply theparser  args)
  ))
           
;; run the calculator on the given input-port       
(define (ws-parse-port ip)
  (port-count-lines! ip)
  (cdr (ws-parse (lambda () (flatten (ws-lex ip)))))
  #;
  (let loop ()        
        (let ((result (ws-parse (lambda () (flatten (ws-lex ip))))))
          (if result
              (cons result (loop))
              '()))))

(define (flatten pt)
  ;(printf " ")
  (let loop ((x pt))
        (if (position-token? (position-token-token x))
            (begin (error 'flatten "No nested position-tokens!")
              (loop (position-token-token x)))
            x)))
  
(define (ws-parse-file-raw f) 
  (fluid-let ([thefile (list f)])    
    (let ([p (open-input-file f)])    
      (let ([res (ws-parse-port p)])
	(close-input-port p)
	res))))

;; This parses the file and does post-processing:
(define (ws-parse-file fn)
  (define raw (ws-parse-file-raw fn))
  ;(wsparse-postprocess raw) 
  raw
  )


#;
(define parsed (reg-parse-file "demos/wavescope/test2.ws"))
;;(define parsed (reg-parse-file "demos/wavescope/bird_detector.ws"))
;;(define parsed (reg-parse-file "demos/wavescope/simple.ws"))
;;(pretty-print parsed)

  
;(define processed (wsparse-postprocess parsed))
;(pretty-print processed)

;(require "plt/hm_type_inference.ss")
#;
(mvlet ([[a b] (annotate-program processed)])
       (pretty-print a)
       ;(pretty-print (print-var-types a))
       )

) ;; End module.

;(require reg_grammar)
;(reg-parse-file "demos/wavescope/demo0_audio.ws")














;================================================================================
; SCRATCH

#|
;(calc (open-input-string "(1 + 2 * 3) - (1+2)*3"))

(define str "fun foo (x) {
  return  3;
}")

(define thelist
  (let ((port (open-input-string str)))
    (let loop ((tok (lex port)))
      (printf "~a  ~a  ~a\n" tok (token-name tok)  (token-value tok))
      (cons tok 
            (if (eq? tok 'EOF)  '()
                (loop (lex port)))))))

(printf "THELIST: ~s\n" thelist)

(define feeder
  (let ([ls thelist])
    (lambda () (let ((x ls)) (if (null? x) 'EOF
                                 (begin (set! ls (cdr ls)) (car x)))))))

(define (go) (parse feeder))

(parse-port (open-input-string str))
;(parse-port (open-input-string "3; 4; 5;"))
;(parse-port (open-input-string "3;"))
|#

#;#;
(define thelist
  (let ((port (open-input-file   "demos/wavescope/test.ws")))
    (let loop ((tok (reg-lex port)))
;      (printf "~a  ~a  ~a\n" tok (token-name tok)  (token-value tok))
#;      (printf "tok: ~a \n" (list (position-token-token tok)
                                 (position-token-start-pos tok)  (position-token-end-pos tok)
                                 (position-offset (position-token-start-pos tok)) 
                                 (position-offset (position-token-end-pos tok))))
      (cons tok 
            (if (eq? (position-token-token tok) 'EOF)  '()
                (loop (reg-lex port)))))) )
(printf "THELIST: ~s\n" ;(map token-name 
        (map position-token-token thelist))

