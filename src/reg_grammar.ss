;; An interactive calculator inspired by the calculator example in the bison manual.

;; TODO:

;; *) Handle complex constants.
;; *) Handle arr[3][4]


(module reg_grammar mzscheme  
  
;; Import the parser and lexer generators.
(require (lib "yacc.ss" "parser-tools")
         (lib "lex.ss" "parser-tools")
         (lib "pretty.ss")
         "plt/iu-match.ss"
         "plt/helpers.ss"
         ;"plt/prim_defs.ss"
         (prefix : (lib "lex-sre.ss" "parser-tools")))

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
   
    : := -> = == != >= <= < > <-
    + - * / ^ 
    +. -. *. /. ^. 
    +: -: *: /: ^: 
    :: ++ 
    AND OR 
    NEG APP SEMI COMMA DOT DOTBRK DOTSTREAM BAR BANG
    fun for to emit include deep_iterate iterate state in if then else true false break let ; Keywords 
    ;; Fake tokens:
    EXPIF STMTIF ONEARMEDIF
    ;SLASHSLASH NEWLINE 
    EOF ))
(define-tokens value-tokens (NUM VAR DOTVARS TYPEVAR CHAR STRING))

(define-lex-abbrevs (lower-letter (:/ "a" "z"))
                    (upper-letter (:/ #\A #\Z))
  ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
                    (digit (:/ "0" "9"))
                    (variable (:seq (:or lower-letter upper-letter "_")
                                    (:* (:or lower-letter upper-letter "_" digit)))))

(define (unescape-chars str)
  (read (open-input-string (string-append "\"" str "\""))))

(define ws-lex
  (lexer-src-pos
   [(eof) 'EOF]
   ;; Ignore all whitespace:
   [(:or #\tab #\space #\newline) (return-without-pos (ws-lex input-port))]
   ;; Throw out the rest of the line:
   ;["//" 'SLASHSLASH]
   
   ;; WARNING: These recursive calls produce NESTED position tokens... only the inner one is valid!!
   ["//" (begin (read-line input-port) (return-without-pos (ws-lex input-port)))]
   ["/*" (begin (read-balanced "/*" "*/" input-port 1) (return-without-pos (ws-lex input-port)))]
;   ["(*" (begin (read-balanced "(*" "*)" input-port 1) (return-without-pos (ws-lex input-port)))]
   
   ;; Since (token-=) returns '=, just return the symbol directly
   [(:or "::" "++" 
	 "->" "<-" ":" ":=" "<=" "<" ">" ">=" "==" "!="
	 "=" "+" "-" "*" "/" "^" 
         "+." "-." "*." "/." "^."
         "+:" "-:" "*:" "/:" "^:"
	 )
    (string->symbol lexeme)]
   ;; Keywords: 
   [(:or "fun" "for" "break" "to" "emit" "include" "deep_iterate" "iterate" "state"  "in" "if" "then" "else" "true" "false" "let")
    (string->symbol lexeme)]
   
   [(:seq "'" lower-letter "'") (token-CHAR (string-ref lexeme 1))]
   [(:seq "'" (:+ lower-letter)) (token-TYPEVAR (string->symbol (substring lexeme 1 (string-length lexeme))))]
   [(:seq "\"" (:* (:- any-char "\"")) "\"") 
    (token-STRING (unescape-chars (substring lexeme 1 (sub1 (string-length lexeme)))))]

   ["&&" 'AND] ["||" 'OR]
   
   ;; Delimiters:
   [";" 'SEMI]  ["," 'COMMA] ;["'" 'QUOTE]
   ["|" 'BAR] ["!" 'BANG]
   ["(" 'LeftParen]  [")" 'RightParen]
   ["{" 'LeftBrace]  ["}" 'RightBrace]
   ["[" 'LeftSqrBrk] ["]" 'RightSqrBrk]
   ["<" 'LeftAngleBrk] [">" 'RightAngleBrk]
;   ["sin" (token-FNCT sin)]

   ;; Variables:
   [variable (token-VAR (string->symbol lexeme))]

   ;; Dot-syntax:
;   [(:seq (:+ (:seq variable ".")) variable)  (token-DOTVARS (map string->symbol (string-split lexeme #\.)))]
   ["." 'DOT]
   ;; Dot/stream-projection
   [".<" 'DOTSTREAM]
   
   [(:+ digit) (token-NUM (string->number lexeme))]

   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]
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

(define (ws-parse . args)
  (if (file-exists? "_parser.log")
      (delete-file "_parser.log"))
  (let ()
   ;; Returns a function that takes a lexer thunk producing a token on each invocation:a
    (define theparser
      (parser
    
   (src-pos)
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
    
          (left emit) ; return

          (right = := -> )
	  (right AND OR )

	  ;(right)
	  (right then else )

    	  (nonassoc SEMI)
	  ;;(nonassoc ONEARMEDIF)
	  (nonassoc EXPIF if STMTIF)

	  (right ++ :)
          (left < > <= >= == !=)
          (left - + +. -.  +: -:)
          (left * / *. /.  *: /:)

;          (left LeftSqrBrk)
;          (left DOTBRK) 
;	  (right BAR)
          (left NEG APP DOT COMMA)
          (right ^ ^. ^:)

	  )


   (debug "_parser.log")

   (grammar
    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           ;[(error start) $2]
           [(decls) (cons 'program $1)])


    (type [(type -> type) `(,$1 -> ,$3)]
          ;; A type constructor, make it right-associative.
          [(VAR type) (prec APP) (list $1 $2)]

	  ;; Hmm: can't make up my mind whether we should single-quote typevars:
          [(VAR) (if (lower-case? $1) `(quote ,$1) $1)]
          [(TYPEVAR) `(quote ,$1)]
          ;; No one-tuples!!
          [(LeftParen type RightParen) $2]
          ;; Tuple types:
          [(LeftParen type COMMA typeargs RightParen) (apply vector (cons $2 $4))]
          )
    (typeargs [(type) (list $1)]
              [(type COMMA typeargs) (cons $1 $3)]
              )

    (decls ;; Top level variable binding
           [(VAR :: type SEMI maybedecls) `((:: ,$1 ,$3) ,@$5)]
           [(VAR = exp optionalsemi maybedecls) `((define ,$1 ,$3) ,@$5)]
           [(let pattern = exp optionalsemi maybedecls) `((define ,$2 ,$4) ,@$6)]
	   
	   ;; b
	   [(include exp SEMI maybedecls) 
	    `((include ,$2) ,@$4)]

           ;; Returning streams to the base station or other "ports"
           [(VAR <- exp optionalsemi maybedecls) `((<- ,$1 ,$3) ,@$5)]
           
           ;; Fundef is shorthand for a variable binding to a function.
           [(fundef optionalsemi maybedecls) (cons $1 $3)]
           )
    (maybedecls [() '()] [(decls) $1])

    (fundef [(fun VAR LeftParen formals RightParen exp) ;LeftBrace stmts RightBrace) 
             `(define ,$2 (lambda ,$4 ,$6))]
#;
            [(VAR : type SEMI fun VAR LeftParen formals RightParen exp)
             (let ([v1 $1] [v2 $6])
               (unless (eq? v1 v2) 
                 (error "Parse Error: top-level type spec must precede function of same name, got ~a ~a, positions ~a ~a\n"
                        v1 v2 (format-pos $1-start-pos) (format-pos $6-start-pos)))
               `(define ,$1 ,$3 (lambda ,$8 ,$10)))]
            )
   (formals [() '()]
	    [(formals+) $1]
	    )
   (formals+ [(pattern) (list $1)]
	     [(pattern COMMA formals) (cons $1 $3)])


   ;; [2006.09.01] For now patterns are just tuples.
   (pattern [(VAR) $1]
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


	 ;;           [(return exp SEMI stmts) (cons `(return ,$2) $4)]	   

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
   (stmts  [() '()]
	   ;; LAME: We require let only when you're going to use a pattern.
           [(let pattern = exp SEMI stmts) `((letrec ([,$2 ,$4]) ,(make-begin $6)))]
           [(VAR = exp SEMI stmts) `((letrec ([,$1 ,$3]) ,(make-begin $5)))]
           [(VAR : type = exp SEMI stmts) `((letrec ([,$1 ,$3 ,$5]) ,(make-begin $7)))]
	   
           [(fundef morestmts)
            (match $1
              [(define ,v ,e) `((letrec ([,v ,e]) ,(make-begin $2)))]
              [(define ,v ,t ,e) `((letrec ([,v ,t ,e]) ,(make-begin $2)))])]

	   ;; This avoids conflicts:
	   [(selfterminated stmt morestmts) (cons $1 (cons $2 $3))]
           [(stmt morestmts) (cons $1 $2)]
           )
   (optionalsemi [() 'OPTIONALSEMI] [(SEMI) 'OPTIONALSEMI])
   (morestmts [() '()] [(SEMI stmts) $2])
   ;; These have a syntax that allows us to know where they terminate and optionally omit SEMI:
   (selfterminated 
    [(for VAR = exp to exp LeftBrace stmts RightBrace) `(for (,$2 ,$4 ,$6) ,(make-begin $8))]
    )

    ;; Kinda redundant, used only for state {} blocks.
    (binds [() ()]
           [(VAR = exp) (list (list $1 $3))]
	   ;; Special stream-binding syntax:
;           [(VAR < formals+ > = exp) (list 
;				      (list $1 $3)
;				      )]

           [(VAR = exp SEMI binds) (cons (list $1 $3) $5)])
    (iter [(iterate) 'iterate]
          [(deep_iterate) 'deep-iterate])

    (tuple [(LeftParen exp COMMA expls+ RightParen) `(tuple ,$2 ,@$4)])

    ;; Hack to enable my syntax for sigseg refs!!
    (exp 
	  ;; Lists:
	  [(LeftSqrBrk expls RightSqrBrk) (consify $2)]
	  [(notlist) $1])
    (expls [() '()]
	   [(expls+) $1])
    (expls+ [(exp) (list $1)]
	    [(exp COMMA expls) (cons $1 $3)])

    ;(innernotlist [(notlist) $1])    

    (notlist
         [(NUM) $1]  
         [(VAR) $1]
         [(CHAR) $1]
         [(STRING) $1]
         [(true) ''#t] 
	 [(false) ''#f]
#;
         [(DOTVARS) 
           (let loop ([ls (cdr $1)] [acc (car $1)])
             (if (null? ls) acc
                 (loop (cdr ls)
                       `(app ,(car ls) ,acc))))]
         [(tuple) $1]

         
;         [(VAR DOT DOT LeftSqrBrk NUM RightSqrBrk) (prec DOTBRK) `(seg-get ,$4 ,$1)]
         [(VAR LeftSqrBrk LeftSqrBrk exp RightSqrBrk RightSqrBrk) `(seg-get ,$1 ,$4)]
;         [(VAR BAR exp BAR)  `(seg-get ,$1 ,$3)]

	 ;; Basic dot syntax:
         [(exp DOT exp) `(app ,$3 ,$1)]
	 ;; Special stream-projection dot syntax.
	 [(exp DOTSTREAM expls+ >) `(dot-project ,$1 ,$3)]
                  
         [(VAR := exp) `(set! ,$1 ,$3)]
         [(VAR LeftSqrBrk notlist RightSqrBrk := exp)  `(arr-set! ,$1 ,$3 ,$6)]

         [(VAR LeftParen expls RightParen) `(app ,$1 ,@$3)]
         [(LeftParen exp RightParen LeftParen expls RightParen) `(app ,$2 ,@$5)]

	 ;; Array references/assignments:
         [(VAR LeftSqrBrk notlist RightSqrBrk) (prec APP) `(arr-get ,$1 ,$3)]
         [(LeftParen exp RightParen LeftSqrBrk notlist RightSqrBrk) `(arr-get ,$2 ,$5)]
         
         ;; Expression with user type annotation:
;         [(LeftParen exp : type RightParen) $1]
         

         ;; Begin/end:
         [(LeftBrace stmts RightBrace) (make-begin $2)]
	         
         ;; Anonymous functions:
         [(fun LeftParen formals RightParen exp) (prec else) `(lambda ,$3 ,$5)]
         
         [(iter LeftParen pattern in exp RightParen LeftBrace stmts RightBrace) 
              `(,$1 (lambda (,$3 ,VIRTQUEUE) ,(make-begin (append $8 (list VIRTQUEUE)))) ,$5)]
         [(iter LeftParen pattern in exp RightParen LeftBrace state LeftBrace binds RightBrace stmts RightBrace)
          `(,$1 (letrec ,$10 (lambda (,$3 ,VIRTQUEUE) ,(make-begin (append $12 (list VIRTQUEUE))))) ,$5)]
         
	 ;; Expression conditional:
;	 [(if exp then exp else exp) (prec EXPIF) `(if ,$2 ,$4 ,$6)]
;	 [(if exp then exp) (prec ONEARMEDIF) `(if ,$2 ,$4)]

	 ;; SUPERHACK: I've INLINED the "stmt" grammar here:

	 [(if exp then exp)  `(if ,$2 ,$4 (tuple))]
	 [(if exp then selfterminated)  `(if ,$2 ,$4 (tuple))]

	 [(if exp then exp else exp)  `(if ,$2 ,$4 ,$6)]
	 [(if exp then selfterminated else selfterminated) `(if ,$2 ,$4 ,$6)]
	 ;[(if exp then exp SEMI else exp)  `(if ,$2 ,$4 ,$7)]
	 ;[(if exp then selfterminated SEMI else selfterminated) (prec EXPIF) `(if ,$2 ,$4 ,$7)]

	 [(break) '(break)]
	 [(emit exp) `(emit ,VIRTQUEUE ,$2)]


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

         [(exp AND exp) `(and ,$1 ,$3)]
         [(exp OR exp) `(or ,$1 ,$3)]

         [(exp ++ exp) `(string-append ,$1 ,$3)]
         [(exp : exp) `(cons ,$1 ,$3)]

         [(exp + exp) `(+ ,$1 ,$3)]
         [(exp - exp) `(- ,$1 ,$3)]
         [(exp * exp) `(* ,$1 ,$3)]
         [(exp / exp) `(/ ,$1 ,$3)]
         [(exp ^ exp) `(^ ,$1 ,$3)]
         
         [(exp +. exp) `(+. ,$1 ,$3)]
         [(exp -. exp) `(-. ,$1 ,$3)]
         [(exp *. exp) `(*. ,$1 ,$3)]
         [(exp /. exp) `(/. ,$1 ,$3)]
         [(exp ^. exp) `(^. ,$1 ,$3)]

         [(exp +: exp) `(+: ,$1 ,$3)]
         [(exp -: exp) `(-: ,$1 ,$3)]
         [(exp *: exp) `(*: ,$1 ,$3)]
         [(exp /: exp) `(/: ,$1 ,$3)]
         [(exp ^: exp) `(^: ,$1 ,$3)]

         [(exp < exp) `(< ,$1 ,$3)]
         [(exp > exp) `(> ,$1 ,$3)]
         [(exp <= exp) `(<= ,$1 ,$3)]
         [(exp >= exp) `(>= ,$1 ,$3)]

         ;[(exp === exp) `(eq? ,$1 ,$3)] ; Do we want to do this?
         [(exp == exp) `(equal? ,$1 ,$3)]
         [(exp != exp) `(not (equal? ,$1 ,$3))]
         
         [(- exp) (prec NEG) `(- ,$2)]


         ;; Parentheses for precedence:
         [(LeftParen exp RightParen) $2]
	 ;; Throw out type annotations for now:  FIXME FIXME
         [(LeftParen exp :: type RightParen) $2]

         ;; Causes 5 shift-reduce conflicts:
;         [(return exp) $2]
         )
    
    (binop [(+) '+]
           [(-) '-]
           [(*) '*]
           [(/) '/]           
           [(^) '^]

           [(+.) '+.]
           [(-.) '-.]
           [(*.) '*.]
           [(/.) '/.]
           [(^.) '^.]

           [(+:) '+:]
           [(-:) '-:]
           [(*:) '*:]
           [(/:) '/:]
           [(^:) '^:]

	   [(:) 'cons]
	   [(++) 'string-append]
           
           [(<) '<]
           [(>) '>]
           [(<=) '<=]
           [(>=) '>=]
           [(==) '=]
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
  (let ([p (open-input-file f)])    
    (let ([res (ws-parse-port p)])
      (close-input-port p)
      res)))

;; This parses the file and does post-processing:
(define (ws-parse-file fn)
  (define raw (ws-parse-file-raw fn))
  ;(ws-postprocess raw) 
  raw
  )


#;
(define parsed (reg-parse-file "demos/wavescope/test2.ws"))
;;(define parsed (reg-parse-file "demos/wavescope/bird_detector.ws"))
;;(define parsed (reg-parse-file "demos/wavescope/simple.ws"))
;;(pretty-print parsed)

  
;(define processed (ws-postprocess parsed))
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

