#!r6rs

;;;; .title c_generator.ss
;;;; .author Ryan Newton

;;;; I already have pass32_emit-nesc.ss, but I'm looking to make a more
;;;; generally useful C-generation library.  This should be very simple.

;;;; The representation used here is that of a list of strings (nested
;;;; arbitrarily).  This list is manipulated extensively, but the
;;;; strings should only be appended once -- at the end.

;;;; For added flexibility these procedures can take straight strings
;;;; and wrap them as lists.  That is:
;;;;  type Text = String | List of Text

(library (ws compiler_components c_generator)
  (export test-cgenerator
	  wrap append-text 
	  indent block 
	  text? text->lines text->string	   
	  mangle-name ;; Mangle a name so it's a C-name.  Could cause collisions.
	  )
  (import  (except (rnrs (6)) error) (rnrs mutable-pairs)
	   (ws common))

;;======================================================================
;;; "Text" ADT implementation:
;;; A more efficient implementation would probably make line structure explicit.
(begin 

    
  (define acceptable_chars 
    '(#\_ 
      #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z 
      #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
      #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  ;; String -> String
  (define (mangle-name str)
    (list->string (filter (lambda (c) (memq c acceptable_chars)) 
		    (string->list str))))

  ;; This takes a Text or a string and returns a Text.
  (define (wrap x)
  ;; If it's a string, then we just wrap it.
  (or (and (string? x) (list x))
      (and (null? x) '())
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
	       [else (error 'text->lines "internal error, bad line: ~s" (car lines2))])
	     lines1]))


  (define (text? x) 
    (cond
      [(string? x) #t]
      [(and (pair? x)
	    (text? (car x))
	    (text? (cdr x))) #t]
      [(null? x) #t]
      [else #f]))

;; Takes a Text and returns a list of Text.
;; NOTE: ADDS A TRAILING NEWLINE.
;; Should be its own fixed point.
(define (text->lines text)
;  (if (deep-assq 'quote text) (inspect text))
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
  (with-output-to-string
    (lambda ()
      (let loop ([txt txt])
	(cond
	 [(null? txt) (void)]
	 [(string? txt) (display txt)]
	 [(not (pair? txt)) (error 'text->string "bad Text: ~s" txt)]
     ;[(null? (car txt)) (loop (cdr txt))]
     [else (loop (car txt))
	   (loop (cdr txt))])))))

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

;; This wraps lines in curly braces:
(define (block header contents . footer)
  `(,header ,(if (equal? header "") "" " ") "{\n"
    ,@(indent contents "  ")
    "} " ,@footer "\n"
    ))


(define testtext
  '("foo();\n"
    "bar();\n"))

(define-testing test-cgenerator
  (default-unit-tester "c_generator.ss: generating C code." 
    `(
    [(',wrap "foo")   ("foo")]
    [(',wrap '("foo" "bar")) ("foo" "bar")]
    
    ,@(IFDEBUG `([(',wrap '("foo" "bar" 39)) error])
	       '())    

    [(apply ',graft! (map ',list-copy '((("a" "\n") ("b" "\n")) (("x" "\n") ("y" "\n")))))
     (("a" "\n") ("b" "\n") ("x" "\n") ("y" "\n"))]
    ["graft! with an incomplete line."
     (apply ',graft! (map ',list-copy '((("a" "\n") ("b" "\n")) (("x") ("y" "\n")))))
     (("a" "\n") ("xb" "\n") ("y" "\n"))]

    [(',text->lines '( "one\ntwo" "too\nthree"))
     (("one" "\n") ("twotoo" "\n") ("three"))]
    [(',text->lines (',text->lines '("one\ntwo" "too\nthree")))
     (("one" "\n") ("twotoo" "\n") ("three"))]
    [(',text->lines '((("one" "\n") ("two")) (("too" "\n") ("three"))))
     (("one" "\n") ("twotoo" "\n") ("three"))]

    ;; This one is overly strict.  text->lines needn't actually flatten to this extent.
    [(',text->lines '((("one" "\n") ("two")) (("too" "\n") (("three" ((("f\nour\n"))))))))
     (("one" "\n") ("twotoo" "\n") ("threef" "\n") ("our" "\n"))]

    [(',text->string "foo") "foo"]
    [(',text->string '("foo" "bar")) "foobar"]
    
    )))

) ;; End module

