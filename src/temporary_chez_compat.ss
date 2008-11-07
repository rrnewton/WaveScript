

#;
(eval-when (eval compile load) 
  (case-sensitive #t)

  (define-syntax IFCHEZ
    (syntax-rules ()
      [(_ a b) a]))

  (define primeval eval)
  (define eval 
    (lambda (x env)
      (import scheme)
      (eval x)))

  ;(optimize-level 2)
  )


(define fxarithmetic-shift-right fxsrl)
(define fxarithmetic-shift-left fxsll)

;; This version uses generative records and should work if you want to use it.
(define-syntax reg:define-struct
  (syntax-rules ()
    [(_ (name field ...))
     (begin (define-record name (field ...))
	    ;; Allows the reader to read in regiment records.  System should work without this:
	    (define reg:struct-dummy-val (record-reader 'name (type-descriptor name)))
	    )]))

(define call/ec call/1cc)
(define-syntax let/ec (syntax-rules () ((_ v exp ...) (call/1cc (lambda (v) exp ...)))))

(define current-error-port console-output-port)

;; Simply reads a line of text.  Embarassing that this isn't in r5rs.
;; Man, they need a standardized set of libraries.
;; .parameter port (optional) Port to read from.
;;
;This read-line will handle line delimination either by #\newline
;or by a consecutive #\return #\newline
;JEEZ, this has some problems right now [01.06.08], Chez for windows
;seems to be totally screwy regarding char-ready?.
(define get-line ;returns false if the port is empty
  (lambda args
    (let ([port (if (null? args)
                    (current-input-port)
                    (car args))])
      (letrec ([loop
                 (lambda (c)
                   (cond
		    [(or (eof-object? c)     (eq? c #\newline))     '()]
		    [(or (eq? c #\linefeed)  (eq? c #\return))
		     (when (and ;;(char-ready? port) ;; [2008.04.27] This had to do with some ancient windows problems.  Nixing.
				(eq? (peek-char port) #\newline))
		       (read-char port))
		     '()]
		    [else (cons c (loop (read-char port)))]))])
        (let ([c (read-char port)])
          (if (eof-object? c) #f
              ;(let ([x (loop c)])(inspect x)(list->string x))
	      (list->string (loop c))
              ))))))

(alias  fx=? fx=)
(alias  fx<? fx<)
(alias  fx>? fx>)
(alias  fxdiv fx/)
(alias  fx>=? fx>=)
(alias  greatest-fixnum most-positive-fixnum)
(alias  least-fixnum    most-negative-fixnum)
(alias  fxmod fxmodulo)
(alias  fxior logior)
(alias  fxand logand)
(alias  fxxor logxor)

(define (fxbit-set? num ind) (logbit? ind num))

(alias bitwise-ior logior)
(alias bitwise-and logand)
(alias bitwise-xor logxor)
(alias bitwise-arithmetic-shift-left ash)


(define-syntax define-values 
  (lambda (x)
    (define iota 
      (case-lambda [(n) (iota 0 n)]
		   [(i n) (if (= n 0) '() (cons i (iota (+ i 1) (- n 1))))]))
    (syntax-case x ()  
		 [(define-values (vars ...) exp)
		  (with-syntax ([(nums ...) (datum->syntax  
					     #'define-values 
					     (iota (length (syntax->datum #'(vars ...)))))])
		    #'(begin  
			(define newtempvar (call-with-values (lambda () exp) vector))
			(define vars (vector-ref newtempvar nums))
			...))])))

(define format-syntax-nicely (lambda (x) x))


(define (environment . _) 'dummy_r6rs_style_env)

#;
(define-syntax file-options
  (syntax-rules ()
    [(_ arg ...) '(arg ...)]))
#;
(define-syntax buffer-mode
  (syntax-rules ()
    [(_ arg ...) '(arg ...)]))

(define repl new-cafe)

(define reg:top-level-eval #%eval)

(define promise? procedure?)

(define native-inspect #%inspect)

;(define generic-inspect #%inspect)
(include "ws/compat/inspector.ss")

(alias inspect generic-inspect)
#;
(define-syntax inspect
  (identifier-syntax 
   generic-inspect
   ;(top-level-value 'generic-inspect)
   ))

;; This is a hack to emulate Chez's #%prim syntax without breaking
;; other readers that can't handle it.
(define-syntax (hash-percent syn)
  (syntax-case syn ()
	       [(_ prim) (datum->syntax-object #'_ `(let () (import scheme) ,(datum prim)))]))


