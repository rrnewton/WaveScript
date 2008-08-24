

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


;; This provides access to C-functions given WS type as an interface,
;; and returning a closure taking the Scheme representations of WS
;; objects as arguments.
(begin 

   ;; [2007.08.30] Feature change for the WS foreign interface.  Now
   ;; the user may assume access to all of libc by default.
  (define ensure-libc-loaded!
    (let ([ranyet? #f])
      (lambda ()
	(unless ranyet?
	  (load-shared-object
	   (case (machine-type)
	     [(ti3osx i3osx ppcosx) "libc.dylib"]
	     [(ti3le i3le) "libc.so.6"]
	     [else (error 'ensure-libc-loaded! 
			  "WaveScript foreign interface not supported on platform: ~s"
			  (machine-type))]
	     ))
	  ;; FIXME: there are other places in the code where we might want to know about this:
	  ;; Let's just put it in an environment variable.
	  (putenv "REGLIBCLOADED" "TRUE")
	  (set! ranyet? #t)))))
  ;; NOTE: This isn't working on 64-bit justice.
  (define __foreign
    (let ()

      (define (Convert T)
	(match T
	  [Int     'fixnum]
	  [Float   'single-float]
	  ;[Double  'double-float] ;; TODO: [2008.08.22] Need to update to unwrap!
	  [Boolean 'boolean]
	  [Char    'char]
	  [String  'string]
	  [(Pointer ,_) 'uptr]
					;	[(ExclusivePointer ,_) 'uptr]
	  [#()     'void]
					;[(Char) char]
	  [,else (error '__foreign:Convert "this type is not supported by the Chez foreign interface: ~s" T)]))

      (define (DynamicLink out files)
	(when (file-exists? out) (delete-file out))
	(let* ([files (apply string-append (map (curry format " ~s") files))]
	       [code (s:case (machine-type)
			     [(i3le ti3le)  		      
					;(printf "EXEC: ~a\n" (format "cc -fPIC -shared -o \"~a.so\" ~a" out files))
			      (system (format "cc -fPIC -shared -o \"~a.so\" ~a" out files))]
			     [(i3osx ti3osx) (system (format "cc -fPIC -dynamiclib -o \"~a.so\" ~a" out files))]
			     [else (error 'foreign "don't know how to compile files ~s on machine type ~s: ~s\n" files (machine-type))]
			     )])
	  ;; This is actually not guaranteed to work by the chez spec:
	  (unless (zero? code)
	    (error 'foreign "C compiler returned error code ~s." code))
	  ;; Returns the name of the output file:
	  ;; Should use a dylib extension for mac os X:
	  (string-append out ".so")
	  ))
      
      (define (LoadFile! file)
	(let ([ext (extract-file-extension file)]
	      [sharedobject file])
	  (cond
	   [(or (string=? ext "so") 
		(string=? ext "dylib")
		(substring? ".so." file) ;; This is a hack to handle files like libc.so.6
		) (void)]
	   ;; This is a bit sketchy... because of course the user *COULD* put function definitions in headers.
	   ;; The assumption for now is that headers can be ignored.
	   [(member ext '("h" "hpp")) (set! sharedobject #f)]
	   
	   [(s:equal? ext "o")
	    (printf "  Attempting to convert object (.o) to shared object (.so:) ~s\n" file)
	    (let ([target (remove-file-extension file)])	      
	      (set! sharedobject (DynamicLink target (list file))))]

	   [(s:equal? ext "a")
	    (printf "  Attempting to convert static library (.a) to shared .so: ~s\n" file)
	    (let ([target  (remove-file-extension file)]
		  [tempfile (format ".__tempfile_~a.txt" (random 1000000))])
	      ;; This assumes bash!!
	      (system-to-str (format "ar xv \"~a\" | awk '{ print $3 }' > ~a " file tempfile))
	      (let ([objfiles (filter (lambda (s) (not (s:equal? s "")))
				(file->lines tempfile))])
		;; Now relink the .o files into a shared object:
		(set! sharedobject (DynamicLink target objfiles))
		))]

	   [(member ext '("c" "cpp"))
	    ;; This is really stretching it.  Attempt to compile the file.
	    (let ([target  (remove-file-extension file)])	      
	      (printf "  Attempting to compile ~s to ~s.so\n" file target)
	      (set! sharedobject (DynamicLink target (list file))))]
	   [else (error 'foreign "this type of foreign file not supported in scheme backend: ~s" file)])

	  ;; Load the file containing the C code.
	  (when (and sharedobject (not (member sharedobject (unbox already-loaded-object-files))))
	    (load-shared-object sharedobject)
	    (set-box! already-loaded-object-files (cons sharedobject (unbox already-loaded-object-files)))
	    (printf "  Shared object file (~a) loaded.\n" sharedobject))
	  ))

      (lambda (name files type)
	;; First make sure that the C standard library is loaded.
	(ensure-libc-loaded!)

	(printf "Dynamically loading foreign entry ~s from files ~s.\n" name files)
	(for-each LoadFile! files)
	;; After it's loaded there'd better be access:
	(unless (foreign-entry? name)
	  (error 'foreign "failure to register foreign function in Scheme: ~s" name))
	(match type
	  [(,[Convert -> args] ... -> ,ret)
	   (let ([foreignfun (eval `(foreign-procedure ,name ,args ,(Convert ret)))])
	     foreignfun
	     #;	   
	     (if (match? ret (ExclusivePointer ,_))
		 (lambda args
		   (let ([ret (apply foreignfun args)])
		     ;; Now we add the pointer we get back to our guardian:
		     (fprintf (current-error-port) " !!ADDING TO GUARDIAN!! ~s\n" ret)
		     ((foreign-guardian) (box ret))
		     ret
		     ))
		 foreignfun))
	   ]))))
  )
