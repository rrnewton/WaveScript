
;;;; .title Regiment Helper Functions (regiment_helpers.ss)

;;;; <br> [2005.12.01] <br>

;;;; I'm going to start the (slow) process of seperating all the
;;;; Regiment-specific support utilities from my generic scheme
;;;; utilities.  Previously all of this had resided in the bloated
;;;; helpers.ss file.

(module regiment_helpers mzscheme
  
    (require	
            "prim_defs.ss"
	     "../../plt/iu-match.ss"
	     "../constants.ss"
	     "../../plt/hashtab.ss"
             
             "../util/helpers.ss"
	     )

  (provide   
     
	  reg:random-int reg:random-real reg:get-random-state reg:set-random-state!
	  randomize-list randomize-vector! list-get-random vector-get-random

	  sense-spatial-sine-wave
	  sense-dist-from-origin 
	  sense-sine-wave
	  sense-noisy-rising
	  sense-random-1to100
	  ;sense-fast-sine-wave

	  unique-name unique-name-counter extract-suffix make-begin make-nested-lets
	  deep-reg:struct->list ;deep-reg:struct->list2 reg:very-deep-map
	  strip-illegal deunique-name  reunique-names
	  get-formals 
	  

	  ;; Hmm, not sure what meaning immediate has here...
	  ;immediate? 
	  simple-constant? complex-constant? datum? qinteger? qinteger->integer
	  formalexp? cast-formals fit-formals-to-args
	  simple-expr? maybe-let

	  token-machine? token-machine->program token-machine-keyword?

	  ;; Token names:
	  token-name? new-token-name token-names get-names get-formation-name get-membership-name
	  token->name token->subtok
	  destructure-tokbind handler->tokname handler->formals handler->body handler->subtokid handler->stored

	  project-metadata
	  apply-to-program-body

	  regiment-print-params	 

	  test-regiment_helpers

	  parse-readFile-modestring
	  annotation? peel-annotations
	  let-spine
	  )

  (chezimports prim_defs)


; ======================================================================
;;; The various sensor-reading stubs.  
;;; 
;;; Used by SimAlpha. <br>
;;; These are all simple functions that compute fake sensor values. <br><br>
;;;
;;; Sensor-function constructors are thunks that allocate state for a
;;; simulated sensed-world and return a sensor function. <br><br>
;;;
;;; Sensor-functions are called regularly with the current time.  They
;;; are also called when a sensor value needs to be read, in which
;;; case the node-id, x/y coords, and sensor type are all provided. <br><br>
;;;   
;;; If we wanted the simulator to be less synchronous, we would need
;;; to have seperate sense objects for each node, each maintaining its
;;; own state according to its own clock.  That gets a little tricky,
;;; because presumably you're measuring a phenomena defined globally. <br><br>
;;; 
;;; All told, sensor-function constructors have a type like the following: 
;;; <br>
;;;   ()   --{Newsim}--> 
;;;   Time --{UpdateState}--> 
;;;   (type, id, x, y)    --> 
;;;    SensorReading
;;; <br> 
;;; 
;;; Alternatively, these could be implemented as an object with a
;;; constructor and two methods AdvanceClock, and and ReadSensor.  But
;;; this way ensures that AdvanceClock is called before ReadSensor.

;; This one changes amplitude across space and time.
(define sense-spatial-sine-wave 
  (let ([fun (lambda ()
	       (lambda (t)
		 (lambda (type id x y)
					;(printf "(sensing ~a ~a ~a ~a) " id x y t)
					;(exact->inexact
		   (inexact->exact 
		    (floor
		     (let ((waveamp (+ 127.5 (* 127.5 (sin (* t (/ 3.14 1000))))))
			   (distorigin (sqrt (+ (* x x) (* y y))))
			   (maxdist (sqrt (+ (expt world-xbound 2) (expt world-ybound 2)))))
		       (* waveamp (/ distorigin maxdist))))))))])
    ;; This is the default sensing function right now, so we set the parameter.
    (simalpha-sense-function-constructor fun)
    fun))

;; This globally defined functions decides the sensor values.
;; Here's a version that makes the sensor reading the distance from the origin:
(define sense-dist-from-origin 
  (lambda ()
    (lambda (t)
      (lambda (type id x y)
	(sqrt (+ (expt x 2) (expt y 2)))))))

(define sense-sine-wave
  (lambda ()
    (lambda (t)    
      (lambda (type id x y)
  ;(printf "(sensing ~a ~a ~a ~a) " id x y t)
  ;(exact->inexact
	(inexact->exact 
	 (floor
	  (+ 127.5 (* 127.5 (sin (* t (/ 3.14 1000)))))))))))

;; TODO: add noise to this, store state per ID: curry inputs:
(define sense-noisy-rising
  (lambda ()
    (lambda (t)
      (lambda (type id x y)
	(/ t 100.)))))

(define sense-random-1to100
  (lambda ()
    (lambda (t)
      (lambda (type id x y)
	(add1 (reg:random-int 100))))))

#;
(define (sense-fast-sine-wave id x y t)
  (printf "(sensing ~a ~a ~a ~a) " id x y t)
  (inexact->exact 
   (floor
    (+ 127.5 (* 127.5 (sin (* t (/ 3.14 1000))))))))


;(include "prim_defs_OLD.ss")

;======================================================================
;;; Some methods for handling Abstract Syntax.

;; create a "flattened" begin from list of expressions
;; e.g., (make-begin '(1 (begin 2) (begin 3 4) 5)) => (begin 1 2 3 4 5)
;; 
(define make-begin
  (lambda  (expr*)
    (IFCHEZ (import rn-match) (begin))
    (let ([initlst (match expr*
			  [(begin ,x* ...) x*]
			  [,ls ls])])
      (match (match `(begin ,@initlst)
		    [(begin ,[expr*] ...) (apply append expr*)]
		    [,other (list other)])
      [() (void)]
      [(,x) x]
      [(,x ,x* ...) `(begin ,x ,@x*)]))))

(define (make-nested-lets binds body)
  (if (null? binds) body
      `(let (,(car binds)) ,(make-nested-lets (cdr binds) body))))

;; A little helper to project out a metadata-tag from a program form.
;; TODO: Move this somewhere else:
(define (project-metadata tag prog)
  (match prog
    [(,lang '(program ,body ,meta ... ,ty))  (assq tag meta)]
    [,else #f]))

;; Another little helper to avoid destructuring the program syntax all the time.
(define (apply-to-program-body f prog)
  (match prog
    [(,lang '(program ,body ,meta ... ,ty))  `(,lang '(program ,(f body) ,@meta ,ty))]
    [,_ (error 'apply-to-program-body "bad program form: ~s" prog)]))


     
;=============================================================

;;; Identifiers and Constants.
;;;
;;; These are the symbols I use for different types right now: <br>&nbsp;&nbsp;
;;;   Bool Char Float64 Int32 List Object                      
;;;   Number Pair Port String Symbol Vector Void


;; The subset of Scheme keywords we support. <br>
;; [2004.06.28] RRN: Removed 'let'
(define base-keyword?
  (lambda (x)
    (and (memq x '(quote set! if begin letrec lambda)) #t)))

(define (simple-expr? x)
  (IFCHEZ (import rn-match) (begin))
  (match x
    ;; [2007.03.11] Making complex constants *NON* simple
    [(quote ,imm) (guard ;(not (null? imm)) 
		         (simple-constant? imm)
			 ;(not (pair? imm)) (not (vector? imm))
			 ) #t]
    ;; Void value:
    ;[(tuple) #t] ;; [2007.03.19] Why had I commented this before?
    [,var (guard (symbol? var) (not (regiment-constant? var))) #t]

    ;; Annotations can still be simple.
    [(,annot ,_ ,[x]) (guard (annotation? annot)) x]

    ;; [2007.10.16] This is now considered simple:
    [(deref ,var) (DEBUGASSERT symbol? var) #t]

    [,otherwise #f]))


;; This is a litlte helper that introduces a new binding ONLY if the
;; RHS in question is not simple.  This avoids unnecessary aliasing
;; and code-bloat.
(define (maybe-let rhs ty k)
  (if (simple-expr? rhs)
      (k rhs)
      (let ([name (unique-name 'tmp)])
	`(let ([,name ,ty ,rhs])
	   ,(k name)))))


;; This describes what can go inside a (quote _) expression
;; after the complex constants have been removed.
(define simple-constant?
  (lambda (x)
    (or ;(fx-integer? x)
        ;(flonum? x)
        ;(bg-integer? x)
					;(ratnum? x)
        (number? x)  ;; replacing chez specific...
        (null? x) ;; This means you can type () without a quote.
        (boolean? x)
        (char? x)
        (string? x) 

	(eq? x 'BOTTOM)
	(eq? x 'UNIT)
	
        )))

;; Valid complex constants in Regiment.
;; (Includes simple constants as well.)
(define complex-constant?
  (lambda (x)
    (or (simple-constant? x)
	(and (pair? x) 
	     (complex-constant? (car x)) 
	     (complex-constant? (cdr x)))
	(and (vector? x)
	     (vector-andmap complex-constant? x))
	(and (tuple? x)
	     (andmap complex-constant? (tuple-fields x)))
	;; This is only for nullseg, it's a bit sketchy.
	(and (sigseg? x)
	     (fx= 0 (vector-length (sigseg-vec x))))
	)))

;; A potentially quoted integer.
(define (qinteger? n)
  (IFCHEZ (import rn-match) (begin))
  (match n
    [,i (guard (integer? i)) #t]
    [',i (guard (integer? i)) #t]
    [,else #f]))
;; Get the value of a potentially quoted integer.
(define (qinteger->integer n)
  (IFCHEZ (import rn-match) (begin))
  (match n
    [,i (guard (integer? i)) i]
    [(quote ,i) 
     (DEBUGASSERT (integer? i))
     i]
    [else (error 'qinteger->integer "this is not a qinteger: ~a" n)]))
  

;; Datums include structured constants as well as atoms.
(define datum?
  (lambda (x)
    (or (simple-constant? x)
        (null? x)
        (symbol? x)
        ;(string? x)
        (if (pair? x)
            (and (datum? (car x)) (datum? (cdr x)))
            (and (vector? x) (andmap datum? (vector->list x)))))))


;; [2006.03.01] 
;;<br>  Flatten out a whole struct (record) into a list.
;;<br>  This handles cyclic structures.
(define (deep-reg:struct->list x)
  (let ([touched (make-default-hash-table)]) 
      (let loop ((x x))
	(cond
	 [(hashtab-get touched x) => (lambda (v) v)]
	 [(pair? x) 
	  ;; Add the original cell to the touched list.
	  (let ([newcell (cons #f #f)])
	    ;; Associate the old cell with the new cell.
	    (hashtab-set! touched x newcell)
	    (set-car! newcell (loop (car x)))
	    (set-cdr! newcell (loop (cdr x)))
	    newcell)]
	 [(vector? x) 
	  (let ([newvec (make-vector (vector-length x) #f)])
	    (hashtab-set! touched x newvec)
	    (for i = 0 to (sub1 (vector-length x))
		 (vector-set! newvec i (loop (vector-ref x i))))
	    newvec)]
	 [(reg:struct? x)
	  (let ([newlst (reg:struct->list x)])
	    (hashtab-set! touched x newlst)
	    (let innerloop ([lst (cdr newlst)])
	      (unless (null? lst)
		(set-car! lst (loop (car lst)))
		(innerloop (cdr lst))))
	    newlst)]
	 [(atom? x) x]
	 [else (error 'deep-reg:struct->list "what on earth is this object? ~a" x)]))))


;=============================================================================

;;; Unique names.

;; Remove the numeric suffix.
(define deunique-name
  (lambda (sym)
    (let* ([str (symbol->string sym)]
	   [segments (string-split str #\_)])
      (string->symbol
       (apply string-append
	      (insert-between "_"
	       (if (string->number (rac segments))
		   (rdc segments)
		   segments)))))))
  
;; [2005.11.03] This is for comparing test outputs that differ only in unique names.
;; It goes through the structure in a fixed, deterministic order, and introduces 
;; unique names that are platform independent.
(define reunique-names
  (lambda (expr)
    (define table (make-default-hash-table))
    (define (make-entry) (cons -1 (make-default-hash-table)))
    (define (process s) (let* ((basesym (deunique-name s))
			       (entry (hashtab-get table basesym)))
			  (when (not entry)
			    (set! entry (make-entry))
			    (hashtab-set! table basesym entry))
			  (when (not (hashtab-get (cdr entry) s))
			    ;; Increment our counter:
			    (set-car! entry (add1 (car entry)))
			    (hashtab-set! (cdr entry) s 
					  (string->symbol 
					   (if (zero? (car entry))
					       ""   (format "_~s" (car entry))))))
			  (symbol-append basesym (hashtab-get (cdr entry) s))))
      (let loop ((ls expr))
	(cond
	 [(null? ls) ()]
	 [(or (simple-constant? (car ls))
	      (match (car ls) [(quote ,c) (guard (simple-constant? c)) #t] [,_ #f]))
	  (cons (car ls) (loop (cdr ls)))]
	 [(symbol? (car ls)) (let ((first (process (car ls))))
			       (cons first (loop (cdr ls))))]
	 [(list? (car ls)) (let ((first (loop (car ls))))
			     (cons first (loop (cdr ls))))]
	 [(vector? (car ls)) 
	  (cons (list->vector (loop (vector->list (car ls))))
		(loop (cdr ls)))]
	 [else (error 'reunique-names "bad subexpression: ~s" (car ls))]))))

;; unique-name produces a unique name derived the input name by
;; adding a unique suffix of the form .<digit>+.  creating a unique
;; name from a unique name has the effect of replacing the old
;; unique suffix with a new one.
;;
;; code-name takes a unique name and replaces its suffix ".nnn"
;; with "$nnn", e.g., f.3 => f$3.  It is used by convert-closure.
;;
;; extract-suffix returns the numeric portion of the unique suffix
;; of a unique name or code-name, or #f if passed a non unique name.
;;(module (unique-name reset-name-count! extract-suffix
;;                     code-name label-name #;method-name)
        ;RRN [01.09.16] -- We need to phase out code-name...

;; <br><br> 
;; [2004.06.28] I am replacing this with a version that uses
;; a hash-table to keep a counter per seed-name.
(begin
        ;; This is a single counter that is incremented every time a fresh variable is coined.
        (define unique-name-counter 
	  (make-parameter 0
			  (lambda (x) (if (integer? x) x (error 'unique-name-counter "bad value: ~a" x)))))
        (define (unique-suffix ignored)
            (unique-name-counter (+ (unique-name-counter) 1))
            (number->string (unique-name-counter)))

        (define extract-root
          (lambda (sym)
            (list->string
              (let ([chars (string->list (symbol->string sym))])
                (define (s0 ls)
                  (cond
                    [(null? ls) chars]
                    [(char-numeric? (car ls)) (s1 (cdr ls))]
                    [else chars]))
                (define (s1 ls)
                  (cond
                    [(null? ls) chars]
                    [(char-numeric? (car ls)) (s1 (cdr ls))]
                    [(memv (car ls) '(#\. #\$ #\_ ))
                     (reverse (cdr ls))]
                    [else chars]))
                (s0 (reverse chars))))))
        (define extract-suffix
          (lambda (sym)
            (let ([str (symbol->string sym)])
              (let ([n (string-length str)]
                    [m (string-length (extract-root sym))])
                (and (not (= n m))
                     (substring str (+ m 1) n))))))
        (define strip-illegal
          (lambda (str)
            (list->string
	     ;; [2007.03.10] Changing it to insert underscores rather than just removing the character:
	     (map (lambda (c) (if (or (char-alphabetic? c)
				      (char-numeric? c))
				  c
				  #\_))
	       (string->list str)))))
#;
	(define illegal-chars
            '(#\! #\@ #\# #\$ #\% #\^ #\& #\* #\. #\-))
#;
	(define strip-illegal
            (lambda (str)
              (let loop ([ls illegal-chars]
                         [chars (string->list str)])
                (if (null? ls) (list->string chars)
                    (loop (cdr ls) (remq (car ls) chars))))))

        ;;Ok, this is designed so that it can extract the root from
        ;;either a
        (define unique-name
          (lambda args
	    (let ((sym (if (null? args) 'gensym (car args))))
	      (let ((sym (cond
			  [(string? sym) (string->symbol sym)]
			  [(symbol? sym) sym]
			  [else (error 'unique-name "invalid name root: ~a" sym)])))
;		(printf "  Coining name, counter ~s\n" (unique-name-counter))
            (string->symbol
              (string-append
                (strip-illegal ;;RRN - THIS IS STUPID, CHANGE ME
                  (extract-root sym))
                "_" (unique-suffix sym)))))))
)

;; [2004.06.28]  NEW VERSION, counter per seed name:
;; Just overwriting definitions from above:
#;
(begin
        (define unique-name-count (make-default-hash-table))

        (define (unique-suffix sym)
	  (let ((entry (hashtab-get unique-name-count sym)))
	    (number->string
	     (if entry
		 (begin (hashtab-set! unique-name-count sym (add1 entry))
			entry)
		 (begin (hashtab-set! unique-name-count sym 1)
			0)))))

        (define reset-name-count! 
	  (lambda opt
	    (match opt
		   [() (set! unique-name-count (make-default-hash-table))]
		   [(,n) 
		    (error 'reset-name-count!
			   "this version of reset-name-count! cannot handle argument: ~s"
			   n)])))
)

;===============================================================================

;;; Manipulation of lambda-arguments (formals).
;;;
;;; TODO: Phase out!

(define formalexp?
  (lambda (formalexp)
    (match formalexp
      [(,v* ...) (list-is-set? v*)]
      [,v (guard (symbol? v)) #t]
      [(,v* ... . ,extra) (list-is-set? (cons extra v*))]
      [,else #f])))


;; [2004.04.24] We're not using most of these forms, but this is still
;; a valid procedure:
(define get-formals
  (lambda (formalexp)
    (match formalexp
      [(,v* ...) v*]
      [,v (guard (symbol? v)) (list v)]
      [(,v* ... . ,extra) (append v* (list extra))]
      [,else (error 'get-formals "invalid formals expression: ~a"
                    formalexp)])))

(define get-normal-formals
  (lambda (formalexp)
    (match formalexp
      [(,v* ...) v*]
      [,v (guard (symbol? v)) '()]
      [(,v* ... . ,extra) v*]
      [,else (error 'get-normal-formals "invalid formals expression: ~a"
                    formalexp)])))

(define get-list-formals
  (lambda (formalexp)
    (match formalexp
      [(,v* ...) '()]
      [,v (guard (symbol? v)) (list v)]
      [(,v* ... . ,extra) (list extra)]
      [,else (error 'get-list-formals "invalid formals expression: ~a"
                    formalexp)])))

(define cast-formals
  (lambda (formals formalexp)
    (match formalexp
      [,v (guard (symbol? v)) (car formals)]
      [(,v* ...) formals]
      [(,v* ... . ,extra)
       (match formals
         [(,v* ... ,last) `(,v* ... . ,last)])]
      [,else (error 'cast-formals
                    "invalid formals expression: ~a" formalexp)])))

;; [2005.11.28]
(define fit-formals-to-args
  (lambda (formals args)
    (let loop ((f formals) (a args))
      (cond
       [(and (null? f) (null? a)) ()]
       [(and (symbol? f) (null? a)) ()]
       [(and (pair? f) (pair? a))
	(cons (car f) (loop (cdr f) (cdr a)))]
       [(and (symbol? f) (pair? a))
	(cons f (loop f (cdr a)))]
       [else (error 'fit-formals-to-args
		    "cannot fit formals ~a to args ~a" formals args)]))))


;======================================================================

;;; Dealing with token-machines in sexp form

;; Predicate to test 
(define (token-machine? x)
  (match x
    [(,input-lang '(program (bindings ,nodebinds ...)
			    (socpgm (bindings ,socbinds ...) 
				    ,socstmts ...)
			    (nodepgm (tokens ,nodetoks ...)
				     (startup ,starttoks ...))))
     #t]
    [(quote (program (bindings ,nodebinds ...)
		     (socpgm (bindings ,socbinds ...) 
			     ,socstmts ...)
		     (nodepgm (tokens ,nodetoks ...)
			      (startup ,starttoks ...))))
     #t]
    [(program (bindings ,nodebinds ...)
		     (socpgm (bindings ,socbinds ...) 
			     ,socstmts ...)
		     (nodepgm (tokens ,nodetoks ...)
			      (startup ,starttoks ...)))
     #t]
    [,else #f]))

;; Project out the program portion.  Strip off the language-name wrapper.
(define (token-machine->program x)
  (if (token-machine? x)
      (match x
	     [(,input-lang ',x) x]
	     [(quote ,x) x]
             [,x x])
      (error 'token-machine->program "bad token machine: ~n ~s~n" x)))


;;; Token Handler Bindings.

;; There are different syntactic ways to write down token handler bindings.
;; (Thanks to optional forms in the syntax.)  This parses tokbinds.
;; (Note: I am no longer using handler-local constant "bindings", but
;; it doesn't hurt that this function handles them:)
(define (destructure-tokbind tbind)
  (define (process-stored s)
    (match s
	   [(,v ,e) `(,v ,e)]
	   [,v (guard (symbol? v))
	       `(,v '#f)]))
  (define (process-bods x)
    (match x
      
      [((stored ,s ...) (bindings ,b ...) ,bods ...)
       (values (map process-stored s)
	       b
	       (make-begin `((begin ,bods ...))))]
      [((bindings ,b ...) (stored ,s ...) ,bod ,bods ...)
       (values (map process-stored s)
	       b
	       (make-begin `((begin ,bod ,bods ...))))]
      [((stored ,s ...) ,bod ,bods ...)
       (values (map process-stored s)
	       '()
	       (make-begin `((begin ,bod ,bods ...))))]
            
      [((bindings ,b ...) ,bod ,bods ...)
       (values '() b
		    (make-begin `((begin ,bods ,bods ...))))]

      ;; This is missing an expression:
      [(,_ ... (,keyword ,s ...)) (guard (memq keyword '(stored bindings)))
       (error 'destructure-tokbind "Missing body from token handler declaration: ~a" x)]

      [,bods 
       (values '() '()
	       (make-begin `((begin ,bods ...))))]))
  (match tbind
	 [(,t (,a ...) ,bds ...)
	  (mvlet ([(stored bindings body) (process-bods bds)])
		 (values t DEFAULT_SUBTOK_VAR a stored bindings body))]
	 [(,t ,i (,a ...) ,bds ...)
	  (mvlet ([(stored bindings body) (process-bods bds)])
		 (values t i a stored bindings body))]
	 [,other (error 'destructure-tokbind "bad tokbind: ~a" other)]))

;; tokname accessor
(define (handler->tokname tb)
  ;; More efficient for now:
  (car tb)
  ;(mvlet ([(tok id args stored bindings body) (destructure-tokbind tb)])
  ;tok))
  )
;; formals accessor
(define (handler->formals tb)
  (mvlet ([(tok id args stored bindings body) (destructure-tokbind tb)])
    args))
;; body accessor
(define (handler->body tb)
  (mvlet ([(tok id args stored bindings body) (destructure-tokbind tb)])
    body))
;; subtokid accessor
(define (handler->subtokid tb)
  (mvlet ([(tok id args stored bindings body) (destructure-tokbind tb)])
    id))
(define (handler->stored tb)
  (mvlet ([(tok id args stored bindings body) (destructure-tokbind tb)])
    stored))


;=============================================================
;;; Dealing with the names of Tokens.
;;;
;;; Sloppy interface right now.  
;;; Used by the deglobalize pass.

;; [2004.06.13] Tokens will be more complex later.
(define (token-name? t) 
  (IFCHEZ (import rn-match) (begin))
  ;(or (symbol? t)
  ;(and (pair? t) (symbol? (car t)) (integer? (cdr t))))
  (match t
    [(tok ,name) #t]
    [(tok ,name ,num) #t]
    [,s (guard (symbol? s)) #t]
    [else #f]))

(define (token->name t)
  (IFCHEZ (import rn-match) (begin))
  (match t
	 [(tok ,name) name]
	 [(tok ,name ,num) name]
	   ;[(,name . ,_) name]
	 [,name (guard (symbol? name)) name]
	 [,other (error 'token->name "bad token: ~a" other)]))
(define (token->subtok t)
  (IFCHEZ (import rn-match) (begin))
  (match t
	 [(tok ,name) 0]
	 [(tok ,name ,num) num]
	 ;[(,_ . ,subtok) subtok]
	 [,name (guard (symbol? name)) 0]
	 [,other (error 'token->name "bad token: ~a" other)]))


;; Allocate a token name, possibly with a seed name.
(define new-token-name
  (lambda args
    (if (null? args)
	(unique-name 'token)
	(unique-name (car args)))))

(define token-names
  (case-lambda 
   [() (let ((n (unique-name 'token)))
	     (values (symbol-append 'f_ n)
		     (symbol-append 'm_ n)))]
   [(sym) 
    (if (symbol? sym)
	(values (symbol-append 'f_token_ sym)
		(symbol-append 'm_token_ sym))
	(error 'deglobalize.token-names 
	       "takes a symbol argument not this: ~s" sym))]))


    ;; Get's the token name that corresponds with the edge of a
    ;; dataflow graph that corresponds with a variable name.
    ;; For the moment token-names is deterministic!!  So we just do this:
(define get-names
  (lambda (v) (token-names v)))
;	(mvlet ([(f m) (token-names v)]) f)))
;    (define get-membership-name
;      (lambda (v)
;	(mvlet ([(f m) (token-names v)]) m)))
(define (get-formation-name v) (mvlet ([(f m) (token-names v)]) f))
(define (get-membership-name v) (mvlet ([(f m) (token-names v)]) m))




  ;;; Regiment Random Number Interface. <br>
  ;;;
  ;;;   These provide a simple random number generator interface for use
  ;;; within the Regiment codebase. <br>
  ;;;    The simulator should only use this RNG interface to maintain
  ;;; determinism.  (Currently this just uses the primitive Chez
  ;;; Scheme RNG, so there is no proper seperation which would be
  ;;; necessary for other concurrently running code to not ruin the
  ;;; simulators determininms.)
  ;;;
  ;;; <br><br>
  ;;;  TODO: FIXME: [2006.03.01]
  ;;; I should overhall this.  Really, what I want is a separate RNG for simulator-alpha.
  ;;; And then I'd like another seperate RNG for the sensor data-generator.

  ;; NOTE: This is currently unisolated anyways.  So it's just a
  ;; common interface into Chez/PLT's RNG's.  Need to go further than that.
  (IFCHEZ
   (begin 
   ;; A random integer. 
   (define reg:random-int
     (case-lambda 
       [() (#%random (#%most-positive-fixnum))]
       [(k) (#%random k)]))
   
   ;; A random real number.
   (define reg:random-real
     (case-lambda
       [() (#%random 1.0)]
       [(n) (#%random n)]))
   
   ;; Get the state of the RNG.
   (define (reg:get-random-state) (random-seed)) ;; This doesn't work!!! [2005.10.05]
   
   ;; Set the state of the RNG.
   (define (reg:set-random-state! s) (random-seed s))
   )
   (begin
     (define reg:random-int
       (case-lambda
         [() (reg:random-int (- (expt 2 31) 1))]
         [(k) (random k)]))
     (define reg:random-real
       (case-lambda
         [() (random)]
         [(k) (* k (random))]))
     
     (define (reg:get-random-state)
       (pseudo-random-generator->vector (current-pseudo-random-generator)))
     (define (reg:set-random-state! s)
       (current-pseudo-random-generator (vector->pseudo-random-generator s)))   
     ))
      
;; [2006.03.01] UNFINISHED
#;
(define (reg:very-deep-map f x)
  (let ([touched (make-default-hash-table)]) 
      (let loop ((x x))
	;(printf "MAPPING DOWN: ~a\n" (reg:struct? x))
	(cond
	 ;; Function gets applied for each instance of a repeated object:
	 [(hashtab-get touched x) => (lambda (v) ;(printf "HIT REPEAT\n")
					     (f v))]
	 [(pair? x) 
	  ;; Add the original cell to the touched list.
	  (let ([newcell (cons #f #f)])
	    ;; Associate the old cell with the new cell.
	    (hashtab-set! touched x newcell)
	    (set-car! newcell (loop (car x)))
	    (set-cdr! newcell (loop (cdr x)))
	    (f newcell))]
	 [(vector? x) 
	  (let ([newvec (make-vector (vector-length x) #f)])
	    (hashtab-set! touched x newvec)
	    (for i = 0 to (sub1 (vector-length x))
		 (vector-set! newvec i (loop (vector-ref x i))))
	    (f newvec))]
	 [(reg:struct? x)
	  ;; Copy the structure:
	  (let* ([lst (reg:struct->list x)]
		 [type (record-type-descriptor x)]
		 [new (reg:list->struct x (cdr lst))])
	    (hashtab-set! touched x new)
	    ;; CHEZ SPECIFIC.
	    ;; CURRENT BEHAVIOR IS THAT IMMUTABLE FIELDS ARE OPAQUE TO THIS MAP FUNCTION:
	    (for-each (lambda (decls val)
			;(printf "DECLS & val: ~s ~s\n" decls val)
			(unless (memq 'immutable decls)
			  ((record-field-mutator type (caddr decls))
			   new (loop val))))
	      (record-type-field-decls type)
	      (cdr lst))
	    (f new))]
	 [(atom? x) (f x)]
	 [else (error 'reg:very-deep-map "what on earth is this object? ~a" x)]))))

; (reg:very-deep-map (\\ x (if (integer? x) (add1 x) x)) 
;                       (let ((x '#0=(1 2 #(3 #0#)))) (set-car! x (make-node 99 99)) x))

;  (deep-reg:struct->list2 (make-node 1 2))
;  (deep-reg:struct->list2 (make-node 1 (make-node 3 4)))

;; THIS DOESN'T WORK RIGHT NOW:
;  (deep-reg:struct->list2 (let ((x (make-node 1 (make-node 3 4)))) (set-node-id! x x) x))

;; DOESN'T WORK YET:
#;
(define (deep-reg:struct->list2 ob)
  (reg:very-deep-map
   (let ([touched (make-default-hash-table)])
     (lambda (x)
       (cond
	[(not (reg:struct? x)) x]
	[(hashtab-get touched x) => (lambda (v) v)]
	[else (let ([ret (reg:struct->list x)])
		(hashtab-set! touched x ret)
		ret)]
	)))
   ob))

#;
;; UNFINISHED:
;; This is like generic-traverse, but for any list/vector/record data structure.
;; TODO: How to handle cyclic structures.
(define (reg:very-deep-traverse driver fuser x)
  (let loop ((e x))
    (driver e 
	    (lambda (x)
	      (cond
	       [(atom? x) (fuser x () (lambda () x))]
	       [(pair? x) (fuser x (list (loop (car x)) (loop (cdr x)))
				 (lambda (a d) (cons a d)))]
	       [(vector? x) (fuser x (map loop (vector->list x))
				   (lambda args (list->vector args)))]
	       [(reg:struct? x)
		(fuser x (cdr (reg:struct->list x))
		       (lambda args (reg:list->struct x args)))]
	       [else
		(error 'reg:very-deep-traverse "unknown object: ~s" otherwise)])))))



;=============================================================

;;; A few utilities that depend on Regiment's isolated RNG.

;; [2004.06.15] Copying this from generic utils file.
(define list-get-random
  (lambda (ls)
    (if (null? ls)
        (error 'list-get-random "cannot get random element from null list.")
        (list-ref ls (reg:random-int (length ls))))))
;; This too:
(define randomize-list
  (lambda (ls)
    (let* ([vec (list->vector ls)])
      (vector->list (randomize-vector! vec)))))

(define vector-get-random
  (lambda (vec)
    (let ([len (vector-length vec)])
      (if (fxzero? len)
	  (error 'list-get-random "cannot get random element from empty vector.")
	  (vector-ref vec (reg:random-int len))
	  ))))
(define (randomize-vector! vec)
  (let ([len (vector-length vec)])
    (let ([swap (lambda (i j)
		  (let ([temp (vector-ref vec i)])
		    (vector-set! vec i (vector-ref vec j))
		  (vector-set! vec j temp)))])
      (do ([i 0 (add1 i)]) ((= i len))
	;; Swap with a later position:
	(swap i (+ i (reg:random-int (- len i)))))
      vec)))

; ======================================================================

;;; Misc.

;; This prints the valus of the most important parameters.  It is for
;; recording the state of the compiler/simulator.  It's primarily for
;; good book-keeping while benchmarking.  It only records parameters
;; relevent to the resulting data.
(define (regiment-print-params prefix . port)
  (let ([port (if (null? port) (current-output-port) (car port))]
	[pad-width (if #f;(top-level-bound? 'pad-width) ;; [2006.02.27] FIXME
		       (top-level-value 'pad-width)
		       (lambda (_ x) (format "~a" x)))])
    (for-each 
	(lambda (param-name)	 
	  (fprintf port "~a ~a = ~s\n" prefix 
		   (pad-width 30 param-name)
		   ((top-level-value param-name))))
      (filter (lambda (p) 
		(not (memq p 
			   ;; This is an omit list:
			   '(repl-stream-depth

			     simalpha-label-sensorvals
			     simalpha-label-msgcounts
			     simulator-write-sims-to-disk
			     simalpha-graphics-on
			     simalpha-stream-result

			     simalpha-realtime-mode
			     simalpha-output-port
			     default-unit-tester-retries
			     reg:comment-code
			     simulation-logger-count
			     simulation-logger-level
			     simulation-logger
			     regiment-emit-debug
			     regiment-verbose
			     ))))
	(regiment-parameters))
      )))


;; The utility used for parsing the modestring arguments to readFile.
;; Returns:  a __readfile expression with its many fields.
(define (parse-readFile-modestring annot str typ fn src)
  (ASSERT string? str)
  ;; Defaults:
  (let* ([mode "text"]
	 [repeats 0]
	 [winsize 1] ;; Another meaningless default.
	 [skipbytes 0]
	 [offset 0]
	 [p (open-input-string str)]
	 [params (let loop ([x (read p)])
		   (if (eof-object? x) '()
		       (cons x (loop (read p)))))]		
	 [pairs (match params
		  [() '()]
		  [(,a ,b . ,[tl]) (cons (list a b) tl)]
		  [,oth (error 'readFile "invalid parameter string to readFile primitive: ~s" str)])]
	 [num (lambda (n) 
		(if (integer? n) n			   
		    (error 'readFile "expected numeric parameter, got: ~s" n)))]
	 [types (match typ
		  [#(,t* ...)  t*]
		  [(Sigseg ,[t]) t]
		  [,t   	(list t)])])
    (for-each (match-lambda ((,flag ,val))
		(case flag
		  [(mode:) (set! mode (case val 
					[(text) "text"]
					[(binary) "binary"]
					[else (error 'readFile "unsupported mode: ~s" val)]))]
		  [(repeats:)   (set! repeats (num val))]
		  [(skipbytes:) (set! skipbytes (num val))]
		  [(offset:)    (set! offset  (num val))]
		  [(window:)    (set! winsize (num val))]
		  [else (error 'readFile "unknown option flag \"~s\"\n Valid flags are: ~s\n" 
			       flag 
			       '(mode: repeats: skipbytes: offset: window:))])
		) pairs)
    (when (equal? mode "text")
      (unless (= offset 0)
	(error 'readFile "doesn't support 'offset:' option in conjunction with text mode"))
      (unless (= skipbytes 0)
	(error 'readFile "doesn't support 'skipbytes:' option in conjunction with text mode")))
    
    ;; If we're not producing a sigseg, we must set the winsize to zero:
    (match typ
      [(Sigseg ,t) (void)]
      [,else (set! winsize 0)])
    `(__readFile ,annot ,fn ,src ',mode ',repeats ',skipbytes ',offset ',winsize ',types)

    )
  )

;; This defines the set of "annotations" recognized by the compiler.
(define (annotation? sym)
  (memq sym '(assert-type src-pos data-rate)))

;; Takes off just the outer layer of annotations
(define (peel-annotations e)
  (match e
    [(,ann ,_ ,[e]) (guard (annotation? ann)) e]
    [,e                    e]))

;; This variant returns all the annotations to you.
;; Not used yet:
#;
(define (peel/collect-annotations e)
  (match e
    [(,ann ,_ ,[e a*]) (guard (annotation? ann))
     (values e (cons (list ann _) a*))]
    [,e (values e ())]))


;; This is a simple interactive debugging tool.  It shows the binding
;; spine of the program.
;;
;; Levels:
;;  
(define let-spine 
  (case-lambda 
    [(arg)  (if (number? arg) 
		(lambda (e) (let-spine arg e))
		(let-spine 0 arg))]
    [(level e)      
      (match e
       [,v (guard (symbol? v)) v]
       [(,lett ([,lhs* ,ty* ,[rhs*]] ...) ,[bod])
	(guard (memq lett '(let* let letrec lazy-letrec)))
	`(,lett ,(map (lambda (v rhs) (list v (if (>= level 1) rhs '_))) 
		   lhs* rhs*)
		,bod)]

       [(begin ,[e*] ...)
	(if (>= level 3) `(begin ,@e*) '_)]

       [(lambda ,args ,ty ,[bod]) 
	(if (>= level 2) `(lambda ,args ,bod) '_)]
       [(iterate ,annot ,[fun] ,[bod])
	(if (>= level 3) 
	    (if (>= level 4)
		`(iterate ,annot ,fun ,bod)
		`(iterate ,annot _ ,bod))
	    '_)]

       [(assert-type ,_ ,[e]) e]
       [(src-pos ,_ ,[e]) e]
       [(using ,_ ,[e]) e]
       
       [(,lang '(program ,[e] ,_ ...)) e]

       ;; duplicated code:
       [(,lett ([,lhs* ,[rhs*]] ...) ,[bod])
	(guard (memq lett '(let* let letrec lazy-letrec)))
	`(,lett ,(map (lambda (v rhs) (list v (if (>= level 1) rhs '_))) 
		   lhs* rhs*)
		,bod)]
       ;; duplicated code:
       [(lambda ,args ,[bod]) (if (>= level 2) `(lambda ,args ,bod) '_)]

       [,else '_])]))

; ======================================================================

(define-testing these-tests
  `( 
    ["Reunique names" 
     (reunique-names '(foo_3 foo_43 foo_3 foo))
     (foo foo_1 foo foo_2)]
    ["Reunique names #2"
     (reunique-names '(foo_3 (bar_3) foo_43 foo_3 (bar_3 bar_4)))
     (foo (bar) foo_1 foo (bar bar_1))]

    ["deep reg:struct->list"
     (let* ([v (vector 'a 'b)]
	   [s (list 1 v 2)])       
       (reg:define-struct (foo a b))
       (vector-set! v 0 s)
       (set-car! s v)
       (let ([foo (make-foo 1 2)])
	 (set-foo-b! foo foo)
	 (set-foo-a! foo s)
	 (deep-reg:struct->list foo)
	 ))
     ,(lambda (x)
	(and (list? x)
	     (= (length x) 3)
	     (equal? (car x) "foo")))]

    ))

(define-testing test-this (default-unit-tester "regiment_helpers.ss: Regiment-specific utils." these-tests))
(define test-regiment_helpers test-this)

) ; End Module.
