
;;;; .title Regiment Helper Functions (regiment_helpers.ss)

;;;; <br> [2005.12.01] <br>

;;;; I'm going to start the (slow) process of seperating all the
;;;; Regiment-specific support utilities from my generic scheme
;;;; utilities.  Previously all of this had resided in the bloated
;;;; helpers.ss file.


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
;;; The various sensor-reading stubs.  Used by SimAlpha. <br>
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
;; DEALING WITH TOKEN NAMES.  
;; Sloppy interface right now.  
;; Used by the deglobalize pass.

;; [2004.06.13] Tokens will be more complex later.
(define (token-name? t) 
  ;(or (symbol? t)
  ;(and (pair? t) (symbol? (car t)) (integer? (cdr t))))
  (match t
    [(tok ,name) #t]
    [(tok ,name ,num) #t]
    [,s (guard (symbol? s)) #t]
    [else #f]))

(define (token->name t)
  (match t
	 [(tok ,name) name]
	 [(tok ,name ,num) name]
	   ;[(,name . ,_) name]
	 [,name (guard (symbol? name)) name]
	 [,other (error 'token->name "bad token: ~a" other)]))
(define (token->subtok t)
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

;=============================================================
;; Dealing with token-machines in sexp form

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


(define (token-machine->program x)
  (if (token-machine? x)
      (match x
	     [(,input-lang ',x) x]
	     [(quote ,x) x]
             [,x x])
      (error 'token-machine->program "bad token machine: ~n ~s~n" x)))
     
;=============================================================

;;; .section-id regiment-prims
;;; The regiment primitives definitions.                       <br>
;;; These are the symbols I use for different types right now: <br>&nbsp;&nbsp;
;;;   Bool Char Float64 Int32 List Object                      
;;;   Number Pair Port String Symbol Vector Void


;; The subset of Scheme keywords we support. <br>
;; [2004.06.28] RRN: Removed 'let'
(define base-keyword?
  (lambda (x)
    (and (memq x '(quote set! if begin letrec lambda)) #t)))

;; Test for constants, which can all occur unquoted. <br>
;; Note that symbol constants may not occur unquoted.  Otherwise
;; they'd be variables!  Thus in a lot of places you will see: <br>
;;   (or (symbol? x) (constant? x))                            <br><br>
;; Because this describes what can go inside a (quote _) expression
;; after the complex constants have been removed.
(define constant?
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
	
        )))

;; A potentially quoted integer.
(define (qinteger? n)
  (match n
    [,i (guard (integer? i)) #t]
    [',i (guard (integer? i)) #t]
    [,else #f]))
;; Get the value of a potentially quoted integer.
(define (qinteger->integer n)
  (match n
    [,i (guard (integer? i)) i]
    [(quote ,i) 
     (DEBUGASSERT (integer? i))
     i]
    [else (error 'qinteger->integer "this is not a qinteger: ~a" n)]))


#; ; Not used:
(define constant->type
  (lambda (imm)
    (cond
      [(fx-integer? imm) 'Int32]
      [(flonum? imm) 'Float64]
      [(bg-integer? imm) 'Number]
      [(ratnum? imm) 'Number]
      [(boolean? imm) 'Bool]
      [(char? imm) 'Char]
      [(string? imm) 'String]
      [(null? imm) 'List]
;      [(symbol? imm) 'Symbol]
      [else (error 'constant->type
                   "unknown constant: ~s" imm)])))


;; Datums include structured constants as well as atoms.
(define datum?
  (lambda (x)
    (or (constant? x)
        (null? x)
        (symbol? x)
        ;(string? x)
        (if (pair? x)
            (and (datum? (car x)) (datum? (cdr x)))
            (and (vector? x) (andmap datum? (vector->list x)))))))


;; create a "flattened" begin from list of expressions
;; e.g., (make-begin '(1 (begin 2) (begin 3 4) 5)) => (begin 1 2 3 4 5)
;; 
(define make-begin
  (lambda  (expr*)
    (let ([initlst (match expr*
			  [(begin ,x* ...) x*]
			  [,ls ls])])
      (match (match `(begin ,@initlst)
		    [(begin ,[expr*] ...) (apply append expr*)]
		    [,other (list other)])
      [() (void)]
      [(,x) x]
      [(,x ,x* ...) `(begin ,x ,x* ...)]))))


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

; RRN: [2005.11.20] Not currently used:

; Presently defined for either Scheme or internal Scheme primitives
#|
(define predicate-primitive?
  (lambda (x)
    (cond
      [(or (assq x scheme-primitives)
           (assq x internal-scheme-primitives)) =>
       (lambda (a) (eq? (caddr a) 'test))]
      [else #f])))

(define value-primitive?
  (lambda (x)
    (cond
      [(or (assq x scheme-primitives)
           (assq x internal-scheme-primitives)) =>
       (lambda (a) (eq? (caddr a) 'value))]
      [else #f])))

(define effect-primitive?
  (lambda (x)
    (cond
      [(or (assq x scheme-primitives)
           (assq x internal-scheme-primitives)) =>
       (lambda (a) (eq? (caddr a) 'effect))]
      [else #f])))
|#




; =======================================================================

;;; Unique names.

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
	 [(or (constant? (car ls))
	      (match (car ls) [(quote ,c) (guard (constant? c)) #t] [,_ #f]))
	  (cons (car ls) (loop (cdr ls)))]
	 [(symbol? (car ls)) (let ((first (process (car ls))))
			       (cons first (loop (cdr ls))))]
	 [(list? (car ls)) (let ((first (loop (car ls))))
			     (cons first (loop (cdr ls))))]
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
              (filter (lambda (c) (or (char-alphabetic? c)
                                      (char-numeric? c)))
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

(define formalexp?
  (lambda (formalexp)
    (match formalexp
      [(,v* ...) (set? v*)]
      [,v (guard (symbol? v)) #t]
      [(,v* ... . ,extra) (set? (cons extra v*))]
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

#;(define cast-args
  (lambda (argexps formalexp)
    (match formalexp
      [,v (guard (symbol? v)) (list (cons 'list argexps))]
      [(,v* ...) argexps]
      [(,v* ... . ,extra)
       (let ([len (length v*)])
         (append (list-head argexps len)
                 (list (cons 'list (list-tail argexps len)))))]
      [,else (error 'cast-args
                    "invalid formals expression: ~a" formalexp)])))
       

;; TODO: TEST THIS
(define (regiment-free-vars expr)
  (list->set 
   (let loop ((env ()) (expr expr))
     (match expr	 
       [,var (guard (symbol? var) (not (regiment-constant? var)))
	     (if (memq var env) '() (list var))]
       [(quote ,x) '()]
       [(,prim ,rand* ...) (regiment-primitive? prim)
	(let ((frees (map (lambda (x) (loop env x)) rand*)))
	  (apply append frees))]
       [(lambda (,formals) ,expr)
	(loop (append formals env) expr)]
       [(lambda (,formals) ,types ,expr)
	(loop (append formals env) expr)]
       [(,letrec ([,lhs* ,ty* ,rhs*] ...) ,expr) (memq letrec '(letrec lazy-letrec))
	(let ([newenv (append lhs* env)])
	  (apply append (loop newenv expr)
		 (map (lambda (rhs) (loop newenv rhs)) rhs*)))]
       [(,letrec ([,lhs* ,rhs*] ...) ,expr) (memq letrec '(letrec lazy-letrec))
	(let ([newenv (append lhs* env)])
	  (apply append (loop newenv expr)
		 (map (lambda (rhs) (loop newenv rhs)) rhs*)))]
       [(,app ,[opera*] ...)
	(apply append opera*)]       
       [,else (error 'free-vars "not simple expression: ~s" expr)]))))

; ======================================================================

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
			     simalpha-write-sims-to-disk
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

; ======================================================================

(define these-tests
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

(define test-this (default-unit-tester "regiment_helpers.ss: Regiment-specific utils." these-tests))
(define test-regiment_helpers test-this)
