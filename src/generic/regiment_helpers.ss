
;;;; Regiment Helper Functions

;;;; <br> [2005.12.01] <br>

;;;; I'm going to start the (slow) process of seperating all the
;;;; Regiment-specific support utilities from my generic scheme
;;;; utilities.  Previously all of this had resided in the bloated
;;;; helpers.ss file.


;; [2004.07.28] Introducing 'Area'.  Note that a Region is also an Area.
;; Ok, redoing primitive listings with type information:
;; The types I'm using right now are:
;;   Anchor, Area, Region, Signal, Event, Node, Location, Reading
;;   Function, Number, Integer, Float, Bool, Object, Void
;;   List, Array

;; Since I'm going to go statically typed eventually, Object is just
;; my way of signifying "for all alpha" right now.

;; And the old types from the Snet compiler were:
;;   Bool Char Float64 Int32 List Object
;;   Number Pair Port String Symbol Vector Void


;; Then some types that are used only in the local language are:
;;   Token NodeID


;; These are really the *local* primitives:
(define regiment-basic-primitives 
    ; value primitives
  '((cons (Object Object) Pair)
    (cdr (Pair) Object)
    (car (Pair) Object)
    (append (List List) List)
    (list Object List)
;    (cons (Object List) List) 
;    (cdr (List) List)
;    (car (List) Object)
;; [2005.10.20] Allowing improper lists for the moment ^^^

    (+ (Integer Integer) Integer) 
    (- (Integer Integer) Integer) 
    (* (Integer Integer) Integer) 
    (/ (Integer Integer) Integer) 

    (+. (Float Float) Float) 
    (-. (Float Float) Float) 
    (*. (Float Float) Float) 
    (/. (Float Float) Float) 
    (max (Number Number) Number)
    (min (Number Number) Number)
    (abs (Number) Number)

    (vector Object Array)
    ;(make-vector (Object Integer) Array)
    (vector-ref (Array Integer) Object)
    ;(vector-set! (Array Integer Object) Void)

    (locdiff (Location Location) Float)

    (not (Bool) Bool)
    (or Bool Bool)
    (and Bool Bool)

    ; predicates
    (=  (Number Number) Bool)
    (<  (Number Number) Bool)
    (>  (Number Number) Bool)
    (<=  (Number Number) Bool)
    (>=  (Number Number) Bool)
;    (eq? (Object Object) Bool)
    (equal? (Object Object) Bool)
    (eq? (Object Object) Bool)  ;; This should just be = when it comes down to it.
    (null? (List) Bool)

    ;; These are dynamically typed primitives: 
    (pair? (Object) Bool)
    (number? (Object) Bool)
    (even? (Integer) Bool)
    (odd? (Integer) Bool)

    ;; Shouldn't this be local??
    ;; I'm not sure...
    (sense         (Node) Float)
    (nodeid        (Node) Integer)

    ))

;; These are pretty much compiler-internal primitives which can
;; operate on the local node.
(define local-node-primitives 
  '(
    (my-id () NodeID)
    ;(gdist (Token) Integer) ;; Phase this out "dist" is wrong.
    ;(ghopcount (Token) Integer)
    ;(gparent (Token) NodeID)
    ;(gorigin (Token) NodeID)
    ;(gversion (Token) Integer)
    
    (check-tok (Token) Bool)
    ;; Gotta have a way to remove tokens also!
    ;; Not to mention expiration dates!!

    (list Object List)
    (append List List)

    (rfoldwith (Token Function Object Region) Signal)
    ))

;; These count as primitives also.
;; All regiment constants are presumed to be "slow prims" for
;; now. (see add-heartbeats)
(define regiment-constants
  '(
    (world          Constant         Region)
    (anchor         Constant         Anchor)
    ))

;; These are the distributed primitives.  The real Regiment combinators.
(define regiment-distributed-primitives 
  '(
    
    (rmap           (Function Area) Area)

    (rfold          (Function Object Area) Signal)
    (smap           (Function Signal) Signal)
    
    ;; This joins two signals in the network.
    (smap2          (Function Signal Signal) Signal)

    ;; This is the identity function on regions.  
    ;; However it also lights an LED.
    (light-up (Area) Area)

    (anchor-at      (Number Number) Anchor)
    (anchor-dist    (Anchor Anchor) Number)

    (anchor-optimizing (Function Region) Anchor)

    ;; Takes a function to optimize, and an optional refresh rate to re-elect at:
    ;; If no refresh rate is zero, the election only happens once.
    (anchor-maximizing (Function Number) Anchor)

    (circle         (Anchor Dist)   Region)
    (circle-at      (Number Number Dist) Region)
    (k-neighborhood (Anchor Number) Region)
    ;; Shorthand: 
    (khood          (Anchor Number) Region)
    (khood-at       (Number Number Number) Region)

    ;; This lifts a node value into the Signal monad:
    (node->anchor   (Node) Anchor)

    ;; This one returns a region of regions:
    (cluster        (Area) Area)
    (sparsify       (Area) Area)
    (border         (Area) Area)
;    (planarize      (Area) Area)
;    (treeize        (Area) Area)

    (rfilter         (Function Area) Area)
    (runion           (Area Area) Area)
    (rintersect       (Area Area) Area)

    ;; Prolly not the right type:
    (until          (Event Signal Signal) Signal)
    (areaWhen       (Event Area) Area)
    ;; The float is a time in seconds.
    (constEvent     (Object Float) Event)

    ;; What was this one supposed to do and what was it's type?
;    (when           (Event Signal) Signal)
    (rwhen-any        (Function Area) Event)
    (swhen-any        (Function Signal) Event)
    (when-percentage (Float Function Area) Event)

;     neighbors 
;    time-of
;    (time (Node) Time)
     ))
  
; [2004.03.31] - I don't know what the system's going to be called so
; I'm using the placeholder "blanko" which I will replace later. 
; OK, replacing with "regiment"
(define regiment-primitives
  (append regiment-basic-primitives
	  regiment-distributed-primitives
	  regiment-constants))

;; [2004.06.24] This is for the regiment primitives:
(define get-primitive-entry
  (lambda (prim)
    (or (assq prim regiment-primitives)
	(assq prim token-machine-primitives)
        (error 'get-primitive-entry
               "no entry for this primitive: ~a" prim))))

;; This lets you easily line up a primitives arguments with the expected types.
(define map-prim-w-types
  (lambda (f prim origargs)
    (let loop ([args origargs] [types (cadr (get-primitive-entry prim))])
      (cond
       [(null? args) '()]
       [(null? types)
	(error 'map-prim-w-types "too many arguments to prim ~a: ~a" prim origargs)]
       [(pair? types) 
	(cons (f (car args) (car types))
	      (loop (cdr args) (cdr types)))]
       [else (cons (f (car args) types)
		   (loop (cdr args) types))]))))

#;
(define (get-primitive-arity prim)
  (let* ([entry (get-primitive-entry prim)]
	 [args (cadr entry)])
    (cond
     [(eq? 'Constant args) #f]
     [else (length args)])))

;; Is it a regiment primitive?
(define (regiment-primitive? x)
  (if (assq x regiment-primitives) #t #f))

;; Is it a regiment constant?
(define (regiment-constant? x)
  (if (assq x regiment-constants) #t #f))

;; More specific classification of Regiment primitives.
(define (basic-primitive? x) 
  (if (assq x regiment-basic-primitives) #t #f))
;; More specific classification of Regiment primitives.
(define (distributed-primitive? x) 
  (if (assq x regiment-distributed-primitives) #t #f))

;; Predicate for token machine primitives.
(define (token-machine-primitive? x)
  (if (assq x token-machine-primitives) #t #f))

;; [2004.06.09]<br>  Many of these are actually language forms.  I gotta
;; get this sorted out eventually.
;; 
;; TODO: add some kind of type info.
;; <br>[2004.10.22]  For now everything that handles tokens is a syntax not a prim.
;; <br>[2005.05] I revoked that.  Basically everything is a prim now.
(define token-machine-primitives
    ; Arithmetic prims:
  '((+ (Integer Integer) Integer) 
    (- (Integer Integer) Integer) 
    (* (Integer Integer) Integer) 
    (/ (Integer Integer) Integer) 
    (max (Number Number) Number)
    (min (Number Number) Number)
    (abs (Number) Number)
    (cos (Float) Float)
    (sin (Float) Float)
    (tan (Float) Float)
    (acos (Float) Float)
    (asin (Float) Float)
    (atan (Float) Float)

    (+. (Float Float) Float) 
    (-. (Float Float) Float) 
    (*. (Float Float) Float) 
    (/. (Float Float) Float) 
    (int->float (Integer) Float)
    (float->int (Float) Integer)

    (not (Bool) Bool)
    ; predicates
    (=  (Number Number) Bool)
    (<  (Number Number) Bool)
    (>  (Number Number) Bool)
    (<=  (Number Number) Bool)
    (>=  (Number Number) Bool)
;    (eq? (Object Object) Bool)
    (equal? (Object Object) Bool)
    (eq? (Object Object) Bool)
    (even? (Integer) Bool)
    (odd? (Integer) Bool)
    (random (Integer) Integer)
    
    ;; Takes an optional second argument, that's the fun to optimize.
    (elect-leader Token . Token)
    (flood Token) ;; These are actually macros, but what the heck

;     (greturn)
;     (emit)
;     (relay)
;     (call)
;     (timed-call)
;     (activate)
;     (dist) 
     (light-node (Integer Integer Integer) Void)
;     (sense)
     (my-id  () Integer)
     (my-clock () Integer)
     (loc () List) ;(loc () Location)
     (locdiff (List List) Float) ;(locdiff (Location Location) Float)

     (dbg (String . Object) Void)

     (call (Token . Object) Void)
     (bcast (Token . Object) Void)
     (call-fast (Token . Object) Void)
     (timed-call (Integer Token . Object) Void)

     (subcall (Token . Object) Object)
     ;; This one happens immediately, possibly by inlining:
     (direct-subcall (Token . Object) Object)
     (return (Object) Void)
     ;(greturn (Object) Void) ;; This is a syntax, not a primitive.
     
     (token-scheduled? (Token) Bool)
     (token-deschedule (Token) Void)
     (token-present? (Token) Bool)
     (evict (Token) Void)
     ;; This one ignores subid and evicts all instances sharing the token name.
     (evict-all (Token) Void)

     (token->subid (Token) Integer)

     (void () Object)

     (this () Object)
     ;; TODO: add different kinds of sensors.
     ;; This desugars into (subcall (tok SenseTok 0)):
     (sync-sense () Number)
     (soc-return (Number) Void)
     (soc-return-finished (Number) Void)
     
     ;; LISTS: TEMPORARY, just for debugging/testing:
     (cons (Object List) List) 
     (car (List) Object)
     (cdr (List) List)
     (cadr (List) Object)
     (null? (List) Bool)
     (list Object List)
     (append List List)
    
     (vector Object Array)
     (make-vector (Integer Object) Array)
     (vector-ref (Array Integer) Object)
     (vector-set! (Array Integer Object) Void)
     (vector-length (Array) Integer)

     ;; For debugging only:
     (sim-print-queue Number Void)
     (error (Object String . Object) Void)
     (printf (String . Object) Void)
     (procedure? (Object) Bool)
     (pad-width (Number String) Void)
     ;; For simulator only:

     ;; This just displays text by a nodes' icon.
     (setlabel (String . Object) Void)
     (highlight-edge (Integer) Void)
     ; [2005.04.30] Disabling these for now, will get them back up later.
     (draw-mark (List) Void)
     (rgb (Integer Integer Integer) Object)
     ))

;; Keywords allowed in the restricted token machine language.
(define token-machine-keyword?
  (lambda (x)
    (and (memq x '(quote set! if begin letrec let let-stored)) #t)))


;; There are different syntactic ways to write down token handler bindings.
;; (Thanks to optional forms in the syntax.)  This parses tokbinds.
;; (Note: I am no longer using handler-local constant "bindings", but
;; it doesn't hurt that this function handles them:)
(define (destructure-tokbind tbind)
  (define (process-stored s)
    (match s
	   [(,v ,e) `(,v ,e)]
	   [,v `(,v '#f)]))
  (define (process-bods x)
    (match x
	   [((stored ,s ...) (bindings ,b ...) ,bods ...)
	    (values (map process-stored s)
		    b
		    (make-begin `((begin ,bods ...))))]
	   [((bindings ,b ...) (stored ,s ...) ,bods ...)
	    (values (map process-stored s)
		    b
		    (make-begin `((begin ,bods ...))))]
	   [((stored ,s ...) ,bods ...)
	    (values (map process-stored s)
		    '()
		    (make-begin `((begin ,bods ...))))]
	   [((bindings ,b ...) ,bods ...)
	    (values '() b
		    (make-begin `((begin ,bods ...))))]
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

;;; constants  (which can all occur unquoted)
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
      [(symbol? imm) 'Symbol]
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


;; Gotta remember to update this if I change the format of prim entries..
(define (get-primitive-return-type prim)
  (last (get-primitive-entry prim)))


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
       

