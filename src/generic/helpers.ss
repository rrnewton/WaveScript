;;; RRN: this file needs some serious cleaning-out.  The .NET compiler doesn't;;
;;; use a lot of the stuff in here.                                           ;;
;==============================================================================;

;; REQUIRES: On chez/plt primitive open-output-string.
;; REQUIRES: (On other chez/plt features which I'm not aware of...)


;; This is not a very appropriate place for this definition, but it's the most convenient
;; so that it can be had from.
(define-syntax DEBUGMODE (syntax-rules () [(_ expr ...) (begin expr ...)]))

(define-syntax DEBUGASSERT 
  (syntax-rules () 
    [(_ expr ...) (if (and expr ...) #t 
		      (error 'DEBUGASSERT "failed: ~s" (quote (and expr ...))))]))

(define Regiment-Log-File "~/tmp/Regiment.log.ss")
;(define Regiment-Log-File (string-append (current-directory) "/Regiment.log.ss"))
;(define Regiment-Log-File (string-append "Regiment.log.ss"))
;; Delete that logfile at load time:
(if (file-exists? Regiment-Log-File)
    (delete-file Regiment-Log-File))

;; This one prints nothing at all:
(define-syntax DEBUGPRINT 
  (syntax-rules ()
;    [(_ expr ...) (begin expr ...)]))
    [(_ expr ...) (void)]))

;; This is a SECOND print channel for even lamer information:
(define-syntax DEBUGPRINT2
  (syntax-rules ()
;    [(_ expr ...) (begin expr ...)]))
    [(_ expr ...) (void)]))


;; This one prints to the logifle, openning and closing it every time:
#;(define-syntax DEBUGPRINT 
  (syntax-rules ()
    [(_ expr ...)
     (let ((p (open-output-file Regiment-Log-File 'append)))
       (parameterize ((current-output-port p))
		     (begin expr ... (flush-output-port))
		     )
       (close-output-port p))]))

(define regiment-parameters (make-parameter '()))
(define-syntax define-regiment-parameter
  (syntax-rules () 
    [(_ name args ...)
     (define name 
       (begin (regiment-parameters (cons (quote name) (regiment-parameters)))
	      (make-parameter args ...)))]))


;; This version leaves the file open:
;; Shouldn't exist at this time:
#;(define Regiment-Log-Port (open-output-file Regiment-Log-File))
#;(define-syntax DEBUGPRINT 
  (syntax-rules ()
    [(_ expr ...)
     (parameterize ((current-output-port Regiment-Log-Port))
		   (begin expr ... (flush-output-port)))]))
	   
;==============================================================================;

;; This is not strictly R5RS but it should work in both chez and plt,
;; as long as plt has "compat.ss" loaded.  
;; (For example, it requires flush-output-port)

;(define region-primitives)
;(define anchor-primitives)

(define id (lambda (x) x))

'(define regiment-basic-primitives 
  '(cons car cdr 
	 + - * / 
	 < > <= >= = eq? equal?
	 null? pair? number? 
	 not

	 locdiff
	 ))

'(define regiment-distributed-primitives 
  '(rmap rfold smap time-of
	 circle circle-at anchor anchor-at anchor-where k-neighborhood time
	 cluster sparsify border planarize treeize filter union intersect
	 until when swhen-any rwhen-any when-percentage 
	 sense neighbors ))


;; [2004.07.28] Introducing 'Area'.  Note that a Region is also an Area.
;; Ok, redoing those with type information:
;; The types I'm using right now are:
;;   Anchor, Area, Region, Signal, Event, Node, Location, Reading
;;   Function, Number, Integer, Float, Bool, Object
;;   List


;; Since I'm going to go statically typed eventually, Object is just
;; my way of signifying "for all alpha" right now.

;; And the old types from the Snet compiler were:
;;   Bool Char Float64 Int32 List Object
;;   Number Pair Port String Symbol Vector Void


;; Then some types that are used only in the local language are:
;;   Token NodeID

;; These are really the *local* primitives:
(define regiment-basic-primitives 
  '(
  
    ; value primitives
    (cons (Object List) List) 
    (car (List) Object)
    (cdr (List) List)
    (+ (Number Number) Number) 
    (- (Number Number) Number) 
    (* (Number Number) Number) 
    (/ (Number Number) Number) 

    (locdiff (Location Location) Float)

    (not (Bool) Bool)

    ; predicates
    (=  (Number Number) Bool)
    (<  (Number Number) Bool)
    (>  (Number Number) Bool)
    (<=  (Number Number) Bool)
    (>=  (Number Number) Bool)
;    (eq? (Object Object) Bool)
    (equal? (Object Object) Bool)
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
    (dist (Token) Integer)
    
    (check-tok (Token) Bool)
    ;; Gotta have a way to remove tokens also!
    ;; Not to mention expiration dates!!

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

(define regiment-distributed-primitives 
  '(
    
    (rmap           (Function Area) Area)

    (rfold          (Function Object Area) Signal)
    (smap           (Function Signal) Signal)

    (anchor-at      (Number Number) Anchor)
    (anchor-where   (Function) Anchor)

    (circle         (Anchor Dist)   Region)
    (circle-at      (Number Number Dist) Region)
    (k-neighborhood (Anchor Number) Region)
    ;; Shorthand: 
    (khood          (Anchor Number) Region)
    (khood-at       (Number Number Number) Region)

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
  
;;; 2004.03.31 - I don't know what the system's going to be called so
;;; I'm using the placeholder "blanko" which I will replace later. 
;;; OK, replacing with "regiment"
(define regiment-primitives
  (append regiment-basic-primitives
	  regiment-distributed-primitives
	  regiment-constants))

;; [2004.06.24] This is for the regiment primitives:
(define get-primitive-entry
  (lambda (prim)
    (or (assq prim regiment-primitives)
        (error 'get-primitive-entry
               "no entry for this primitive: ~a" prim))))

#;(define (get-primitive-arity prim)
  (let* ([entry (get-primitive-entry prim)]
	 [args (cadr entry)])
    (cond
     [(eq? 'Constant args) #f]
     [else (length args)])))

(define (regiment-primitive? x)
  (if (assq x regiment-primitives) #t #f))

(define (regiment-constant? x)
  (if (assq x regiment-constants) #t #f))

(define (basic-primitive? x) 
  (if (assq x regiment-basic-primitives) #t #f))
(define (distributed-primitive? x) 
  (if (assq x regiment-distributed-primitives) #t #f))

(define (token-machine-primitive? x)
  (if (assq x token-machine-primitives) #t #f))

;; [2004.06.09]  Many of these are actually language forms.  I gotta
;; get this sorted out eventually.
;; TODO: add some kind of type info.
;; [2004.10.22]  For now everything that handles tokens is a syntax not a prim.
(define token-machine-primitives
  '( ;(elect-leader) (flood) ;; These are actually macros, but what the heck
;     (return)
;     (emit)
;     (relay)
;     (call)
;     (timed-call)
;     (activate)
     (dist) 
     (light-up) 
;     (sense)
     (my-id)
     (loc )
     (void () Object)

     (this () Object)
     (local-sense () Number)
     (soc-return (Number) Object)
     (soc-finished () Object)
     
     ;; For simulator only:
     (draw-mark)
     (rgb)

     (dbg (String) Void)

    ;; This is a syntax:
    ;(leds ...
     ))

;;============================================================
;; DEALING WITH TOKEN NAMES.  
;; Sloppy interface right now.  
;; Used by the deglobalize pass.

;; [2004.06.13] Tokens will be more complex later.
(define (token? t) (symbol? t))
;; Allocate a token name, possibly with a seed name.
(define new-token-name
  (lambda args
    (if (null? args)
	(unique-name 'token)
	(unique-name (car args)))))

(define symbol-append
  (lambda args
    (string->symbol (apply string-append (map symbol->string args)))))

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
;;============================================================
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
     

;; I thought the primitive equal? did this by default?  This is just a
;; version that accepts any number of arguments.  Just a throw-away helper function.
(define (myequal? . args)
  (if (null? args) #t
      (let ((first (car args)))
	(let myeqloop ((args (cdr args)))
	  (cond
	   [(null? args)  #t]
	   [(equal? first (car args)) (myeqloop (cdr args))]
	   [else #f])))))

;;[2004.06.13] Making this not allow an error to match against unspecified!
(define (lenient-compare? o1 o2)
  (or (eq? o1 o2)
      ;; Strings are not deep structures according to eq-deep,
      ;; So we compare them with equal?
      (and (string? o1) (equal? o1 o2))
      (and (eq? o1 'unspecified) (not (eq? o2 'error)))
      (and (eq? o2 'unspecified) (not (eq? o1 'error)))))

;; This provides a weird sort of interface to a deep equal.  It walks
;; down the tree, applying the input comparator at every intermediate
;; node, only proceeding downward on negative comparisons.
;; [2004.07.21] - Fixed it's behaviour against dotted pairs.
(define eq-deep 
  (lambda (eq)
    (lambda (obj1 obj2)
      (let loop ((o1 obj1) (o2 obj2))
	(cond
	 [(eq o1 o2) #t]
	 [(and (list? o1) (list? o2))
	  (if (= (length o1) (length o2))
	      (andmap loop o1 o2)
	      #f)]
	 [(and (pair? o1) (pair? o2)) ;; Kinda silly to have both these.
	  (and (loop (car o1) (car o2)) ;; the above should save stack space though..
	       (loop (cdr o1) (cdr o2)))]
	 [(and (vector? o1) (vector? 02))
	  (andmap loop (vector->list o1) (vector->list o2))]
	 [else #f])))))

(define tester-eq? (eq-deep lenient-compare?))
(define tester-equal? (eq-deep lenient-compare?))


;; [2004.04.21] I've started using the (ad-hoc) convention that every
;; file should define "these-tests" and "test-this" for unit testing.
;; This is inspired by the drscheme philosophy of every file being an
;; executable unit...  But it *was* unbearable to duplicate this
;; little tester code across every file 
;; 
;; [2004.05.24] Replacing the default tester with a better one.
;; [2004.06.03] Adding optional preprocessor function
;; [2004.07.21] Added a 'quiet flag.  
;; [2005.02.06] Made the quiet flag also suppress warnings.
;; Forms:
;;  (default-unit-tester message these-tests)
;;  (default-unit-tester message these-tests equalfun)
;;  (default-unit-tester message these-tests equalfun preprocessor)
(define default-unit-tester
  (lambda (message these-tests . extras)
    (let (;; If the second argument is a procedure, use it as the equal.
	  [teq? (if (or (null? extras) (not (procedure? (car extras))))
		    tester-equal?		    
		    (eq-deep (car extras)))]
	  [preprocessor 
	   (if (and (> (length extras) 1)
		    (procedure? (cadr extras)))
	       (cadr extras)
	       (lambda (x) x))])

    (lambda args 
    (call/cc
     (lambda (return)
       (let ([entries
	      ;; This canonicalizes them so that they're all three-long:
	      (map 
	       (lambda (entry)
		 (cond
		  [(= 3 (length entry))  entry]
		  [(= 2 (length entry))
		   ;; Pads out with #f for entry name:
		   (cons #f entry)]
		  [else (error 'default-unit-tester 
			       " This is a bad test-case entry!: ~s~n" entry)]))
	       these-tests)])
	 (let (;; Flag to suppress test output.  This had better be passed
	       ;; *after* any comparison or preprocessor arguments.
	       [quiet (memq 'quiet args)]
	       ;; Flag to print test descriptions as well as code/output.
	       [verbose (memq 'verbose args)]
	       [descriptions (map car entries)]
	       [tests (map cadr entries)]
	       [intended (map caddr entries)]
	       [success #t])

	  (if verbose 
	      (begin (printf ";; Testing module: ~s~n" message)
		     (if quiet (printf ";; (with test output suppressed)~n"))
		     ))
	  (flush-output-port)
	  (for-each 
	   (lambda (num expr descr intended)
	       (flush-output-port)
	       ;; This prints a name, or description for the test:
	       (if (and verbose descr) (printf "   ~s~n" descr))
	       (display-constrained `(,num 10) "  " `(,expr 40)
				    " -> ")
	       (if (procedure? intended)
		   (display-constrained "Satisfy oracle? " 
					`(,intended 30) ": ")
		   (display-constrained `(,intended 20) ": "))
	       
	       (flush-output-port)
	       (let ([result 
		      (call/cc 
		       (lambda (escape-eval)
			 (with-error-handlers (lambda args 
						(printf "default-unit-tester, got ERROR: ~n  ~s~n"
							args)
						;(if (car args) (printf "~s: " (car args)))
						;(printf "~a~n" (apply format (cdr args)))
						)
					      (lambda () (escape-eval 'error))
					      (lambda () 
						(if quiet						    
						    (let ([trash (open-output-string)])
						      (fluid-let ([warning (lambda args (void))])
						      (parameterize ([current-output-port trash])
								    (eval (preprocessor expr)))))
						    (eval (preprocessor expr)))
						))))])
;	       (newline)
	       (if (or (and (procedure? intended) ;; This means its an oracle
			    (intended result))
		       (teq? intended result)) ;; Otherwise its an expected answer
		   (begin
;		     (if (procedure? intended)
;			 (printf "~s, " result))
		     (printf "PASS~n"))

		   (begin (set! success #f)
			  (newline)
			  (if (procedure? intended)
			      (printf "FAIL: Expected result to satisfy procedure: ~s~n" intended)
			      (begin 
				(printf "FAIL: Expected: ~n")			  
				(pretty-print intended)))
			  (printf "~n      Received: ~n")
			  (write result)
;			  (display-constrained `(,intended 40) " got instead " `(,result 40))  
			  (printf "~n~nFor Test: ~n")
			  (pretty-print expr)
			  (newline)
			  (eval `(define failed-unit-test ',expr))
			  (printf "Violating test bound to global-variable \"failed-unit-test\"~n")
;; I decided to make this crash after all:
;			  (return (void))
			  (error 'default-unit-tester "failed test")
			  ))))
	     (iota (length tests))
	     tests descriptions intended)
	    ))))))))
;;; OLD VER:
'(define (default-unit-tester MESSAGE TESTS)
    (lambda args 
      (let ((verbose (memq 'verbose args)))
	
	(let ((tests (map car TESTS))
	      (intended (map cadr TESTS)))
	  (let ((results (map eval tests)))
	    (if verbose 
		(begin
		  (display MESSAGE)
		  (newline)
		  (newline) (display "Here are intended results:") (newline)
		  (write intended) (newline) (newline)
		  (newline) (display "Here are actual results:") (newline)
		  (write results) (newline) (newline)))
	    (andmap tester-eq? intended results)
	    )))))
			

#;(define marshal
(lambda (x)
  (let ((str (format "~s" x)))
    (let ((in (open-input-string str)))
      (read in)))))
#;(define (marshaltest x) (eq? x (marshal x)))

#;(define load:dump-expansion
(lambda (file dumpfile)
  (parameterize ([current-output-port (open-output-file dumpfile 'replace)]
                 [print-graph #t]
                 [current-eval
                   (case-lambda
                     [(exp)
                      (unless (and (pair? exp) (equal? "noexpand" (car exp)))
                        (write (expand exp)) (newline)(newline))
                      (eval exp)]
                     [(exp envspec)
                      (error
                        'load:dump-expansion
                        "Proxy eval procedure not prepared~s~a"
                        " to receive env-spec:" envspec)])])
    (load file)
    (close-port (current-output-port)))))

;===============================================================================
;;; helpers.ss
;;; Kent Dybvig
;;; January 4, 2001

;;; This file contains some useful helpers, including some generally
;;; useful for everyday Scheme programming and some specific to the
;;; p423 compilers.

;;; 03/08/2002 clc added new object-related keywords, attributes,
;;;                and primitives

;;; 04/19/2001 rkd changed register-mapping to avoid use of %o6
;;; 04/15/2001 rkd added operand-constraints nad register-mapping
;;;                to the machine definition, and added new helper
;;;                with-output-to-string.
;;; 04/03/2001 rkd moved ap into the caller-save-registers list where
;;;                it should have been all along
;;; 03/25/2001 rkd added pick, rem, edge-maker, home-of, neighbors,
;;;                neighbor-homes, extended-neighbor-homes; added
;;;                align-shift to machine definition; added ap to
;;;                machine-definition list of all-registers; added
;;;                sll to list of uil primitives
;;; 03/04/2001 rkd added generalized-member?, generalized-union?, and
;;;                new-frame-var?
;;; 02/26/2001 rkd added caller-save-registers and all-registers to the
;;;                machine definition.  knocked number of parameter
;;;                registers down to two for now.  added frame-var,
;;;                new-frame-var, frame-var?, frame-var->index, and
;;;                register?  fixed a bug in code-name caused by the
;;;                change to extract-suffix.
;;; 02/25/2001 rkd added parameter-registers and return-value-register
;;;                to the machine definition.  modified extract-suffix
;;;                to return only the numeric part of the suffix.
;;;                eliminated fref from uil-primitives (for now).
;;; 02/11/2001 rkd added definitions of uil-primitives, uil-primitive?,
;;;                uil-effect-primitive?, uil-predicate-primitive?, and
;;;                uil-value-primitive?
;;; 01/31/2001 rkd added definitions of logand, sll, sra, hybrid->datum,
;;;                and ptr->datum
;;; 01/28/2001 rkd added definitions of internal-scheme-primitives,
;;;                extended-scheme-primitive?, value-primitive?,
;;;                predicate-primitive?, and effect-primitive?
;;; 01/21/2001 rkd changed implementation of code-name
;;; 01/14/2001 rkd changed datum? to permit improper lists.
;;; 01/14/2001 rkd added immediate?
;;; 2002.05.22 rrn added get-formals, cast-formals, and cast-args

;;; the subset of Scheme keywords we support
;;; [2004.06.28] RRN: Removed 'let'
(define base-keyword?
  (lambda (x)
    (and (memq x '(quote set! if begin letrec lambda)) #t)))

;;; CC
;;; new keywords added for object system
(define obj-keyword?
  (lambda (x)
    (and (memq x '(define-class let-class open-package open-instance
                    new program)) #t)))
(define keyword?
  (lambda (x)
    (or (base-keyword? x) (obj-keyword? x))))

;; These are the symbols I use for different types right now:
;;   Bool Char Float64 Int32 List Object
;;   Number Pair Port String Symbol Vector Void

;;; In the below primitive table, you will also see 'Tag, which indicatess
;;; that the specified position simply holds information for the compiler,
;;; these tags are always symbols, so the recursion on operands spills over
;;; them harmlessly.  It would be nice if the primitive application case in
;;; each pass would abstain from recurring on Tag operands, but that would
;;; require bloating the code unnecessarily.  The tags at this point have
;;; the limited purpose of storing class names and such; it should not be
;;; a problem to keep track of.

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
        (string? x)  ;; This is semantically straying;
        ;;              these ARE not necessarily constant;
        ;;              they can be modified by string-set!
        )))

#;(define constant->type
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


;; Things that need boxing (sigh):
;; TODO: Fix this up when my language actually becomes a bit more concrete:
;; I'm not even sure what meaning this has.  These are simple constants...
(define (immediate? x)
  (or (number? x)
      (symbol? x)
      (char? x)
      (null? x) ;; RRN added. [2004.04.28]
      ))

#;(define immediate?
  (lambda (x)
    (or ;(null? x)
      (char? x)
      (fx-integer? x) ;; Not bignums
      (flonum? x)
      ;(boolean? x)
      )))

#;(define boxed-type?
  (lambda (type)
    (case type
      [(Char Int32 Float64) #t]
      [(Bool List Object Number Pair Port String Symbol Vector Void)
       #f]
      [(Tag) (error 'boxed-type?
                    "it is not reasonable to ask whether tags are boxed: ~a"
                    rand-type)]
      [else (error 'boxed-type?
                   "unknown type name: ~s" type)])))

;;; structured data
(define datum?
  (lambda (x)
    (or (constant? x)
        (null? x)
        (symbol? x)
        ;(string? x)
        (if (pair? x)
            (and (datum? (car x)) (datum? (cdr x)))
            (and (vector? x) (andmap datum? (vector->list x)))))))

;;; These are the basic scheme primitives:
;;; Entries are of the format:
;;   (<prim> <arity> <context> <rand-type(s)> <return-type>)
;;  <rand-type(s)> follows the same shape as the corresponding formals:
;;     (Int32 Int32)       -- Two fixint arguments
;;      Int32              -- Any number of fixint arguments
;;     (Char Char . Int32) -- Two characters, followed by any # of fixints
;;; For the latter two cases, <arity> will be the symbol "variable"

(define base-scheme-primitives
  '(
     ;; Incompletely implemented:
     (console-output-port 0 value () Port)
     (console-input-port 0 value () Port)
     (make-vector 1 value (Int32) Vector) ;; needs case lambda
     (ash 2 value (Number Number) Number) ;; needs right shifting
     
     ; not is a special case
     (not 1 not)
     
     ; predicates
     (= variable test (Number . Number) Bool)
     (< variable test (Number . Number) Bool)
     (fx= variable test (Int32 . Int32) Bool)
     (fx< variable test (Int32 . Int32) Bool)
     (fx<= variable test (Int32 . Int32) Bool)
     (fx> variable test (Int32 . Int32) Bool)
     (fx>= variable test (Int32 . Int32) Bool)
     (fl= variable test (Float64 . Float64) Bool)
     (fl< variable test (Float64 . Float64) Bool)
     (fl<= variable test (Float64 . Float64) Bool)
     (fl> variable test (Float64 . Float64) Bool)
     (fl>= variable test (Float64 . Float64) Bool)
     (char=? 2 test (Char Char) Bool)
     (eq? 2 test (Object Object) Bool)
     (boolean? 1 test (Object) Bool)
     (null? 1 test (Object) Bool)
     (pair? 1 test (Object) Bool)
     (procedure? 1 test (Object) Bool)
     (vector? 1 test (Object) Bool)
     (zero? 1 test (Int32) Bool)
     (char? 1 test (Object) Bool)
     (port? 1 test (Object) Bool)
     (string? 1 test (Object) Bool)
     (fixnum? 1 test (Object) Bool)
     (flonum? 1 test (Object) Bool)
     (bignum? 1 test (Object) Bool)
     (ratnum? 1 test (Object) Bool)
     
     ; value-producing
     (* variable value Number Number)
     (fx+ variable value Int32 Int32)
     (fx- variable value (Int32 . Int32) Int32)
     (fx* variable value Int32 Int32)
     (fx/ variable value (Int32 . Int32) Int32)
     (fl+ variable value Float64 Float64)
     (fl- variable value (Float64 . Float64) Float64)
     (fl* variable value Float64 Float64)
     (fl/ variable value (Float64 . Float64) Float64)
     (+ variable value Number Number)
     (- variable value (Number . Number) Number)
     (add1 1 value (Int32) Int32)
     (sub1 1 value (Int32) Int32)
     (car 1 value (Pair) Object)
     (cdr 1 value (Pair) Object)
     (cons 2 value (Object Object) Pair)
     (vector-length 1 value (Vector) Int32)
     (vector-ref 2 value (Vector Int32) Object)
     (list-ref 2 value (List Int32) Object)
     (list-tail 2 value (List Int32) Object)
     (string-ref 2 value (String Int32) Char)
     (string-append variable value String String)
     (string-length 1 value (String) Int32)
     (string->list 1 value (String) List)
     (list->string 1 value (List) String)
     (void 0 value () Void)
     (eval 1 value (Object) Object)
     (apply variable value (Object Object . Object) Object)
     (current-directory 0 value () Object)
     (oblist 0 value () List)
     (real-time 0 value () Int32)
     (integer-length 1 value (Number) Number)
     (char->integer 1 value (Char) Int32)
     (integer->char 1 value (Int32) Char)
     (fixnum->flonum 1 value (Int32) Float64)
     (flonum->fixnum 1 value (Float64) Int32)
     ;(symbol->string 1 value (Symbol) String)
     ;(string->symbol 1 value (String) Symbol)
     
     ; side-effecting
     (exit 0 effect () Void)
     (load 1 effect (Object) Void)
     (display 1 effect (Object) Void)
     (newline 0 effect () Void)
     (set-car! 2 effect (Pair Object) Void)
     (set-cdr! 2 effect (Pair Object) Void)
     (vector-set! 3 effect (Vector Int32 Object) Void)
     (string-set! 3 effect (String Int32 Char) Void)
     ))

;; Internal primitives are ones the compiler introduces in the
;; compilation process.  These system-primitives are there present
;; from the start, but should not be used by users.
;; Many of these do not have closures associated with them--inline only.
(define system-scheme-primitives
  '( (\#system-ref 1 value (Symbol) Object)
     (\#system-set! 2 effect (Symbol Object) Void)
     (\#real-error 2 effect (Object String) Void)
     
     (\#bignum->flonum 1 value (Number) Float64)
     (\#bignum-digitcount 1 value (Number) Int32)
     (\#bignum-length 1 value (Number) Int32)
     (\#bignum-collapse 1 value (Number) Number)
     (\#bignum-printbanks 1 effect (Number) Void)
     (\#set-closure-name! 2 effect (Object String) Void)
     
     ;; This is temporary, just until we get operational ports:
     (\#write-to-string 1 value (Object) String)
     
     (\#flonum->integer 1 value (Float64) Number)
     (\#flexpt 2 value (Float64 Float64) Float64)
     (\#flsqrt 1 value (Float64) Float64)
     (\#flfloor 1 value (Float64) Float64)
     (\#flceiling 1 value (Float64) Float64)
     (\#flround 1 value (Float64) Float64)
     ))

;;; These are scheme primitives that we support, but which get
;;; reduced into base scheme primitives.
(define derived-scheme-primitives
  '(
     ;; These two are treated oddly, they are in here, but they
     ;; also have library definitions.  This is redundant but efficient.
     (vector variable value Object Object)
     (list variable value Object Object)
     (<= variable test (Number . Number) Bool)
     (> variable test (Number . Number) Bool)
     (>= variable test (Number . Number) Bool)))

;; These primitives are defined in terms of other primitives, and are
;; accessible only through closures (not in any "inline" form).
;; All of these parameter and return types need to be reference types:
;; (A scheme closure can only return an object presently.)
(define library-scheme-primitives
  '(
     (floor 1 value (Number) Number)
     (ceiling 1 value (Number) Number)
     (round 1 value (Number) Number)
     ;;(ash 1 test (Number) Object)
     (gcd variable value Number Number)
     ;(lcm variable value Number Number) -- this needs apply
     (max variable value (Number . Number) Number)
     (min variable value (Number . Number) Number)
     (remainder 2 value (Number Number) Number)
     (quotient 2 value (Number Number) Number)
     (|#quotient-remainder| 2 value (Number Number) Number)
     (exact->inexact 1 value (Number) Number)
     (inexact->exact 1 value (Number) Number)
     (expt 2 value (Number Number) Number)
     (sqrt 1 value (Number) Number)
     
     (map variable value (Object Object . Object) Object)
     (append variable value List List)
     (reverse 1 value (List) List)
     (memq 2 value (Object List) Object)
     (memv 2 value (Object List) Object)
     (member 2 value (Object List) Object)
     (assq 2 value (Object List) Pair)
     (assv 2 value (Object List) Pair)
     (assoc 2 value (Object List) Pair)
     (length 1 value (List) Number)
     (format variable value (String . Object) String)
     (caar 1 value (Pair) Object)
     (cadr 1 value (Pair) Object)
     (cdar 1 value (Pair) Object)
     (cddr 1 value (Pair) Object)
     (caaar 1 value (Pair) Object)
     (caadr 1 value (Pair) Object)
     (cadar 1 value (Pair) Object)
     (caddr 1 value (Pair) Object)
     (cdaar 1 value (Pair) Object)
     (cdadr 1 value (Pair) Object)
     (cddar 1 value (Pair) Object)
     (cdddr 1 value (Pair) Object)
     (caaaar 1 value (Pair) Object)
     (caaadr 1 value (Pair) Object)
     (caadar 1 value (Pair) Object)
     (caaddr 1 value (Pair) Object)
     (cadaar 1 value (Pair) Object)
     (cadadr 1 value (Pair) Object)
     (caddar 1 value (Pair) Object)
     (cadddr 1 value (Pair) Object)
     (cdaaar 1 value (Pair) Object)
     (cdaadr 1 value (Pair) Object)
     (cdadar 1 value (Pair) Object)
     (cdaddr 1 value (Pair) Object)
     (cddaar 1 value (Pair) Object)
     (cddadr 1 value (Pair) Object)
     (cddadr 1 value (Pair) Object)
     (cddddr 1 value (Pair) Object)
     
     (integer? 1 test (Object) Bool)
     (equal? 2 value (Object Object) Bool)
     (eqv? 2 value (Object Object) Bool)
     (number? 1 test (Object) Bool)
     (exact? 1 test (Object) Bool)
     (inexact? 1 test (Object) Bool)
     (positive? 1 test (Number) Bool)
     (negative? 1 test (Number) Bool)
     (even? 1 test (Number) Bool)
     (odd? 1 test (Number) Bool)
     (list? 1 test (Object) Bool)
     (string=? 2 test (String String) Bool)
     
     (error variable effect (object String . Object) Void)
     ))

(define continuation-scheme-primitives
  '(
     (call/cc 1 value (Object) Object)
     ;(letk x in e) (setk v e) (nullk 0)
     ;(newp 0) (pushpk 2) (composek 2)
     ;(kabove 2)
     ;(kbelow 2)
     ))

;;; the subset of Scheme primitives we support
(define scheme-primitives
  `(
     ,@base-scheme-primitives
     ,@system-scheme-primitives
     ,@derived-scheme-primitives
     ,@continuation-scheme-primitives
     ,@library-scheme-primitives
     ))

;;; new primitives added during compilation
(define internal-scheme-primitives
  '(
     ; value-producing
     (make-bignum 2 value (Bool Vector) Object)
     (closure-code 1 value (Object) Object)
     (closure-freevar 1 value (Tag) Object)
     
     ;(closure-code 1 value)
     ;(closure-ref 2 value)
     (make-closure 1 value (Tag) Object)
     
     ; side-effecting
     ;(closure-set! 3 effect)
     (closure-set! 3 effect (Tag Tag Object Object) Void)
     ))
    
(define scheme-primitive?
  (lambda (x)
    (assq x scheme-primitives)))

(define library-scheme-primitive?
  (lambda (x)
    (assq x library-scheme-primitives)))

(define internal-scheme-primitive?
  (lambda (x)
    (assq x internal-scheme-primitives)))

(define extended-scheme-primitive?
  (lambda (x)
    (and (or (assq x scheme-primitives)
             (assq x internal-scheme-primitives))
         #t)))

;; If the numargs is variable, this function returns a pair
;; such as (a . b) indicating the range of possible argument counts
;; (+inf.0 is a very likely candidate for b).
#;(define (get-primitive-numargs prim)
  (let ([entrynum (list-ref (get-primitive-entry prim) 1)])
    (if (eq? 'variable entrynum)
        (match (get-primitive-rand-types prim)
          [,v (guard (symbol? v)) '(0 . +inf.0)]
          [(,v ...) (cons (length v) (length v))]
          [(,v ... . ,l) (cons (length v) +inf.0)])
        entrynum)))

#;(define (get-primitive-context prim)
  (list-ref (get-primitive-entry prim) 2))
#;(define get-primitive-rand-types
  (case-lambda
    [(prim) (list-ref (get-primitive-entry prim) 3)]
    [(prim len)
     (match (list-ref (get-primitive-entry prim) 3)
       [,t (guard (symbol? t)) (make-list len t)]
       [(,t* ...)
        (if (= (length t*) len) t*
            (error 'get-primitive-rand-types
                   "primitive ~s is fixed arity at length ~s~a~s"
                   prim (length t*) " you can't warp it to length " len))]
       [(,t* ... . ,spill)
        (let ([minlen (length t*)])
          (if (>= len minlen)
              (append t* (make-list (- len minlen) spill))
              (error 'get-primitive-rand-types
                     "primitive ~s requires at least ~s operand(s),~a~s~a"
                     prim minlen " the request to type it for "
                     len " is incorrect.")))])]))

;; Gotta remember to update this if I change the format of prim entries..
(define (get-primitive-return-type prim)
  (last (get-primitive-entry prim)))


;;;============================================================================
;;;
;;; CC
;;;
;;; The following are new "primitive?"-esque things relating to the object
;;;  system.
;;;

(define access-attributes
  '(public protected private))
(define access-attribute?
  (lambda (x)
    (memv x access-attributes)))

(define storage-attributes
  '(static instance))
(define storage-attribute?
  (lambda (x)
    (memv x storage-attributes)))

(define attributes
  (append
    access-attributes
    storage-attributes))

(define attribute?
  (lambda (x)
    (memv x attributes)))

;;;
;;;============================================================================


;;; presently defined for either Scheme or internal Scheme primitives
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


;===============================================================================

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

(define cast-args
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

;; [2004.10.04]  Finds the element in a list maximizing a metric:
(define (find-maximizing f ls)
  (if (null? ls) (error 'find-maximizing "cannot take null list.")
      
      (let loop ([ls (cdr ls)] 
		 [maxelem (car ls)]
		 [maxval (f (car ls))])
	(if (null? ls) maxelem
	    (let ([thisval (f (car ls))])
	      (if (> thisval maxval)
		  (loop (cdr ls) (car ls) thisval)
		  (loop (cdr ls) maxelem maxval)))))))	    

(define list-head
  (lambda (lst n)
    (cond
      [(zero? n) '()]
      [(null? lst) (error 'list-head "list is not long enough: ~s ~s"
                          lst n)]
      [else (cons (car lst) (list-head (cdr lst) (sub1 n)))])))

(define filter
  (lambda (pred lst)
    (cond
      [(null? lst) '()]
      [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
      [else (filter pred (cdr lst))])))

;; This is mainly used by the system in language-mechanism.  
;;   Clumps a list into sublists (clumps) all of whose members satisfy
;; the predicate function with some other member of that clump.
;; Basically it gives you "connected" components.  Assumes the
;; predicate function is commutative.
(define (clump f ls)
  (let outer ([clumps '()] [ls ls])
    (if (null? ls)
	clumps
	(let ([x (car ls)])
	  (outer 
	   (let inner ([clumps clumps])
	     (cond 
	      [(null? clumps) `((,x))]
	      [(null? (filter (lambda (y) (f x y)) (car clumps)))
	       (cons (car clumps) (inner (cdr clumps)))]
	      [else ;; We've actually matched this clump.		   		    
	       (cons (cons x (car clumps)) (cdr clumps))]))
	   (cdr ls))))))

(define (average ls)
  (let loop ((sum 0) (count 0) (ls ls))
    (if (null? ls) (/ sum count)
	(loop (+ (car ls) sum) (+ 1 count) (cdr ls)))))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

;; USES equal? !!
(define list-remove-after
  (lambda (x ls)
    (let loop ([ls ls] [acc '()])
      (cond
       [(null? ls) (reverse acc)]
       [(equal? x (car ls)) (reverse (cons (car ls) acc))]
       [else (loop (cdr ls) (cons (car ls) acc))]))))

(define list-remove-before
  (lambda (x ls)
    (let loop ([ls ls])
      (cond
       [(null? ls) '()]
       [(equal? x (car ls)) ls]
       [else (loop (cdr ls))]))))

(define list-remove-first
  (lambda (x lst)
    (cond
      [(null? lst) '()]
      [(equal? x (car lst)) (cdr lst)]
      [else (cons (car lst) (list-remove-first x (cdr lst)))])))


(define list-remove-all
  (lambda (x lst)
    (cond
      [(null? lst) '()]
      [(equal? x (car lst)) (list-remove-all x (cdr lst))]
      [else (cons (car lst) (list-remove-all x (cdr lst)))])))

#;(define timeeval
  (lambda (x)
    (let ([start (real-time)])
      (eval x)
      (- (real-time) start))))

#;(define cpueval
  (lambda (x)
    (let ([start (cpu-time)])
      (eval x)
      (- (cpu-time) start))))

(define snoc
  (lambda (a ls)
    (append ls (list a)) ))

(define rac
  (lambda (ls)
    (if (null? ls)
        (error 'rac "cannot take the rac of the empty-list")
        (let rac-loop ([cur (car ls)] [lst (cdr ls)])
          (if (null? lst)
              cur
              (rac-loop (car lst) (cdr lst)))))))
(define last rac)

(define rdc
  (lambda (ls)
    (if (null? ls)
        (error 'rdc "cannot take the rdc of the empty-list")
        (let rdc-loop ([lst ls])
          (if (null? (cdr lst))
              '()
              (cons (car lst) (rdc-loop (cdr lst))))))))

(define mapleft
  (lambda (f ls)
    (if (null? ls)
        '()
        (let ([rst (mapleft f (cdr ls))])
          (cons (f (car ls)) rst)))))

(define mapright
  (lambda (f ls)
    (if (null? ls)
        '()
        (cons (f (car ls))
              (mapright f (cdr ls))))))

(define list-set
  (lambda (origls origpos v)
    (let list-set-loop ([ls origls] [pos origpos])
      (cond
        [(null? ls)
         (error 'list-set
                "list ~a is not long enough to reference pos ~a"
                origls origpos)]
        [(zero? pos) (cons v (cdr ls))]
        [else (cons (car ls)
                    (list-set-loop (cdr ls) (sub1 pos)))]))))

;===============================================================================

;; Repeatedly execute an expression some number of times:
(define-syntax rep
  (syntax-rules ()
    [(_ reps exp ...)
     (let loop ([n reps])
       (if (< n 1)
           (void)
           (begin exp ...
                  (loop (sub1 n)))))]))

(define insert-between
  (lambda (x lst)
    (let loop ([lst lst])
      (cond
        [(null? lst) '()]
        [(null? (cdr lst)) lst]
        [else (cons (car lst)
                    (cons x (loop (cdr lst))))]))))

;; Removed mvlet! [2004.04.28]

;;; procedures for manipulating sets
(define set?
  (lambda (ls)
    (or (null? ls)
        (and (not (memq (car ls) (cdr ls)))
             (set? (cdr ls))))))

;; Inefficient for ordered types:
(define (subset? l1 l2)
  (and (andmap (lambda (a) (member a l2))
	       l1)
       #t))

(define set-equal?
  (lambda (lst1 lst2)
    (letrec ((loop (lambda (lst1 lst2)
                     (cond
                       [(and (null? lst1) (null? lst2)) #t]
                       [(or (null? lst1) (null? lst2)) #f]
                       [(member (car lst1) lst2) (loop (cdr lst1) (list-remove-all (car lst1) lst2))]
                       [else #f]))))
      (if (and (set? lst1) (set? lst2))
          (loop lst1 lst2)
          (error 'set-equal? "must take two sets, improper arguments: ~s ~s" lst1 lst2)))))

(define list->set
  (lambda (ls)
    (if (null? ls) '()
        (set-cons (car ls) (list->set (cdr ls))))))

(define set-cons
  (lambda (x set)
    (cond
      [(null? set) (list x)]
      [(eq? x (car set)) set]
      [else (cons (car set) (set-cons x (cdr set)))])))

(define union
  (case-lambda
    [(set1 set2)
     (if (not (list? set1)) (error 'union "not a list: ~s" set1))
     (if (not (list? set2)) (error 'union "not a list: ~s" set2))
     (let loop ([set1 set1])
       (cond
         [(null? set1) set2]
         [(memq (car set1) set2) (loop (cdr set1))]
         [else (cons (car set1) (loop (cdr set1)))]))]
    [() '()]
    [(set1 . sets)
     (let loop ([set1 set1] [sets sets])
       (if (null? sets)
           set1
           (loop (union set1 (car sets)) (cdr sets))))]))

(define intersection
  (case-lambda
    [(set1 set2)
     (let loop ([set1 set1])
       (cond
         [(null? set1) '()]
         [(memq (car set1) set2) (cons (car set1) (loop (cdr set1)))]
         [else (loop (cdr set1))]))]
    [(set1 . sets)
     (let loop ([set1 set1] [sets sets])
       (if (null? sets)
           set1
           (loop (intersection set1 (car sets)) (cdr sets))))]))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memq (car set1) set2) (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))

(define generalized-member?
  (lambda (pred?)
    (lambda (x ls)
      (let f ([ls ls])
        (and (not (null? ls))
             (or (pred? (car ls) x)
                 (f (cdr ls))))))))

(define generalized-union
  (lambda (pred?)
    (let ([member? (generalized-member? pred?)])
      (lambda args
        (let f ([args args])
          (if (null? args)
              '()
              (let ([set1 (car args)] [set2 (f (cdr args))])
                (let loop ([set1 set1])
                  (cond
                    [(null? set1) set2]
                    [(member? (car set1) set2) (loop (cdr set1))]
                    [else (cons (car set1) (loop (cdr set1)))])))))))))

;;; (iota n) => (0 1 ... n-1)
;;; (iota i n) => (i i+1 ... i+n-1)
(define iota
  (case-lambda
    [(n) (iota 0 n)]
    [(i n)
     (if (= n 0)
         '()
         (cons i (iota (+ i 1) (- n 1))))]))

;;; unique-name produces a unique name derived the input name by
;;; adding a unique suffix of the form .<digit>+.  creating a unique
;;; name from a unique name has the effect of replacing the old
;;; unique suffix with a new one.
;;;
;;; reset-name-count! resets the internal counter used to produce
;;; the unique suffix.
;;;
;;; code-name takes a unique name and replaces its suffix ".nnn"
;;; with "$nnn", e.g., f.3 => f$3.  It is used by convert-closure.
;;;
;;; extract-suffix returns the numeric portion of the unique suffix
;;; of a unique name or code-name, or #f if passed a non unique name.
;;(module (unique-name reset-name-count! extract-suffix
;;                     code-name label-name #;method-name)
        ;RRN [01.09.16] -- We need to phase out code-name...

;; [2004.06.28] I am replacing this with a version that uses
;; a hash-table to keep a counter per seed-name.
(begin
        (define unique-name-count 0)
        (define (unique-suffix ignored)
            (set! unique-name-count (+ unique-name-count 1))
            (number->string unique-name-count))
        (define reset-name-count! 
	  (lambda opt
	    (match opt
		   [() (set! unique-name-count 0)]
		   [(,n) 
		    (if (number? n)
			(set! unique-name-count n)
			(error 'reset-name-count "bad arg: ~a" n))])))

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
       '(define illegal-chars
            '(#\! #\@ #\# #\$ #\% #\^ #\& #\* #\. #\-))
       '(define strip-illegal
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
	      (let ((sym (if (string? sym) (string->symbol sym) sym)))
            (string->symbol
              (string-append
                (strip-illegal ;;RRN - THIS IS STUPID, CHANGE ME
                  (extract-root sym))
                "_" (unique-suffix sym)))))))
)

;; [2004.06.28]  NEW VERSION, counter per seed name:
;; Just overwriting definitions from above:
'(begin
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



;;; create a "flattened" begin from list of expressions
;;; e.g., (make-begin '(1 (begin 2) (begin 3 4) 5)) => (begin 1 2 3 4 5)
(define make-begin
  (lambda (expr*)
    (match (match `(begin ,@expr*)
             [(begin ,[expr*] ...) (apply append expr*)]
             [,expr (list expr)])
      [(,x) x]
      [(,x ,x* ...) `(begin ,x ,x* ...)])))
;;RRN [01.09.17] :
(define make-code
  (lambda (expr*)
    (match (match `(code ,@expr*)
             [(code ,[expr*] ...) (apply append expr*)]
             [,expr (list expr)])
      [(,x) x]
      [(,x ,x* ...) `(code ,x ,x* ...)])))

(define with-output-to-string
  (lambda (th)
    (parameterize ([current-output-port (open-output-string)])
      (th)
      (get-output-string (current-output-port)))))


;;; we can only handle exact integers in the fixnum range
(define fx-integer?
  (let ()
    ;;RRN: Unfortunately MSIL can't handle boxed immediates, so fixnums
    ;; are the full size of a word:
    (define fixnum-minimum (- (expt 2 31)))
    (define fixnum-maximum (- (expt 2 31) 1))
    (lambda (x)
      (and (integer? x)
           (exact? x)
           (<= fixnum-minimum x fixnum-maximum)))))

(define bg-integer?
  (lambda (x)
    (and (integer? x)
         (exact? x)
         (not (fx-integer? x)))))

(define disp
  (lambda args
    (let loop ((args args))
      (if (null? args)
          (begin (newline) (newline))
          (begin (display (car args))(display " ")
                 (loop (cdr args)))))))

(define (list-remove-last! ls)
  (if (null? ls)
      (error 'list-remove-last "cannot remove last of the null list!"))
  (let loop ((cell ls) (next (cdr ls)))
    (if (null? (cdr next))
	(set-cdr! cell '())
	(loop next (cdr next)))))

;[2004.07.21] - This one applies a given function (better be lenient)
; against every interemediate node in the tree.  Returns a list of
; *every* match.  Returns them in the order it hits them as it does a
; depth-first traversal.
;;   This is heavy-weight, expensive function, but darn useful!!
(define (deep-all-matches f struct)
  (letrec ([against 
	    (lambda (struct)
	      (if (f struct) 
		  (cons struct (down struct))
		  (down struct)))]
	   [down 
	    (lambda (struct)
	      (cond
	       [(vector? struct)
		(let vloop ([i 0])
		  (if (= i (vector-length struct)) 
		      '()
		      (append (against (vector-ref struct i))
			      (vloop (add1 i)))))]
	       [(pair? struct)
		(append (against (car struct))
			(against (cdr struct)))]
	       [else '()]))])
    (against struct)))

;; [2004.07.28] Occassionally useful.
(define (deep-filter pred struct)
  (let loop ([struct struct])
    (cond
     [(list? struct) (map loop (filter pred struct))]
     [(vector? struct)
      (list->vector (map loop (filter pred (vector->list struct))))]
     [else struct])))
	   
;[01.10.23] - I'm surprised this wasn't added a million years ago:
(define (deep-member? ob struct)
  (let outer ([struct struct])
    (or (eqv? ob struct)
	(and (vector? struct)
	     (let inner ([i 0])
	       (cond
		[(= i (vector-length struct)) #f]
		[(outer (vector-ref struct i)) #t]
		[else (inner (add1 i))])))
	(and (pair? struct)
	     (or (outer (car struct))
		 (outer (cdr struct)))))))

;; [2004.06.11] This one doesn't do vectors:
(define (deep-assq ob struct)
  (let outer ([struct struct])
    (if (pair? struct)
	(if (eq? ob (car struct))
	    struct
	    (or (outer (car struct))
		(outer (cdr struct))))
	#f)))

;; [2004.06.15] Copying this from generic utils file.
(define list-get-random
  (lambda (ls)
    (if (null? ls)
        (error 'list-get-random "cannot get random element from null list.")
        (list-ref ls (random (length ls))))))
;; This too:
(define randomize-list
  (lambda (ls)
    (let* ([vec (list->vector ls)]
	   [len (vector-length vec)])
      (let ([swap (lambda (i j)
		    (let ([temp (vector-ref vec i)])
		      (vector-set! vec i (vector-ref vec j))
		      (vector-set! vec j temp)))])
	(do ([i 0 (add1 i)]) ((= i len))
	  ;; Swap with a later position:
	  (swap i (+ i (random (- len i)))))
	(vector->list vec)))))


;;  [2004.06.11] Man also can't believe that I've never written this
;;  before.  This is dinky; a real version of this would do an
;;  alignment of the structures intelligently.  I don't know how
;;  actually hard of a problem that is.  (Graph matching is a bitch,
;;  no?)
(define (diff obj1 obj2)
  (let loop ([o1 obj1] 
	     [o2 obj2]
	     [index '()])
    (printf "~s: len ~s ~s~n" index 
	    (if (list? o1) (length o1) #f)
	    (if (list? o2) (length o2) #f))    
    (cond
       [(equal? o1 o2) (void)]
       [(and (list? o1) (list? o2))
	(cond 
	 [(not (= (length o1) (length o2)))
	  (printf "Lengths differ at list index ~s.  ~s elements vs. ~s elements.~n."
		  (reverse index) (length o1) (length o2))
	  (printf "List one:~n")
	  (pretty-print o1)
	  (printf "Vs. List two: ~n")
	  (pretty-print o2)]
	 [else (for-each (lambda (a b i) (loop a b (cons i index)))
			 o1 o2 (iota (length o1)))])]

       [(and (vector? o1) (vector? 02))
	(do ([i 0 (add1 i)])
	    ((= i (vector-length o1)))
	  (loop (vector-ref o1 i) 
		(vector-ref o2 i)
		(cons i (cons 'v index))))])))
       
;; GRAPHS
;; ======================================================================

;; SIMPLE, VERTICAL, and HORIZONTAL graphs.

;; graph-map and graph-get-connected work for "horizontal" graphs.  That is (a b c)
;; means two edges linking a to b and to c.  For vertical graphs 
;; (a b c) instead signifies two edges linknig a to b and b to c.
;; Simple graphs are just graphs where each edge is listed individualy 
;; ((a b) (b c)).


;; Should make this use a hash table.
(define graph-map
  (lambda (f graph)
    (let ([node-table (let ((nodes (map car graph)))
			(map list nodes (map f nodes)))]
	  [collect '()])
      (letrec ([add-obj 
		(lambda (node)
		  (let ((entry (assq node node-table)))
		    (if entry (cadr entry)
			(begin 
;			  (printf "Erk! Link to nonexistent node: ~s\n" node)
			  ;(for-each add-obj (node-neighbors node))
			  (let ((newobj (f node)))
			    (set! node-table 
				  (cons (list node newobj)
					node-table))
			    newobj)
			  ))))]
	       [do-entry
		(lambda (entry) 
		  (map add-obj entry))]
	       )
	(map do-entry graph)))))

(define gmap graph-map)

;; [2004.07.31] This returns the connected component containing the given object:
;; This needs to be tested!!
(define (graph-get-connected obj graph)
  (let loop ([todo (list obj)] [acc '()])
    (cond
     [(null? todo) acc]
     [(memq (car todo) acc) (loop (cdr todo) acc)]
     [else 
      (let* ([entry (assq (car todo) graph)]
	     [children (if entry (cdr entry) '())]
	     [viable (filter (lambda (obj) (not (memq obj acc))) 
			     children)])
	(loop (append viable (cdr todo))
	      (cons (car todo) acc)))])))


;; This takes a graph in "single edge" form (where each edge is a
;; 2 element list), and produces a strange representation.  In the
;; output representation an edge (a b c) does not mean { a->b, a->c } 
;; but instead means { a->b, b->c }.
;;   I use this representation because we get long "chains" of control flow.
;;------
;; Inefficient: I think this can be as bad as n^3
(define (graph:simple->vertical g)  
  (let outerloop ([oldgraph #f] [graph g])
    (define (get-heads x) (filter (lambda (e) (eq? x (car e))) graph))
    (define (get-tails x) (filter (lambda (e) (eq? x (rac e))) graph))
    
    (define (fit e1 e2)
	  (let ([h1 (car e1)] [t1 (rac e1)]
		[h2 (car e2)] [t2 (rac e2)])
;	    (disp "      fitting" h1 t1 " to " h2 t2)
	    (cond 
	     [(and (eq? t1 h2)
		   (= 1 (length (get-heads h2)))
		   (= 1 (length (get-tails t1))))
	      (append e1 (cdr e2))]
	     [(and (eq? t2 h1)
		   (= 1 (length (get-heads h1)))
		   (= 1 (length (get-tails t2))))
	      (append e2 (cdr e1))]
	     [else #f])))
;    (disp "outerloop" oldgraph graph)		     
    (if (equal? oldgraph graph)
	graph
	(outerloop graph
		  (let inner1 ((edges1 graph))
;		    (disp "  inner1" edges1)
		    ;; If we get to the end there were no changes to make:
		    (if (null? edges1) 
			graph
			(mvlet ([(new old1 old2)
				 (let ((first (car edges1)))
				   (let inner2 ((edges2 (cdr edges1)))
;				     (disp "    inner2" first ": " edges2)
				     (if (null? edges2)
					 (values #f #f #f)
					 (let ((f (fit first (car edges2))))
					   (if f 
					       (values f first (car edges2))
					       (inner2 (cdr edges2)))))))])
			       (if new 
				   ;; This is the new graph for the next round:
				   (cons new (remq old1 (remq old2 graph)))
                                   ;; Otherwise keep looking for a change to make:
				   (inner1 (cdr edges1))))))))))

(define (graph:vertical->simple g)
  (map (lambda (edge)
	 (if (= 2 (length edge)) edge	     
	     (map list (rdc edge) (cdr edge))))
       g))

;; ======================================================================





(define partition
  (lambda (lst f)
    (letrec ((loop
               (lambda (lst acc1 acc2)
                 (cond
                   [(null? lst) (list acc1 acc2)]
                   [(f (car lst)) (loop (cdr lst) (cons (car lst) acc1) acc2)]
                   [else (loop (cdr lst) acc1 (cons (car lst) acc2))]))))
      (loop lst '() '()))))

(define partition-equal
  (lambda (lst eq)
    (if (not (or (pair? lst) (null? lst)))
	(error 'partition-equal "input must be a list: ~s" lst))
    (let loop ((lst lst))
      (if (null? lst) '()
	  (let* ([first (car lst)]
		 [pr (partition (cdr lst) 
				(lambda (x) (eq x first)))]
		 [ingroup (car pr)]
		 [outgroup (cadr pr)])
	    (cons (cons first ingroup) 
		  (loop outgroup)))))))


;; Tells whether or not a graph is cyclic.  
;; Requires canonical form where each node has exactly one entry.
;; If the graph is acyclic, return #f.
;; Otherwise, return the list of nodes participating in cycles.
;; 
;; REQUIRES: 
(define (cyclic? g . compare)
  ;; Umm is this really bad form to perform this INSIDE the cyclic? function??
  ;; How efficient is require??  A linear search through a list of things loaded?
  ;; I'm considering defining "let-run-once"
  ;;(slib:require 'tsort)
  (let ((eq (if (null? compare) eq?
		(if (> (length compare) 1)
		    (error 'cyclic? "too many optional arguments: %s" compare)
		    (if (procedure? (car compare))
			(car compare)
			(error 'cyclic? 
			       "optional argument must contain a comparison procedure, received : %s"
			       (car compare)))))))
    (let ((flat (topological-sort g eq?)))
      (let ((cycles
	     (filter 
	      (lambda (x) x)
	      (map 
	       (lambda (entry)
		 (let ((lst (memq (car entry) flat)))
		   (if (not lst) (error 'cyclic? "uhh, definition broken")
		       (if (andmap (lambda (edge) (memq edge lst))
				   (cdr entry))
			   #f
			   (car entry)))))
	       g))))
	(if (null? cycles)
	    #f
	    cycles)))))


;; <TODO> <TOIMPLEMENT> Ryan, write a function that changes the direction of links:
;(define graph-flip...

;; This strings out a list of all the cons cells in a given list (not
;; traversing car's).
(define (unfold-list lst)
  (let loop ((lst lst))
    (if (null? lst) '()
	(cons lst (loop (cdr lst))))))

(define display-constrained
  (lambda args
    (for-each 
     (lambda (arg)
       (if (string? arg)
	   (display arg)
	   (let ([arg (car arg)]
		 [bound (cadr arg)]
		 [port (open-output-string)])
;	     (pretty-print arg port)
	     (write arg port)   (newline port)
	     (let ((str (get-output-string port)))
	       (if (> (string-length str) bound)
		   (begin (display (substring str 0 (max 0 (- bound 3))))
			  (display "..."))
		   ;; Gotta cut off the newline.
		   (display (substring str 0 (- (string-length str) 1))))))))
	      args)))

;;======================================================================
;; [2004.06.17] These functions deal with streams that are represented
;; as a list, promise, or improper list with a promise as its final
;; cdr-pointer.  That is:
;;  Stream ::= (item*)
;;           | (item* . promise)
;;           | promise

;; This version has a look-ahead of one, so the stream had better not
;; contain bottom!


(define (stream? s)
  ;; Is it a proper list?
  (or (list? s)
      ;; Or an improper list that's still being computed?
      (live-stream? s)))

;; A live stream is one not all of whom's values have been computed yet.
(define (live-stream? s)
  (or (promise? s)
      (and (pair? s) (stream? (cdr s)))))

(define stream-empty? 
  (lambda (s)
    (cond 
     [(null? s) #t]
     [(promise? s) (stream-empty? (force s))]
     [else #f])))

(define stream-cons cons)
(define stream-append append)

(define stream-car
  (lambda (s)
    (let scloop ((s s))
      (cond
       [(promise? s)
	;; We have no way of mutating the prior cell, so just return this:
	(stream-car (force s))]
       [(pair? s) (car s)]
       [(null? s) (error 'stream-car "Stream is null!")]
       [else (error 'stream-car "invalid stream: ~s" s)]))))

(define (stream-cdr s)
  (cond
   [(promise? s)      
    ;; Again, this one isn't structured as a pair, so we can't mutate and extend.
    (stream-cdr (force s))]
   [(null? s) (error 'stream-cdr "Stream is null!")]
   [(pair? s)
      (if (promise? (cdr s))
	  (begin (set-cdr! s (force (cdr s)))
		 ;; Might need to keep going, a promise may return a promise:
		 (stream-cdr s))
	  (cdr s))]
   [else (error 'stream-cdr "invalid stream: ~s" s)]))

;; Take N elements from a stream
(define stream-take 
  (lambda (n s)
    (let stloop ((n n) (s s))    
      (cond
       [(zero? n) '()]
       [(null? s)
	(error 'stream-take "Stream ran out of elements before the end!")]
       [else 
	(cons (stream-car s)
	      (stloop (sub1 n) (stream-cdr s)))]))))
  
;; Layer on those closures!
(define stream-map 
  (lambda (f s)
    (let loop ((s s))
      (delay (cons (f (stream-car s))
		   (loop (stream-cdr s)))))))
    
	  
;; A stream of non-negative integers:
(define counter-stream
  (let loop ((i 0))
    (delay (cons i (loop (add1 i))))))

;; A silly little stream of random number
(define (random-stream inp)
  (let loop ()
    (delay (cons (random inp) (loop)))))

;;==============================

;; [2004.06.18] This displays the changes in a piece of state only
;; when the changes accumulate to greater than a certain delta.
;; Basically it integrates the signal and pops off as display command
;; everytime it surpasses a certain threshold.  This is used to make a
;; reasonable readout for rapidly changing values.  Works only for
;; numeric values.
(define (periodic-display delta) ;obj accessor delta)
  (if (not (number? delta)) 
      (error 'periodic-display "must be called with a numeric delta: ~s" delta))
  (let ([last #f] [changes 0])
    (lambda (str newval)
      (if (not (number? newval))
	  (error 'periodic-display "each new value must be a number: ~s" newval))
      (if (not last) 
	  (begin (set! last newval) (printf str newval)))
      ;; Add the changes into the abs.
      (set! changes (+ changes (abs (- newval last))))
      (if (>= changes delta)
	  (begin (set! changes 0)
		 (printf str newval)))
      (set! last newval))))

 
;;======================================================================
;; And here are the unit tests for this file... Don't have many of these yet.

(define these-tests
  `(



    [(deep-all-matches null? '(1 (3) (4 . 5)))
     (() ())]
    [(deep-all-matches list? '(1 (3) (4 . 5)))
     ((1 (3) (4 . 5)) ((3) (4 . 5)) (3) () ((4 . 5)) ())]
    [(deep-all-matches (lambda (x) (and (number? x) (even? x))) '(1 (3) (4 . 5)))
     (4)]
    [(deep-all-matches (lambda (x) (and (number? x) (odd? x))) '(1 (3) (4 . 5)))
     (1 3 5)]
    [(deep-all-matches (lambda (x) (and (number? x) (odd? x))) '#(1 #(3) (4 . 5)))
     (1 3 5)]

    [(deep-assq 3 '(9 (4 (1 2 a ) 4)))        #f]
    [(deep-assq 3 '(9 (4 (1 2 3 9 a ) 4)))    (3 9 a)]
    [(deep-assq 3 '(9 (4 ((3 9 a ) 4))))      (3 9 a)]

    [(deep-member? 3 '(9 (4 ((3 9 a ) 4))))   #t]
    [(deep-member? 3 '(9 (4 ((9 a ) 4))))     #f]

    [(let ((s (open-output-string)))
       (parameterize ((current-output-port s))
		     (display-constrained "test  " '(abcdefghijklmnop 10) 
					  " again  " '(abcdefghijklmnop 16) 
					  '(abcdefghijklmnop 17)))
       (get-output-string s))
     "test  abcdefg... again  abcdefghijklm...abcdefghijklmnop"]

    [(stream-take 5 counter-stream)
     (0 1 2 3 4)]
    [(stream-take 3 `(1 2 . ,(delay '(3))))
     (1 2 3)]
    [(stream-take 5 `(1 2 . ,(delay '(3))))
     error]

    [(stream-cdr '()) error]
    [(stream-cdr (delay 1)) error]
    [(stream-cdr (delay '(1))) ()]
    [(stream-car (delay '(1))) 1]

    [(unfold-list '(a b c d))
     ((a . #0=(b . #1=(c . #2=(d)))) #0# #1# #2#)]

    [(graph-get-connected 'e '((a b c) (b c) (c) (d e f) (e g h)))   (h g e)]
    [(graph-get-connected 'h '((a b c) (b c) (c) (d e f) (e g h)))   (h)]
    [(graph-get-connected 'a '((a b c) (b c) (c) (d e f) (e g h)))   (c b a)]
    [(graph-get-connected 'a '((a b) (b a)))                         (b a)]
    [(graph-get-connected 'a '((a b) (b a c)))                       (c b a)]


    ))

(define test-this (default-unit-tester "heplers.ss: my messy utils file." these-tests))
(define testhelpers test-this)

'
;(call/cc (lambda (out)
	   (with-error-handlers (lambda args (disp "error disp:" args))
				(lambda args (disp "error escape:" args) 999)
				(lambda () (disp "handlers: " 
						 (error-display-handler)
						 (error-escape-handler) )
					(error 'foo "bar")
					3 ))



