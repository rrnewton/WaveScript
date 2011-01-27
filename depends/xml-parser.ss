; 			My Standard Scheme "Prelude"
; Version for Petite Chez Scheme 6.0a
; $Id: myenv-chez.scm,v 1.2 2004/07/08 21:53:32 oleg Exp $

(define-syntax include
  (syntax-rules ()
    ((include "myenv.scm") (begin #f))
    ((include file) (load file))))

(define pp pretty-print)
;(define gensym gentemp)


(define-syntax declare	; Gambit-specific compiler-decl
  (syntax-rules () ((declare . x) (begin #f))))

; A few convenient functions that are not Chez
(define (call-with-input-string str proc)
    (proc (open-input-string str)))
(define (call-with-output-string proc)
  (let ((port (open-output-string)))
    (proc port)
    (get-output-string port)))
(define (with-input-from-string str thunk)
  (parameterize ((current-input-port (open-input-string str))) (thunk)))
(define (with-output-to-string thunk)
  (let ((port (open-output-string)))
    (parameterize ((current-output-port port)) (thunk))
    (get-output-string port)))


; Frequently-occurring syntax-rule macros

; A symbol? predicate at the macro-expand time
;	symbol?? FORM KT KF
; FORM is an arbitrary form or datum
; expands in KT if FORM is a symbol (identifier), Otherwise, expands in KF

(define-syntax symbol??
  (syntax-rules ()
    ((symbol?? (x . y) kt kf) kf)	; It's a pair, not a symbol
    ((symbol?? #(x ...) kt kf) kf)	; It's a vector, not a symbol
    ((symbol?? maybe-symbol kt kf)
      (let-syntax
	((test
	   (syntax-rules ()
	     ((test maybe-symbol t f) t)
	     ((test x t f) f))))
	(test abracadabra kt kf)))))

; A macro-expand-time memv function for identifiers
;	id-memv?? FORM (ID ...) KT KF
; FORM is an arbitrary form or datum, ID is an identifier.
; The macro expands into KT if FORM is an identifier, which occurs
; in the list of identifiers supplied by the second argument.
; All the identifiers in that list must be unique.
; Otherwise, id-memv?? expands to KF.
; Two identifiers match if both refer to the same binding occurrence, or
; (both are undefined and have the same spelling).

; (id-memv??			; old code. 
;   (syntax-rules ()
;     ((_ x () kt kf) kf)
;     ((_ x (y . rest) kt kf)
;       (let-syntax
; 	((test 
; 	   (syntax-rules (y)
; 	     ((test y _x _rest _kt _kf) _kt)
; 	     ((test any _x _rest _kt _kf)
; 	       (id-memv?? _x _rest _kt _kf)))))
; 	(test x x rest kt kf)))))


(define-syntax id-memv??
  (syntax-rules ()
    ((id-memv?? form (id ...) kt kf)
      (let-syntax
	((test
	   (syntax-rules (id ...)
	     ((test id _kt _kf) _kt) ...
	     ((test otherwise _kt _kf) _kf))))
	(test form kt kf)))))

; Test cases
; (id-memv?? x (a b c) #t #f)
; (id-memv?? a (a b c) 'OK #f)
; (id-memv?? () (a b c) #t #f)
; (id-memv?? (x ...) (a b c) #t #f)
; (id-memv?? "abc" (a b c) #t #f)
; (id-memv?? x () #t #f)
; (let ((x 1))
;   (id-memv?? x (a b x) 'OK #f))
; (let ((x 1))
;   (id-memv?? x (a x b) 'OK #f))
; (let ((x 1))
;   (id-memv?? x (x a b) 'OK #f))

; Commonly-used CPS macros
; The following macros follow the convention that a continuation argument
; has the form (k-head ! args ...)
; where ! is a dedicated symbol (placeholder).
; When a CPS macro invokes its continuation, it expands into
; (k-head value args ...)
; To distinguish such calling conventions, we prefix the names of
; such macros with k!

(define-syntax k!id			; Just the identity. Useful in CPS
  (syntax-rules ()
    ((k!id x) x)))

; k!reverse ACC (FORM ...) K
; reverses the second argument, appends it to the first and passes
; the result to K

(define-syntax k!reverse
  (syntax-rules (!)
    ((k!reverse acc () (k-head ! . k-args))
      (k-head acc . k-args))
    ((k!reverse acc (x . rest) k)
      (k!reverse (x . acc) rest k))))


; (k!reverse () (1 2 () (4 5)) '!) ;==> '((4 5) () 2 1)
; (k!reverse (x) (1 2 () (4 5)) '!) ;==> '((4 5) () 2 1 x)
; (k!reverse (x) () '!) ;==> '(x)


; assert the truth of an expression (or of a sequence of expressions)
;
; syntax: assert ?expr ?expr ... [report: ?r-exp ?r-exp ...]
;
; If (and ?expr ?expr ...) evaluates to anything but #f, the result
; is the value of that expression.
; If (and ?expr ?expr ...) evaluates to #f, an error is reported.
; The error message will show the failed expressions, as well
; as the values of selected variables (or expressions, in general).
; The user may explicitly specify the expressions whose
; values are to be printed upon assertion failure -- as ?r-exp that
; follow the identifier 'report:'
; Typically, ?r-exp is either a variable or a string constant.
; If the user specified no ?r-exp, the values of variables that are
; referenced in ?expr will be printed upon the assertion failure.


(define-syntax assert
  (syntax-rules ()
    ((assert _expr . _others)
     (letrec-syntax
       ((write-report
	  (syntax-rules ()
			; given the list of expressions or vars,
			; create a cerr form
	    ((_ exprs prologue)
	      (k!reverse () (cerr . prologue)
		(write-report* ! exprs #\newline)))))
	 (write-report*
	   (syntax-rules ()
	     ((_ rev-prologue () prefix)
	       (k!reverse () (nl . rev-prologue) (k!id !)))
	     ((_ rev-prologue (x . rest) prefix)
	       (symbol?? x
		 (write-report* (x ": " 'x #\newline . rev-prologue) 
		   rest #\newline)
		 (write-report* (x prefix . rev-prologue) rest "")))))
	  
			; return the list of all unique "interesting"
			; variables in the expr. Variables that are certain
			; to be bound to procedures are not interesting.
	 (vars-of 
	   (syntax-rules (!)
	     ((_ vars (op . args) (k-head ! . k-args))
	       (id-memv?? op 
		 (quote let let* letrec let*-values lambda cond quasiquote
		   case define do assert)
		 (k-head vars . k-args) ; won't go inside
				; ignore the head of the application
		 (vars-of* vars args (k-head ! . k-args))))
		  ; not an application -- ignore
	     ((_ vars non-app (k-head ! . k-args)) (k-head vars . k-args))
	     ))
	 (vars-of*
	   (syntax-rules (!)
	     ((_ vars () (k-head ! . k-args)) (k-head vars . k-args))
	     ((_ vars (x . rest) k)
	       (symbol?? x
		 (id-memv?? x vars
		   (vars-of* vars rest k)
		   (vars-of* (x . vars) rest k))
		 (vars-of vars x (vars-of* ! rest k))))))

	 (do-assert
	   (syntax-rules (report:)
	     ((_ () expr)			; the most common case
	       (do-assert-c expr))
	     ((_ () expr report: . others) ; another common case
	       (do-assert-c expr others))
	     ((_ () expr . others) (do-assert (expr and) . others))
	     ((_ exprs)
	       (k!reverse () exprs (do-assert-c !)))
	     ((_ exprs report: . others)
	       (k!reverse () exprs (do-assert-c ! others)))
	     ((_ exprs x . others) (do-assert (x . exprs) . others))))

	 (do-assert-c
	   (syntax-rules ()
	     ((_ exprs)
	       (or exprs
		 (begin (vars-of () exprs
			  (write-report ! 
			    ("failed assertion: " 'exprs nl "bindings")))
		   (error "assertion failure"))))
	     ((_ exprs others)
	       (or exprs
		 (begin (write-report others
			  ("failed assertion: " 'exprs))
		   (error "assertion failure"))))))
	 )
       (do-assert () _expr . _others)
       ))))


(define-syntax assure
  (syntax-rules ()
    ((assure exp error-msg) (assert exp report: error-msg))))

(define (identify-error msg args . disposition-msgs)
  (let ((port (console-output-port)))
    (newline port)
    (display "ERROR" port)
    (display msg port)
    (for-each (lambda (msg) (display msg port))
	      (append args disposition-msgs))
    (newline port)))

; (define-syntax assert
;   (syntax-rules ()
;     ((_ expr ...)
;      (or (and expr ...)
;        (begin (error "failed assertion: " '(expr ...)))))))
    

(define chez-error error)
(define error
  (lambda (msg . args)
    (chez-error 'runtime-error "~a~%" (cons msg args))))

; like cout << arguments << args
; where argument can be any Scheme object. If it's a procedure
; (without args) it's executed rather than printed (like newline)

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

;(define cerr cout)
(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x) (x (console-output-port))
		(display x (console-output-port))))
            args))

(define nl (string #\newline))

; Some useful increment/decrement operators

(define-syntax inc!		; Mutable increment
  (syntax-rules ()
    ((inc! x) (set! x (+ 1 x)))))
(define-syntax inc               ; Read-only increment
  (syntax-rules ()
    ((inc x) (+ 1 x))))

(define-syntax dec!		; Mutable decrement
  (syntax-rules ()
    ((dec! x) (set! x (- x 1)))))
(define-syntax dec		; Read-only decrement
  (syntax-rules ()
    ((dec x) (- x 1))))

; Some useful control operators

			; if condition is true, execute stmts in turn
			; and return the result of the last statement
			; otherwise, return unspecified.
			; Native in Petite
; (define-syntax when
;   (syntax-rules ()
;     ((when condition . stmts)
;       (and condition (begin . stmts)))))
  

			; if condition is false execute stmts in turn
			; and return the result of the last statement
			; otherwise, return unspecified.
			; This primitive is often called 'unless'
(define-syntax whennot
  (syntax-rules ()
    ((whennot condition . stmts)
      (or condition (begin . stmts)))))


			; Execute a sequence of forms and return the
			; result of the _first_ one. Like PROG1 in Lisp.
			; Typically used to evaluate one or more forms with
			; side effects and return a value that must be
			; computed before some or all of the side effects happen.
(define-syntax begin0
  (syntax-rules ()
    ((begin0 form form1 ... ) 
      (let ((val form)) form1 ... val))))

			; Prepend an ITEM to a LIST, like a Lisp macro PUSH
			; an ITEM can be an expression, but ls must be a VAR
(define-syntax push!
  (syntax-rules ()
    ((push! item ls)
      (set! ls (cons item ls)))))

			; Is str the empty string?
			; string-null? str -> bool
			; See Olin Shiver's Underground String functions
(define-syntax string-null?
  (syntax-rules ()
    ((string-null? str) (zero? (string-length str)))))


; A rather useful utility from SRFI-1
; cons* elt1 elt2 ... -> object
;    Like LIST, but the last argument provides the tail of the constructed
;    list -- i.e., (cons* a1 a2 ... an) = (cons a1 (cons a2 (cons ... an))).
;
;   (cons* 1 2 3 4) => (1 2 3 . 4)
;   (cons* 1) => 1
(define (cons* first . rest)
  (let recur ((x first) (rest rest))
    (if (pair? rest)
	(cons x (recur (car rest) (cdr rest)))
	x)))

; Support for let*-values form: SRFI-11

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () . bodies) (begin . bodies))
    ((let*-values (((var) initializer) . rest) . bodies)
      (let ((var initializer))		; a single var optimization
	(let*-values rest . bodies)))
    ((let*-values ((vars initializer) . rest) . bodies)
      (call-with-values (lambda () initializer) ; the most generic case
	(lambda vars (let*-values rest . bodies))))))

			; assoc-primitives with a default clause
			; If the search in the assoc list fails, the
			; default action argument is returned. If this
			; default action turns out to be a thunk,
			; the result of its evaluation is returned.
			; If the default action is not given, an error
			; is signaled

(define-syntax assq-def
  (syntax-rules ()
    ((assq-def key alist)
      (or (assq key alist)
	(error "failed to assq key '" key "' in a list " alist)))
    ((assq-def key alist #f)
      (assq key alist))
    ((assq-def key alist default)
      (or (assq key alist) (if (procedure? default) (default) default)))))

(define-syntax assv-def
  (syntax-rules ()
    ((assv-def key alist)
      (or (assv key alist)
	(error "failed to assv key '" key "' in a list " alist)))
    ((assv-def key alist #f)
      (assv key alist))
    ((assv-def key alist default)
      (or (assv key alist) (if (procedure? default) (default) default)))))

(define-syntax assoc-def
  (syntax-rules ()
    ((assoc-def key alist)
      (or (assoc key alist)
	(error "failed to assoc key '" key "' in a list " alist)))
    ((assoc-def key alist #f)
      (assoc key alist))
    ((assoc-def key alist default)
      (or (assoc key alist) (if (procedure? default) (default) default)))))

			; Convenience macros to avoid quoting of symbols
			; being deposited/looked up in the environment
(define-syntax env.find
  (syntax-rules () ((env.find key) (%%env.find 'key))))
(define-syntax env.demand
  (syntax-rules () ((env.demand key) (%%env.demand 'key))))
(define-syntax env.bind
  (syntax-rules () ((env.bind key value) (%%env.bind 'key value))))

			; Implementation of SRFI-0
			; Only feature-identifiers srfi-0, chez, and
			; petite-chez are assumed predefined.
			; See below why this
			; syntax-rule may NOT use an let-syntax.
(define-syntax cond-expand
  (syntax-rules (else chez petite-chez srfi-0 and or not)
    ((cond-expand)
      (error "Unfulfilled cond-expand"))
    ((cond-expand (else . cmd-or-defs*))
      (begin . cmd-or-defs*))
    ((cond-expand "feature-id" chez kt kf) kt)
    ((cond-expand "feature-id" petite-chez kt kf) kt)
    ((cond-expand "feature-id" srfi-0 kt kf) kt)
    ((cond-expand "feature-id" x kt kf) kf)
    ((cond-expand "satisfies?" (and) kt kf) kt)
    ((cond-expand "satisfies?" (and clause) kt kf)
      (cond-expand "satisfies?" clause kt kf))
    ((cond-expand "satisfies?" (and clause . rest) kt kf)
      (cond-expand "satisfies?" clause
	(cond-expand "satisfies?" (and . rest) kt kf) kf))
    ((cond-expand "satisfies?" (or) kt kf) kf)
    ((cond-expand "satisfies?" (or clause) kt kf)
      (cond-expand "satisfies?" clause kt kf))
    ((cond-expand "satisfies?" (or clause . rest) kt kf)
      (cond-expand "satisfies?" clause kt
	(cond-expand "satisfies?" (or . rest) kt kf)))
    ((cond-expand "satisfies?" (not clause) kt kf)
      (cond-expand "satisfies?" clause kf kt))
    ((cond-expand "satisfies?" x kt kf)
      (cond-expand "feature-id" x kt kf))

    ((cond-expand (feature-req . cmd-or-defs*) . rest-clauses)
      (cond-expand "satisfies?" feature-req
	  (begin . cmd-or-defs*)
	  (cond-expand . rest-clauses)))))


; define-opt: A concise definition allowing optional arguments.
; Example:
;
; (define-opt (foo arg1 arg2 (optional arg3 (arg4 init4))) body)
;
; The form define-opt is designed to be as compatible with DSSSL's
; extended define as possible -- while avoiding the non-standard
; lexical token #!optional. On systems that do support DSSSL (e.g.,
; Gambit, Bigloo, Kawa) our define-opt expands into DSSSL's extended
; define, which is implemented efficiently on these systems.
;
; Here's the relevant part of the DSSSL specification, lifted
; from Gambit's online documentation:

;   define-formals = formal-argument-list | r4rs-define-formals
;   formal-argument-list = reqs opts rest keys
;   reqs = required-formal-argument*
;   required-formal-argument = variable
;   opts = #!optional optional-formal-argument* | empty
;   optional-formal-argument = variable | ( variable initializer )
;   rest = #!rest rest-formal-argument | empty
;   rest-formal-argument = variable
;   keys = #!key keyword-formal-argument* | empty
;   keyword-formal-argument = variable | ( variable initializer )
;   initializer = expression
;   r4rs-lambda-formals = ( variable* ) | ( variable+ . variable ) | variable
;   r4rs-define-formals = variable* | variable* . variable
;
;   1. Variables in required-formal-arguments are bound to successive actual
;      arguments starting with the first actual argument. It shall be an error
;      if there are fewer actual arguments than required-formal-arguments.
;   2. Next variables in optional-formal-arguments are bound to remaining
;      actual arguments. If there are fewer remaining actual arguments than
;      optional-formal-arguments, then the variables are bound to the result
;      of evaluating initializer, if one was specified, and otherwise to #f.
;      The initializer is evaluated in an environment in which all previous
;      formal arguments have been bound.
;   It shall be an error for a variable to appear more than once in a
;   formal-argument-list.
;   It is unspecified whether variables receive their value by binding or by
;   assignment.
;
; Our define-opt does not currently support rest and keys arguments.
; Also, instead of #optional optional-formal-argument ...
; we write (optional optional-formal-argument ...)
; 
; Our define-opt is similar to PLT Scheme's opt-lambda. However, 
; the syntax of define-opt guarantees that optional arguments are 
; really at the very end of the arg list.


; Chez does not support DSSSL extended defines and lambdas.
; Caveat: (define-opt name-bindings body) cannot expand into
; (let-syntax ((helper-macro ...)) (helper-macro name-bindings body))
; where helper-macro will generate the valid define.
; The mere appearance of (let-syntax ...) tells the Scheme system
; that whatever define will be generated, it is meant for the _internal_
; context. For example, the following code
;
; (define-syntax tdefine
;   (syntax-rules ()
;     ((tdefine _args . _bodies)
;       (letrec-syntax
; 	((helper
; 	   (syntax-rules ()
; 	     ((helper args bodies) (define args . bodies)))))
; 	(helper _args _bodies)))))
; (tdefine (foo x) (display "OK") (display x) (newline))
; (foo 42)
;
; runs OK on Petite Chez but gives an error "definition in expression context"
; on Scheme48 and SCM (and, consequently, the binding to foo does not occur).


(define-syntax define-opt
  (syntax-rules (optional)
    ((define-opt (name . bindings) . bodies)
      (define-opt "seek-optional" bindings () ((name . bindings) . bodies)))

    ((define-opt "seek-optional" ((optional . _opt-bindings))
       (reqd ...) ((name . _bindings) . _bodies))
      (define (name reqd ... . _rest)
	(letrec-syntax
	  ((handle-opts
	     (syntax-rules ()
	       ((_ rest bodies (var init))
		 (let ((var (if (null? rest) init
			      (if (null? (cdr rest)) (car rest)
				(error "extra rest" rest)))))
		   . bodies))
	       ((_ rest bodies var) (handle-opts rest bodies (var #f)))
	       ((_ rest bodies (var init) . other-vars)
		 (let ((var (if (null? rest) init (car rest)))
		       (new-rest (if (null? rest) '() (cdr rest))))
		   (handle-opts new-rest bodies . other-vars)))
	       ((_ rest bodies var . other-vars)
		 (handle-opts rest bodies (var #f) . other-vars))
	       ((_ rest bodies)		; no optional args, unlikely
		 (let ((_ (or (null? rest) (error "extra rest" rest))))
		   . bodies)))))
	  (handle-opts _rest _bodies . _opt-bindings))))

    ((define-opt "seek-optional" (x . rest) (reqd ...) form)
      (define-opt "seek-optional" rest (reqd ... x) form))

    ((define-opt "seek-optional" not-a-pair reqd form)
      (define . form))			; No optional found, regular define

    ((define-opt name body)		; Just the definition for 'name',
      (define name body))		; for compatibilibility with define
))


;      AND-LET* -- an AND with local bindings, a guarded LET* special form
;
; AND-LET* (formerly know as LAND*) is a generalized AND: it evaluates
; a sequence of forms one after another till the first one that yields
; #f; the non-#f result of a form can be bound to a fresh variable and
; used in the subsequent forms.
; It is defined in SRFI-2 <http://srfi.schemers.org/srfi-2/>
; This macro re-writes the and-let* form into a combination of
; 'and' and 'let'.
; See vland.scm for the denotational semantics and
; extensive validation tests.

(define-syntax and-let*
  (syntax-rules ()
    ((_ ()) #t)
    ((_ claws)    ; no body
       ; re-write (and-let* ((claw ... last-claw)) ) into
       ; (and-let* ((claw ...)) body) with 'body' derived from the last-claw
     (and-let* "search-last-claw" () claws))
    ((_ "search-last-claw" first-claws ((exp)))
     (and-let* first-claws exp))	; (and-let* (... (exp)) )
    ((_ "search-last-claw" first-claws ((var exp)))
     (and-let* first-claws exp))	; (and-let* (... (var exp)) )
    ((_ "search-last-claw" first-claws (var))
     (and-let* first-claws var))	; (and-let* (... var) )
    ((_ "search-last-claw" (first-claw ...) (claw . rest))
     (and-let* "search-last-claw" (first-claw ... claw) rest))
    
    ; now 'body' is present
    ((_ () . body) (begin . body))	; (and-let* () form ...)
    ((_ ((exp) . claws) . body)		; (and-let* ( (exp) claw... ) body ...)
     (and exp (and-let* claws . body)))
    ((_ ((var exp) . claws) . body)	; (and-let* ((var exp) claw...)body...)
     (let ((var exp)) (and var (and-let* claws . body))))
    ((_ (var . claws) . body)		; (and-let* ( var claw... ) body ...)
     (and var (and-let* claws . body)))
))

;		A subset of SRFI-13 functions
;
; This file collects various SRFI-13 functions that are used in the
; input parsing library, the XML parser, and other code.
;
; It is always better to import the functions below from SRFI-13, if a
; Scheme implementation supports that SRFI natively. The present file
; can be used on a Scheme system that does not currently support SRFI-13.
;
; In implementing the string utility functions, we try to use whatever
; native facilities are available. That fact explains a fair number of
; cond-expand.
;
; $Id: srfi-13-local.scm,v 1.2 2004/07/08 21:53:32 oleg Exp $

 ; Implementations of string-xcopy! of SRFI-13, using
 ; whatever native facilities are available
(cond-expand
  (bigloo
    (define (string-xcopy! target tstart s sfrom sto)
      (blit-string! s sfrom target tstart (- sto sfrom))))
  (else
    (define (string-xcopy! target tstart s sfrom sto)
      (do ((i sfrom (inc i)) (j tstart (inc j)))
	((>= i sto))
	(string-set! target j (string-ref s i)))))
)


; procedure string-concatenate-reverse STRINGS FINAL END
(define (string-concatenate-reverse strs final end)
  (if (null? strs) (substring final 0 end)
    (let*
      ((total-len
	 (let loop ((len end) (lst strs))
	   (if (null? lst) len
	     (loop (+ len (string-length (car lst))) (cdr lst)))))
	(result (make-string total-len)))
      (let loop ((len end) (j total-len) (str final) (lst strs))
	(string-xcopy! result (- j len) str 0 len)
	(if (null? lst) result
	  (loop (string-length (car lst)) (- j len)
	    (car lst) (cdr lst)))))))


; string-concatenate/shared STRING-LIST -> STRING
(define (string-concatenate/shared strs)
  (cond
    ((null? strs) "")			; Test for the fast path first
    ((null? (cdr strs)) (car strs))
    (else
      (let*
	((total-len
	   (let loop ((len (string-length (car strs))) (lst (cdr strs)))
	   (if (null? lst) len
	     (loop (+ len (string-length (car lst))) (cdr lst)))))
	  (result (make-string total-len)))
	(let loop ((j 0) (str (car strs)) (lst (cdr strs)))
	  (string-xcopy! result j str 0 (string-length str))
	  (if (null? lst) result
	    (loop (+ j (string-length str))
	      (car lst) (cdr lst))))))))


; string-concatenate-reverse/shared STRING-LIST [FINAL-STRING END] -> STRING
; We do not use the optional arguments of this procedure. Therefore,
; we do not implement them. See SRFI-13 for the complete
; implementation.
(define (string-concatenate-reverse/shared strs)
  (cond
    ((null? strs) "")			; Test for the fast path first
    ((null? (cdr strs)) (car strs))
    (else
      (string-concatenate-reverse (cdr strs)
	(car strs) (string-length (car strs))))))



; Return the index of the first occurence of a-char in str, or #f
; This is a subset of the corresponding SRFI-13 function.
; The latter is more generic.

(define (string-index str a-char)
  (let loop ((pos 0))
    (cond
      ((>= pos (string-length str)) #f) ; whole string has been searched, in vain
      ((char=? a-char (string-ref str pos)) pos)
      (else (loop (inc pos))))))

; Return the index of the last occurence of a-char in str, or #f
; This is a subset of the corresponding SRFI-13 function.
; The latter is more generic.

(define (string-index-right str a-char)
  (let loop ((pos (dec (string-length str))))
    (cond
      ((negative? pos) #f) 	; whole string has been searched, in vain
      ((char=? a-char (string-ref str pos)) pos)
      (else (loop (dec pos))))))


; string-contains    s1 s2 [start1 end1 start2 end2] -> integer or false
; string-contains-ci s1 s2 [start1 end1 start2 end2] -> integer or false
;     Does string s1 contain string s2?
;     Return the index in s1 where s2 occurs as a substring, or false. The
;     optional start/end indices restrict the operation to the indicated
;     substrings.
; We do not support the optional arguments
(define (string-contains str pattern)
  (let* ((pat-len (string-length pattern))
         (search-span (- (string-length str) pat-len))
         (c1 (if (zero? pat-len) #f (string-ref pattern 0)))
         (c2 (if (<= pat-len 1) #f (string-ref pattern 1))))
    (cond
     ((not c1) 0)           ; empty pattern, matches upfront
     ((not c2) (string-index str c1)) ; one-char pattern
     (else                  ; matching a pattern of at least two chars
	(let outer ((pos 0))
          (cond
	    ((> pos search-span) #f)	; nothing was found thru the whole str
            ((not (char=? c1 (string-ref str pos)))
                (outer (+ 1 pos)))	; keep looking for the right beginning
            ((not (char=? c2 (string-ref str (+ 1 pos))))
                (outer (+ 1 pos)))	; could've done pos+2 if c1 == c2....
            (else                  	; two char matched: high probability
				   	; the rest will match too
		(let inner ((i-pat 2) (i-str (+ 2 pos)))
                   (if (>= i-pat pat-len) pos ; whole pattern matched
                      (if (char=? (string-ref pattern i-pat)
                                  (string-ref str i-str))
                        (inner (+ 1 i-pat) (+ 1 i-str))
                        (outer (+ 1 pos))))))))))))	; mismatch after partial match


; Here are some specialized substring? functions
;
; -- procedure+: string-prefix? PATTERN STRING
; -- procedure+: string-prefix-ci? PATTERN STRING
; checks to make sure that PATTERN is a prefix of STRING
;
;          (string-prefix? "pir" "pirate")             =>  #t
;          (string-prefix? "rat" "outrage")            =>  #f
;          (string-prefix? "" any-string)              =>  #t
;          (string-prefix? any-string any-string)      =>  #t

(define (string-prefix? pattern str)
  (let loop ((i 0))
    (cond
      ((>= i (string-length pattern)) #t)
      ((>= i (string-length str)) #f)
      ((char=? (string-ref pattern i) (string-ref str i))
        (loop (inc i)))
      (else #f))))

(define (string-prefix-ci? pattern str)
  (let loop ((i 0))
    (cond
      ((>= i (string-length pattern)) #t)
      ((>= i (string-length str)) #f)
      ((char-ci=? (string-ref pattern i) (string-ref str i))
        (loop (inc i)))
      (else #f))))

; -- procedure+: string-suffix? PATTERN STRING
; -- procedure+: string-suffix-ci? PATTERN STRING
; checks to make sure that PATTERN is a suffix of STRING
;
;          (string-suffix? "ate" "pirate")             =>  #t
;          (string-suffix? "rag" "outrage")            =>  #f
;          (string-suffix? "" any-string)              =>  #t
;          (string-suffix? any-string any-string)      =>  #t

(define (string-suffix? pattern str)
  (let loop ((i (dec (string-length pattern))) (j (dec (string-length str))))
    (cond
      ((negative? i) #t)
      ((negative? j) #f)
      ((char=? (string-ref pattern i) (string-ref str j))
        (loop (dec i) (dec j)))
      (else #f))))

(define (string-suffix-ci? pattern str)
  (let loop ((i (dec (string-length pattern))) (j (dec (string-length str))))
    (cond
      ((negative? i) #t)
      ((negative? j) #f)
      ((char-ci=? (string-ref pattern i) (string-ref str j))
        (loop (dec i) (dec j)))
      (else #f))))


;    Raise or lower the case of the alphabetic characters in the string.

(cond-expand
  (bigloo #f)				; Bigloo implements them directly
  (else

; Return a new string made of characters of the
; original string in the lower case
(define (string-downcase str)
  (do ((target-str (make-string (string-length str))) (i 0 (inc i)))
      ((>= i (string-length str)) target-str)
      (string-set! target-str i (char-downcase (string-ref str i)))))

; Return a new string made of characters of the
; original string in the upper case
(define (string-upcase str)
  (do ((target-str (make-string (string-length str))) (i 0 (inc i)))
      ((>= i (string-length str)) target-str)
      (string-set! target-str i (char-upcase (string-ref str i)))))

; Lower the case of string's characters inplace
(define (string-downcase! str)
  (do ((i 0 (inc i))) ((>= i (string-length str)))
    (string-set! str i (char-downcase (string-ref str i)))))

; Raise the case of string's characters inplace
(define (string-upcase! str)
  (do ((i 0 (inc i))) ((>= i (string-length str)))
    (string-set! str i (char-upcase (string-ref str i)))))
))


;****************************************************************************
;			My Scheme misc utility functions
;		(mainly dealing with string and list manipulations)
;
; myenv.scm, myenv-bigloo.scm or similar prelude is assumed.
; From SRFI-13, import many functions
; If a particular implementation lacks SRFI-13 support, please
; include the file srfi-13-local.scm
;
; $Id: util.scm,v 1.5 2004/07/07 16:02:31 sperber Exp $

;------------------------------------------------------------------------
;                               Iterator ANY?
;
; -- procedure+: any? PRED COLLECTION
;       Searches for the first element in the collection satisfying a
;       given predicate
;       That is, the procedure applies PRED to every element of the
;       COLLECTION in turn.
;       The first element for which PRED returns non-#f stops the iteration;
;       the value of the predicate is returned.
;       If none of the elements of the COLLECTION satisfy the predicate,
;       the return value from the procedure is #f
;       COLLECTION can be a list, a vector, a string, or an input port.
; See vmyenv.scm for validation tests.

(define (any? <pred?> coll)
  (cond
    ((list? coll)
      (let loop ((curr-l coll))
        (if (null? curr-l) #f
          (or (<pred?> (car curr-l)) (loop (cdr curr-l))))))
          
    ((vector? coll)
      (let ((len (vector-length coll)))
       (let loop ((i 0))
        (if (>= i len) #f
          (or (<pred?> (vector-ref coll i)) (loop (inc i)))))))

    ((string? coll)
      (let ((len (string-length coll)))
       (let loop ((i 0))
        (if (>= i len) #f
          (or (<pred?> (string-ref coll i)) (loop (inc i)))))))

    ((input-port? coll)
      (let loop ((c (read-char coll)))
        (if (eof-object? c) #f
          (or (<pred?> c) (loop (read-char coll))))))

    (else (error "any? on an invalid collection"))))


;------------------------------------------------------------------------
;		Some list manipulation functions

; -- procedure+: list-intersperse SRC-L ELEM
; inserts ELEM between elements of the SRC-L, returning a freshly allocated
; list (cells, that is)
      
(define (list-intersperse src-l elem)
  (if (null? src-l) src-l
    (let loop ((l (cdr src-l)) (dest (cons (car src-l) '())))
      (if (null? l) (reverse dest)
        (loop (cdr l) (cons (car l) (cons elem dest)))))))


; -- procedure+: list-intersperse! SRC-L ELEM
; inserts ELEM between elements of the SRC-L inplace
      
(define (list-intersperse! src-l elem)
  (if (null? src-l) src-l
    (let loop ((l src-l))
      (let ((next-l (cdr l)))
        (if (null? next-l) src-l
          (begin
            (set-cdr! l (cons elem next-l))
            (loop next-l)))))))


	; List-tail-difference: given two lists, list1 and list2 where
	; list2 is presumably a tail of list1, return 
	; a (freshly allocated) list which is a difference between list1 
	; and list2. If list2 is *not* a tail of list1, the entire list1
	; is returned.
(define (list-tail-diff list1 list2)
  (let loop ((l1-curr list1) (difference '()))
    (cond
      ((eq? l1-curr list2) (reverse difference))
      ((null? l1-curr) (reverse difference))
      (else (loop (cdr l1-curr) (cons (car l1-curr) difference))))))


;------------------------------------------------------------------------
;			String utilities
; See SRFI-13 or srfi-13-local.scm


; Return the index of the last occurence of a-char in str, or #f
; See SRFI-13
(define string-rindex string-index-right)

; -- procedure+: substring? PATTERN STRING
;     Searches STRING to see if it contains the substring PATTERN.
;     Returns the index of the first substring of STRING that is equal
;     to PATTERN; or `#f' if STRING does not contain PATTERN.
;
;          (substring? "rat" "pirate")             =>  2
;          (substring? "rat" "outrage")            =>  #f
;          (substring? "" any-string)              =>  0
(define (substring? pattern str) (string-contains str pattern))


; -- procedure+: string->integer STR START END
;
; Makes sure a substring of the STR from START (inclusive) till END
; (exclusive) is a representation of a non-negative integer in decimal
; notation. If so, this integer is returned. Otherwise -- when the
; substring contains non-decimal characters, or when the range from
; START till END is not within STR, the result is #f.
;
; This procedure is a simplification of the standard string->number.
; The latter is far more generic: for example, it will try to read
; strings like "1/2" "1S2" "1.34" and even "1/0" (the latter causing
; a zero-divide error). Note that to string->number,  "1S2" is a valid
; representation of an _inexact_ integer (100 to be precise).
; Oftentimes we want to be more restrictive about what we consider a
; number; we want merely to read an integral label.

(define (string->integer str start end)
  (and (< -1 start end (inc (string-length str)))
    (let loop ((pos start) (accum 0))
      (cond
        ((>= pos end) accum)
        ((char-numeric? (string-ref str pos))
          (loop (inc pos) (+ (char->integer (string-ref str pos)) 
              (- (char->integer #\0)) (* 10 accum))))
        (else #f)))))


; 
; -- procedure+: string-split STRING
; -- procedure+: string-split STRING '()
; -- procedure+: string-split STRING '() MAXSPLIT
;
; Returns a list of whitespace delimited words in STRING.
; If STRING is empty or contains only whitespace, then the empty list
; is returned. Leading and trailing whitespaces are trimmed.
; If MAXSPLIT is specified and positive, the resulting list will
; contain at most MAXSPLIT elements, the last of which is the string
; remaining after (MAXSPLIT - 1) splits. If MAXSPLIT is specified and
; non-positive, the empty list is returned. "In time critical
; applications it behooves you not to split into more fields than you
; really need."
;
; -- procedure+: string-split STRING CHARSET
; -- procedure+: string-split STRING CHARSET MAXSPLIT
;
; Returns a list of words delimited by the characters in CHARSET in
; STRING. CHARSET is a list of characters that are treated as delimiters.
; Leading or trailing delimeters are NOT trimmed. That is, the resulting
; list will have as many initial empty string elements as there are
; leading delimiters in STRING.
;
; If MAXSPLIT is specified and positive, the resulting list will
; contain at most MAXSPLIT elements, the last of which is the string
; remaining after (MAXSPLIT - 1) splits. If MAXSPLIT is specified and
; non-positive, the empty list is returned. "In time critical
; applications it behooves you not to split into more fields than you
; really need."
;
; This is based on the split function in Python/Perl
;
; (string-split " abc d e f  ") ==> ("abc" "d" "e" "f")
; (string-split " abc d e f  " '() 1) ==> ("abc d e f  ")
; (string-split " abc d e f  " '() 0) ==> ()
; (string-split ":abc:d:e::f:" '(#\:)) ==> ("" "abc" "d" "e" "" "f" "")
; (string-split ":" '(#\:)) ==> ("" "")
; (string-split "root:x:0:0:Lord" '(#\:) 2) ==> ("root" "x:0:0:Lord")
; (string-split "/usr/local/bin:/usr/bin:/usr/ucb/bin" '(#\:))
; ==> ("/usr/local/bin" "/usr/bin" "/usr/ucb/bin")
; (string-split "/usr/local/bin" '(#\/)) ==> ("" "usr" "local" "bin")

(define (string-split str . rest)
		; maxsplit is a positive number
  (define (split-by-whitespace str maxsplit)
    (define (skip-ws i yet-to-split-count)
      (cond
        ((>= i (string-length str)) '())
        ((char-whitespace? (string-ref str i))
          (skip-ws (inc i) yet-to-split-count))
        (else (scan-beg-word (inc i) i yet-to-split-count))))
    (define (scan-beg-word i from yet-to-split-count)
      (cond
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word i from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((char-whitespace? (string-ref str i))
          (cons (substring str from i) 
            (skip-ws (inc i) (- yet-to-split-count 1))))
        (else (scan-word (inc i) from yet-to-split-count))))
    (skip-ws 0 (- maxsplit 1)))

		; maxsplit is a positive number
		; str is not empty
  (define (split-by-charset str delimeters maxsplit)
    (define (scan-beg-word from yet-to-split-count)
      (cond
        ((>= from (string-length str)) '(""))
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word from from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((memq (string-ref str i) delimeters)
          (cons (substring str from i) 
            (scan-beg-word (inc i) (- yet-to-split-count 1))))
        (else (scan-word (inc i) from yet-to-split-count))))
    (scan-beg-word 0 (- maxsplit 1)))

			; resolver of overloading...
			; if omitted, maxsplit defaults to
			; (inc (string-length str))
  (if (string-null? str) '()
    (if (null? rest) 
      (split-by-whitespace str (inc (string-length str)))
      (let ((charset (car rest))
          (maxsplit
            (if (pair? (cdr rest)) (cadr rest) (inc (string-length str)))))
        (cond 
          ((not (positive? maxsplit)) '())
          ((null? charset) (split-by-whitespace str maxsplit))
          (else (split-by-charset str charset maxsplit))))))
)


; make-char-quotator QUOT-RULES
;
; Given QUOT-RULES, an assoc list of (char . string) pairs, return
; a quotation procedure. The returned quotation procedure takes a string
; and returns either a string or a list of strings. The quotation procedure
; check to see if its argument string contains any instance of a character
; that needs to be encoded (quoted). If the argument string is "clean",
; it is returned unchanged. Otherwise, the quotation procedure will
; return a list of string fragments. The input straing will be broken
; at the places where the special characters occur. The special character
; will be replaced by the corresponding encoding strings.
;
; For example, to make a procedure that quotes special HTML characters,
; do
;	(make-char-quotator
;	    '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;")))

(define (make-char-quotator char-encoding)
  (let ((bad-chars (map car char-encoding)))

    ; Check to see if str contains one of the characters in charset,
    ; from the position i onward. If so, return that character's index.
    ; otherwise, return #f
    (define (index-cset str i charset)
      (let loop ((i i))
	(and (< i (string-length str))
	     (if (memv (string-ref str i) charset) i
		 (loop (inc i))))))

    ; The body of the function
    (lambda (str)
      (let ((bad-pos (index-cset str 0 bad-chars)))
	(if (not bad-pos) str	; str had all good chars
	    (let loop ((from 0) (to bad-pos))
	      (cond
	       ((>= from (string-length str)) '())
	       ((not to)
		(cons (substring str from (string-length str)) '()))
	       (else
		(let ((quoted-char
		       (cdr (assv (string-ref str to) char-encoding)))
		      (new-to 
		       (index-cset str (inc to) bad-chars)))
		  (if (< from to)
		      (cons
		       (substring str from to)
		       (cons quoted-char (loop (inc to) new-to)))
		      (cons quoted-char (loop (inc to) new-to))))))))))
))

;		Character-encoding module
;
; This module deals with particular character-encoding issues such as
; conversions between characters and their ASCII or UCS2 codes, Scheme
; representations of "Carriage Return" (CR), "tabulation" (TAB) and
; other control characters.
;
; This module by necessity is platform-specific as character encoding
; issues are hardly addressed in R5RS. For example, the result of
; char->integer is generally not an ASCII code of an integer (although
; it is, on many Scheme systems, with the important exception of
; Scheme48 and SCSH). The level of support for character sets other
; than ASCII varies widely among Scheme systems.
;
; This file collects various character-encoding functions that are
; necessary for the SSAX XML parser. The functions are of general use
; and scope.
;
; $Id: char-encoding.scm,v 1.1 2003/04/09 20:34:28 oleg Exp $


;	ascii->char INT -> CHAR
; return a character whose ASCII code is INT
; Note, because ascii->char is injective (there are more characters than
; ASCII characters), the inverse transformation is not defined.
(cond-expand
  (scheme48  #f)		; ascii->char is built into Scheme48
  (scsh #f)			; ascii->char is built into Scheme48
  (else
    (define ascii->char integer->char)
  )
)


;	ucscode->char INT -> CHAR
; Return a character whose UCS (ISO/IEC 10646) code is INT
; Note
; This function is required for processing of XML character entities:
; According to Section "4.1 Character and Entity References"
; of the XML Recommendation:
;  "[Definition: A character reference refers to a specific character
;   in the ISO/IEC 10646 character set, for example one not directly
;   accessible from available input devices.]"

(define (ucscode->char code)
  (cond-expand
    (bigloo
      (ucs2->char (integer->ucs2 code)))
    ((or scheme48 scsh)			; Scheme48 has no support for UCS
      (ascii->char code))
    (else
      (integer->char code))))

; Commonly used control characters

(define char-return (ascii->char 13))
(define char-tab    (ascii->char 9))
(define char-newline (ascii->char 10)) ; a.k.a. #\newline, per R5RS
;****************************************************************************
;			Simple Parsing of input
;
; The following simple functions surprisingly often suffice to parse
; an input stream. They either skip, or build and return tokens,
; according to inclusion or delimiting semantics. The list of
; characters to expect, include, or to break at may vary from one
; invocation of a function to another. This allows the functions to
; easily parse even context-sensitive languages.
;
; EOF is generally frowned on, and thrown up upon if encountered.
; Exceptions are mentioned specifically. The list of expected characters 
; (characters to skip until, or break-characters) may include an EOF
; "character", which is to be coded as symbol *eof*
;
; The input stream to parse is specified as a PORT, which is usually
; the last (and optional) argument. It defaults to the current input
; port if omitted.
;
; IMPORT
; This package relies on a function parser-error, which must be defined
; by a user of the package. The function has the following signature:
;	parser-error PORT MESSAGE SPECIALISING-MSG*
; Many procedures of this package call parser-error to report a parsing
; error.  The first argument is a port, which typically points to the
; offending character or its neighborhood. Most of the Scheme systems
; let the user query a PORT for the current position. MESSAGE is the
; description of the error. Other arguments supply more details about
; the problem.
; myenv.scm, myenv-bigloo.scm or a similar prelude is assumed.
; From SRFI-13, string-concatenate-reverse
; If a particular implementation lacks SRFI-13 support, please
; include the file srfi-13-local.scm
;
; $Id: input-parse.scm,v 1.7 2004/07/07 16:02:31 sperber Exp $

;------------------------------------------------------------------------

; -- procedure+: peek-next-char [PORT]
; 	advances to the next character in the PORT and peeks at it.
; 	This function is useful when parsing LR(1)-type languages
; 	(one-char-read-ahead).
;	The optional argument PORT defaults to the current input port.

(define-opt (peek-next-char (optional (port (current-input-port))))
  (read-char port) 
  (peek-char port)) 


;------------------------------------------------------------------------

; -- procedure+: assert-curr-char CHAR-LIST STRING [PORT]
;	Reads a character from the PORT and looks it up
;	in the CHAR-LIST of expected characters
;	If the read character was found among expected, it is returned
;	Otherwise, the procedure writes a nasty message using STRING
;	as a comment, and quits.
;	The optional argument PORT defaults to the current input port.
;
(define-opt (assert-curr-char expected-chars comment
			      (optional (port (current-input-port))))
  (let ((c (read-char port)))
    (if (memv c expected-chars) c
    (parser-error port "Wrong character " c
    	   " (0x" (if (eof-object? c) "*eof*"
    	   	    (number->string (char->integer c) 16)) ") "
    	   comment ". " expected-chars " expected"))))
    	   

; -- procedure+: skip-until CHAR-LIST [PORT]
;	Reads and skips characters from the PORT until one of the break
;	characters is encountered. This break character is returned.
;	The break characters are specified as the CHAR-LIST. This list
;	may include EOF, which is to be coded as a symbol *eof*
;
; -- procedure+: skip-until NUMBER [PORT]
;	Skips the specified NUMBER of characters from the PORT and returns #f
;
;	The optional argument PORT defaults to the current input port.


(define-opt (skip-until arg (optional (port (current-input-port))) )
  (cond
   ((number? arg)		; skip 'arg' characters
      (do ((i arg (dec i)))
      	  ((not (positive? i)) #f)
      	  (if (eof-object? (read-char port))
      	    (parser-error port "Unexpected EOF while skipping "
			 arg " characters"))))
   (else			; skip until break-chars (=arg)
     (let loop ((c (read-char port)))
       (cond
         ((memv c arg) c)
         ((eof-object? c)
           (if (memq '*eof* arg) c
             (parser-error port "Unexpected EOF while skipping until " arg)))
         (else (loop (read-char port))))))))


; -- procedure+: skip-while CHAR-LIST [PORT]
;	Reads characters from the PORT and disregards them,
;	as long as they are mentioned in the CHAR-LIST.
;	The first character (which may be EOF) peeked from the stream
;	that is NOT a member of the CHAR-LIST is returned. This character
;	is left on the stream.
;	The optional argument PORT defaults to the current input port.

(define-opt (skip-while skip-chars (optional (port (current-input-port))) )
  (do ((c (peek-char port) (peek-char port)))
      ((not (memv c skip-chars)) c)
      (read-char port)))
 
; whitespace const

;------------------------------------------------------------------------
;				Stream tokenizers


; -- procedure+: 
;    next-token PREFIX-CHAR-LIST BREAK-CHAR-LIST [COMMENT-STRING] [PORT]
;	skips any number of the prefix characters (members of the
;	PREFIX-CHAR-LIST), if any, and reads the sequence of characters
;	up to (but not including) a break character, one of the
;	BREAK-CHAR-LIST.
;	The string of characters thus read is returned.
;	The break character is left on the input stream
;	The list of break characters may include EOF, which is to be coded as
;	a symbol *eof*. Otherwise, EOF is fatal, generating an error message
;	including a specified COMMENT-STRING (if any)
;
;	The optional argument PORT defaults to the current input port.
;
; Note: since we can't tell offhand how large the token being read is
; going to be, we make a guess, pre-allocate a string, and grow it by
; quanta if necessary. The quantum is always the length of the string
; before it was extended the last time. Thus the algorithm does
; a Fibonacci-type extension, which has been proven optimal.
; Note, explicit port specification in read-char, peek-char helps.

; Procedure: input-parse:init-buffer
; returns an initial buffer for next-token* procedures.
; The input-parse:init-buffer may allocate a new buffer per each invocation:
;	(define (input-parse:init-buffer) (make-string 32))
; Size 32 turns out to be fairly good, on average.
; That policy is good only when a Scheme system is multi-threaded with
; preemptive scheduling, or when a Scheme system supports shared substrings.
; In all the other cases, it's better for input-parse:init-buffer to
; return the same static buffer. next-token* functions return a copy
; (a substring) of accumulated data, so the same buffer can be reused.
; We shouldn't worry about an incoming token being too large:
; next-token will use another chunk automatically. Still, 
; the best size for the static buffer is to allow most of the tokens to fit in.
; Using a static buffer _dramatically_ reduces the amount of produced garbage
; (e.g., during XML parsing).

(define input-parse:init-buffer
  (let ((buffer (make-string 512)))
    (lambda () buffer)))
  

		; See a better version below
(define-opt (next-token-old prefix-skipped-chars break-chars
			(optional (comment "") (port (current-input-port))) )
  (let* ((buffer (input-parse:init-buffer))
	 (curr-buf-len (string-length buffer))
	 (quantum curr-buf-len))
    (let loop ((i 0) (c (skip-while prefix-skipped-chars port)))
      (cond
        ((memv c break-chars) (substring buffer 0 i))
    	((eof-object? c)
    	  (if (memq '*eof* break-chars)
    	    (substring buffer 0 i)		; was EOF expected?
    	    (parser-error port "EOF while reading a token " comment)))
    	(else
    	  (if (>= i curr-buf-len)	; make space for i-th char in buffer
    	    (begin			; -> grow the buffer by the quantum
    	      (set! buffer (string-append buffer (make-string quantum)))
    	      (set! quantum curr-buf-len)
    	      (set! curr-buf-len (string-length buffer))))
    	  (string-set! buffer i c)
    	  (read-char port)			; move to the next char
    	  (loop (inc i) (peek-char port))
    	  )))))


; A better version of next-token, which accumulates the characters
; in chunks, and later on reverse-concatenates them, using
; SRFI-13 if available.
; The overhead of copying characters is only 100% (or even smaller: bulk
; string copying might be well-optimised), compared to the (hypothetical)
; circumstance if we had known the size of the token beforehand.
; For small tokens, the code performs just as above. For large
; tokens, we expect an improvement. Note, the code also has no
; assignments. 
; See next-token-comp.scm

(define-opt (next-token prefix-skipped-chars break-chars
		  (optional (comment "") (port (current-input-port))) )
  (let outer ((buffer (input-parse:init-buffer)) (filled-buffer-l '())
	      (c (skip-while prefix-skipped-chars port)))
    (let ((curr-buf-len (string-length buffer)))
      (let loop ((i 0) (c c))
	(cond
	  ((memv c break-chars)
	    (if (null? filled-buffer-l) (substring buffer 0 i)
	      (string-concatenate-reverse filled-buffer-l buffer i)))
	  ((eof-object? c)
	    (if (memq '*eof* break-chars)	; was EOF expected?
	      (if (null? filled-buffer-l) (substring buffer 0 i)
		(string-concatenate-reverse filled-buffer-l buffer i))
	      (parser-error port "EOF while reading a token " comment)))
	  ((>= i curr-buf-len)
	    (outer (make-string curr-buf-len)
	      (cons buffer filled-buffer-l) c))
	  (else
	    (string-set! buffer i c)
	    (read-char port)			; move to the next char
	    (loop (inc i) (peek-char port))))))))

; -- procedure+: next-token-of INC-CHARSET [PORT]
;	Reads characters from the PORT that belong to the list of characters
;	INC-CHARSET. The reading stops at the first character which is not
;	a member of the set. This character is left on the stream.
;	All the read characters are returned in a string.
;
; -- procedure+: next-token-of PRED [PORT]
;	Reads characters from the PORT for which PRED (a procedure of one
;	argument) returns non-#f. The reading stops at the first character
;	for which PRED returns #f. That character is left on the stream.
;	All the results of evaluating of PRED up to #f are returned in a
;	string.
;
;	PRED is a procedure that takes one argument (a character
;	or the EOF object) and returns a character or #f. The returned
;	character does not have to be the same as the input argument
;	to the PRED. For example,
;	(next-token-of (lambda (c)
;			  (cond ((eof-object? c) #f)
;				((char-alphabetic? c) (char-downcase c))
;				(else #f))))
;	will try to read an alphabetic token from the current
;	input port, and return it in lower case.
; 
;	The optional argument PORT defaults to the current input port.
;
; This procedure is similar to next-token but only it implements
; an inclusion rather than delimiting semantics.

(define-opt (next-token-of incl-list/pred
			   (optional (port (current-input-port))) )
  (let* ((buffer (input-parse:init-buffer))
	 (curr-buf-len (string-length buffer)))
  (if (procedure? incl-list/pred)
    (let outer ((buffer buffer) (filled-buffer-l '()))
      (let loop ((i 0))
	(if (>= i curr-buf-len)		; make sure we have space
	  (outer (make-string curr-buf-len) (cons buffer filled-buffer-l))
	  (let ((c (incl-list/pred (peek-char port))))
	    (if c
	      (begin
		(string-set! buffer i c)
		(read-char port)			; move to the next char
		(loop (inc i)))
	      ; incl-list/pred decided it had had enough
	      (if (null? filled-buffer-l) (substring buffer 0 i)
		(string-concatenate-reverse filled-buffer-l buffer i)))))))

    ; incl-list/pred is a list of allowed characters
    (let outer ((buffer buffer) (filled-buffer-l '()))
      (let loop ((i 0))
	(if (>= i curr-buf-len)		; make sure we have space
	  (outer (make-string curr-buf-len) (cons buffer filled-buffer-l))
	  (let ((c (peek-char port)))
	    (cond
	      ((not (memv c incl-list/pred))
		(if (null? filled-buffer-l) (substring buffer 0 i)
		  (string-concatenate-reverse filled-buffer-l buffer i)))
	      (else
		(string-set! buffer i c)
		(read-char port)			; move to the next char
		(loop (inc i))))))))
    )))


; -- procedure+: read-text-line [PORT]
;	Reads one line of text from the PORT, and returns it as a string.
;	A line is a (possibly empty) sequence of characters terminated
;	by CR, CRLF or LF (or even the end of file).
;	The terminating character (or CRLF combination) is removed from
;	the input stream. The terminating character(s) is not a part
;	of the return string either.
;	If EOF is encountered before any character is read, the return
;	value is EOF.
; 
;	The optional argument PORT defaults to the current input port.

(define *read-line-breaks* (list char-newline char-return '*eof*))

(define-opt (read-text-line (optional (port (current-input-port))) )
  (if (eof-object? (peek-char port)) (peek-char port)
    (let* ((line
             (next-token '() *read-line-breaks*
			 "reading a line" port))
           (c (read-char port)))	; must be either \n or \r or EOF
       (and (eqv? c char-return) (eqv? (peek-char port) #\newline)
         (read-char port))			; skip \n that follows \r
       line)))


; -- procedure+: read-string N [PORT]
;	Reads N characters from the PORT, and  returns them in a string.
;	If EOF is encountered before N characters are read, a shorter string
;	will be returned.
;	If N is not positive, an empty string will be returned.
;	The optional argument PORT defaults to the current input port.

(define-opt (read-string n (optional (port (current-input-port))) )
  (if (not (positive? n)) ""
    (let ((buffer (make-string n)))
      (let loop ((i 0) (c (read-char port)))
        (if (eof-object? c) (substring buffer 0 i)
          (let ((i1 (inc i)))
            (string-set! buffer i c)
            (if (= i1 n) buffer
              (loop i1 (read-char port)))))))))

; 			Catcher of errors
; catches an error when it occurs, preventing an abort that would otherwise
; follow.
; It is very useful in the validation code to make sure a test case that
; was supposed to fail fails indeed. An expected error would not
; cause the inferior REPL be entered, nor will it abort the execution,
; letting the validation process continue.
;
; $Id: catch-error.scm,v 1.2 2003/10/30 21:33:10 oleg Exp $

	; Try to execute the thunk, and return #f if execution succeeded
	; If an error occurred during the execution, it is caught, and
	; the thunk-failed? procedure returns #t
(define (thunk-failed? thunk)
  (let
    ((orig-error error)		; save the original 'error' primitive
     (caught #f))
    (call-with-current-continuation 
     (lambda (cont)
       (set! error		; redefine the error primitive
             (lambda (msg . args)
               (identify-error msg args "The error above has been caught")
               ;(cerr "caught error: " msg)
               ;(for-each (lambda (arg) (cerr arg)) args) 
               ;(cerr nl)
               (set! caught #t)
               (cont #t)))
       (thunk)
       #f))
    (set! error orig-error)
    caught))

(cond-expand
  ((or bigloo gambit)
    (define-macro (failed? . stmts)
      `(thunk-failed? (lambda () ,@stmts))))
  (else
    (define-syntax failed?
      (syntax-rules ()
	((failed? . stmts)
	  (thunk-failed? (lambda () . stmts)))))))


; like cout << arguments << args
; where argument can be any Scheme object. If it's a procedure
; (without args) it's executed rather than printed (like newline)

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x)
		  (x (current-error-port))
		  (display x (current-error-port))))
            args))

;(##define-macro (nl) '(newline))
(define nl (string #\newline))
; Implementation of the structure parser-errors-vanilla
; for systems other than Scheme48 and PLT Scheme
; $Id: parser-errors-vanilla.scm,v 1.1 2004/08/06 23:01:09 oleg Exp $

(define (parser-error port message . rest)
  (apply error message rest))
; -- Function: find-string-from-port? STR IN-PORT MAX-NO-CHARS
;    Looks for a string STR within the first MAX-NO-CHARS chars of the
;    input port IN-PORT
;    MAX-NO-CHARS may be omitted: in that case, the search span would be
;    limited only by the end of the input stream.
;    When the STR is found, the function returns the number of
;    characters it has read from the port, and the port is set
;    to read the first char after that (that is, after the STR)
;    The function returns #f when the string wasn't found
; Note the function reads the port *STRICTLY* sequentially, and does not
; perform any buffering. So the function can be used even if the port is open
; on a pipe or other communication channel.
;
; Probably can be classified as misc-io.
;
; Notes on the algorithm.
; A special care should be taken in a situation when one had achieved a partial
; match with (a head of) STR, and then some unexpected character appeared in
; the stream. It'll be rash to discard all already read characters. Consider
; an example of string "acab" and the stream "bacacab...", specifically when
;    a  c  a _b_
; b  a  c  a  c  a  b ...
; that is, when 'aca' had matched, but then 'c' showed up in the stream
; while we were looking for 'b'. In that case, discarding all already read
; characters and starting the matching process from scratch, that is,
; from 'c a b ...', would miss a certain match.
; Note, we don't actually need to keep already read characters, or at least
; strlen(str) characters in some kind of buffer. If there has been no match,
; we can safely discard read characters. If there was some partial match,
; we already know the characters before, they are in the STR itself, so
; we don't need a special buffer for that.

;;; "MISCIO" Search for string from port.
; Written 1995 by Oleg Kiselyov (oleg@ponder.csci.unt.edu)
; Modified 1996 by A. Jaffer (jaffer@ai.mit.edu)
;
; This code is in the public domain.

(define (MISCIO:find-string-from-port? str <input-port> . max-no-char)
  (set! max-no-char (if (null? max-no-char) #f (car max-no-char)))
  (letrec
      ((no-chars-read 0)
       (my-peek-char			; Return a peeked char or #f
	(lambda () (and (or (not max-no-char) (< no-chars-read max-no-char))
			(let ((c (peek-char <input-port>)))
			  (if (eof-object? c) #f c)))))
       (next-char (lambda () (read-char <input-port>)
			  (set! no-chars-read  (inc no-chars-read))))
       (match-1st-char			; of the string str
	(lambda ()
	  (let ((c (my-peek-char)))
	    (if (not c) #f
		(begin (next-char)
		       (if (char=? c (string-ref str 0))
			   (match-other-chars 1)
			   (match-1st-char)))))))
       ;; There has been a partial match, up to the point pos-to-match
       ;; (for example, str[0] has been found in the stream)
       ;; Now look to see if str[pos-to-match] for would be found, too
       (match-other-chars
	(lambda (pos-to-match)
	  (if (>= pos-to-match (string-length str))
	      no-chars-read		; the entire string has matched
	      (let ((c (my-peek-char)))
		(and c
		     (if (not (char=? c (string-ref str pos-to-match)))
			 (backtrack 1 pos-to-match)
			 (begin (next-char)
				(match-other-chars (inc pos-to-match)))))))))

       ;; There had been a partial match, but then a wrong char showed up.
       ;; Before discarding previously read (and matched) characters, we check
       ;; to see if there was some smaller partial match. Note, characters read
       ;; so far (which matter) are those of str[0..matched-substr-len - 1]
       ;; In other words, we will check to see if there is such i>0 that
       ;; substr(str,0,j) = substr(str,i,matched-substr-len)
       ;; where j=matched-substr-len - i
       (backtrack
	(lambda (i matched-substr-len)
	  (let ((j (- matched-substr-len i)))
	    (if (<= j 0)
	      (match-1st-char)	; backed off completely to the begining of str
	      (let loop ((k 0))
	        (if (>= k j)
	           (match-other-chars j) ; there was indeed a shorter match
	           (if (char=? (string-ref str k)
	           	       (string-ref str (+ i k)))
	             (loop (inc k))
	             (backtrack (inc i) matched-substr-len))))))))
       )
    (match-1st-char)))

(define find-string-from-port? MISCIO:find-string-from-port?)


;-----------------------------------------------------------------------------
;   This is a test driver for miscio:find-string-from-port?, to make sure it
;			really works as intended

; moved to vinput-parse.scm
(define-syntax run-test (syntax-rules (define) ((run-test "scan-exp" (define vars body)) (define vars (run-test "scan-exp" body))) ((run-test "scan-exp" ?body) (letrec-syntax ((scan-exp (syntax-rules (quote . `!) ((scan-exp '() (k-head ! . args)) (k-head '() . args)) ((scan-exp '(hd . tl) k) (scan-lit-lst (hd . tl) (do-wrap ! . `k))) ((scan-exp `(hd . tl) k) (scan-lit-lst (hd . tl) (do-wrap ! . `k))) ((scan-exp 'x (k-head ! . args)) (k-head (if (string? 'x) (string->symbol 'x) 'x) . args)) ((scan-exp (hd . tl) k) (scan-exp hd (do-tl ! scan-exp tl k))) ((scan-exp x (k-head ! . args)) (k-head x . args)))) (do-tl (syntax-rules (!) ((do-tl processed-hd fn () (k-head ! . args)) (k-head (processed-hd) . args)) ((do-tl processed-hd fn old-tl k) (fn old-tl (do-cons ! processed-hd k))))) (do-cons (syntax-rules (!) ((do-cons processed-tl processed-hd (k-head ! . args)) (k-head (processed-hd . processed-tl) . args)))) (do-wrap (syntax-rules (!) ((do-wrap val fn (k-head ! . args)) (k-head (fn val) . args)))) (do-finish (syntax-rules () ((do-finish new-body) new-body))) (scan-lit-lst (syntax-rules (quote unquote . ,@!) ((scan-lit-lst '() (k-head ! . args)) (k-head '() . args)) ((scan-lit-lst '(hd . tl) k) (do-tl quote scan-lit-lst ((hd . tl)) k)) ((scan-lit-lst ,x k) (scan-exp x (do-wrap ! . ,k))) ((scan-lit-lst ,@x k) (scan-exp x (do-wrap ! . ,@k))) ((scan-lit-lst 'x (k-head ! . args)) (k-head ,(if (string? 'x) (string->symbol 'x) 'x) . args)) ((scan-lit-lst (hd . tl) k) (scan-lit-lst hd (do-tl ! scan-lit-lst tl k))) ((scan-lit-lst x (k-head ! . args)) (k-head x . args))))) (scan-exp ?body (do-finish !)))) ((run-test body ...) (begin (run-test "scan-exp" body) ...))))
(define (make-xml-token kind head) (cons kind head))
(define xml-token? pair?)
(define-syntax xml-token-kind (syntax-rules () ((xml-token-kind token) (car token))))
(define-syntax xml-token-head (syntax-rules () ((xml-token-head token) (cdr token))))
(define (string-whitespace? str) (let ((len (string-length str))) (cond ((zero? len) #t) ((= 1 len) (char-whitespace? (string-ref str 0))) ((= 2 len) (and (char-whitespace? (string-ref str 0)) (char-whitespace? (string-ref str 1)))) (else (let loop ((i 0)) (or (>= i len) (and (char-whitespace? (string-ref str i)) (loop (inc i)))))))))
(define (assq-values val alist) (let loop ((alist alist) (scanned '())) (cond ((null? alist) (values #f scanned)) ((equal? val (caar alist)) (values (car alist) (append scanned (cdr alist)))) (else (loop (cdr alist) (cons (car alist) scanned))))))
(define (fold-right kons knil lis1) (let recur ((lis lis1)) (if (null? lis) knil (let ((head (car lis))) (kons head (recur (cdr lis)))))))
(define (fold kons knil lis1) (let lp ((lis lis1) (ans knil)) (if (null? lis) ans (lp (cdr lis) (kons (car lis) ans)))))
(define ssax:S-chars (map ascii->char '(32 10 9 13)))
(define (ssax:skip-S port) (skip-while ssax:S-chars port))
(define (ssax:ncname-starting-char? a-char) (and (char? a-char) (or (char-alphabetic? a-char) (char=? #\_ a-char))))
(define (ssax:read-NCName port) (let ((first-char (peek-char port))) (or (ssax:ncname-starting-char? first-char) (parser-error port "XMLNS [4] for '" first-char "'"))) (string->symbol (next-token-of (lambda (c) (cond ((eof-object? c) #f) ((char-alphabetic? c) c) ((string-index "0123456789.-_" c) c) (else #f))) port)))
(define (ssax:read-QName port) (let ((prefix-or-localpart (ssax:read-NCName port))) (case (peek-char port) ((#\:) (read-char port) (cons prefix-or-localpart (ssax:read-NCName port))) (else prefix-or-localpart))))
(define ssax:Prefix-XML (string->symbol "xml"))
(define name-compare (letrec ((symbol-compare (lambda (symb1 symb2) (cond ((eq? symb1 symb2) '=) ((string<? (symbol->string symb1) (symbol->string symb2)) '<) (else '>))))) (lambda (name1 name2) (cond ((symbol? name1) (if (symbol? name2) (symbol-compare name1 name2) '<)) ((symbol? name2) '>) ((eq? name2 ssax:largest-unres-name) '<) ((eq? name1 ssax:largest-unres-name) '>) ((eq? (car name1) (car name2)) (symbol-compare (cdr name1) (cdr name2))) (else (symbol-compare (car name1) (car name2)))))))
(define ssax:largest-unres-name (cons (string->symbol "#LARGEST-SYMBOL") (string->symbol "#LARGEST-SYMBOL")))
(define ssax:read-markup-token (let () (define (skip-comment port) (assert-curr-char '(#\-) "XML [15], second dash" port) (if (not (find-string-from-port? "-->" port)) (parser-error port "XML [15], no -->")) (make-xml-token 'COMMENT #f)) (define (read-cdata port) (assert (string=? "CDATA[" (read-string 6 port))) (make-xml-token 'CDSECT #f)) (lambda (port) (assert-curr-char '(#\<) "start of the token" port) (case (peek-char port) ((#\/) (read-char port) (begin0 (make-xml-token 'END (ssax:read-QName port)) (ssax:skip-S port) (assert-curr-char '(#\>) "XML [42]" port))) ((#\?) (read-char port) (make-xml-token 'PI (ssax:read-NCName port))) ((#\!) (case (peek-next-char port) ((#\-) (read-char port) (skip-comment port)) ((#\[) (read-char port) (read-cdata port)) (else (make-xml-token 'DECL (ssax:read-NCName port))))) (else (make-xml-token 'START (ssax:read-QName port)))))))
(define (ssax:skip-pi port) (if (not (find-string-from-port? "?>" port)) (parser-error port "Failed to find ?> terminating the PI")))
(define (ssax:read-pi-body-as-string port) (ssax:skip-S port) (string-concatenate/shared (let loop () (let ((pi-fragment (next-token '() '(#\?) "reading PI content" port))) (if (eqv? #\> (peek-next-char port)) (begin (read-char port) (cons pi-fragment '())) (cons* pi-fragment "?" (loop)))))))
(define (ssax:skip-internal-dtd port) (if (not (find-string-from-port? "]>" port)) (parser-error port "Failed to find ]> terminating the internal DTD subset")))
(define ssax:read-cdata-body (let ((cdata-delimiters (list char-return #\newline #\] #\&))) (lambda (port str-handler seed) (let loop ((seed seed)) (let ((fragment (next-token '() cdata-delimiters "reading CDATA" port))) (case (read-char port) ((#\newline) (loop (str-handler fragment nl seed))) ((#\]) (if (not (eqv? (peek-char port) #\])) (loop (str-handler fragment "]" seed)) (let check-after-second-braket ((seed (if (string-null? fragment) seed (str-handler fragment "" seed)))) (case (peek-next-char port) ((#\>) (read-char port) seed) ((#\]) (check-after-second-braket (str-handler "]" "" seed))) (else (loop (str-handler "]]" "" seed))))))) ((#\&) (let ((ent-ref (next-token-of (lambda (c) (and (not (eof-object? c)) (char-alphabetic? c) c)) port))) (cond ((and (string=? "gt" ent-ref) (eqv? (peek-char port) #\;)) (read-char port) (loop (str-handler fragment ">" seed))) (else (loop (str-handler ent-ref "" (str-handler fragment "&" seed))))))) (else (if (eqv? (peek-char port) #\newline) (read-char port)) (loop (str-handler fragment nl seed)))))))))
(define (ssax:read-char-ref port) (let* ((base (cond ((eqv? (peek-char port) #\x) (read-char port) 16) (else 10))) (name (next-token '() '(#\;) "XML [66]" port)) (char-code (string->number name base))) (read-char port) (if (integer? char-code) (ucscode->char char-code) (parser-error port "[wf-Legalchar] broken for '" name "'"))))
(define ssax:predefined-parsed-entities `((,(string->symbol "amp") . "&") (,(string->symbol "lt") . "<") (,(string->symbol "gt") . ">") (,(string->symbol "apos") . "'") (,(string->symbol "quot") . "\"")))
(define (ssax:handle-parsed-entity port name entities content-handler str-handler seed) (cond ((assq name entities) => (lambda (decl-entity) (let ((ent-body (cdr decl-entity)) (new-entities (cons (cons name #f) entities))) (cond ((string? ent-body) (call-with-input-string ent-body (lambda (port) (content-handler port new-entities seed)))) ((procedure? ent-body) (let ((port (ent-body))) (begin0 (content-handler port new-entities seed) (close-input-port port)))) (else (parser-error port "[norecursion] broken for " name)))))) ((assq name ssax:predefined-parsed-entities) => (lambda (decl-entity) (str-handler (cdr decl-entity) "" seed))) (else (parser-error port "[wf-entdeclared] broken for " name))))
(define (make-empty-attlist) '())
(define (attlist-add attlist name-value) (if (null? attlist) (cons name-value attlist) (case (name-compare (car name-value) (caar attlist)) ((=) #f) ((<) (cons name-value attlist)) (else (cons (car attlist) (attlist-add (cdr attlist) name-value))))))
(define attlist-null? null?)
(define (attlist-remove-top attlist) (values (car attlist) (cdr attlist)))
(define (attlist->alist attlist) attlist)
(define attlist-fold fold)
(define ssax:read-attributes (let ((value-delimeters (append ssax:S-chars '(#\< #\&)))) (define (read-attrib-value delimiter port entities prev-fragments) (let* ((new-fragments (cons (next-token '() (cons delimiter value-delimeters) "XML [10]" port) prev-fragments)) (cterm (read-char port))) (cond ((or (eof-object? cterm) (eqv? cterm delimiter)) new-fragments) ((eqv? cterm char-return) (if (eqv? (peek-char port) #\newline) (read-char port)) (read-attrib-value delimiter port entities (cons " " new-fragments))) ((memv cterm ssax:S-chars) (read-attrib-value delimiter port entities (cons " " new-fragments))) ((eqv? cterm #\&) (cond ((eqv? (peek-char port) #\#) (read-char port) (read-attrib-value delimiter port entities (cons (string (ssax:read-char-ref port)) new-fragments))) (else (read-attrib-value delimiter port entities (read-named-entity port entities new-fragments))))) (else (parser-error port "[CleanAttrVals] broken"))))) (define (read-named-entity port entities fragments) (let ((name (ssax:read-NCName port))) (assert-curr-char '(#\;) "XML [68]" port) (ssax:handle-parsed-entity port name entities (lambda (port entities fragments) (read-attrib-value '*eof* port entities fragments)) (lambda (str1 str2 fragments) (if (equal? "" str2) (cons str1 fragments) (cons* str2 str1 fragments))) fragments))) (lambda (port entities) (let loop ((attr-list (make-empty-attlist))) (if (not (ssax:ncname-starting-char? (ssax:skip-S port))) attr-list (let ((name (ssax:read-QName port))) (ssax:skip-S port) (assert-curr-char '(#\=) "XML [25]" port) (ssax:skip-S port) (let ((delimiter (assert-curr-char '(#\' #\") "XML [10]" port))) (loop (or (attlist-add attr-list (cons name (string-concatenate-reverse/shared (read-attrib-value delimiter port entities '())))) (parser-error port "[uniqattspec] broken for " name))))))))))
(define (ssax:resolve-name port unres-name namespaces apply-default-ns?) (cond ((pair? unres-name) (cons (cond ((assq (car unres-name) namespaces) => cadr) ((eq? (car unres-name) ssax:Prefix-XML) ssax:Prefix-XML) (else (parser-error port "[nsc-NSDeclared] broken; prefix " (car unres-name)))) (cdr unres-name))) (apply-default-ns? (let ((default-ns (assq '*DEFAULT* namespaces))) (if (and default-ns (cadr default-ns)) (cons (cadr default-ns) unres-name) unres-name))) (else unres-name)))
(define (ssax:uri-string->symbol uri-str) (string->symbol uri-str))
(define ssax:complete-start-tag (let ((xmlns (string->symbol "xmlns")) (largest-dummy-decl-attr (list ssax:largest-unres-name #f #f #f))) (define (validate-attrs port attlist decl-attrs) (define (add-default-decl decl-attr result) (let*-values (((attr-name content-type use-type default-value) (apply values decl-attr))) (and (eq? use-type 'REQUIRED) (parser-error port "[RequiredAttr] broken for" attr-name)) (if default-value (cons (cons attr-name default-value) result) result))) (let loop ((attlist attlist) (decl-attrs decl-attrs) (result '())) (if (attlist-null? attlist) (attlist-fold add-default-decl result decl-attrs) (let*-values (((attr attr-others) (attlist-remove-top attlist)) ((decl-attr other-decls) (if (attlist-null? decl-attrs) (values largest-dummy-decl-attr decl-attrs) (attlist-remove-top decl-attrs)))) (case (name-compare (car attr) (car decl-attr)) ((<) (if (or (eq? xmlns (car attr)) (and (pair? (car attr)) (eq? xmlns (caar attr)))) (loop attr-others decl-attrs (cons attr result)) (parser-error port "[ValueType] broken for " attr))) ((>) (loop attlist other-decls (add-default-decl decl-attr result))) (else (let*-values (((attr-name content-type use-type default-value) (apply values decl-attr))) (cond ((eq? use-type 'FIXED) (or (equal? (cdr attr) default-value) (parser-error port "[FixedAttr] broken for " attr-name))) ((eq? content-type 'CDATA) #t) ((pair? content-type) (or (member (cdr attr) content-type) (parser-error port "[enum] broken for " attr-name "=" (cdr attr)))) (else (ssax:warn port "declared content type " content-type " not verified yet"))) (loop attr-others other-decls (cons attr result))))))))) (define (add-ns port prefix uri-str namespaces) (and (equal? "" uri-str) (parser-error port "[dt-NSName] broken for " prefix)) (let ((uri-symbol (ssax:uri-string->symbol uri-str))) (let loop ((nss namespaces)) (cond ((null? nss) (cons (cons* prefix uri-symbol uri-symbol) namespaces)) ((eq? uri-symbol (cddar nss)) (cons (cons* prefix (cadar nss) uri-symbol) namespaces)) (else (loop (cdr nss))))))) (define (adjust-namespace-decl port attrs namespaces) (let loop ((attrs attrs) (proper-attrs '()) (namespaces namespaces)) (cond ((null? attrs) (values proper-attrs namespaces)) ((eq? xmlns (caar attrs)) (loop (cdr attrs) proper-attrs (if (equal? "" (cdar attrs)) (cons (cons* '*DEFAULT* #f #f) namespaces) (add-ns port '*DEFAULT* (cdar attrs) namespaces)))) ((and (pair? (caar attrs)) (eq? xmlns (caaar attrs))) (loop (cdr attrs) proper-attrs (add-ns port (cdaar attrs) (cdar attrs) namespaces))) (else (loop (cdr attrs) (cons (car attrs) proper-attrs) namespaces))))) (lambda (tag-head port elems entities namespaces) (let*-values (((attlist) (ssax:read-attributes port entities)) ((empty-el-tag?) (begin (ssax:skip-S port) (and (eqv? #\/ (assert-curr-char '(#\> #\/) "XML [40], XML [44], no '>'" port)) (assert-curr-char '(#\>) "XML [44], no '>'" port)))) ((elem-content decl-attrs) (if elems (cond ((assoc tag-head elems) => (lambda (decl-elem) (values (if empty-el-tag? 'EMPTY-TAG (cadr decl-elem)) (caddr decl-elem)))) (else (parser-error port "[elementvalid] broken, no decl for " tag-head))) (values (if empty-el-tag? 'EMPTY-TAG 'ANY) #f))) ((merged-attrs) (if decl-attrs (validate-attrs port attlist decl-attrs) (attlist->alist attlist))) ((proper-attrs namespaces) (adjust-namespace-decl port merged-attrs namespaces))) (values (ssax:resolve-name port tag-head namespaces #t) (fold-right (lambda (name-value attlist) (or (attlist-add attlist (cons (ssax:resolve-name port (car name-value) namespaces #f) (cdr name-value))) (parser-error port "[uniqattspec] after NS expansion broken for " name-value))) (make-empty-attlist) proper-attrs) namespaces elem-content)))))
(define (ssax:read-external-id port) (let ((discriminator (ssax:read-NCName port))) (assert-curr-char ssax:S-chars "space after SYSTEM or PUBLIC" port) (ssax:skip-S port) (let ((delimiter (assert-curr-char '(#\' #\") "XML [11], XML [12]" port))) (cond ((eq? discriminator (string->symbol "SYSTEM")) (begin0 (next-token '() (list delimiter) "XML [11]" port) (read-char port))) ((eq? discriminator (string->symbol "PUBLIC")) (skip-until (list delimiter) port) (assert-curr-char ssax:S-chars "space after PubidLiteral" port) (ssax:skip-S port) (let* ((delimiter (assert-curr-char '(#\' #\") "XML [11]" port)) (systemid (next-token '() (list delimiter) "XML [11]" port))) (read-char port) systemid)) (else (parser-error port "XML [75], " discriminator " rather than SYSTEM or PUBLIC"))))))
(define (ssax:scan-Misc port) (let loop ((c (ssax:skip-S port))) (cond ((eof-object? c) c) ((not (char=? c #\<)) (parser-error port "XML [22], char '" c "' unexpected")) (else (let ((token (ssax:read-markup-token port))) (case (xml-token-kind token) ((COMMENT) (loop (ssax:skip-S port))) ((PI DECL START) token) (else (parser-error port "XML [22], unexpected token of kind " (xml-token-kind token)))))))))
(define ssax:read-char-data (let ((terminators-usual (list #\< #\& char-return)) (terminators-usual-eof (list #\< '*eof* #\& char-return)) (handle-fragment (lambda (fragment str-handler seed) (if (string-null? fragment) seed (str-handler fragment "" seed))))) (lambda (port expect-eof? str-handler seed) (if (eqv? #\< (peek-char port)) (let ((token (ssax:read-markup-token port))) (case (xml-token-kind token) ((START END) (values seed token)) ((CDSECT) (let ((seed (ssax:read-cdata-body port str-handler seed))) (ssax:read-char-data port expect-eof? str-handler seed))) ((COMMENT) (ssax:read-char-data port expect-eof? str-handler seed)) (else (values seed token)))) (let ((char-data-terminators (if expect-eof? terminators-usual-eof terminators-usual))) (let loop ((seed seed)) (let* ((fragment (next-token '() char-data-terminators "reading char data" port)) (term-char (peek-char port))) (if (eof-object? term-char) (values (handle-fragment fragment str-handler seed) term-char) (case term-char ((#\<) (let ((token (ssax:read-markup-token port))) (case (xml-token-kind token) ((CDSECT) (loop (ssax:read-cdata-body port str-handler (handle-fragment fragment str-handler seed)))) ((COMMENT) (loop (handle-fragment fragment str-handler seed))) (else (values (handle-fragment fragment str-handler seed) token))))) ((#\&) (case (peek-next-char port) ((#\#) (read-char port) (loop (str-handler fragment (string (ssax:read-char-ref port)) seed))) (else (let ((name (ssax:read-NCName port))) (assert-curr-char '(#\;) "XML [68]" port) (values (handle-fragment fragment str-handler seed) (make-xml-token 'ENTITY-REF name)))))) (else (if (eqv? (peek-next-char port) #\newline) (read-char port)) (loop (str-handler fragment (string #\newline) seed))))))))))))
(define (ssax:assert-token token kind gi error-cont) (or (and (xml-token? token) (eq? kind (xml-token-kind token)) (equal? gi (xml-token-head token))) (error-cont token kind gi)))
(define-syntax ssax:make-pi-parser (syntax-rules () ((ssax:make-pi-parser orig-handlers) (letrec-syntax ((loop (syntax-rules (*DEFAULT*) ((loop () #f accum port target seed) (make-case ((else (ssax:warn port "Skipping PI: " target nl) (ssax:skip-pi port) seed) . accum) () target)) ((loop () default accum port target seed) (make-case ((else (default port target seed)) . accum) () target)) ((loop ((*DEFAULT* . default) . handlers) old-def accum port target seed) (loop handlers default accum port target seed)) ((loop ((tag . handler) . handlers) default accum port target seed) (loop handlers default (((tag) (handler port target seed)) . accum) port target seed)))) (make-case (syntax-rules () ((make-case () clauses target) (case target . clauses)) ((make-case (clause . clauses) accum target) (make-case clauses (clause . accum) target))))) (lambda (port target seed) (loop orig-handlers #f () port target seed))))))
(define-syntax ssax:make-elem-parser (syntax-rules () ((ssax:make-elem-parser my-new-level-seed my-finish-element my-char-data-handler my-pi-handlers) (lambda (start-tag-head port elems entities namespaces preserve-ws? seed) (define xml-space-gi (cons ssax:Prefix-XML (string->symbol "space"))) (let handle-start-tag ((start-tag-head start-tag-head) (port port) (entities entities) (namespaces namespaces) (preserve-ws? preserve-ws?) (parent-seed seed)) (let*-values (((elem-gi attributes namespaces expected-content) (ssax:complete-start-tag start-tag-head port elems entities namespaces)) ((seed) (my-new-level-seed elem-gi attributes namespaces expected-content parent-seed))) (case expected-content ((EMPTY-TAG) (my-finish-element elem-gi attributes namespaces parent-seed seed)) ((EMPTY) (ssax:assert-token (and (eqv? #\< (ssax:skip-S port)) (ssax:read-markup-token port)) 'END start-tag-head (lambda (token exp-kind exp-head) (parser-error port "[elementvalid] broken for " token " while expecting " exp-kind exp-head))) (my-finish-element elem-gi attributes namespaces parent-seed seed)) (else (let ((preserve-ws? (cond ((assoc xml-space-gi attributes) => (lambda (name-value) (equal? "preserve" (cdr name-value)))) (else preserve-ws?)))) (let loop ((port port) (entities entities) (expect-eof? #f) (seed seed)) (let*-values (((seed term-token) (ssax:read-char-data port expect-eof? my-char-data-handler seed))) (if (eof-object? term-token) seed (case (xml-token-kind term-token) ((END) (ssax:assert-token term-token 'END start-tag-head (lambda (token exp-kind exp-head) (parser-error port "[GIMatch] broken for " term-token " while expecting " exp-kind exp-head))) (my-finish-element elem-gi attributes namespaces parent-seed seed)) ((PI) (let ((seed ((ssax:make-pi-parser my-pi-handlers) port (xml-token-head term-token) seed))) (loop port entities expect-eof? seed))) ((ENTITY-REF) (let ((seed (ssax:handle-parsed-entity port (xml-token-head term-token) entities (lambda (port entities seed) (loop port entities #t seed)) my-char-data-handler seed))) (loop port entities expect-eof? seed))) ((START) (if (eq? expected-content 'PCDATA) (parser-error port "[elementvalid] broken for " elem-gi " with char content only; unexpected token " term-token)) (let ((seed (handle-start-tag (xml-token-head term-token) port entities namespaces preserve-ws? seed))) (loop port entities expect-eof? seed))) (else (parser-error port "XML [43] broken for " term-token)))))))))))))))
(define-syntax ssax:make-parser/positional-args (syntax-rules () ((ssax:make-parser/positional-args *handler-DOCTYPE *handler-UNDECL-ROOT *handler-DECL-ROOT *handler-NEW-LEVEL-SEED *handler-FINISH-ELEMENT *handler-CHAR-DATA-HANDLER *handler-PI) (lambda (port seed) (define (handle-decl port token-head seed) (or (eq? (string->symbol "DOCTYPE") token-head) (parser-error port "XML [22], expected DOCTYPE declaration, found " token-head)) (assert-curr-char ssax:S-chars "XML [28], space after DOCTYPE" port) (ssax:skip-S port) (let*-values (((docname) (ssax:read-QName port)) ((systemid) (and (ssax:ncname-starting-char? (ssax:skip-S port)) (ssax:read-external-id port))) ((internal-subset?) (begin (ssax:skip-S port) (eqv? #\[ (assert-curr-char '(#\> #\[) "XML [28], end-of-DOCTYPE" port)))) ((elems entities namespaces seed) (*handler-DOCTYPE port docname systemid internal-subset? seed))) (scan-for-significant-prolog-token-2 port elems entities namespaces seed))) (define (scan-for-significant-prolog-token-1 port seed) (let ((token (ssax:scan-Misc port))) (if (eof-object? token) (parser-error port "XML [22], unexpected EOF") (case (xml-token-kind token) ((PI) (let ((seed ((ssax:make-pi-parser *handler-PI) port (xml-token-head token) seed))) (scan-for-significant-prolog-token-1 port seed))) ((DECL) (handle-decl port (xml-token-head token) seed)) ((START) (let*-values (((elems entities namespaces seed) (*handler-UNDECL-ROOT (xml-token-head token) seed))) (element-parser (xml-token-head token) port elems entities namespaces #f seed))) (else (parser-error port "XML [22], unexpected markup " token)))))) (define (scan-for-significant-prolog-token-2 port elems entities namespaces seed) (let ((token (ssax:scan-Misc port))) (if (eof-object? token) (parser-error port "XML [22], unexpected EOF") (case (xml-token-kind token) ((PI) (let ((seed ((ssax:make-pi-parser *handler-PI) port (xml-token-head token) seed))) (scan-for-significant-prolog-token-2 port elems entities namespaces seed))) ((START) (element-parser (xml-token-head token) port elems entities namespaces #f (*handler-DECL-ROOT (xml-token-head token) seed))) (else (parser-error port "XML [22], unexpected markup " token)))))) (define element-parser (ssax:make-elem-parser *handler-NEW-LEVEL-SEED *handler-FINISH-ELEMENT *handler-CHAR-DATA-HANDLER *handler-PI)) (scan-for-significant-prolog-token-1 port seed)))))
(define-syntax ssax:define-labeled-arg-macro (syntax-rules () ((ssax:define-labeled-arg-macro labeled-arg-macro-name (positional-macro-name (arg-name . arg-def) ...)) (define-syntax labeled-arg-macro-name (syntax-rules () ((labeled-arg-macro-name . kw-val-pairs) (letrec-syntax ((find (syntax-rules (arg-name ...) ((find k-args (arg-name . default) arg-name val . others) (next val . k-args)) ... ((find k-args key arg-no-match-name val . others) (find k-args key . others)) ((find k-args (arg-name default)) (next default . k-args)) ...)) (next (syntax-rules () ((next val vals key . keys) (find ((val . vals) . keys) key . kw-val-pairs)) ((next val vals) (rev-apply (val) vals)))) (rev-apply (syntax-rules () ((rev-apply form (x . xs)) (rev-apply (x . form) xs)) ((rev-apply form ()) form)))) (next positional-macro-name () (arg-name . arg-def) ...))))))))
(ssax:define-labeled-arg-macro ssax:make-parser (ssax:make-parser/positional-args (DOCTYPE (lambda (port docname systemid internal-subset? seed) (when internal-subset? (ssax:warn port "Internal DTD subset is not currently handled ") (ssax:skip-internal-dtd port)) (ssax:warn port "DOCTYPE DECL " docname " " systemid " found and skipped") (values #f '() '() seed))) (UNDECL-ROOT (lambda (elem-gi seed) (values #f '() '() seed))) (DECL-ROOT (lambda (elem-gi seed) seed)) (NEW-LEVEL-SEED) (FINISH-ELEMENT) (CHAR-DATA-HANDLER) (PI ())))
(define (ssax:reverse-collect-str fragments) (cond ((null? fragments) '()) ((null? (cdr fragments)) fragments) (else (let loop ((fragments fragments) (result '()) (strs '())) (cond ((null? fragments) (if (null? strs) result (cons (string-concatenate/shared strs) result))) ((string? (car fragments)) (loop (cdr fragments) result (cons (car fragments) strs))) (else (loop (cdr fragments) (cons (car fragments) (if (null? strs) result (cons (string-concatenate/shared strs) result))) '())))))))
(define (ssax:reverse-collect-str-drop-ws fragments) (cond ((null? fragments) '()) ((null? (cdr fragments)) (if (and (string? (car fragments)) (string-whitespace? (car fragments))) '() fragments)) (else (let loop ((fragments fragments) (result '()) (strs '()) (all-whitespace? #t)) (cond ((null? fragments) (if all-whitespace? result (cons (string-concatenate/shared strs) result))) ((string? (car fragments)) (loop (cdr fragments) result (cons (car fragments) strs) (and all-whitespace? (string-whitespace? (car fragments))))) (else (loop (cdr fragments) (cons (car fragments) (if all-whitespace? result (cons (string-concatenate/shared strs) result))) '() #t)))))))
(define (ssax:xml->sxml port namespace-prefix-assig) (letrec ((namespaces (map (lambda (el) (cons* #f (car el) (ssax:uri-string->symbol (cdr el)))) namespace-prefix-assig)) (RES-NAME->SXML (lambda (res-name) (string->symbol (string-append (symbol->string (car res-name)) ":" (symbol->string (cdr res-name))))))) (let ((result (reverse ((ssax:make-parser NEW-LEVEL-SEED (lambda (elem-gi attributes namespaces expected-content seed) '()) FINISH-ELEMENT (lambda (elem-gi attributes namespaces parent-seed seed) (let ((seed (ssax:reverse-collect-str-drop-ws seed)) (attrs (attlist-fold (lambda (attr accum) (cons (list (if (symbol? (car attr)) (car attr) (RES-NAME->SXML (car attr))) (cdr attr)) accum)) '() attributes))) (cons (cons (if (symbol? elem-gi) elem-gi (RES-NAME->SXML elem-gi)) (if (null? attrs) seed (cons (cons '@ attrs) seed))) parent-seed))) CHAR-DATA-HANDLER (lambda (string1 string2 seed) (if (string-null? string2) (cons string1 seed) (cons* string2 string1 seed))) DOCTYPE (lambda (port docname systemid internal-subset? seed) (when internal-subset? (ssax:warn port "Internal DTD subset is not currently handled ") (ssax:skip-internal-dtd port)) (ssax:warn port "DOCTYPE DECL " docname " " systemid " found and skipped") (values #f '() namespaces seed)) UNDECL-ROOT (lambda (elem-gi seed) (values #f '() namespaces seed)) PI ((*DEFAULT* lambda (port pi-tag seed) (cons (list '*PI* pi-tag (ssax:read-pi-body-as-string port)) seed)))) port '())))) (cons '*TOP* (if (null? namespace-prefix-assig) result (cons (list '@ (cons '*NAMESPACES* (map (lambda (ns) (list (car ns) (cdr ns))) namespace-prefix-assig))) result))))))
(define SSAX:XML->SXML ssax:xml->sxml)







(define current-error-port console-output-port)
(define (ssax:warn port msg . other-msg)
  (apply cerr (cons* nl "Warning: " msg other-msg)))

