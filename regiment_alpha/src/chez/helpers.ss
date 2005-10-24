
(module helpers   
	;; Remember to update the plt equivalent when you update this:
	(;; Syntax:
	  mvlet let-match (match-lambda match-lambda-helper)
	  ^ ;; Exponentiation
	   
	  ;; For plt compat:
	  foldl

	  make-default-hash-table hashtab-get hashtab-set! hashtab-for-each hashtab-remove!
	  make-n-list
	  id
	  substring? periodic-display all-equal?

	  with-error-handlers with-warning-handler
	  current-error-port

	  ;; Values:	    
	  gnuplot display-progress-meter count-nodes
	  
	  unique-name unique-name-counter extract-suffix make-begin strip-illegal deunique-name 
	  get-formals

	  ;; Hmm, not sure what meaning immediate has here...
	  ;immediate? 
	  constant? datum? 
	  formalexp? cast-formals default-unit-tester tester-eq?
	  ;default-unit-tester-retries

	  regiment-primitives regiment-primitive? 
	  token-machine-primitives token-machine-primitive? 
	  token-machine? token-machine->program token-machine-keyword?
	  basic-primitive? distributed-primitive?
	  get-primitive-entry regiment-constants regiment-constant? ;get-primitive-arity
	  get-primitive-return-type
	  map-prim-w-types

	  ;; Token names:
	  token-name? new-token-name token-names get-names get-formation-name get-membership-name
	  token->name token->subtok
	  destructure-tokbind

	  set? subset? set-equal? list->set set-cons union intersection difference
	  alist-remove list-remove-first list-remove-last! list-remove-after 
	  filter list-index snoc rac rdc last 
	  list-find-position list-remove-before
	  randomize-list  insert-between iota disp crit-printf
	  extract-file-extension remove-file-extension file->string string->file file->slist slist->file pad-width
	  graph-map graph-get-connected-component graph-neighbors ;cyclic? 
	  graph:simple->vertical graph:vertical->simple
	  deep-assq deep-assq-all deep-member? deep-all-matches deep-filter
	  list-get-random unfold-list average clump
	  partition partition-equal split-before
	  myequal?
	  stream? live-stream? stream-empty? stream-car stream-cdr stream-map stream-take 
	  counter-stream stream-append ;random-stream 
	  display-constrained
	  symbol-append 

	  testhelpers testshelpers test-this these-tests

	  reg:random-int reg:get-random-state reg:set-random-state!
					;reg:all-unit-tests 
	  
					;   (all-except (lib "rutils_generic.ss")
					;               list->set union intersection difference set?
					;               list-head filter list-index snoc rac rdc 
					;               insert-between iota disp)
					;   (all-from (lib "rutils_generic.ss") )
					;   (all-from-except (lib "rutils_generic.ss") 
					;                    list->set union intersection difference set?) 


	  )

;(import (except topsort-module test-this these-tests))
(import scheme)

;; This doesn't seem to work in PLT.  Besides, let-values is a perfect
;; substitute.  That's the kind of thing I'd like my
;; scheme-meta-language/package-manager to do for me!!

;;; multiple-value let
(define-syntax mvlet
  (lambda (x)
    (define domvlet
      (lambda (bindings ids tmps body)
        (if (null? bindings)
            `((,#'lambda ,ids ,@body) ,@tmps)
            (syntax-case (car bindings) ()
              [(*ids expr)
               (with-syntax ([*tmps (generate-temporaries #'*ids)])
                 (with-syntax ([body (domvlet (cdr bindings)
                                              (append #'*ids ids)
                                              (append #'*tmps tmps)
                                              body)])
                   #'(call-with-values
                       (lambda () expr)
                       (lambda *tmps body))))]))))
    (syntax-case x ()
      [(_ (((id ...) expr) ...) form ...)
       (andmap (lambda (ls) (andmap identifier? ls))
               #'((id ...) ...))
       (domvlet #'(((id ...) expr) ...) '() '() #'(form ...))])))


(define-syntax let/ec
  (syntax-rules ()
    [(_ v exp ...)
     (call/1cc (lambda (v) exp ...))]))


;(define-syntax define-values
;  (syntax-rules ()
;    [(_ (v ...) exp)
;     (begin 
;       (call-with-values
		
;; ========================================  
;; This defines a *simple* and unified interface into hash-tables.
;; It assumes *equal?* type key equivalence!
;; It returns #f for a failed hashtab-get (which is sloppy, but that's slib)

;; First we require hash-tables from slib:
#;(begin
  (define ___ (require 'hash-table))
  (define (make-default-hash-table) (make-hash-table 5)) ;50))
  (define hashtab-get (hash-inquirer equal?))
  (define hashtab-set! (hash-associator equal?))
  (define hashtab-for-each hash-for-each)
  (define hashtab-remove! (hash-remover equal?)))

;; [2005.10.18]
;; Switching this to chez's native hash tables rather than slib's:
(begin
 (define make-default-hash-table #%make-hash-table)
 (define hashtab-remove! #%remove-hash-table!)
 (define (hashtab-for-each f ht) (#%hash-table-for-each ht f))
 (IFDEBUG  
  (begin
    (define (hashtab-get ht k)
      (if (not (immediate? k)) (error 'hashtab-get "this key is not an atom: ~s" k))
      (#%get-hash-table ht k #f))
    (define (hashtab-set! ht k v)
      (if (not (immediate? k)) (error 'hashtab-set! "this key is not an atom: ~s" k))
      (#%put-hash-table! ht k v)))
  (begin 
    (define (hashtab-get ht k) (#%get-hash-table ht k #f))
    (define hashtab-set! #%put-hash-table!)))
 )

;; This is implementation specific, these are the types for which eq? <=> equal?
(define (immediate? x)
  (or (fixnum? x) 
      ; (flonum? x) ;; Floats don't follow eq? properly.
      ; (char? x)   ;; Chars don't follow eq? properly.
      (symbol? x)
      (null? x)
      (boolean? x)
      ))
      
;; ========================================  



 ;; [2004.06.13] Matches the function defined in plt, provides
 ;; functionality used by the generic code.
 ;; The escape handler had *better* escape.
 (define (with-error-handlers display escape th)
   (parameterize ([#%error-handler (lambda args 
				   (apply display args)
				   (escape))])
		 (th)))
(define (with-warning-handler fun th)
  (parameterize ([#%warning-handler fun])
    (th)))


;; This is too lenient, but there's no other option.
(define promise? procedure?)

(define current-error-port current-output-port)


;; Moved include!

;; We play nasty tricks with symbolic links here. 
;; It doesn't matter if we load this file from "src" or "src/chez"
;; because we've linked the "generic" subdir from both locations.
(include "generic/helpers.ss")

(define (crit-printf . args)
  (critical-section (apply printf args)))


(define make-n-list
  (lambda (n func)
    (letrec ((loop (lambda (n acc)
                     (if (zero? n)
                         acc
                         (loop (sub1 n) (cons (func n) acc))))))
      (loop n '()))))


;; From PLT's list.ss
  (define foldl
     (letrec ((fold-one
               (lambda (f init l)
                 (letrec ((helper
                           (lambda (init l)
                             (cond
			       [(null? l) init]
			       [else (helper (f (car l) init) (cdr l))]))))
                   (helper init l))))
              (fold-n
               (lambda (f init  l)
                 (cond
		   [(ormap null? l)
		    (if (andmap null? l) 
			init
			(error 'foldl "received non-equal length input lists"))]
		   [else (fold-n
			  f
			  (apply f (mapadd car l init))
			  (map cdr l))]))))
       (case-lambda
        [(f init l) (fold-one f init l)]
        [(f init l . ls) (fold-n f init (cons l ls))])))


;; This is a simple random number generator interface for use in this Regiment codebase:
(define reg:random-int
  (case-lambda 
   [() (#%random (#%most-positive-fixnum))]
   [(k) (#%random k)]))
(define (reg:get-random-state) (random-seed)) ;; This doesn't work!!! [2005.10.05]
(define (reg:set-random-state! s) (random-seed s))

)
