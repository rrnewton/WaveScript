#cs ;; Case Sensitivity
(module helpers mzscheme 
  (require "iu-match.ss"
           (lib "include.ss")
           (lib "pretty.ss")
           (all-except (lib "list.ss") filter)
           (all-except "tsort.ss" test-this these-tests)
           
           "constants.ss")
  
  ;; This might not be necessary: 
  ; (require "~/scheme/plt/utils/rutils_generic.ss")
;; Temporarily eliminating:
#;  (require (all-except (lib "rutils_generic.ss")
                       id list->set union intersection difference set?
                       list-head filter list-index snoc rac rdc last
                       insert-between iota disp ))

  ;; Here we include the SLIB initialization directly.  This is the only 
  ;; way I can make slib play nice with the PLT module system at all.
;  (load (build-path (collection-path "slibinit") "init.ss"))
;  (include "/usr/local/plt/collects/slibinit/init.ss")  

    (define-syntax rec
      (syntax-rules ()
	((_ x e) (letrec ((x e)) x))))

    (define-syntax mvlet
      (syntax-rules ()
	  [(mvlet stuff ...) (let-values stuff ...)]))

  (define (define-top-level-value sym obj)
    (eval `(define ,sym ',obj)))
  (define (set-top-level-value! sym obj)
    (eval `(set! ,sym ',obj)))
  (define (top-level-value sym) (eval sym))
  (define (top-level-bound? sym)
    (error 'top-level-bound?
	   "This is a chez function which can't ~a"
	     "be emulated right now in Plt. -RRN"))

  (define flush-output-port flush-output)
  (define pp pretty-print)                   
  (define cpu-time current-process-milliseconds)
  (define print-level pretty-print-depth)
  ;(define print-length pretty-print-length)
  ;; Can't adjust the length from PLT:
  (define print-length (lambda _ #f))

;; Temporary! < FIXME>:
  (define crit-printf printf)

  ;; This isn't working right now.
  (define (with-error-handlers display escape th)
      (call/cc (lambda (out)
                 (parameterize ([error-display-handler display]
                                [error-escape-handler 
                                 (lambda args
                                   (let ((result (apply escape args)))
                                   ;; If the escape procedure is not called, we must destroy the 
                                     (out result)))])
                   (th)))))
  
  (define (warning sym . args)
    (fprintf (current-error-port) "Warning ~s: ~a " sym (apply format args)))


(define (make-default-hash-table) (make-hash-table))
(define (hashtab-get t s) (hash-table-get t s (lambda () #f)))
(define hashtab-set! hash-table-put!)
(define (hashtab-for-each f h) (hash-table-for-each h f))
(define hashtab-remove!  hash-table-remove!)


;; This matches the chez parameter, but does nothing.
(define pretty-maximum-lines (make-parameter #f))

;; This is a simple random number generator interface for use in this Regiment codebase:
(define reg:random-int
  (case-lambda
   [() (reg:random-int (- (expt 2 31) 1))]
   [(k) (random k)]))
(define (reg:get-random-state)
  (pseudo-random-generator->vector (current-pseudo-random-generator)))
(define (reg:set-random-state! s)
  (current-pseudo-random-generator (vector->pseudo-random-generator s)))

;; ======================================================================  

  (include (build-path "generic" "helpers.ss"))

;; ======================================================================
   
    ;; This is a cludge and not a true implementation of chez's "error-handler"
    ;; Erk, this version doesn't make any sense, we could have all sorts of 
    ;; repeated and parallel calls on different threads: 
  ;; [2004.06.13] THIS WONT WORK.  Giving up and defining with-error-handler
    #;(define (error-handler . arg)
      (let ([display-set #f]
	    [escape-set #f]
	    [s #f] [e #f])
      (if (null? arg) 'umm
	  (error-display-handler
	   (lambda (str exn) (if escape-set 
				 ((car arg) str exn)
				 (begin 
				   (set! display-set)
				   (set! s str) 
				   (set! e exn)))))
	  (error-escape-handler
	   (lambda () 
	     (if display-set
		 ((car arg) s e)
		 (set! escape-set #t)))))))
  #;  (define (error-handler . arg)
      (if (null? arg)
	  (error-display-handler)
	  (begin ;; This is lame and dangerous: <WARNING>
	    ;; Ack, this in particular breaks everything:
  ;          (error-escape-handler (lambda () (void)))
	    (error-display-handler (car arg)))))
  
  (provide    
   ;; Syntax:
   mvlet rec 
   
   ;; Values:
   ;; For chez compatibility:
   define-top-level-value set-top-level-value! top-level-bound? top-level-value
   flush-output-port with-error-handlers warning cpu-time ;; error-handler 
   pretty-maximum-lines
   
   ;; Meet in the middle with chez:
   print-level print-length
   make-default-hash-table hashtab-get hashtab-set! hashtab-for-each hashtab-remove!
   
   get-formals
   unique-name unique-name-counter extract-suffix make-begin strip-illegal
   
   ;; Hmm, not sure what meaning immediate has here...
   immediate? constant? datum? 
   formalexp? cast-formals default-unit-tester tester-eq? 

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
   alist-remove list-head list-remove-first list-remove-last! list-remove-after 
   filter list-index snoc rac rdc last
   list-find-position list-remove-before
   randomize-list  insert-between iota disp pp pretty-print crit-printf
   extract-file-extension remove-file-extension file->string string->file file->slist slist->file
   graph-map graph-get-connected-component graph-neighbors cyclic? 
   graph:simple->vertical graph:vertical->simple
   deep-assq deep-member? deep-all-matches deep-filter
   list-get-random unfold-list average clump
   partition partition-equal split-before
   myequal?
   stream? live-stream? stream-empty? stream-car stream-cdr stream-map stream-take 
   counter-stream random-stream stream-append
   display-constrained
   symbol-append 

   testhelpers

   reg:random-int reg:get-random-state reg:set-random-state!
   reg:all-unit-tests 
   
;   (all-except (lib "rutils_generic.ss")
;               list->set union intersection difference set?
;               list-head filter list-index snoc rac rdc 
;               insert-between iota disp)
;   (all-from (lib "rutils_generic.ss") )
   ;   (all-from-except (lib "rutils_generic.ss") 
   ;                    list->set union intersection difference set?) 
   )

  )

;(require helpers)

