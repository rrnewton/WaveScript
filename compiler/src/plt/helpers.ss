
(module helpers mzscheme 
  (require "iu-match.ss"
           (lib "include.ss")
           (lib "pretty.ss")
           (all-except (lib "list.ss") filter)
           (all-except "tsort.ss" test-this these-tests))
  
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
  
  (include (build-path ".." "generic" "helpers.ss"))
  
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
  

    (define-syntax rec
      (syntax-rules ()
	((_ x e) (letrec ((x e)) x))))

    (define-syntax mvlet
      (syntax-rules ()
	  [(mvlet stuff ...) (let-values stuff ...)]))

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

    ;; This isn't working right now.
    (define (with-error-handlers display escape th)
      (parameterize ([error-display-handler display]
		     [error-escape-handler escape])
         (th)))

  
  (define (warning sym . args)
    (printf "Warning ~s: ~a " sym (apply format args)))
  

  (provide    
   ;; Syntax:
   DEBUGMODE DEBUGPRINT DEBUGPRINT2 DEBUGASSERT
   mvlet rec 
   
   ;; Values:
   ;; For chez compatibility:
   define-top-level-value set-top-level-value! top-level-bound? top-level-value
   flush-output-port with-error-handlers warning cpu-time ;; error-handler 
   
   ;; Meet in the middle with chez:
   print-level print-length
   
   get-formals
   unique-name reset-name-count! extract-suffix make-begin strip-illegal
   
   ;; Hmm, not sure what meaning immediate has here..
   immediate? constant? datum? 
   formalexp? cast-formals default-unit-tester tester-eq? 

   define-regiment-parameter
   regiment-primitives regiment-primitive? 
   token-machine-primitives token-machine-primitive? 
   token-machine? token-machine->program
   basic-primitive? distributed-primitive?
   get-primitive-entry regiment-constants regiment-constant? ;get-primitive-arity
   get-primitive-return-type
   
   ;; Token names:
   token? new-token-name token-names get-names get-formation-name get-membership-name

   set? subset? set-equal? list->set set-cons union intersection difference
   list-head list-remove-first list-remove-last! list-remove-after filter list-index snoc rac rdc last
   list-find-position list-remove-before
   randomize-list  insert-between iota disp pp crit-printf
   graph-map graph-get-connected cyclic? 
   graph:simple->vertical graph:vertical->simple
   deep-assq deep-member? deep-all-matches deep-filter
   list-get-random unfold-list average clump
   partition partition-equal
   myequal?
   stream? live-stream? stream-empty? stream-car stream-cdr stream-map stream-take 
   counter-stream random-stream stream-append
   display-constrained
   symbol-append 

;   (all-except (lib "rutils_generic.ss")
;               list->set union intersection difference set?
;               list-head filter list-index snoc rac rdc 
;               insert-between iota disp)
;   (all-from (lib "rutils_generic.ss") )
   ;   (all-from-except (lib "rutils_generic.ss") 
   ;                    list->set union intersection difference set?) 
   )
  
  )


