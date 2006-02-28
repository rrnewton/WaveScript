
#cs ;; Case Sensitivity
(module helpers mzscheme 
  (require "iu-match.ss"
           (lib "include.ss")
           (lib "date.ss")
           (lib "pretty.ss")
           (prefix plt: (lib "process.ss"))
	   (lib "compat.ss")
           (all-except (lib "list.ss") filter)
;           (all-except "tsort.ss" test-this these-tests)
           "constants.ss"
           "hashtab.ss"
           "engine.ss"
           (prefix swindle: (lib "misc.ss" "swindle"))
           )

  ;; [2006.02.26] 
  ;; How can it possibly take 20sec on faith to native-code compile this one file?
  ;; What's wrong with PLT's native-code compilation setup?

 (provide     	
  ;(all-from "constants.ss")
  
  ;; Remember to update the plt equivalent when you update this:
   ;; Syntax:
   for grep mvlet let-match match-lambda
   ++ ^ ;; Exponentiation
   define-id-syntax
   rec 
   ;;reg:define-struct ;; Moved to constants.ss 
   apply-ordered
   
   ;; Values:
   ;; For chez compatibility:
   define-top-level-value set-top-level-value! top-level-bound? top-level-value
;   record?
   with-error-handlers warning with-warning-handler cpu-time ;; error-handler 
   console-output-port flush-output-port 
   make-list
   fx+ fx- fx* fx/ fx< fx> fx= fx<= fx>= fixnum? fxmin fxmax flonum->fixnum
   fl+ fl- fl* fl/ fl< fl> fl= fl<= fl>= flonum? fixnum->flonum
   most-positive-fixnum
   remq list-head merge sort merge! sort! atom?
   date-and-time collect

   fasl-write
   pretty-maximum-lines pretty-line-length pretty-print
   ;; Meet in the middle with chez:
   print-level print-length print-gensym
   system/echoed system system-to-str 
   with-evaled-params
   
   cd
   ;; Other values 
   id gnuplot histogram date
   display-progress-meter progress-dots count-nodes
   string-split periodic-display all-equal?   
	  
   set->hashtab
   
   ;; Hmm, not sure what meaning immediate has here...
   ;immediate? 
;   constant? datum? 
;   formalexp? cast-formals fit-formals-to-args
   default-unit-tester tester-eq?
   ;default-unit-tester-retries ;; This is in constants.
   substring?
     
   gaussian

   list-repeat! make-repeats
   mapi for-eachi diff
   set? subset? set-equal? list->set set-cons union intersection difference
   remq-all assq-remove-all list-remove-first list-remove-last! list-remove-after 
   filter list-index snoc rac rdc last 
   list-find-position list-remove-before

   insert-between iota disp pp  crit-printf
   extract-file-extension remove-file-extension 
   file->string string->file file->slist slist->file file->linelists
   pad-width round-to uppercase lowercase symbol-uppercase symbol-lowercase
   graph-map graph-get-connected-component graph-neighbors graph-label-dists 
   graph:simple->vertical graph:vertical->simple
   deep-assq deep-assq-all deep-member? deep-all-matches deep-filter
   unfold-list average clump
    partition partition-equal split-before
   myequal?
   
   stream? live-stream? stream-empty? stream-cons stream-car stream-cdr
   stream-map stream-filter stream-take stream-take-all 
   counter-stream stream-append ;random-stream 
   
   display-constrained
   symbol-append 

   testhelpers testshelpers test-this these-tests

   
;   (all-except (lib "rutils_generic.ss")
;               list->set union intersection difference set?
;               list-head filter list-index snoc rac rdc 
;               insert-between iota disp)
;   (all-from (lib "rutils_generic.ss") )
   ;   (all-from-except (lib "rutils_generic.ss") 
   ;                    list->set union intersection difference set?) 
   ) ;; End provide

  
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
    
  (define cd current-directory) ;; shorthand

  ;;; Chez Compatability

  ;; Chez functions for access to top level environment.
  (define (define-top-level-value sym obj)
    (eval `(define ,sym ',obj)))
  (define (set-top-level-value! sym obj) ;; ditto
    (eval `(set! ,sym ',obj)))
  (define (top-level-value sym) (eval sym)) ;; ditto
  (define (top-level-bound? sym) ;; ditto
    (error 'top-level-bound?
	   "This is a chez function which can't ~a"
	     "be emulated right now in Plt. -RRN"))
  
;  (define record? struct?)
  ;(define flush-output-port flush-output)
  ;; [2005.11.05] HACK: This gets set to the normal console output, and stays that way:
  (define console-output-port (make-parameter (current-output-port)))
  (define pp pretty-print)
  (define cpu-time current-process-milliseconds)   ;; Same param, diff name
  (define print-level pretty-print-depth)          ;; Same param, diff name
  (define pretty-line-length pretty-print-columns) ;; Same param, diff name
  
  ;(define print-length pretty-print-length)
  ;; Can't adjust the length from PLT:
  ;; So this does nothing:
  (define print-length (make-parameter #f))
  ;; There is also no print-gensym in PLT:
  (define print-gensym (make-parameter #f))

  ;; We don't have fixnum/flonum arithmetic in PLT:
  ;; Tried to make a generic "alias", but that didn't work, so here are a bunch of defines.
  (define-syntax fx+ (syntax-rules () [(_ e ...) (+ e ...)]))
  (define-syntax fx- (syntax-rules () [(_ e ...) (- e ...)]))
  (define-syntax fx* (syntax-rules () [(_ e ...) (* e ...)]))
  (define-syntax fx/ (syntax-rules () [(_ e ...) (/ e ...)]))
  (define-syntax fx= (syntax-rules () [(_ e ...) (= e ...)]))
  (define-syntax fx< (syntax-rules () [(_ e ...) (< e ...)]))
  (define-syntax fx> (syntax-rules () [(_ e ...) (> e ...)]))
  (define-syntax fx<= (syntax-rules () [(_ e ...) (<= e ...)]))
  (define-syntax fx>= (syntax-rules () [(_ e ...) (>= e ...)]))
  (define-syntax fxmin (syntax-rules () [(_ e ...) (min e ...)]))
  (define-syntax fxmax (syntax-rules () [(_ e ...) (max e ...)]))
  (define-syntax fl+ (syntax-rules () [(_ e ...) (+ e ...)]))
  (define-syntax fl- (syntax-rules () [(_ e ...) (- e ...)]))
  (define-syntax fl* (syntax-rules () [(_ e ...) (* e ...)]))
  (define-syntax fl/ (syntax-rules () [(_ e ...) (/ e ...)]))
  (define-syntax fl= (syntax-rules () [(_ e ...) (= e ...)]))
  (define-syntax fl< (syntax-rules () [(_ e ...) (< e ...)]))
  (define-syntax fl> (syntax-rules () [(_ e ...) (> e ...)]))
  (define-syntax fl<= (syntax-rules () [(_ e ...) (<= e ...)]))
  (define-syntax fl>= (syntax-rules () [(_ e ...) (>= e ...)]))
  ;; [2005.11.03] This is lame but apparently PLT doesn't have any such thing.
  ;; Further, this might be dangerous.  Chez promises that fixnum's are eq?
  ;; but I haven't found a place in the PLT documentation where they guarantee that.
  (define most-positive-fixnum
    (let ((x (- (expt 2 30) 1)))  ;; HACK.
      (lambda () x)))
  (define fixnum?
    (let ((x (expt 2 30)))
      (lambda (n) (and (integer? n) (< n x)))))
  (define (flonum? x) (inexact? x))
  (define (flonum->fixnum x) (inexact->exact (floor x)))
  (define (fixnum->flonum x) (exact->inexact x))

  (define ffofofo atom?);(define (atom? x) (or (symbol? x) (number? x) (null? x) (boolean? x) (char? x) (string? x)))
  
  ;; [2005.11.03] This will work for our purposes, but should stick in an actual definition here at some point.
  ;(define (merge! a b) (merge a b))
  (define (merge! p a b) (swindle:merge! a b p))
  (define (sort! p l)    (swindle:sort! l p))
  (define (list-copy l) (reverse! (reverse l))) ;; Reverse had better be tail-recursive!
  
  (define (date-and-time)
    (let ((d (seconds->date (current-seconds))))
      (format "~a, ~a:~a:~a" (date->string d) (date-hour d) (date-minute d) (date-second d))))
  
;; Temporary! < FIXME>:
  (define crit-printf printf) ;; Thread safe, critical section printf.

  ;; This isn't working right now.
  (define (with-error-handlers displayproc escape th)
    (let/ec out
      (parameterize ([error-display-handler 
                      ;(lambda (ob s) (printf "Error ~s in context ~s\n" s ob))
                      displayproc]
                     [error-escape-handler 
                      (lambda args
		      (let ((result (apply escape args)))
			;; If the escape procedure is not called, we must destroy the continuation:
			(out result)))])
        (th))))

  ;; Chez's system for warnings -- same as error.
  (define (warning sym . args)
    (fprintf (current-error-port) "Warning ~s: ~a " sym (apply format args)))
  (define (with-warning-handler fun th)
    (fluid-let ((warning fun))
      (th)))  

  ;; Make this return a fake error code to act like the Chez version.
  (define system plt:system/exit-code)
  ;; There's no problem with this in PLT:
  (define system/echoed plt:system/exit-code)

  (define (system-to-str str)
    (let-match ([(,in ,out ,id ,err ,fun) (plt:process str)])
      (let ((p (open-output-string)))
        (let loop ((c (read-char in)))
          (if (and (eof-object? c)
                   (not (eq? 'running (fun 'status))))
              (begin 
                (close-input-port in)
                (close-output-port out)
                (close-input-port err)
                (get-output-string p))
              (begin (unless (eof-object? c) (display c p))
                     (loop (read-char in))))))))
  
  ;; PLT doesn't support anything like fasl-writing as far as I can see.
  (define fasl-write write)
  
  ;; This matches the chez parameter, but does nothing.
  (define pretty-maximum-lines (make-parameter #f))

  ;; Primitive in chez:
  (define (make-list n x)
    (let loop ((acc ()) (n n))
      (if (zero? n) acc
          (loop (cons x acc) (sub1 n)))))
  (define list-head
    (lambda (lst n)
      (cond
        [(zero? n) '()]
        [(null? lst) (error 'list-head "list is not long enough: ~s ~s"
                            lst n)]
        [else (cons (car lst) (list-head (cdr lst) (sub1 n)))])))
  ;; RRN: Non-tail recursive version
  '(define (merge f l1 l2)
     ;(let loop ((acc '()) (l1 l1) (l2 l2))
     (cond
       [(null? l1) l2]
       [(null? l2) l1]
       [(f (car l1) (car l2))
        ;(loop (cons (car l1) acc)
    (cons (car l1)
	  (merge f (cdr l1) l2))]
       ;; This gives us stability:
   [(f (car l1) (car l2))
    (cons (car l2)
	  (merge f l1 (cdr l2)))]
   ;; If they're equal, l1 goes first:
   [else 
    (cons (car l1)
	  (merge f (cdr l1) l2))]))

;; From Swindle:
;;>> (merge less? a b)
;;>   Takes two lists `a' and `b' such that both (sorted? a less?) and
;;>   (sorted? b less?) are true, and returns a new list in which the
;;>   elements of `a' and `b' have been stably interleaved so that (sorted?
;;>   (merge less? a b) less?) is true.  Note: this does not accept vectors.
'(define (merge less? a b)
  (cond [(null? a) b]
        [(null? b) a]
        [else (let loop ([x (car a)] [a (cdr a)] [y (car b)] [b (cdr b)])
                ;; The loop handles the merging of non-empty lists.  It has
                ;; been written this way to save testing and car/cdring.
                (if (less? y x)
                  (if (null? b)
                    (cons y (cons x a))
                    (cons y (loop x a (car b) (cdr b))))
                  ;; x <= y
                  (if (null? a)
                    (cons x (cons y b))
                    (cons x (loop (car a) (cdr a) y b)))))]))

  (define merge
    (lambda (pred? l1 l2)
      (cond
        ((null? l1) l2)
        ((null? l2) l1)
        ((pred? (car l2) (car l1))
         (cons (car l2) (merge pred? l1 (cdr l2))))
        (else (cons (car l1) (merge pred? (cdr l1) l2))))))

  ;; More Chez compat:
  (define (block-read inp str count)
    (read-bytes-avail! str inp 0 count))
  (define (block-write outp str count)
    (write-bytes str outp 0 count))
  (define collect collect-garbage)
  
  (define (system-to-string cmd)
    (eval 'TODO-IMPLEMENTM))
            
           
; =======================================================================  

  (include (build-path "generic" "helpers.ss"))

; =======================================================================
   
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
  

  )

;(require helpers)
;(time (testhelpers))

