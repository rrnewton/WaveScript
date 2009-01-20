#!r6rs

;;;; .title Utility Function Library (helpers.ss)

;;;; RRN: this file needs some serious cleaning-out.  It still has a bunch
;;;; of old junk in it that's not used by the Regiment system.

;;;; Another gronky aspect of this file is that it makes dirty use of
;;;; the CHEZ/PLT switches.  This module exports a different interface
;;;; for Chez vs. PLT.  Some functions only work in Chez.  Others are
;;;; builtin in one....

;==============================================================================;
;;;; <br> REQUIRES: On chez/plt primitive open-output-string.
;;;; <br> REQUIRES: (On other chez/plt features which I'm not aware of...)

;==============================================================================;

(library  (ws util helpers)
  (export     	
  
  ;; Syntax:
  values->list
  first-value second-value
  curry curry2
  
   ;; Values:
     
   ;; Other values 
   id gnuplot gnuplot_pipe histogram ; date
   display-progress-meter progress-dots count-nodes
   periodic-display all-equal?   
	  
   set->hashtab hashtab->list
   
   ;; Hmm, not sure what meaning immediate has here...
   ;immediate? 
;   constant? datum? 
;   formalexp? cast-formals fit-formals-to-args
   default-unit-tester tester-eq? test-units
   with-error-handlers
   ;default-unit-tester-retries ;; This is in constants.
   substring?
     
   gaussian

   iota list-repeat! list-head make-repeats
   mapi map-filter for-eachi diff
   list-is-set? list-subset? set-equal? list->set set->list union intersection difference set-cons:list
   list-rem-dups ;define-values 
   list-is-setq? list-subsetq? set-eq?
   remq-all assq-remove assq-remove-all list-remove-first list-remove-last! list-remove-after 
   list-index snoc rac rdc rdc! rac&rdc! last  mapleft mapright
   list-find-position list-remove-before
   list-build list-copy
   foldl foldl1
   
   vector-map! vector-fold
   vector-build vector-blit! vector-andmap
   and-list or-list

   insert-between  compose compose/values disp pp
   extract-file-extension remove-file-extension 
   file->string string->file file->slist slist->file file->linelists
   file->lines string->lines string->slist port->slist
   string-split

   pad-width round-to uppercase lowercase symbol-uppercase symbol-lowercase
   graph-map graph-get-connected-component graph-neighbors graph-label-dists 
   graph:simple->vertical graph:vertical->simple
   deep-memq deep-assq deep-assq-all deep-member? deep-all-matches deep-filter blaze-path-to blaze-path-to/assq
   unfold-list average clump
   partition-equal split-before group
   myequal?
      
   with-evaled-params
   display-constrained
   symbol-append symbol<? symbol<=?

   eprintf log-opt

   testhelpers 

   median stddev 

   ; --mic
   find-in-flags
   n-times

   read-line andmap ormap
   
   ;; Might should go to compat.sls:
   ;; ===============================
   fx/ fx= fx< fx> fx<= fx>= add1 sub1 atom? complex-conjugate flonum->fixnum
   cfl+ cfl- cfl* cfl/
   cflonum? ;; Adding this for r6rs
   vector-copy subst
   ;; ===============================

;; These WERE provided ONLY for Chez... need to look into these periodically.
     foldr let/ec call/ec 
     with-warning-handler current-error-port
     system/echoed system-to-str 
     add-parameter-hook chomp shell-expand-string
     seconds-since-1970 ignore  comma-number runN
     
;with-evaled-params crit-preintf  gobj?  grep-oblist make-n-list define-values 

     force-open-output-file

   )

(import (except (rnrs (6)) error)
	(rnrs mutable-strings (6))
	(rnrs mutable-pairs (6))
	(rnrs exceptions (6))
	(except (rnrs r5rs (6)) delay force)
	(rnrs eval (6))
	(ws compat compat)
	(ws globals)
	(ws util hashtab)
	(ws util iu-match)
	(ws util reg_macros)
   )


;; ================================================================================
;; I'm sticking some basic compatibility functions here which MIGHT be
;; moved into compat.sls in the future so as to take advantage of
;; implementation-native versions.  For the time being keeping them
;; here removes the complexity of duplicating them in the different
;; compat.*.sls files.
;; ================================================================================

  ;; This is used to distinguish if something is complex but NOT real:
  ;; It's in contradiction with the current semantics of "cflonum?" under chez:
  (IFCHEZ (define (cflonum? x) (and ((hash-percent cflonum?) x) (not (flonum? x))))
	  ;; And this version doesn't work for chez because it's 'real?' misbehaves with 0.0 imaginary component.
	  ;; Adding this for r6rs.
	  (define (cflonum? x) (and (complex? x) (not (real? x)))))

  (define (add1 x) (+ x 1))
  (define (sub1 x) (- x 1))
  (define (atom? x) (not (pair? x)))
  (define (fxadd1 x) (fx+ 1 x))
  ;; Backwards compatibility:
  (define-syntax fx= (identifier-syntax  fx=?))
  (define-syntax fx< (identifier-syntax  fx<?))
  (define-syntax fx> (identifier-syntax  fx>?))
  (define-syntax fx<= (identifier-syntax  fx<=?))
  (define-syntax fx>= (identifier-syntax  fx>=?))
  (define-syntax fx/ (identifier-syntax  fxdiv))

  ;; No direct R6RS support for this:
  (define-syntax cfl+ (identifier-syntax +))
  (define-syntax cfl- (identifier-syntax -))
  (define-syntax cfl* (identifier-syntax *))
  (define-syntax cfl/ (identifier-syntax /))
  ;; Not in r6rs, probably want to use chez's native:
  (define (vector-copy v)
    (let ([new (make-vector (vector-length v))])
      (vector-blit! v new 0 0 (vector-length v))
      new))

  ;; May need to add this to compat.sls if we want to use implementation's native versions:
  (define (complex-conjugate c)
    (make-rectangular (real-part c) (- (imag-part c)))
    )

  (define (flonum->fixnum fl) (exact (truncate fl)))

  (define (subst new old tree)
    (let loop ([tree tree])
      (if (pair? tree)
	  (if (equal? old (car tree))
	      (cons new (if (equal? old (cdr tree))
			    new (loop (cdr tree))))
	      (cons (loop (car tree))
		    (if (equal? old (cdr tree))
			new (loop (cdr tree)))))
	  tree)))

  ;(define (box x) (vector x))
  ;(define (unbox x) (vector-ref x 0))
  ;(define (set-box! b x) (vector-set! b 0 x))


;; ================================================================================



#; ;; TODO FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME  NEED TO MAP THIS ONTO R6RS:
(cond-expand
 [chez 
  ;; [2004.06.13] Matches the function defined in plt, provides
  ;; functionality used by the generic code.
  ;; The escape handler had *better* escape.
  (define (with-error-handlers display escape th)
    (let ([orig-error ((hash-percent error-handler))])
      (parameterize ([error-handler (lambda args 
				      (parameterize ([error-handler orig-error])
					(apply display args)
					(escape)))])
	(th))))
  (define (with-warning-handler fun th)
    (parameterize ([warning-handler fun])
      (th)))
  ;; This is a hack, it's not safe because it can report false
  ;; positives.  This is used to tell when something is a graphics
  ;; screen object as constructed with SWL's (make <foo> ...):
  (define gobj?
    (lambda (x)
      (and (vector? x)
	   (> (vector-length x) 2)
	   (procedure? (vector-ref x 0))
	   (vector? (vector-ref x 1))
	   (> (vector-length (vector-ref x 1)) 1)
	   (eq? 'class (vector-ref (vector-ref x 1) 0)))))
  (define current-error-port (reg:make-parameter stderr))
  ]
 [larceny
  (define (with-error-handlers display escape th)
    (let ([orig-error (error-handler)])
      (parameterize ([error-handler (lambda args 
				      (parameterize ([error-handler orig-error])
					(decode-error args);(apply display args)
					(escape)))])
	(th))))
  (define (with-warning-handler fun th) 
    (parameterize ([warning-handler fun]) (th)))]
 [plt])

;; Here's the R6RS version
(define (with-error-handlers display escape th)
  (with-exception-handler 
   (lambda (exn)
     ;; (#<record &error> #<record &who> #<record &message> #<record &irritants>) 
     ;; Dissect the R6RS error record into "who" "string":
     (display exn "") ;; <- Hack, for now just do this
     (escape))
   th))
(define (with-warning-handler display th)
  (parameterize ([warning-handler display])
    (th)))

; ======================================================================

;; The unit tester lives in its own file:
#;
(cond-expand
 ;; We play nasty tricks with symbolic links here. 
 ;; It doesn't matter if we load this file from "src" or "src/chez"
 ;; because we've linked the "generic" subdir from both locations.
 [(or larceny chez) (include "generic/util/unit_tester.ss")]
 [(or plt) (include "unit_tester.ss")])

;(define-syntax andmap (identifier-syntax for-all))
;(define-syntax ormap  (identifier-syntax exists))
(define andmap for-all)
(define ormap exists)

;; Produce a list of consecutive integers.
;; .example (iota n) => (0 1 ... n-1)
;; .example (iota i n) => (i i+1 ... i+n-1)
(define iota
    (case-lambda
      [(n) (iota 0 n)]
      [(i n) (ASSERT integer? i) (ASSERT integer? n)
       (if (= n 0)
	   '()
         (cons i (iota (+ i 1) (- n 1))))]))

(define display-constrained
  (lambda args
    ;(pretty-print args)
    (for-each 
     (lambda (arg)
       (if (string? arg)
	   (display arg)
	   (let-values ([(arg) (car arg)]
			[(bound) (cadr arg)]
			[(port extraction) (open-string-output-port)])
	     (write arg port)   (newline port)
	     (let ((str (extraction)))
	       (if (> (string-length str) bound)
		   ;; Here we chop the string.  I go to a little extra
		   ;; work to make sure we don't print a lone double
		   ;; quote -- for emacs' sake.
		   (let ((substr (substring str 0 (max 0 (- bound 3)))))
		     (do ([i 0 (add1 i)])
			 ((= i (string-length substr)))
		       (if (eq? (string-ref substr i) #\")
			   (string-set! substr i #\')))
		     (display substr)
		     (display "..."))
		   ;; Gotta cut off the newline:
		   (display (substring str 0 (- (string-length str) 1))))))))
	      args)))


;; [2005.10.16] 
(define (for-eachi f ls)
  (let foreachi-loop ((i 0) (ls ls))
    (if (null? ls)
	(void)
	(begin (f i (car ls))
	       (foreachi-loop (add1 i) (cdr ls))))))

(include "ws/testing/unit_tester.ss")


; ======================================================================
;;;  BEGIN Generic core of helpers.ss:
; ======================================================================

;; For evaluating a multiple-valued expression.
(define-syntax values->list 
  (syntax-rules ()
    [(_ e) (call-with-values (lambda () e) list)]))
(define-syntax first-value
  (syntax-rules () [(_ e) (car (values->list e))]))
(define-syntax second-value
  (syntax-rules () [(_ e) (cadr (values->list e))]))

(define-syntax curry
  (syntax-rules () [(_ f x ...) (lambda (y) (f x ... y))]))
(define-syntax curry2
  (syntax-rules () [(_ f x ...) (lambda (y z) (f x ... y z))]))


;==============================================================================;

;; This is not strictly R5RS but it should work in both chez and plt,
;; as long as plt has "compat.ss" loaded.  
;; (For example, it requires flush-output-port (current-output-port))

;(define region-primitives)
;(define anchor-primitives)

(define (id x) x)
(define (ignore x) (void))


;; List operations
;==================================================

(define (mapadd f l last)
  (let loop ([l l])
    (if (null? l)
        (list last)
        (cons (f (car l)) (loop (cdr l))))))

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
		   [(exists null? l)
		    (if (for-all null? l) 
			init
			(error 'foldl "received non-equal length input lists"))]
		   [else (fold-n
			  f
			  (apply f (mapadd car l init))
			  (map cdr l))]))))
       (case-lambda
        [(f init l) (fold-one f init l)]
        [(f init l . ls) (fold-n f init (cons l ls))])))

(define (foldl1 f ls)  
  (if (null? ls)
      (error 'foldl1 "list must have at least on element.")
      (foldl f (car ls) (cdr ls))))

;; Also from PLT's list.ss
(define foldr
  (letrec ((fold-one
	    (lambda (f init l)
	      (letrec ((helper
			(lambda (init l)
			  (cond
			   [(null? l) init]
			   [else (f (car l) (helper init (cdr l)))]))))
		(helper init l))))
	   (fold-n
	    (lambda (f init l)
	      (cond
	       [(exists null? l)
		(if (for-all null? l)
		    init
		    (error 'foldr "received non-equal length input lists"))]
	       [else (apply f
			    (mapadd car l
				    (fold-n f init (map cdr l))))]))))
    (case-lambda
      [(f init l) (fold-one f init l)]
      [(f init l . ls) (fold-n f init (cons l ls))])))

(define list-build
  (lambda (n f)
    (let loop ([i 0] [acc '()])
      (if (fx= i n)
	  (reverse! acc)
	  (loop (fx+ 1 i) (cons (f i) acc))))))

(define list-index
  (lambda (pred ls)
    (DEBUGASSERT procedure? pred)
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

(define (assq-remove key ls)
  (let loop ((ls ls))
    (cond
     [(null? ls) '()]
     [(eq? (caar ls) key) (cdr ls)]
     [else (cons (car ls) (loop (cdr ls)))])))
;; This removes all!  Not just first.
(define (assq-remove-all x ls)
  (let loop ((ls ls))
    (cond
     [(null? ls) '()]
     [(eq? (caar ls) x) (loop (cdr ls))]
     [else (cons (car ls) (loop (cdr ls)))])))

(define (remq-all x ls)
  (let loop ((ls ls) (acc '()))
    (cond 
     [(null? ls) (reverse! acc)]
     [(eq? x (car ls)) (loop (cdr ls) acc)]
     [else (loop (cdr ls) (cons (car ls) acc))])))

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
  
(define rdc!
  (lambda (ls)
    (cond 
      [(null? ls)
       (error 'rdc "cannot take the rdc of the empty-list")]
      [(null? (cdr ls)) '()]
      [else (let loop ([first ls] [second (cdr ls)])
              (if (null? (cdr second))
                  (set-cdr! first '())
                  (loop (cdr first) (cdr second))))
            ls])))
  
(define rac&rdc!
  (lambda (ls)
    (cond 
      [(null? ls)
       (error 'rac&rdc "cannot take the rdc of the empty-list")]
      [(null? (cdr ls)) (values (car ls) '())]
      [else (values (let loop ([first ls] [second (cdr ls)])
                      (if (null? (cdr second))
                          (begin (set-cdr! first '())
                                 (car second))
                          (loop (cdr first) (cdr second))))
                    ls)])))

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
	(let ([result (f (car ls))])
	  (cons result
		(mapright f (cdr ls)))
	  ))))


(define (list-repeat! ls)
  (if (null? ls) (error 'list-repeat! "cannot create infinite list from null list."))
  (let loop ((p ls))
    (if (null? (cdr p))
	(set-cdr! p ls)
	(loop (cdr p))))
  ls)

;; Could use the primitive versions under some implementations:
(define list-head
    (lambda (lst n)
      (cond
        [(zero? n) '()]
        [(null? lst) (error 'list-head "list is not long enough: ~s ~s"
                            lst n)]
        [else (cons (car lst) (list-head (cdr lst) (sub1 n)))])))

(define (list-copy l) (reverse! (reverse l))) ;; Reverse had better be tail-recursive!
#;
;; Not tail recursive:
(define (list-copy ls)
  (if (null? ls) '()
      (cons ls (list-copy (cdr ls)))))

(define (make-repeats ls numelems)
  (list-head (list-repeat! (list-copy ls)) numelems))


;; Vector operations
;==================================================

(define vector-map!
  (lambda (f v)
    (do ([i (sub1 (vector-length v)) (sub1 i)])
        ((= -1 i) v)
        (vector-set! v i (f (vector-ref v i))))))

(define vector-andmap
  (lambda (pred v)
    (let ([len (vector-length v)])
      (let loop ([i 0])
	(cond
	 [(= i len) #t]
	 [(pred (vector-ref v i)) (loop (fx+ i 1))]
	 [else #f])))))
(define vector-fold
  (lambda (f z v)
    (define len (vector-length v))
    (let loop ([i 0] [x z])
      (if (fx= i len) x
	  (loop (fx+ 1 i) 
		(f x (vector-ref v i)))))))
(define vector-blit!
  (lambda (src dest ind1 ind2 len)
    (let loop ([i 0])
      (if (fx= i len)
	  (void)
	  (begin (vector-set! dest (fx+ ind2 i) (vector-ref src (fx+ ind1 i)))
		 (loop (fx+ i 1)))))))

(define vector-build
  (lambda (n f)
    (let ([v (make-vector n)])
      (do ([i 0 (fx+ i 1)])
	  ((= i n) v)
	(vector-set! v i (f i))
	))))

;==================================================



(define symbol-append
  (lambda args
    (string->symbol (apply string-append (map symbol->string args)))))
(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))
(define (symbol<=? a b) (string<=? (symbol->string a) (symbol->string b)))

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
  (if (null? ls)
      (error 'average "list of numbers cannot be null."))
  (let loop ((sum 0) (count 0) (ls ls))
    (if (null? ls) (/ sum count)
	(loop (+ (car ls) sum) (+ 1 count) (cdr ls)))))
(define (stddev ls)
  (if (null? ls)
      (error 'stddev "list of numbers cannot be null."))
  (let* ([sum (foldl + 0 ls)]
	 [len (length ls)]
	 [av  (/ sum len)]
	 [var (/ (foldl (lambda (n acc) (+ (^ (- n av) 2) acc)) 0 ls)
		 len ;(sub1 len)
		 )])
    (sqrt var)))
(define (median ls)
  (if (null? ls)
      (error 'median "list of numbers cannot be null."))
  (list-ref (list-sort < ls) (quotient (length ls) 2)))

;; This does a size-measure on lists and vectors.
;; Really crappy implementation, don't rely on this:
(define count-nodes
  (lambda (lsvec)
    (cond
     [(or (number? lsvec)
	  (boolean? lsvec)
;	  (port? lsvec)
	  (null? lsvec)
	  (symbol? lsvec)
	  (string? lsvec)
	  (char? lsvec)
	  (procedure? lsvec))
      1]
      [(pair? lsvec) (+ 1 (count-nodes (car lsvec))
			  (count-nodes (cdr lsvec)))]
      [(vector? lsvec) (+ (vector-length lsvec)			  
                          (let loop ((i (sub1 (vector-length lsvec))))
                            (if (fx< i 0) 0                                
                                (+ (count-nodes (vector-ref lsvec i))
                                   (loop (sub1 i))))))]
      ;; Well, don't know how to go in here.  So it's an atom.
      [else 1]
#;
      [(record? lsvec) 1]
#;
      [else (error 'count-nodes
                   "only knows how to count the nodes of a list or vector, not ~a" lsvec
                   )]
      )))

;; [2005.10.16] Not tail recursive!!
(define (mapi f ls)
  (let mapi-loop ((i 0) (ls ls))
    (if (null? ls)
	'()
	(cons (f i (car ls))
	      (mapi-loop (add1 i) (cdr ls))))))
;; Apply a function, discard any results that are #f
(define (map-filter f ls)
  (if (null? ls) '()
      (let ([val (f (car ls))])
	(if val 
	    (cons val (map-filter f (cdr ls)))
	    (map-filter f (cdr ls))
	     ))))


(define (with-evaled-params params th)
  (let loop ((ls params))
    (if (null? ls) 
	(begin 
	  ;(disp "PARAMED TIME: " (sim-timeout))
	  ;(inspect sim-timeout)
	  (newline)
	  (th))
	(parameterize ([(simple-eval (caar ls)) (simple-eval (cadar ls))])
	  (printf "Setting parameter: ~a ~a\n" 
		  (caar ls) ;(pad-width 23 (caar ls))
		  (cadar ls))
	  (loop (cdr ls))))))


;; [2006.02]
#;
(define progress-dots 
  (cond-expand
   [chez (case-lambda 
	   [(th)      (progress-dots th 50000000)]
	   [(th fuel) (progress-dots th fuel 
				     (lambda () (display #\.) (flush-output-port (current-output-port))))]
	   [(th fuel progress)
	    (let loop ((engine (make-engine th)))
	      (progress)
	      (engine fuel
		      (lambda (time-remaining val) (newline) (flush-output-port (current-output-port)) val)
		      loop))])]
   [else (lambda args (error 'progress-dots "not implemented except in chez"))]))
(define (progress-dots . args) (error 'progress-dots "not implemented in R6RS yet!!!"))


;; [2006.02.20] A simple utility for running a long-running expression for only N ticks.
(define-syntax runN
  (syntax-rules ()
    [(_ n exp) (let ((e (make-engine (lambda () exp))))
		 (e n (lambda (_ val) val)
		    (lambda (ee) (void))))]))


;; [2006.02.01] <br>
;; This takes the name of a top-level parameter, and dangles a function off
;; the parameter which is then called on the new parameter value when
;; it changes.
(define (add-parameter-hook pname hook)
  (let ([origfun (top-level-value pname)])
    (set-top-level-value! pname 
			  (case-lambda
			    [() (origfun)]
			    [(v) (origfun v) (hook v)]))))

(define (or-list ls)
  (cond 
   [(null? ls) #f]
   [(car ls) #t]
   [else (or-list (cdr ls))]))
(define (and-list ls)
  (cond 
   [(null? ls) #t]
   [(car ls) (and-list (cdr ls))]
   [else #f]))


#;
(define timeeval
  (lambda (x)
    (let ([start (real-time)])
      (eval x)
      (- (real-time) start))))
#;
(define cpueval
  (lambda (x)
    (let ([start (cpu-time)])
      (eval x)
      (- (cpu-time) start))))

;;----------------------------------------

;; Strings, Files and Ports:

;; [2006.08.16] More efficient:
;; Splits up a string by a separator character:
(define (string-split str char)
  (let loop ([start 0] [i 0] [acc '()])
    (cond
     [(= i (string-length str)) 
      (reverse! (cons (substring str start i) acc))]
     [(eq? (string-ref str i) char)
      (loop (fx+ 1 i) (fx+ 1 i)
	    (cons (substring str start i) acc))]
     [else (loop start (fx+ 1 i) acc)])))

;; inefficient
(define substring?
  (lambda (s1 s2)
    (let ([l1 (string-length s1)] [l2 (string-length s2)])
    (if (< l2 l1)
	#f
	(let loop ((i 0))
	  (if (> i (- l2 l1))
	      #f
	      (or (equal? s1 (substring s2 i (+ i l1)))
		  (loop (add1 i)))))))))

;[2001.07.15]
(define port->slist
  (lambda (p)
    (let porttoslistloop ([exp (read p)] [acc '()])
        (if (eof-object? exp)
            (begin (close-input-port p)
                   (reverse! acc))
            (porttoslistloop (read p) (cons exp acc))))))
;; [2008.05.07] Adding a hack to tolerate #! lines at the top of the file.
(define file->slist
  (lambda (filename . opts)
    (ASSERT string? filename)
    (let* ([p (apply open-input-file filename opts)]
	   [line1 (get-line p)])
      ;; Doesn't allow whitespace!
      (if (and (>= (string-length line1) 2)
	       (string=? "#!" (substring line1 0 2)))
	  ;; Read the rest of it straight away:
	  (port->slist p)
	  ;; Oops, we need to "put back" that first line  We do that by just starting over.
	  (begin (close-input-port p)
		 (port->slist (apply open-input-file filename opts))))
      )))
(define (string->slist str) (port->slist (open-string-input-port str)))

;; "Error" printf, goes to stderr:
(define (eprintf . args) (apply fprintf (current-error-port) args))

(define (log-opt . args) (apply fprintf (current-error-port) args))

;; prints each expression to file.
(define slist->file
  (case-lambda 
   [(slist fn) (slist->file slist fn 'write)]
   [(slist fn method)
    (let ([p (force-open-output-file fn)])
      (parameterize ([print-level #f]
		     [print-length #f]
		     [pretty-maximum-lines #f])
	  (for-each (lambda (x) 
		      (case method
			[(write plain) (write x p)(newline p)]
			[(display) (display x p)];; Should handle it's own newlines!
			[(pretty pretty-print) 
			 (parameterize ([print-level #f]
					[print-graph #f])
			   (pretty-print x p))])
		      (newline p))
	    slist))
      (close-output-port p))]))

;; [2006.02.20] Rewrote to have an efficient version:
(IFCHEZ
 (define file->string
  (lambda (filename)
    (let-values ([(outp extract) (open-string-output-port)])
      (let* ([inp (open-input-file filename)]
	     [block-size 1024]
	     [block (make-string block-size)])
	(let loop ([count (block-read inp block block-size )])
	  (if (eof-object? count)
	      (begin (close-input-port inp)
		     (extract))
	      (begin (block-write outp block count)
		     (loop (block-read inp block block-size ))))))
      )))
(define (file->string filename)
  (let* ([p (open-input-file filename)]
	 [res (get-string-all p)])
    (close-input-port p)
    res)))
(define string->file
  (lambda (str fn)
    (if (file-exists? fn) (delete-file fn))
    (let ([p (force-open-output-file fn)]) 
      ;(fprintf p str)
      (display str p)
      (close-output-port p))))


(define read-line get-line)

;; [2006.02.22] <br>
;; Reades all the expressions on each line into a list.
;; Useful for space deliminited files of numbers.
;; Potentially respects a comment character (other than semi-colon).
;; (I use this for reading gnuplot data files.)
(define file->linelists
  (case-lambda 
    [(f) (file->linelists f #\;)]
    [(f comment-char)
     (let ([in (if (input-port? f) f
		   (open-input-file f))])
       (let loop ([acc '()])
	 (let ([str (read-line in)])
	   ;; This loop crops the end of the line off if its a comment.
	   (if (or (not str) (eof-object? str))
	       (reverse! acc)
	       (begin
		 (let scan ([i 0])
		   (cond
		    [(= i (string-length str)) (void)]
		    [(eq? comment-char (string-ref str i))
		     (set! str (substring str 0 i))]
		    [else (scan (fx+ 1 i))]))
		 (loop (cons
			(let ([port (open-string-input-port str)])
			  (let munch-line ()
			    (let ([x (read port)])
			      (if (eof-object? x)
				  '()
				  (cons x (munch-line))))))
			acc)))))))]))

;; This one just reads the file as a list of lines.
(define (file->lines fn) (string->lines (file->string fn)))

;; This assumes newlines for now... the more robust version would
;; accept all the different line-ending variants.
(define (string->lines str) (string-split str #\newline))

(define extract-file-extension
  (lambda (filename)
    (let loop ([ls (reverse (string->list filename))]
               [acc '()])
      (cond
        [(null? ls) ""]
        [(eq? (car ls) #\.) (list->string acc)]
        [else (loop (cdr ls) (cons (car ls) acc))]))))

(define remove-file-extension
  (lambda (filename)
    (let loop ([ls (reverse (string->list filename))])
      (cond
        [(null? ls) filename] ;no extension to remove
        [(eq? (car ls) #\.)
         (list->string (reverse (cdr ls)))]
        [else (loop (cdr ls))]))))
;; TODO: These need to by system independent, but aren't yet.


;; [2005.10.09] Making this global, its useful.
  (define (pad-width w ob)
    (let ((s (format "~a" ob)))
      (if (< (string-length s) w)
	  (string-append s (make-string (- w (string-length s)) #\space))
	  s)))

(define (uppercase str)  ;; Uppercase a string
  (let ((new (string-copy str)))
    (for i = 0 to (sub1 (string-length str))
	 (string-set! new i (char-upcase (string-ref new i))))
    new))
(define (lowercase str)  ;; Lowercase a string
  (let ((new (string-copy str)))
    (for i = 0 to (sub1 (string-length str))
	 (string-set! new i (char-downcase (string-ref new i))))
    new))
;; Lifted over symbols:
(define (symbol-uppercase s)  (string->symbol (uppercase (symbol->string s))))
(define (symbol-lowercase s)  (string->symbol (lowercase (symbol->string s)))) ;; ditto

;; This rounds a number to a given # of decimal points. -[2005.11.20] 
(define (round-to dec n)
  (if (integer? n) n
      (let ((shift (expt 10.0 dec)))
	(/ (round (* shift n)) shift))))

(define insert-between
  (lambda (x lst)
    (let loop ([lst lst])
      (cond
        [(null? lst) '()]
        [(null? (cdr lst)) lst]
        [else (cons (car lst)
                    (cons x (loop (cdr lst))))]))))

; Removed mvlet! [2004.04.28]


;;; SET ADT.
;;;
;;; FIXME TODO: seal this type off to prevent invalid accesses to the list representation.
;; =================================================================================

;; Here we make sets a separate datatype:
(reg:define-struct (setadt internal))

(define (list-is-set? ls)
  (or (null? ls)
      (and (not (member (car ls) (cdr ls)))
	   (list-is-set? (cdr ls)))))
;; eq? based:
(define (list-is-setq? ls)
  (or (null? ls)
      (and (not (memq (car ls) (cdr ls)))
	   (list-is-setq? (cdr ls)))))

;; Inefficient for ordered types:
(define (list-subset? l1 l2)
  (andmap (lambda (a) (member a l2)) l1))
(define (list-subsetq? l1 l2)
  (andmap (lambda (a) (memq a l2)) l1))

(define set-comparator
  (lambda (testfun memfun)
    (lambda (set1 set2)
      (define lst2 (setadt-internal set1))
      (define lst1 (setadt-internal set2))
      (letrec ((loop (lambda (lst1 lst2)
		       (cond
			[(and (null? lst1) (null? lst2)) #t]
			[(or (null? lst1) (null? lst2)) #f]
			[(memfun (car lst1) lst2) (loop (cdr lst1) (list-remove-all (car lst1) lst2))]
			[else #f]))))
	(if (and (testfun lst1) (testfun lst2))
	    (loop lst1 lst2)
	    (error 'set-eq/equal? "must take two sets, improper arguments: ~s ~s" lst1 lst2))))))
(define set-eq?    (set-comparator list-is-setq? memq))
(define set-equal? (set-comparator list-is-set?  member))

;; [2005.10.11]  Added reverse! to make the result in the same order as orig. <br>
;; NOTE: Uses eq? !
(define list-rem-dups
  (case-lambda
    [(ls) (list-rem-dups ls eq?)]
    [(ls comp)
    (let loop ([ls ls])
       (if (null? ls) '()
	 (reverse! 
	  (set-cons:list (car ls) (loop (cdr ls)) comp))))
#;
     (make-setadt
  )]))

(define (set->list set) (setadt-internal set))
(define (list->set . args) (make-setadt (apply list-rem-dups args)))

;; On lists, port to sets:
;; NOTE: Uses eq? !
(define set-cons:list
  (case-lambda
    [(x set equ?)
     (cond
      [(null? set) (list x)]
      [(equ? x (car set)) set]
      [else (cons (car set) (set-cons:list x (cdr set)))])]
    [(x set) (set-cons:list x set eq?)]))

;; On lists, port to sets:
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
    [all
     (let loop ([set1 (car all)] [sets (cdr all)])
       (if (null? sets)
           set1
           (loop (union set1 (car sets)) (cdr sets))))]))

;; On lists, port to sets:
(define intersection
  (case-lambda
    [(set1 set2)
     (let loop ([set1 set1])
       (cond
         [(null? set1) '()]
         [(memq (car set1) set2) (cons (car set1) (loop (cdr set1)))]
         [else (loop (cdr set1))]))]
    [all 
     (let loop ([set1 (car all)] [sets (cdr all)])
       (if (null? sets)
           set1
           (loop (intersection set1 (car sets)) (cdr sets))))]))

;; On lists, port to sets:
(define difference
  (lambda (set1 set2)
    (let loop ([set1 set1]
	       [set2 set2])
    (cond
     ((null? set1) '())
     ((memq (car set1) set2) (loop (cdr set1) set2))
     (else (cons (car set1)  (loop (cdr set1) set2)))))
    ))

;; CURRENTLY UNUSED:
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

;; ============================================================



;; For one-argument functions:
(define compose
  (case-lambda
    [(f) f]
    [(f g) (lambda (x) (f (g x)))]
    [args
     (let ([funs (reverse args)])
       (lambda (x) 
	 (let loop ([x x] [ls funs])
	   (if (null? ls) x
	       (loop ((car ls) x) (cdr ls))))))]) )
;; 1000 of the same function:
;;   10,000 invocations: 100ms
;; 1000 different functions:
;;   10,000 invocations: 130ms
;;
;; 5 of the same function:
;;   10^7 invocations: 720ms
;; 5 different functions:
;;   10^7 invocations: 910ms

;; This behaves much worse on both large and small numbers of functions:
#;
(define compose
  (case-lambda
    [(f) f]
    [(f g) (lambda (x) (f (g x)))]
    [(f . rest)
     (lambda (x)
       (f ((apply compose rest) x)))]))
;; 1000 of the same function:
;;   10 invocations: 420ms
;; 1000 different functions:
;;   10 invocations: 360ms 
;;
;; 5 of the same function:
;;   10^7 invocations: 2940ms
;; 5 different functions:
;;   10^7 invocations: 3100ms


;; Works for multiple arguments/return values:
(define compose/values
  (lambda (f g)
    (lambda args
      (call-with-values 
	  (lambda () (apply g args))
	  f))))


(define disp
  (lambda args
    (let loop ((args args))
      (if (null? args)
          (begin (newline) (newline))
          (begin (display (car args))(display " ")
                 (loop (cdr args)))))))

(define (pp . args)
  (parameterize ([pretty-line-length 130]
		 [print-level 40]
		 [print-length 100])
    (apply pretty-print args)))


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
;;   This is a heavy-weight, expensive function, but darn useful!!
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

(define (deep-flatten input)
  (let loop ((x input))
    (cond
     [(list? x) (apply append (map loop x))]
     [(vector? x) (apply append (map loop (vector->list x)))]
     [else (list x)])))
          
;[01.10.23] - I'm surprised this wasn't added a million years ago:
(define (deep-memq ob struct)
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

;; This could be done with a much more effcient (and complex) structure matching.
(define (deep-member? ob struct)
  (let outer ([struct struct])
    (or (equal? ob struct)
;    (or (eqv? ob struct)
	(and (vector? struct)
	     (let inner ([i 0])
	       (cond
		[(= i (vector-length struct)) #f]
		[(outer (vector-ref struct i)) #t]
		[else (inner (add1 i))])))
	(and (pair? struct)
	     (or (outer (car struct))
		 (outer (cdr struct)))))))


;; [2007.03.17] This one will go inside vectors, but the search item
;; must be found as the car of a pair.  The search item in the
;; beginning (or middle) of a vector won't count as a match.
(define (deep-assq ob struct)
  (let outer ([struct struct])
    (cond 
     [(pair? struct)
      (if (eq? ob (car struct))
	  struct
	  (or (outer (car struct))
	      (outer (cdr struct))))]
     [(vector? struct) 
      (let ([len (vector-length struct)])
	(let vloop ([i 0])
	  (cond
	   [(fx= i len) #f]
	   [(deep-assq ob (vector-ref struct i)) => id]
	   [else (vloop (fx+ 1 i))])))]
     [else #f])))


;; This is very useful for visualizing large SExp's with one or more
;; interesting small bits inside them.  You want to see the small
;; bits, and you want to see their position in the larger expression,
;; but you don't want to be swamped by everything else.
;; .param exp - The expression to search inside.
;; .param pred - A predicate indicating a substructure of interest.
(define (blaze-path-to exp pred)
  (define (show x) (or x '_))
  (let loop ([x exp])    
    (match x
     [,hit (guard (pred hit)) hit]
     [(,first ,[rest] ...)
      (cond 
       [(pred first)
	(cons first (map show rest))]
       [(loop first)
	=> (lambda (fst) (cons fst (map show rest)))]
       [(ormap id rest)
	(if (symbol? first)
	    (cons first (map show rest))
	    (cons '_ (map show rest)))]
       [else #f])]
     [,else #f])))
;; Frequent use case:
(define (blaze-path-to/assq exp . syms)
  (let* ([obs (apply append 
		     (map (lambda (sym) (deep-assq-all sym exp))
		       syms))])
    (blaze-path-to exp (lambda (x) (memq x obs)))
    ))

(define (deep-assq-all ob struct)
  (deep-all-matches (lambda (x) (and (pair? x) (eq? ob (car x))))
		    struct))


;(define (deep-count-occurrences ob struct)
;  (length (deep-all-matches (lambda (x) (eq? ob  x))
;			    struct)))


;; Lifted this from the internet, does this really work??
;   float x1, x2, w, y1, y2;
 
;          do {
;                  x1 = 2.0 * ranf() - 1.0;
;                  x2 = 2.0 * ranf() - 1.0;
;                  w = x1 * x1 + x2 * x2;
;          } while ( w >= 1.0 );

;          w = sqrt( (-2.0 * ln( w ) ) / w );
;          y1 = x1 * w;
;          y2 = x2 * w;
;; <br> <br>
;; [2006.02.12] This should have a stddev of 1.0 but it looks like it
;; has a stddev of more like .90.
(define (gaussian)
  (let ((x1 0.0) (x2 0.0) (w 0.0) (y1 0.0) (y2 0.0))
    (let loop ()
      (set! x1 (* 2.0 (- (random 1.0) 1.0)))
      (set! x2 (* 2.0 (- (random 1.0) 1.0)))
      (set! w  (+ (* x1 x1) (* x2 x2)))
      (if (>= w 1.0) (loop)))
    (set! w (sqrt (/ (* -2.0 (log w)) w)))
    (set! y1 (* x1 w))
    ;(set! y2 (* x2 w))
    ;(list y1 y2)
    (if (zero? (random 2))
	y1
	(- y1))
    ))


;;  [2004.06.11] Man also can't believe that I've never written this
;;  before.  This is dinky; a real version of this would do an
;;  alignment of the structures intelligently.  I don't know how
;;  actually hard of a problem that is.  (Graph matching is a bitch,
;;  no?)
(define (diff obj1 obj2)
  (let loop ([o1 obj1] 
	     [o2 obj2]
	     [index '()])
    (printf "~s: len ~s ~s\n" (reverse index)
	    (if (list? o1) (length o1) #f)
	    (if (list? o2) (length o2) #f))
    (cond
       [(equal? o1 o2) (void)]
       [(and (list? o1) (list? o2))
	(cond 
	 [(not (= (length o1) (length o2)))
	  (printf "Lengths differ at list index ~s.  ~s elements vs. ~s elements.\n."
		  (reverse index) (length o1) (length o2))
	  (printf "List one:\n")
	  (pretty-print o1)
	  (printf "Vs. List two: \n")
	  (pretty-print o2)]
	 [else (for-each (lambda (a b i) (loop a b (cons i index)))
			 o1 o2 (iota (length o1)))])]

       [(and (vector? o1) (vector? 02))
	(do ([i 0 (add1 i)])
	    ((= i (vector-length o1)))
	  (loop (vector-ref o1 i) 
		(vector-ref o2 i)
		(cons i (cons 'v index))))]
       [else (printf "\n  Diff at ~a: " (reverse index))
	     (display-constrained (list o1 35) " " (list o2 35))
	     (newline)]
       )))

;; This goes along, it's for performing deep-index lookups
(define (list-ref-deep ls ind)
  (let loop ((x ls) (ind ind))
    (if (null? ind) x
	(loop (list-ref x (car ind)) (cdr ind)))))


;; This strings out a list of all the cons cells in a given list (not
;; traversing car's).
(define (unfold-list lst)
  (let loop ((lst lst))
    (if (null? lst) '()
	(cons lst (loop (cdr lst))))))

(define (all-equal? ls)
  (if (null? ls) #t
      (let ((x (car ls)))
	(let loop ((ls (cdr ls)))
	  (cond
	   [(null? ls) #t]
	   [(equal? x (car ls)) (loop (cdr ls))]
	   [else #f])))))
		  

; ======================================================================

;; [2005.10.31]  This is a simple function to use for efficiency:

;; For example, if you have a set of numbers, you can convert it to a hash for
;; repeated membership checks:
(define (set->hashtab s)
  (let ((h (make-default-hash-table (length s))))
    (for-each (lambda (ob)
		(hashtab-set! h ob #t))
      s)
    h))

(define (hashtab->list h)
  (let ((ls '()))
    (hashtab-for-each (lambda (k x)
			(set! ls (cons (cons k x) ls)))
		      h)
    (reverse! ls)))

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


;; UNIT TESTER WAS HERE....

; =======================================================================
;;; Graph functions
;;; .section-id graphs

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
(define (graph-get-connected-component obj graph)
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

;; This essentially floods a "gradient" from a part of the graph,
;; noting the hopcounts everywhere else.
; This is getting seriously non-linear performance.
(define (graph-label-dists obj graph)
  ; Assumes eq? based hash tables:
  (define len (length graph))
  ;; We make a distance table mapping node-id to distance.
  (define dists (make-default-hash-table len))
  ;; Checklist: this marks the nodes we've been to:
  (define checklist (make-default-hash-table len))
  ; Convert the list to a hashtab for fast lookup:
  (define hgraph (make-default-hash-table len))
  (for-each (lambda (pr) (hashtab-set! hgraph (car pr) (cdr pr))
		         (hashtab-set! checklist (car pr) #f)
		         (hashtab-set! dists  (car pr) #f))
    graph)
  ; Now start at a point and do a breadth first flood:
  (let loop ((current (list obj)) (dist 0))
      (unless (null? current)
	(loop (apply append 
		     (map (lambda (point)
			    (if (hashtab-get dists point) '()
				(begin 
				  (hashtab-set! dists point dist)
				  (let ((nbrs (hashtab-get hgraph point)))
				    (if nbrs nbrs '())))))
		       current))
	      (add1 dist))))

  (map (lambda (pr)
	 (cons (cons (car pr) (hashtab-get dists (car pr)))
	       (cdr pr)))
    graph))


;; Takes a horizontal or simple graph.
(define (graph-neighbors g node)
  (apply append
	 (map cdr
	      (filter (lambda (nd) (eq? (car nd) node)) g))))

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

; =======================================================================


;; Split a list before the first element that satisfies predicate.
(define (split-before f origls)
  (let loop ([acc '()] [ls origls])
   (cond
    [(null? ls) (values origls '())]
    [(f (car ls)) (values (reverse! acc) ls)]
    [else (loop (cons (car ls) acc) (cdr ls))])))
     
;; Filter a list into two parts based ona predicate.
;; Doesn't maintain ordering.
;; [2008.01.12] Phasing out for Chez 7.4 (now primitive):
;; TODO: USE CHEZ'S PRIMITIVE VERSION:
#;
(define partition
    (lambda (f lst)
      (letrec ((loop
		(lambda (lst acc1 acc2)
		  (cond
                   [(null? lst) (values acc1 acc2)]
                   [(f (car lst)) (loop (cdr lst) (cons (car lst) acc1) acc2)]
                   [else (loop (cdr lst) acc1 (cons (car lst) acc2))]))))
	(loop lst '() '()))))


;; Quadratic complexity, should use hash tables:
(define partition-equal
  (lambda (lst eq)
    (if (not (or (pair? lst) (null? lst)))
	(error 'partition-equal "input must be a list: ~s" lst))
    (let loop ((lst lst))
      (if (null? lst) '()
	  (let* ([first (car lst)])
	    (let-values ([(ingroup outgroup) (partition (lambda (x) (eq x first)) (cdr lst))])
	      (cons (cons first ingroup) 
		    (loop outgroup))
	      ))))))

;; Simple utility, group a list into groups of N elements.  List lengt
;; must be divisible by N.
(define (group n origls)
  (let loop ([cnt n] [acc '()] [ls origls])
    (cond
     [(null? ls)
      (if (zero? cnt) (list (reverse! acc))
	  (error 'group "List length ~s was not divisible by: ~s" (length origls) n))]
     [(zero? cnt)
      (cons (reverse! acc) (loop n '() ls))]
     [else (loop (fx- cnt 1) (cons (car ls) acc) (cdr ls))]     
     )))




; =======================================================================

;; This generates a simple text progress meter.  Call it with a total,
;; and then invoke the returned thunk that many times to fill it up.
(define (display-progress-meter totalcount)
  (let ((ticksize (/ totalcount 100))
	(lasttick 0)
	(counter 0))
    (lambda ()
      (when (= 0 counter) 
	(printf "\nProgress:         20%                           50%                                              100%\n")
	(printf "[") (flush-output-port (current-output-port)))
      (set! counter (add1 counter))
      (if (= lasttick 100)
       #f ;; We're finished, signal that there's no more work to do
       (let ((newticks (floor (/ counter ticksize))))
	 (when (> newticks lasttick)
	  (for i = 1 to (- newticks lasttick)
	       (display #\.))
	  (flush-output-port (current-output-port))
	  (set! lasttick newticks))
	 (when (= newticks 100) (display #\]) (newline) (flush-output-port (current-output-port)))
	 #t)))))
;; Example
#;
(let ((f (display-progress-meter 100)))
    (for i = 1 to 100 
	 (for i = 1 to 100000)
	 (f)))


;; [2005.10.16] Making a simple interface to gnuplot for graphing results
;; of queries.
;;
;; Input: Either 
;;   1) A list of (X Y) pairs
;;   2) A list of numbers
;;   3) A stream of numbers/(X Y) pairs.
;; (See stream implementation, streams.ss.)
;; 
;; The flags input may contain 'lines, 'points, or 'boxes.
(define (gnuplot data . flags)
  (let ([fn1 "_temp_gnuplot.script"]
	[fn2 "_temp_gnuplot.dat"])
  (let ([scrip (force-open-output-file fn1)]
	[dat   (force-open-output-file fn2)]
	[setstmnts '()]
	[opts ""]
	[withclause "with linespoints"])

    (define (plot-one i d)
      (if (number? d)
	  (fprintf dat "~s ~s\n" i d)
	  (begin (for-each (lambda (n)
			     (fprintf dat "~s " n))
			   d)
		 (fprintf dat "\n"))))

    ;; Process flags:
    (for-each 
	(lambda (flag)
	  (match flag
	    [(title ,str) (set! setstmnts (cons (format "set title \"~a\"\n" str) setstmnts))]
	    [lines (set! withclause "with linespoints")]
	    [points (set! withclause "with points ps 4")]
	    [boxes (set! withclause "with boxes")
	     (set! setstmnts (cons "set style fill solid 1.000000 border -1;\n" setstmnts))]
	    ;[(opts ,str) (set! opts str)]
	    [,other (error 'gnuplot "unknown additional argument to gnuplot wrapper: ~s" other)]
	    ))
      flags)

    (if (null? data)
	(void)	
    (begin
    ;; Write script file:
    (fprintf scrip "set terminal x11;\n")
    (fprintf scrip "set autoscale;\n")
    (for-each (lambda (setline)
		(fprintf scrip setline))
      setstmnts)

    (fprintf scrip "plot ~s using 1:2 ~a" fn2 withclause)
    (if (list? (car data))
	(for n = 3 to (length (car data))
	     (fprintf scrip ", ~s using 1:~a ~a" fn2 n withclause)))
    (fprintf scrip ";\n")

    ;(fprintf scrip "exit;\n")
    ;; Write data file:
    (cond 
     [(list? data) 
      (if (not (or (andmap number? data) 
		   (andmap (lambda (l) (and (list? l) (andmap number? l)))
			   data)))
	  (error 'gnuplot "did not call gnuplot, invalid data set: ~s" data))
      (for-eachi plot-one data)]

#;
     [(stream? data)
      (error 'to-implement "FIXME not implemented yet!!")
      ]

     [else (error 'gnuplot "unknown input type, bad dataset: ~s" data)])
;    (for i = 1 to 20
;	 (fprintf dat "~a ~a\n" i (* i 2)))
    ;; Done.  Now close files and call it.
    (close-output-port scrip)
    (close-output-port dat)
    (let ([command (format "gnuplot -persist ~a" fn1)])
      (printf "Calling gnuplot with command ~s.\n" command)
      (system command))
    
;    (delete-file fn1)
;    (delete-file fn2)
  
  )))))


;; gnuplot_stream
;; .param flags 
;; .returns A function that takes new data and updates the graph.
;;  The function closes the process when it receives the input 'exit.
;;  The new data must consist of a list of numbers or of (X,Y) pairs.
;;  
(define (gnuplot_pipe . flags)
 (let (;[fn1 "_temp_gnuplot.script"]
	[fn2 (format "/tmp/_temp_gnuplot.dat.~a.pipe" 
		     (+ (real-time) (random 100000)))])
    (if (file-exists? fn2) (delete-file fn2))
    (system (format "mkfifo ~s" fn2))
  (let-values ([(scrip extract) (open-string-output-port)])
    (let ([dat   #f]
	  [first-time #t]
	  [setstmnts '("set terminal x11;\n")]
	  [opts ""]
	  [withclause "with linespoints"])
      
    ;; This plots a point.
    (define (plot-one i d)
      (cond
       [(list? d)
	(for-each (lambda (n) (fprintf dat "~s " n)) d)
	(fprintf dat "\n")]
#;
       [(number? d) (fprintf dat "~s ~s\n" i d)]
#;
       [(or (vector? d) (list? d))
	((if (vector? d) vector-for-each for-each) 
	 (lambda (n) (fprintf dat "~s " n)) d)
	(fprintf dat "\n")]
       [else 'gnuplot_pipe:plot-one "invalid datapoint.  Cannot plot ~s" d]))
    
    (define (normalize-data dat)
      (mapi (lambda (i point)
	      (cond
	       [(number? point) (list i point)]
	       [(vector? point) (vector->list point)]
	       [(list? point)   point]))
	    dat))
    
    (define (set-ranges! data port)
      (define (extend-right a b) 
	(let ([val (+ b (* 0.1 (- b a)))])
	  (if (zero? (- b a)) (+ val 1) val)))
      (define (extend-left  a b) 
	(let ([val (- a (* 0.1 (- b a)))])
	  (if (zero? (- b a)) (- val 1) val)))
      (ASSERT (not (null? data)))

;      (fprintf port "set xrange [~a:~a];\n" -10 10)
;      (fprintf port "set yrange [~a:~a];\n" -10 10)

      ;; Gnuplot's autoscale options are so damn stupid that I have to manually compute the ranges here.
      ;; (Otherwise you can't see the top's and bottoms of my square waves, because it leaves no margin!)
      (let loop ([data data]
		 [xmin +inf.0] [xmax -inf.0]
		 [ymin +inf.0] [ymax -inf.0])
	(cond
	 [(null? data)
;	  (inspect (list (extend-left xmin xmax) (extend-right xmin xmax)))
	  (fprintf port "set xrange [~a:~a];\n" (extend-left xmin xmax) (extend-right xmin xmax))
	  (fprintf port "set yrange [~a:~a];\n" (extend-left ymin ymax) (extend-right ymin ymax))]
	 [else
	  (loop (cdr data)
		(min xmin (caar data))
		(max xmax (caar data))
		(min ymin (cadar data))
		(max ymax (cadar data)))])))
    
    ;; Process flags:
    (for-each 
	(lambda (flag)
	  (match flag
	    [lines (set! withclause "with linespoints")]
	    [boxes (set! withclause "with boxes")
	     (set! setstmnts (cons "set style fill solid 1.000000 border -1;\n" setstmnts))]
	    ;[(opts ,str) (set! opts str)]
	    [,other (error 'gnuplot "unknown additional argument to gnuplot wrapper: ~s" other)]
	    ))
      flags)
    
    ;; Write script file:
    ;(fprintf scrip "set autoscale x;\n")
    
    (for-each (lambda (setline)
		(fprintf scrip setline))
      setstmnts)
    (fprintf scrip "plot ~s using 1:2 ~a" fn2 withclause)
    (fprintf scrip ";\n")
    ;(fprintf scrip "exit;\n")
    ;(close-output-port scrip)
    (let ((scrip-result (extract)))
    ;; Open the process and set the pipes.
    (let-match ([(,inp ,outp ,pid) (process (format "gnuplot -"))])
      (printf "gnuplot process running...\n")

    (lambda msg
      (cond
       [(null? msg) ;; This just does a replot.
	(ASSERT (not first-time))
	(begin (display "replot\n" outp)
	       (flush-output-port outp))
	]
       [(eq? (car msg) 'exit)
	;; Done.  Now close files and call it.
	    (when dat (close-output-port dat))
	    (close-output-port outp)
	    (close-input-port inp)
	    ;;(delete-file fn1)
	    ;;(delete-file fn2)
	    ]
       ;; This is a batch of data:
       [else 
	(ASSERT list? (car msg))
	(ASSERT null? (cdr msg))
	(let ([payload (normalize-data (car msg))])
	  ;; Having weird problems with it drawing two graphs on top of eachother.
	  ;; HACK! HACK! HACK!
	  ;; What if we totally kill the pipe between communications?
	  (if (file-exists? fn2) (delete-file fn2))
	  (system (format "mkfifo ~s" fn2))

	  ;; Send replot message
	  (if first-time
	      (begin (set-ranges! payload outp)
		     (display scrip-result outp)
		     (flush-output-port outp)
		     (set! first-time #f))
	      (begin (set-ranges! payload outp)
		     (display "replot\n" outp)		   
		     (flush-output-port outp)))

	  ;;(printf "Replot message sent.\n")
	  ;;(printf "Writing new dataset to pipe ~s\n" fn2)

	  ;; Open a session on the pipe
	  (error 'gnuplot_pipe "Cannot append to output file yet.")
	  ;(set! dat (open-output-file fn2 'append))
	  ;; [2008.12.05] Ack... is there not an append mode for file-output?
	  (set! dat (open-file-output-port fn2 (file-options no-fail no-truncate) (buffer-mode block) (native-transcoder)))	  
	  
	  ;;(printf "Opened pipe.\n")
	  ;; Write dataset to pipe:
	  #;
	  (if (not (or (andmap number? data) 
		       (andmap (lambda (l) (and (list? l) (andmap number? l)))
			       data)))
	      (error 'gnuplot "did not call gnuplot, invalid data set: ~s" data))
	  (for-eachi plot-one payload)

	  ;; Close pipe to end session.
	  (close-output-port dat)
	  (set! dat #f))
	;;(printf "Data written to pipe.\n")
	])
      ))
      ))))) 
;; Example:
#;
(let* ([ind (map (lambda (x) (* (- x 150) 0.1)) (iota 300))] [ys (map sin ind)]) (gnuplot (map list ind ys)))


(define histogram
  (case-lambda 
    [(ls) (error 'histogram "auto-bin sizes not implemented.")]
    [(ls binsize) (histogram ls binsize (apply min ls))]
    [(ls binsize start)
     (let* ((max (apply max ls))
	   (size (inexact->exact (ceiling (/ (- max start) binsize))))
	   (bins (make-vector (add1 size) 0)))
       (let loop ((ls ls))
	 (if (null? ls)
	     (mapi (lambda (i count)
		     (list (+ (* binsize i) start) count))
		   ;; Fix fencepost condition:
		   (if (zero? (vector-ref bins size))
		       (rdc (vector->list bins))
		       (vector->list bins)))
	     (let ((ind (inexact->exact (floor (/ (- (car ls) start) binsize)))))
	       (vector-set! bins ind (add1 (vector-ref bins ind)))
	       (loop (cdr ls))
	       ))))
     ]))
;;
;(histogram '(1 1 1 2 3 4 5 5 5 5 6 6 7) 2)
;; 1-3 3-5 5-7

;; Return a string which somehow or another includes the date.
#;
(define (date-string)
  (let ([pipesa
	 (case (machine-type)
	   [(ti3le i3le ppcosx) (process "date")]
	   [(i3nt ti3nt) (error 'date "don't know how to do date in windows")]
	   [else (error 'date
			"Ryan, what kind of machine are you on?!: ~a"
			(machine-type))])])
    (let ((res (read-line (car pipes))))
      (close-port (car pipes))
      (close-port (cadr pipes))

      res)))



;; Add commas to a big number for printing. <br>
;; [2006.02.21]  Moved this hear from other utility files.
(define comma-number
  (lambda (n)
    (define bignumber-name-list
      '(; [billion 9] [trillion 12] ;; Don't label these little guys.
	[quadrillion  15]
	[quintillion  18]
	[sextillion  21]
	[septillion  24]
	[octillion  27]
	[nonillion  30]
	[decillion  33]
	[undecillion  36]
	[duodecillion  39]
	[tredecillion  42]
	[quattuordecillion  45]
	[quindecillion  48]
	[sexdecillion  51]
	[septendecillion  54]
	[octodecillion  57]
	[novemdecillion  60]
	[vigintillion  63]
	[unvigintillion  66]
	[duovigintillion  69]
	[trevigintillion  72]
	[quattuorvigintillion 75]
	[quinvigintillion 78]
	[sexvigintillion 81]
	[septenvigintillion  84]
	[octovigintillion  87]
	[novemvigintillion  90]
	[trigintillion  93]
	[untrigintillion  96]
	[duotrigintillion  99]
	[googol 100]
	[tretrigintillion  102]
	[quattuortrigintillion 105]
	[quintrigintillion 108]
	[sextrigintillion 111]
	[septentrigintillion 114]
	[octotrigintillion  117]
	[novemtrigintillion  120]))
    (cond
      [(or (integer? n)) ;(fixnum? n) (bignum? n)) ;(integer? n)
       (let* ([strls (string->list (number->string n))]
              [power10 (sub1 (length strls))]
              [groups 0])
         (string-append
           (list->string
             (reverse
               (let loop ([ls (reverse strls)])
                 (if (< (length ls) 4)
                     ls
                     (begin
                       (set! groups (add1 groups))
                       (append (list (car ls) (cadr ls) (caddr ls) #\,)
                               (loop (cdddr ls)))
                       )))))
           ;This assumes that the list of names is in magnitude order.
           (let ([min (cadar bignumber-name-list)] ;better not be null
                 [max (cadar (reverse bignumber-name-list))])
             (cond
               [(< power10 min)
                ""]
               [(> power10 max)
                (format " (10^~s)" power10)]
               [else
                 (format
                   " (10^~s -- ~a)" power10
                   (let loop ([numnames bignumber-name-list]
                              [curname #f]
                              [curmag #f])
                     ;At this point, the number to be printed is known
                     ;to be between the min and max magnitudes.
                     (if (null? numnames)
                         (error 'comma-number
                                "There is a bug in this function, number was~a"
                                " thought to be within bounds, but isn't.")
                         (let ([name (symbol->string (caar numnames))]
                               [mag (cadar numnames)])
                           (if (< power10 mag)
                               (string-append
                                 (case (- power10 curmag)
                                       [(0) ""]
                                       [(1) "ten "]
                                       [(2) "hundred "]
                                       [(3) "thousand "]
                                       [(4) "ten thousand "]
                                       [(5) "hundred thousand "]
                                       [(6) "million "]
                                       ;There better not be bigger gaps than that.
                                       [else "MANY "])
                                 curname)
                               (loop (cdr numnames) name mag))))))]))))]
      [else n]
;      #;[else (error 'comma-number
;                     "doesn't know how to comma non integers: ~a" n)]
      )))


; =======================================================================



;; <TODO> <TOIMPLEMENT> Ryan, write a function that changes the direction of links:
;(define graph-flip...

;(IFCHEZ (include "generic/util/streams.ss")
;	(include "streams.ss"))

;===============================

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

 
;=======================================================================
;; And here are the unit tests for this file... Don't have many of these yet.

(define-testing testhelpers 
  (default-unit-tester "helpers.ss: my messy utils file." 
    `(
    ;; First some small tests that check compatibility between Chez
    ;; and PLT.  These are really testing chez_compat.ss more than
    ;; this module, but I want them to run under both Chez and PLT.
    [(atom? '#()) #t]
;    [(cflonum? 3.0+0.0i) #t]     ;; [2008.04.28] TEMPTOGGLE - disabling for ikarus

    [99 99]

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

    [(string-split "abc def qet foo faz " #\space)
     ("abc" "def" "qet" "foo" "faz" "")]
    [(string-split "abc def qet foo   faz" #\space)
     ("abc" "def" "qet" "foo" "" "" "faz")]

    ;; Freezes PLT atm:
    [(with-output-to-string 
       (lambda ()
	 (display-constrained "test  " '(abcdefghijklmnop 10) 
			      " again  " '(abcdefghijklmnop 16) 
			      '(abcdefghijklmnop 17))))
     "test  abcdefg... again  abcdefghijklm...abcdefghijklmnop"]

; This doesn't make schemedoc happy, eliminating:
;    [(unfold-list '(a b c d))
;     ((a . #0=(b . #1=(c . #2=(d)))) #0# #1# #2#)]

    [(graph-get-connected-component 'e '((a b c) (b c) (c) (d e f) (e g h)))   (h g e)]
    [(graph-get-connected-component 'h '((a b c) (b c) (c) (d e f) (e g h)))   (h)]
    [(graph-get-connected-component 'a '((a b c) (b c) (c) (d e f) (e g h)))   (c b a)]
    [(graph-get-connected-component 'a '((a b) (b a)))                         (b a)]
    [(graph-get-connected-component 'a '((a b) (b a c)))                       (c b a)]

    [(list (substring? "ab" "abc")
	   (substring? "ab" "a")
	   (substring? "ab" "ab")
	   (substring? "bc" "abc")
	   (substring? "ac" "abc"))
     (#t #f #t #t #f)]


    ["Test with-error-handlers"
     (let ((return '()))
       (let/ec k
	 (with-error-handlers disp
			      (lambda () (k (void)))
			      (lambda ()
				(set! return (cons 1 return))
				(error 'foo "bar")
				(set! return (cons 2 return)))))
       return)
     (1)]

;; [2005.09.27] TEMP:  These are malfunctioning for some reason: ; TODO FIXME:
;; [2005.11.05]  Hmm. The problem went away but now it came back, disabling again:
#|
    ["Test the default unit tester... (retry feature)"
     (parameterize ([,default-unit-tester-retries 1000]) ;; Set retries way up
       (let ([fun (default-unit-tester "testing tester" 
		    `[(3 3) ((reg:random-int 10) 3)]
		    'retry)])
	 (fun 'quiet)))
     #t]
    ["This just gives the retry argument at test time."
     (parameterize ([,default-unit-tester-retries 1000]) ;; Set retries way up
       (let ([fun (default-unit-tester "testing tester" 
		    `[(3 3) ((reg:random-int 3) 0)])])
	 (fun 'quiet 'retry)))
     #t]
    ["This just gives the retry argument within the test itself."
     (parameterize ([,default-unit-tester-retries 1000]) ;; Set retries way up
       (let ([fun (default-unit-tester "testing tester" 
		    `[(3 3) ["" retry (reg:random-int 3) 0]])])
	 (fun 'quiet)))
     #t]
    ["This tests unit tests that call error"
     (let ([fun (,default-unit-tester "testing tester" 
		  `(["" (error 'foo "bar") 
		     error]))])
	 (fun 'quiet))
     #t]
    ["This just makes sure the unit tester returns #f when a test fails."
     (let ([fun (,default-unit-tester "testing tester"
		  `([3 3] [4 5]))])
       (fun ))
     #f]
|#

    ["Apply ordered." 
     (let ((x '())) 
       (apply-ordered list (set! x (cons 1 x)) (set! x (cons 2 x)) (set! x (cons 3 x))) 
       (reverse x))
     (1 2 3)]

    [(map cadr (histogram '(1 1 1 2 3 4 5 5 5 5 6 6) 2))   (4 2 6)]
    [(map cadr (histogram '(1 1 1 2 3 4 5 5 5 5 6 6 7) 2)) (4 2 6 1)]

    )))

; ======================================================================
;;;  END Generic core of helpers.ss:
; ======================================================================



;; [2005.11.14] This is like chez's system command, except it routes
;; the output through the standard scheme output port.  This way it
;; can be captured/redirected by the normal means.
(define (system/echoed str)
  (let-match ([(,in ,out ,id) (process str)])
    (let loop ((line (read-line in)))
      (unless (or (not line) (eof-object? line))
	(display line)(newline)
	(loop (read-line in))))
    ;; Alas, I don't know how to get the error-code through "process",
    ;; so we must assume there was no error:
    0))
;; Example: compare (with-output-to-string (lambda () (system "ls")))
;;               to (with-output-to-string (lambda () (system/echoed "ls")))

;; [2005.11.17] This one is similar 
;; It looks like the chez primitive doesn't provide a handle on stderror.
(define (system-to-str str)
  (let-match ([(,in ,out ,id) (process str)])
    (let-values (((p extract) (open-string-output-port)))
    (let loop ((c (read-char in)))
      (if (eof-object? c)	  
	  (begin 
	    (close-input-port in)
	    (close-output-port out)
	    (extract))
	  (begin (display c p)
		 (loop (read-char in))))))))
 

#;
(define (crit-printf . args)
  (critical-section (apply printf args)))

;; [2006.02.20] Copying here from my other utility files.
;; This is a very useful little utility that allows you to search
;; through all the currently bound top-level symbols.
#;
(define grep-oblist
  ;; Only for unixy-machines
  (lambda (str)
    (let* ([name "___temp___.txt"]
;           [out (open-output-file name 'replace)])
           [out (open-output-file name )])
      (for-each (lambda (o) (display o out) (newline out))
                (oblist))
      (system (format "cat \"~a\" | grep \"~a\"" name str))
      (close-port out)
      (delete-file name)
      (void))))

;; [2005.11.17] This is reminescent of the perl command "chomp" 
(define (chomp s)
  (cond
   [(string? s)
    (let ((ind (sub1 (string-length s))))
      (if (eq? #\newline (string-ref s ind))
	  (substring s 0 ind)
	  s))]
   [(null? s) '()]
   [(pair? s) (cons (chomp (car s)) (chomp (cdr s)))]
   [else (error 'chomp "bad input: ~s" s)]))

;; TODO: write a more efficient version with block-read!!! [2006.02.22]

;; Used for expanding strings with environment variables in them.
(define (shell-expand-string s)
  (chomp (system-to-str (string-append "exec echo " s))))

;; Return the current time in seconds since 1970:
(define seconds-since-1970
  (lambda ()
    (let ((absolute (string->number (chomp (system-to-str "date +%s"))))
	  (syncpoint (quotient (real-time) 1000)))
      (lambda () 
	(+ (- (quotient (real-time) 1000) syncpoint)
	   absolute)))))

; --mic
; find all occurrences of a specific flag in a list of flags.
; the flag may be followed by a fixed number of arguments.
; the return result is a list of the occurances, e.g.:
; (find-in-flags 'disable 1 '(a b c disable d e f disable g h))
;   => ((disable d) (disable g))
;
; gives an error if too few args.
;
(define (find-in-flags sym n flags)
  (cond [(null? flags) '()]
        [(eq? (car flags) sym)
         (cons (list-head flags (+ n 1))
               (find-in-flags sym n (list-tail flags (+ n 1))))]
        [else (find-in-flags sym n (cdr flags))]))


; --mic
; result is (rn ... r2 r1 r0), where r0 is first call, r1 second, etc.
; n must be >= 0
;
(define (n-times p n . args)
  (let loop ((i 0)
             (results '()))
    (if (< i n)
        (loop (+ i 1) (cons (apply p args) results))
        results)))

;; This is just a shorthand:
(IFCHEZ
 (define (force-open-output-file file)
   (open-output-file file 'replace))
 (define (force-open-output-file file)
   (open-file-output-port file  (file-options no-fail) (buffer-mode block) (native-transcoder))
  ))



) ;; End library
