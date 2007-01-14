

;;;; .title Utility Function Library (helpers.ss)
;;;; RRN: this file needs some serious cleaning-out.  It still has a bunch
;;;; of old junk in it that's not used by the Regiment system.


;==============================================================================;

;;;; <br> REQUIRES/DEPENDS: On chez/plt primitive open-output-string.
;;;; <br> REQUIRES/DEPENDS: (On other chez/plt features which I'm not aware of...)

;==============================================================================;

;; This is not strictly R5RS but it should work in both chez and plt,
;; as long as plt has "compat.ss" loaded.  
;; (For example, it requires flush-output-port)

;(define region-primitives)
;(define anchor-primitives)

(define (id x) x)
(define (ignore x) (void))

(define symbol-append
  (lambda args
    (string->symbol (apply string-append (map symbol->string args)))))

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
  (list-ref (sort < ls) (quotient (length ls) 2)))

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
;      [(list? lsvec) (+ (length lsvec)
;                        (apply + (map count-nodes lsvec)))]

      [(pair? lsvec) (+ 1 (count-nodes (car lsvec))
			  (count-nodes (cdr lsvec)))]
;                        (apply + (map count-nodes lsvec)))]

      [(vector? lsvec) (+ (vector-length lsvec)
                          (let loop ((i (sub1 (vector-length lsvec))))
                            (if (zero? i)
                                (count-nodes (vector-ref lsvec 0))
                                (+ (count-nodes (vector-ref lsvec i))
                                   (loop (sub1 i))))))]
      [else (error 'count-nodes
                   "only knows how to count the nodes of a list or vector, not ~a" lsvec
                   )])))

;; [2005.10.16] Not tail recursive!!
(define (mapi f ls)
  (let mapi-loop ((i 0) (ls ls))
    (if (null? ls)
	'()
	(cons (f i (car ls))
	      (mapi-loop (add1 i) (cdr ls))))))
;; [2005.10.16] 
(define (for-eachi f ls)
  (let foreachi-loop ((i 0) (ls ls))
    (if (null? ls)
	(void)
	(begin (f i (car ls))
	       (foreachi-loop (add1 i) (cdr ls))))))

(define (with-evaled-params params th)
  (let loop ((ls params))
    (if (null? ls) 
	(begin 
	  ;(disp "PARAMED TIME: " (sim-timeout))
	  ;(inspect sim-timeout)
	  (newline)
	  (th))
	(parameterize ([(eval (caar ls)) (eval (cadar ls))])
	  (printf "Setting parameter: ~a ~a\n" 
		  (caar ls) ;(pad-width 23 (caar ls))
		  (cadar ls))
	  (loop (cdr ls))))))


;; [2006.02]
(define progress-dots 
  (IFCHEZ
  (case-lambda 
    [(th)      (progress-dots th 50000000)]
    [(th fuel) (progress-dots th fuel 
			      (lambda () (display #\.) (flush-output-port)))]
    [(th fuel progress)
     (let loop ((engine (make-engine th)))
       (progress)
       (engine fuel
	       (lambda (time-remaining val) (newline) (flush-output-port) val)
	       loop))])
  (lambda args (error 'progress-dots "not implemented in chez"))))

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
;; This removes all!  Not just first.
(define (assq-remove-all x ls)
  (let loop ((ls ls))
    (cond
     [(null? ls) '()]
     [(eq? (caar ls) x) (loop (cdr ls))]
     [else (cons (car ls) (loop (cdr ls)))])))

(define (remq-all x ls)
  (let loop ((ls ls) (acc ()))
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

(define vector-for-each
  (lambda (f! v)
    (let ([len (vector-length v)])
      (do ([i 0 (add1 i)])
          ((= i len) (void))
          (f! (vector-ref v i))))))
(define vector-map!
  (lambda (f v)
    (do ([i (sub1 (vector-length v)) (sub1 i)])
        ((= -1 i) v)
        (vector-set! v i (f (vector-ref v i))))))
(define vector-map
  (lambda (f v)
    (let ([newv (make-vector (vector-length v))])
      (do ([i (sub1 (vector-length v)) (sub1 i)])
          ((= -1 i) newv)
          (vector-set! newv i (f (vector-ref v i)))))))


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

#;
(define list->set
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


(define (list-repeat! ls)
  (if (null? ls) (error 'list-repeat! "cannot create infinite list from null list."))
  (let loop ((p ls))
    (if (null? (cdr p))
	(set-cdr! p ls)
	(loop (cdr p))))
  ls)
(define (make-repeats ls numelems)
  (list-head (list-repeat! (list-copy ls)) numelems))


;;----------------------------------------

;[2001.07.15]
(define file->slist
  (lambda (filename . opts)
    (let ([p (if (input-port? filename) filename
		 (apply open-input-file filename opts))])
      (port->slist p))))
(define port->slist
  (lambda (p)
    (let porttoslistloop ([exp (read p)] [acc '()])
        (if (eof-object? exp)
            (begin (close-input-port p)
                   (reverse! acc))
            (porttoslistloop (read p) (cons exp acc))))))

;; prints each expression to file.
(define slist->file
  (case-lambda 
   [(slist fn) (slist->file slist fn 'write)]
   [(slist fn method)
    (let ([p (open-output-file fn 'replace)])
      (parameterize ([print-level #f]
		     [print-length #f]
		     [pretty-maximum-lines #f])
	  (for-each (lambda (x) 
		      (case method
			[(write plain) (write x p)(newline p)]
			[(pretty pretty-print) 
			 (parameterize ([print-level #f]
					[print-graph #f])
			   (pretty-print x p))])
		      (newline p))
	    slist))
      (close-output-port p))]))

;; [2006.02.20] Rewrote to have an efficient version:
(define file->string
  (lambda (filename . opts)
    (let* ([inp (open-input-file filename
				 (if (not (memq 'unbuffered opts))
				     (cons 'buffered opts)
				     opts))]
	   [outp (open-output-string)]
	   [block-size 1024]
	   [block (make-string block-size)])
      (let loop ([count (block-read inp block block-size )])
        (if (eof-object? count)
	    (begin (close-input-port inp)
		   (get-output-string outp))
            (begin (block-write outp block count)
		   (loop (block-read inp block block-size ))))))))

(define string->file
  (lambda (str fn)
    (let ([p (open-output-file fn 'replace)])
      ;(fprintf p str)
      (display str p)
      (close-output-port p))))

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
			(let ([port (open-input-string str)])
			  (let munch-line ()
			    (let ([x (read port)])
			      (if (eof-object? x)
				  '()
				  (cons x (munch-line))))))
			acc)))))))]))

;; This uses read to get an sexp from a string:
;(define string->sexp
;  (lambda (s)    
;    (read (open-input-string s))))

;(define partition-string

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


;;; Procedures for manipulating sets.

#;
(define genmember
  (lambda (eqfun)
    (cond
     [(eq? eqfun eq?) memq]
     [(eq? eqfun equal?) member]
     [else (error 'genmember "unsupported eq-fun: ~a\n" eqfun)])))
;    (lambda (x ls)
;      (

;; eq? based:
(define (set? ls)
  (or (null? ls)
      (and (not (member (car ls) (cdr ls)))
	   (set? (cdr ls)))))
(define (setq? ls)
  (or (null? ls)
      (and (not (memq (car ls) (cdr ls)))
	   (set? (cdr ls)))))

;; Inefficient for ordered types:
(define (subset? l1 l2)
  (andmap (lambda (a) (member a l2)) l1))
(define (subsetq? l1 l2)
  (andmap (lambda (a) (member a l2)) l1))

(define set-comparator
  (lambda (testfun memfun)
    (lambda (lst1 lst2)
      (letrec ((loop (lambda (lst1 lst2)
		       (cond
			[(and (null? lst1) (null? lst2)) #t]
			[(or (null? lst1) (null? lst2)) #f]
			[(memfun (car lst1) lst2) (loop (cdr lst1) (list-remove-all (car lst1) lst2))]
			[else #f]))))
	(if (and (testfun lst1) (testfun lst2))
	    (loop lst1 lst2)
	    (error 'set-eq/equal? "must take two sets, improper arguments: ~s ~s" lst1 lst2))))))
(define set-eq?    (set-comparator setq? memq))
(define set-equal? (set-comparator set?  member))

;; [2005.10.11]  Added reverse! to make the result in the same order as orig. <br>
;; NOTE: Uses eq? !
(define list->set
  (lambda (ls)
    (if (null? ls) '()
	(reverse! 
	 (set-cons(car ls) (list->set (cdr ls)))))))
;; [2006.01.23] Added version that uses equal?  For structure based equivalence.
#;(define list->set_equal
  (lambda (ls)
    (let loop ((ls ls))
      (if (null? ls) '()
	  (if (member (car ls) (cdr ls))
	      (loop (cdr ls))
	      (cons (car ls) (loop (cdr ls))))))))

;; NOTE: Uses eq? !
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
    (let loop ([set1 set1]
	       [set2 set2])
    (cond
     ((null? set1) '())
     ((memq (car set1) set2) (loop (cdr set1) set2))
     (else (cons (car set1)  (loop (cdr set1) set2)))))
    ))

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

;; Produce a list of consecutive integers.
;; .example (iota n) => (0 1 ... n-1)
;; .example (iota i n) => (i i+1 ... i+n-1)
(define iota
  (case-lambda
    [(n) (iota 0 n)]
    [(i n)
     (if (= n 0)
         '()
         (cons i (iota (+ i 1) (- n 1))))]))

;; For one-argument functions:
(define (compose f g) (lambda (x) (f (g x))))

;; Works for multiple arguments/return values:
(define compose/values
  (lambda (f g)
    (lambda args
      (call-with-values 
	  (lambda () (apply g args))
	  f))))

;;RRN [01.09.17] :
(define make-code
  (lambda (expr*)
    (match (match `(code ,@expr*)
             [(code ,[expr*] ...) (apply append expr*)]
             [,expr (list expr)])
      [(,x) x]
      [(,x ,x* ...) `(code ,x ,x* ...)])))

; [2005.09.27] Why redefine this prim?
#;
(define with-output-to-string
  (lambda (th)
    (parameterize ([current-output-port (open-output-string)])
      (th)
      (get-output-string (current-output-port)))))


;; Note: we can only handle exact integers in the fixnum range.
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

(define (deep-flatten input)
  (let loop ((x input))
    (cond
     [(list? x) (apply append (map loop x))]
     [(vector? x) (apply append (map loop (vector->list x)))]
     [else (list x)])))
          
;[01.10.23] - I'm surprised this wasn't added a million years ago:
(define (deep-memq? ob struct)
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


;; [2004.06.11] This one doesn't do vectors:
(define (deep-assq ob struct)
  (let outer ([struct struct])
    (if (pair? struct)
	(if (eq? ob (car struct))
	    struct
	    (or (outer (car struct))
		(outer (cdr struct))))
	#f)))

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
    (printf "~s: len ~s ~s~n" (reverse index)
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
		(cons i (cons 'v index))))]
       [else (printf "~n  Diff at ~a: " (reverse index))
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
  (let ((h (make-default-hash-table)))
    (for-each (lambda (ob)
		(hashtab-set! h ob #t))
      s)
    h))

(define (hashtab->list h)
  (let ((ls ()))
    (hashtab-for-each (lambda (k x)
			(set! ls (cons (cons k x) ls)))
		      h)
    (reverse! ls)))

;; [2006.08.16] More efficient:
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
			    (if (hashtab-get dists point) ()
				(begin 
				  (hashtab-set! dists point dist)
				  (let ((nbrs (hashtab-get hgraph point)))
				    (if nbrs nbrs ())))))
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



(define (split-before f origls)
  (let loop ([acc '()] [ls origls])
   (cond
    [(null? ls) (values origls '())]
    [(f (car ls)) (values (reverse! acc) ls)]
    [else (loop (cons (car ls) acc) (cdr ls))])))
     

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
	(printf "[") (flush-output-port))
      (set! counter (add1 counter))
      (if (= lasttick 100)
       #f ;; We're finished, signal that there's no more work to do
       (let ((newticks (floor (/ counter ticksize))))
	 (when (> newticks lasttick)
	  (for i = 1 to (- newticks lasttick)
	       (display #\.))
	  (flush-output-port)
	  (set! lasttick newticks))
	 (when (= newticks 100) (display #\]) (newline) (flush-output-port))
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
;; (See stream implementation, this file.)
(define (gnuplot data . flags)
  (let ([fn1 "_temp_gnuplot.script"]
	[fn2 "_temp_gnuplot.dat"])
  (let ([scrip (open-output-file fn1 'replace)]
	[dat   (open-output-file fn2 'replace)]
	[command (format "gnuplot ~a -" fn1)]
	[setstmnts '()]
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
	  (case flag
	    [(lines) (set! withclause "with linespoints")]
	    [(boxes) (set! withclause "with boxes")
	     (set! setstmnts (cons "set style fill solid 1.000000 border -1;\n" setstmnts))]))
      flags)

    (if (null? data)
	(void)	
    (begin
    ;; Write script file:
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
     [(stream? data)
      (error 'to-implement "FIXME not implemented yet!!")
      ]
     [else (error 'gnuplot "unknown input type, bad dataset: ~s" data)])
;    (for i = 1 to 20
;	 (fprintf dat "~a ~a\n" i (* i 2)))
    ;; Done.  Now close files and call it.
    (close-output-port scrip)
    (close-output-port dat)
    (printf "Calling gnuplot with command ~s.\n" command)
    (system command)

;    (delete-file fn1)
;    (delete-file fn2)
  
  )))))
;; Example:
#;
(let* ([ind (map (\\ x (* (- x 150) 0.1)) (iota 300))] [ys (map sin ind)]) (gnuplot (map list ind ys)))

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

;(include "generic/util/streams.ss")
;(include (build-path (REGIMENTD) "generic/util/streams.ss"))
(IFCHEZ (include "generic/util/streams.ss")
	;(include (build-path "~/wavescript/src" "generic/util/streams.ss")))
         (include "streams.ss"))
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

    [(string-split "abc def qet foo faz " #\space)
     ("abc" "def" "qet" "foo" "faz" "")]
    [(string-split "abc def qet foo   faz" #\space)
     ("abc" "def" "qet" "foo" "" "" "faz")]

    [(let ((s (open-output-string)))
       (parameterize ((current-output-port s))
		     (display-constrained "test  " '(abcdefghijklmnop 10) 
					  " again  " '(abcdefghijklmnop 16) 
					  '(abcdefghijklmnop 17)))
       (get-output-string s))
     "test  abcdefg... again  abcdefghijklm...abcdefghijklmnop"]

    [(mvlet ([(x _) (stream-take 5 iota-stream)]) x)
     (0 1 2 3 4)]
    [(mvlet ([(x _) (stream-take 3 `(1 2 . ,(delay '(3))))]) x)
     (1 2 3)]

    [(mvlet ([(x _) (stream-take 10 (stream-filter even? iota-stream))]) x)
     (0 2 4 6 8 10 12 14 16 18)]
     
    
;; Having problems with errors in drscheme.
;    [(stream-take 5 `(1 2 . ,(delay '(3))))      error]
;    [(stream-cdr '()) error]
;    [(stream-cdr (delay 1)) error]
    [(stream-cdr (delay '(1))) ()]
    [(stream-car (delay '(1))) 1]
    ["stream-map"
     (mvlet ([(ls _) (stream-take 3 (stream-map add1 '(1 2 3)))]) ls)
     (2 3 4)]
    ["stream-map"
     (mvlet ([(ls _) (stream-take 3 (stream-map add1 
		        (stream-cons 1 (stream-cons 2 (stream-cons 3 (delay '()))))))]) ls)
     (2 3 4)]
    ["stream-filter" 
     (stream-take-all (stream-filter odd?
       (stream-cons 1 (stream-cons 2 (stream-cons 3 (delay '()))))))
     (1 3)]

    ["stream-append: Shouldn't hit the error."
     (stream-car 
      (stream-cdr 
       (stream-append (delay (append '(1 2) (delay (error 'test ""))))
		      '(3 4 5))))
     2]
    ["stream-append: Should hit the error."
     (stream-car 
      (stream-cdr 
       (stream-append (delay (cons 1 (delay (error 'test ""))))
		      '(3 4 5))))
     error]

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
     (let ((return ()))
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
     (let ((x ())) 
       (apply-ordered list (set! x (cons 1 x)) (set! x (cons 2 x)) (set! x (cons 3 x))) 
       (reverse x))
     (1 2 3)]

    [(map cadr (histogram '(1 1 1 2 3 4 5 5 5 5 6 6) 2))   (4 2 6)]
    [(map cadr (histogram '(1 1 1 2 3 4 5 5 5 5 6 6 7) 2)) (4 2 6 1)]

    ))

(define test-this (default-unit-tester "helpers.ss: my messy utils file." these-tests))
(define testhelpers test-this)
(define testshelpers these-tests)

;(call/cc (lambda (out)
#;
	   (with-error-handlers (lambda args (disp "error disp:" args))
				(lambda args (disp "error escape:" args) 999)
				(lambda () (disp "handlers: " 
						 (error-display-handler)
						 (error-escape-handler) )
					(error 'foo "bar")
					3 ))


