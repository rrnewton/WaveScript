

;;; EXPERIMENTAL - playing around with the idea of supporting Larceny...

#|
 echo '(import (err5rs load)) (load "main_larceny.ss")' | larceny -err5rs
|#

;;  Load some basics.
(import  
 (for (rnrs base) expand run (meta 2))
 (rnrs io simple)
 (for (rnrs syntax-case) expand run (meta 2))
 (err5rs load)
 (for (rnrs r5rs) expand run) ;; backwards compat
 (rnrs lists)
 (for (rnrs control) expand run)  ;; case-lambda
 (rnrs arithmetic fixnums)  
 (rnrs arithmetic flonums)  
 )


;; Import other larceny primitives:
(import (for (primitives pretty-print make-parameter 
			 set-box! box unbox gensym getenv
			 dump-heap dump-interactive-heap
			 global-optimization 
			 time repl  exit issue-warnings current-directory
			 random
			 open-output-string flush-output-port error-handler decode-error
			 char-upper-case? char-lower-case?
			 ) run expand))
(define cd current-directory)
;(import (larceny benchmarking))

;; Dummy module syntax.
(define-syntax module
  (syntax-rules (require provide chezprovide chezimports)
    [(_ name parent (require __ ...) (provide exports ...) (chezimports imp ...) exp ...)
     (module name () exp ...)]
    [(_ name parent (require __ ...) (provide exports ...) (chezprovide chezexports ...) (chezimports imp ...) exp ...)
     (module name () exp ...)]
    ;; This is a raw chez module, as opposed to a Regiment/WS module:
    [(_ name (exports ...) exp ...)  
     ;(pretty-print '(begin exp ...))
     (begin exp ...)]
    ))

;; Compatibility bindings:
(library (misc-larceny-compat)
  (export literal-identifier=? ellipsis? delay-values 
	  extend-backquote simple-eval 
	  syntax-error datum->syntax-object syntax-object->datum datum syntax->list
	  andmap call/1cc void 
	  box unbox set-box! printf format
	   include identifier-syntax define-values
	  atom? cflonum? fx= ;fx+ fx- fx* fx/
	  add1 sub1
	  list-copy make-list
	  warning warning-handler get-output-string ;; Chez compat version
	  )
  (import (for (rnrs base) run expand)
	  (for (rnrs io ports) run expand)
	  (for (rnrs io simple) run expand)
	  (rnrs r5rs)  ;(rnrs eval) 
	  (for (rnrs control) run expand)
	  (for (rnrs syntax-case) run expand)
	  (rnrs arithmetic fixnums)
	  ;(primitives (open-output-string get-output-string))
	  (primitives aeryn-evaluator)
	  (prefix (primitives make-parameter format open-output-string get-output-string reset-output-string) builtin:))

  (define (get-output-string sp)
    (let ([res (builtin:get-output-string sp)])
      (builtin:reset-output-string sp)
      res))

  (define datum->syntax-object datum->syntax)
  (define syntax-object->datum syntax->datum)
  ;; [2007.12.28] Using r6rs I don't know how to unwrape only the outside layer without stripping all syntax info.
#;
  (define (syntax->list syn)
    (map (lambda (x) (datum->syntax syn x))
      (syntax->datum syn)))
  (define syntax->list
    (lambda (ls)
      (syntax-case ls ()
	[() '()]
	[(x . r) (cons #'x (syntax->list #'r))])))

  
  (define-syntax datum
    (syntax-rules ()
      [(_ t) (syntax-object->datum (syntax t))]))

  (define-syntax syntax-error
    (syntax-rules ()
      [(kwd form msg) (syntax-violation #f msg form)]
      [(kwd msg)      (syntax-violation #f msg #f)]))

  (define literal-identifier=?
    (lambda (x y)
      (eq? (syntax->datum x)
	   (syntax->datum y))))
	 
  (define ellipsis?
    (lambda (x)
      (and (identifier? x) (literal-identifier=? x #'(... ...)))))

  (define-syntax delay-values
    (syntax-rules ()
      ((_ e)
       (let ((vals #f))
	 (lambda ()
	   (if vals (apply values vals)
	       (call-with-values (lambda () e)
		 (lambda args (set! vals args) (apply values args))))
	   )))))

  ;; A dummy binding that uses the normal backquote
  (define-syntax extend-backquote
    (lambda (x)
      (syntax-case x () ((kwd Template Exp ...) #'(begin Exp ...)))))

  ;(define (simple-eval x) (eval x (environment '(r6rs))))
  ;(define (simple-eval x) (eval x (environment '(rnrs))))
  (define simple-eval aeryn-evaluator)

#;
  (define-syntax identifier-syntax
   (lambda (x)
     (syntax-case x ()
      [(_ e)
       #'(lambda (x)
           (syntax-case x ()
             [id  (identifier? #'id)  #'e]
             [(id x (... ...))  (identifier? #'id)
              #'(e x (... ...))]))])))

  (define-syntax include
    (lambda (x)
      (define read-file
	(lambda (fn k)
	  (let ([p (open-input-file fn)])
	    (let f ([x (read p)])
	      (if (eof-object? x)
		  (begin (close-input-port p) '())
		  (cons (datum->syntax k x)
			(f (read p))))))))
      (syntax-case x ()
	 [(k filename)
	  (let ([fn (syntax->datum (syntax filename))])
	    (with-syntax ([(exp ...) (read-file fn #'k)])
	      #'(begin exp ...)))])))

  (define-syntax define-values 
   (lambda (x)
    (define iota 
      (case-lambda [(n) (iota 0 n)]
		   [(i n) (if (= n 0) '() (cons i (iota (+ i 1) (- n 1))))]))
    (syntax-case x ()  
     [(define-values (vars ...) exp)
      (with-syntax ([(nums ...) (datum->syntax  
				 #'define-values 
				 (iota (length (syntax->datum #'(vars ...)))))])
	#'(begin  
	    (define newtempvar (call-with-values (lambda () exp) vector))
	    (define vars (vector-ref newtempvar nums))
	    ...))])))
  
  (define (andmap fun ls)
    (if (null? ls) #t
	(and (fun (car ls)) (andmap fun (cdr ls)))))

  (define call/1cc call/cc)
  (define-syntax void (syntax-rules () [(_) (if #f #t)]))
  ;(define (void) (if #f #t))

  ;; Could use lists, but that would require mutable pairs:
  (define (box x) (vector x))
  (define (unbox x) (vector-ref x 0))
  (define (set-box! b x) (vector-set! b 0 x))

  (define (format str . args) 
    (call-with-string-output-port
     (lambda (prt)
       (apply builtin:format prt str args))))
  (define (printf str . args) 
    (apply builtin:format (current-output-port) str args))

  (define warning-handler 
    (builtin:make-parameter
     'warning-handler
     (lambda (who str . args)
       (printf "Warning in ~a: ~a" who (apply format str args)))
     procedure?))
  (define (warning who str . args)
    (apply (warning-handler) who str args))
  
  (define fx= fx=?) ;(define-syntax fx= (syntax-case  fx=?))
  (define (cflonum? n) (and (number? n) (inexact? n) (not (eqv? 0 (imag-part n)))))
  (define (add1 n) (+ n 1))
  (define (sub1 n) (- n 1))
  (define (atom? x) (not (pair? x)))
  
  ;; I don't really have a firm idea of whether this should be tail-recursive or not.
  #;
  (define (list-copy ls)
    (let loop ([ls ls] [acc '()])
      (if (null? ls) (reverse! acc)
	  (loop (cdr ls) (cons (car ls) acc)))))
  (define (list-copy ls)
    (let loop ([ls ls])
      (if (null? ls) '()
	  (cons (car ls) (loop (cdr ls))))))
  (define make-list
    (case-lambda 
      [(n) (make-list n #f)]
      [(n x) (let loop ([n n])
	       (if (zero? n) '()
		   (cons x (loop (fx- n 1)))))]))
)

;; Hash tables 
(library (larceny-hashtab)
	 (export make-default-hash-table 
		 hashtab-get  hashtab-set!
		 hashtab-for-each hashtab-remove!)
	 (import (rnrs base)
		 (rnrs hashtables)
		 (rnrs control)
		 (rnrs arithmetic fixnums))
	
  (define make-default-hash-table
    (case-lambda 
      [() (make-default-hash-table 50)]
      [(n) (make-eq-hashtable n)]))

  (define (hashtab-get ht key) (hashtable-ref ht key #f))
  (define hashtab-set!          hashtable-set!)
  (define hashtab-remove!       hashtable-delete!)
  (define (hashtab-for-each fn ht)
    ;; Hmm... a little surprising that there's not a more efficient way to do this.
    (let-values ([(keys vals) (hashtable-entries ht)])
      (define len (vector-length keys))
      (let loop ([i 0])
	(unless (fx=? i len)
	  (fn (vector-ref keys i) (vector-ref vals i)))))))
(import (for (larceny-hashtab) run expand))
(import (for (misc-larceny-compat) run expand))

(define eval simple-eval)

;======================================================================
;;; Extra macros and global settings 


;; Temp, working around larceny 0.96 bug with dumping heaps after hash tables are made.
(import (primitives sro)
	(rnrs hashtables))
(define (report-on-problematic-hashtables)
 (let* ((record-like-objects (sro 3 5 -1))
        (hashtables
         (filter hashtable? (vector->list record-like-objects)))
        (problematic-hashtables
         (filter (lambda (ht) (not (hashtable-hash-function ht)))
                 hashtables)))
   (if (null? problematic-hashtables)
       (begin (display "No problematic hashtables were found.")
              (newline))
       (for-each (lambda (ht)
                   (display "Problematic hashtable found.")
                   (newline)
                   (display (hashtable-size ht))
                   (display " entries with keys:")
                   (newline)
                   (write (hashtable-keys ht))
                   (newline))
                 problematic-hashtables))))



;; Expand the appropriate code for loading in Larceny:
(define-syntax cond-expand
    (lambda (syn)
      (syntax-case syn (and or not else chez plt larceny r6rs r5rs graphics threads)
      ;; Standard clauses:
      ((cond-expand) (syntax-error #'syn "Unfulfilled cond-expand"))
      ((cond-expand (else body ...))  #'(begin body ...))
      ((cond-expand ((and) body ...) more-clauses ...) #'(begin body ...))
      ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
       #'(cond-expand (req1 (cond-expand ((and req2 ...) body ...) more-clauses ...)) more-clauses ...))
      ((cond-expand ((or) body ...) more-clauses ...) #'(cond-expand more-clauses ...))
      ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
       #'(cond-expand (req1 (begin body ...)) (else (cond-expand ((or req2 ...) body ...) more-clauses ...))))
      ((cond-expand ((not req) body ...) more-clauses ...)
       #'(cond-expand (req (cond-expand more-clauses ...)) (else body ...)))

      ;; Enabled features (or potentially enabled):
      ((cond-expand (larceny body ...) more-clauses ...) #'(begin body ...))
      ((cond-expand (r6rs    body ...) more-clauses ...) #'(begin body ...))
      ;; 'threads' and 'graphics' are disabled for now under larceny.
      
      ;; Otherwise, it's disabled:
      ;; It seems that in larceny we can't help but expand the ignored code anyway:
      ((cond-expand (feature-id body ...) more-clauses ...) #'(cond-expand more-clauses ...)))))


;; TEMPORARY: Let's turn this off to load a little faster.
(global-optimization #f)
(issue-warnings #f)

(import (larceny compiler)) 

(define-syntax common:load-source
  (syntax-rules ()
    [(_ file) (begin ;(printf " LOADING FILE: ~a\n" file)
		     (display "\n   LOADING FILE: ")(display file)(newline)
		     (time (load file)))]))

;======================================================================
;;; Print a banner message.

;======================================================================
;;; Begin loading files.  First some setup stuff.

(common:load-source "chez/match.ss")
;(display "TEST: ")(display (match 3 [3 9] [4 2]))(newline)

;======================================================================
;;; Now begin loading Regiment/WaveScript proper. 

;; [2007.12.28] Can't use records right now because of the dump-heap bug.
;(import (rnrs records syntactic))
#;
(define-syntax reg:define-struct
  (syntax-rules ()
    [(_ (name field ...))
     (begin (define-record-type name (fields field ...))
	    ;; Allows the reader to read in regiment records.  System should work without this:
	    ;(define reg:struct-dummy-val (record-reader 'name (type-descriptor name)))
	    )]))
;; TEMPTOGGLE: This is a simple vector based struct instead:
(begin 
  (define-syntax define-structure
  (lambda (x)
    (define gen-id
      (lambda (template-id . args)
        (datum->syntax-object template-id
          (string->symbol
            (apply string-append
                   (map (lambda (x)
                          (if (string? x)
                              x
                              (symbol->string
                                (syntax-object->datum x))))
                        args))))))
    (syntax-case x ()
      ((_ (name field1 ...))
       (andmap identifier? (syntax (name field1 ...)))
       (syntax (define-structure (name field1 ...) ())))
      ((_ (name field1 ...) ((field2 init) ...))
       (andmap identifier? (syntax (name field1 ... field2 ...)))
       (with-syntax
         ((constructor (gen-id (syntax name) "make-" (syntax name)))
          (predicate (gen-id (syntax name) (syntax name) "?"))
          ((access ...)
           (map (lambda (x) (gen-id x (syntax name) "-" x))
                (syntax (field1 ... field2 ...))))
          ((assign ...)
           (map (lambda (x) (gen-id x "set-" (syntax name) "-" x "!"))
                (syntax (field1 ... field2 ...))))
          (structure-length
           (+ (length (syntax (field1 ... field2 ...))) 1))
          ((index ...)
           (let f ((i 1) (ids (syntax (field1 ... field2 ...))))
              (if (null? ids)
                  '()
                  (cons i (f (+ i 1) (cdr ids)))))))
         (syntax (begin
                   (define constructor
                     (lambda (field1 ...)
                       (let* ((field2 init) ...)
                         (vector 'name field1 ... field2 ...))))
                   (define predicate
                     (lambda (x)
                       (and (vector? x)
                            (fx= (vector-length x) structure-length)
                            (eq? (vector-ref x 0) 'name))))
                   (define access
                     (lambda (x)
                       (vector-ref x index)))
                   ...
                   (define assign
                     (lambda (x update)
                       (vector-set! x index update)))
                   ...)))))))
    (define-syntax reg:define-struct
      (syntax-rules ()
	[(_ (name field ...))  (define-structure (name field ...))]))
    ;; This is a very wimpy approximation.
    (define (reg:struct? x)
      (and (vector? x) (> (vector-length x) 0) (symbol? (vector-ref x 0))))
    (define (reg:struct->list x) (cdr (vector->list x))))


 ;; Load this first.  Widely visible constants/parameters.
;(time (compile-file "generic/constants.ss"))

(common:load-source "generic/constants.ss")

;; TEMPTTOGGLE: [2007.12.28]
;; Overwrite the IFWAVESCOPE macro from constants.ss:
;;  Doing wavescope-only for Larceny right now:
(define-syntax IFWAVESCOPE 
  (syntax-rules ()
    [(_ a b) a]
    [(_ a)   a]))

;(time (compile-file "generic/util/reg_macros.ss"))
(common:load-source "generic/util/reg_macros.ss")

;(common:load-source "generic/util/hash.ss") 
;(common:load-source "generic/util/slib_hashtab.ss")

;; Checkpoint:
;(report-on-problematic-hashtables)
;(dump-interactive-heap "larc.heap")(exit)

         (load "common_loader.ss")

;(report-on-problematic-hashtables)
;(repl)

;(dump-heap "larc.heap" (lambda args (display "Starting from heap... YAY \n") (newline)))
