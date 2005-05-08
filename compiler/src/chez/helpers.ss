

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
		
  

;; This defines a *simple* and unified interface into hash-tables.
;; First we require hash-tables from slib:
(require 'hash-table)
;(define (make-default-hash-table) (make-hash-table 50))
(define (make-default-hash-table) (make-hash-table 5))
(define hashtab-get (hash-inquirer equal?))
(define hashtab-set! (hash-associator equal?))
(define hashtab-for-each hash-for-each)
(define hashtab-remove! (hash-remover equal?))

 ;; [2004.06.13] Matches the function defined in plt, provides
 ;; functionality used by the generic code.
 ;; The escape handler had *better* escape.
 (define (with-error-handlers display escape th)
   (parameterize ([error-handler (lambda args 
				   (apply display args)
				   (escape))])
		 (th)))


;; This is too lenient, but there's no other option.
(define promise? procedure?)

;; We play nasty tricks with symbolic links here. 
;; It doesn't matter if we load this file from "src" or "src/chez"
;; because we've linked the "generic" subdir from both locations.
(include "generic/helpers.ss")

(define disp
  (let ((sub-disp
	 (lambda (a)
	   (if (null? a)
	       (newline)
	       (begin (display (car a)) (display " ") (sub-disp (cdr a)))))))
    (lambda args 
      (critical-section
       (newline)
       (sub-disp args)))))


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
   [() (#%random most-positive-fixnum)]
   [(k) (#%random k)]))
(define (reg:get-random-state) (random-seed))
(define (reg:set-random-state! s) (random-seed s))

