

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



;; This defines a *simple* and unified interface into hash-tables.
;; First we require hash-tables from slib:
(require 'hash-table)
;(define (make-default-hash-table) (make-hash-table 50))
(define (make-default-hash-table) (make-hash-table 5))
(define hashtab-get (hash-inquirer eq?))
(define hashtab-set! (hash-associator eq?))



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

(include "../generic/helpers.ss")


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



