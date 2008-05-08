#!r6rs

;;;; Pass 2: remove-unquoted-constant

;;;; This pass replaces unquoted constants with equivalent quoted
;;;; expressions, e.g., 17 => '17 and #t => '#t.

;;;; Output from this pass is in the same language, except that
;;;; there are no unquoted constants:


(library (ws passes normalize_source remove-unquoted-constant)
  (export remove-unquoted-constant
           remove-unquoted-constant-grammar)
  (import (rnrs) (ws common)
	  (ws passes  normalize_source eta-primitives))

;; OUTPUT LANG:
(define remove-unquoted-constant-grammar
  (let ([gram (filter (lambda (prod) (not (eq? (car prod) 'Const))) eta-primitives-grammar)])
    (ASSERT (< (length gram) (length eta-primitives-grammar)))
    (cons '[Const ('quote Datum)] gram)))


(define-pass remove-unquoted-constant
  [OutputGrammar remove-unquoted-constant-grammar]
  [Expr (lambda (x fallthrough)
	  (match x
	    [,const (guard (simple-constant? const))
		    `(quote ,const)]	      
	    [,other (fallthrough other)]))]
  [Program (lambda (prog Expr)
	     ;(unique-name-counter 0) ;; [2007.11.09] Why the heck was this here?
	     (match prog
	       [(,input-language (quote (program ,body ,meta* ... ,type)))
		`(remove-unquoted-constant-language 
		  '(program ,(Expr body) ,meta* ... ,type))]))])

) ; End Module
