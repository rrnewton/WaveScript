; macros.scm is -*- Scheme -*-
;
; Bryan's Object System
;
; (C) 1994 Bryan O'Sullivan <bosullvn@maths.tcd.ie>

; R4RS hygienic macro funnage.

(define *current-anon-class* 0)

(define-syntax define-generic
  (syntax-rules ()
    ((define-generic generic-name)
     (define generic-name (make-generic)))))

(define-syntax define-class
  (syntax-rules ()
    ((define-class class-name (superclasses ...) (members ...))
     (define class-name
       (i-make-class (symbol->string (quote class-name))
		     (list superclasses ...) (quote (members ...)))))))

(define-syntax make-class
  (syntax-rules ()
    ((make-class (superclasses ...) (members ...))
     (let ((anon-class *current-anon-class*))
       (set! *current-anon-class* (+ anon-class 1))
       (i-make-class (string-append "<anonymous-" (number->string anon-class) ">")
		     (list superclasses ...) (quote (members ...)))))))

(define-syntax define-object
  (syntax-rules ()
    ((define-object object-name class-name arguments ...)
     (define object-name (make-object class-name arguments ...)))))
