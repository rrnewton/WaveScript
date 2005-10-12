;;; setof.ss
;;; Copyright (C) 1996 R. Kent Dybvig
;;; from "The Scheme Programming Language, 2ed" by R. Kent Dybvig

;;; set-of uses helper syntactic extension set-of-help, passing it
;;; an initial base expression of '()
(define-syntax set-of
  (syntax-rules ()
    ((_ e m ...)
     (set-of-help e '() m ...))))

;;; set-of-help recognizes in, is, and predicate expressions and
;;; changes them into nested named let, let, and if expressions.
(define-syntax set-of-help
  (syntax-rules (in is)
    ((_ e base)
     (set-cons e base))
    ((_ e base (x in s) m ...)
     (let loop ((set s))
       (if (null? set)
           base
           (let ((x (car set)))
             (set-of-help e (loop (cdr set)) m ...)))))
    ((_ e base (x is y) m ...)
     (let ((x y)) (set-of-help e base m ...)))
    ((_ e base p m ...)
     (if p (set-of-help e base m ...) base))))

;;; set-cons returns the original set y if x is already in y.
(define set-cons
  (lambda (x y)
    (if (memv x y)
        y
        (cons x y))))
