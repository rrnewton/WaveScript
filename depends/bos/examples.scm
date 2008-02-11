; examples.scm is -*- Scheme -*-
;
; Bryan's Object System
;
; (C) 1994 Bryan O'Sullivan <bosullvn@maths.tcd.ie>

; Simple example and test functions.

(define-class <point> (<class>) (x y))

(specialise! initialise <point>
  (lambda (call-next-method self . args)
    (call-next-method)
    (slot-set! self 'x (get-arg args 'x))
    (slot-set! self 'y (get-arg args 'y))))

(define-object pt <point> 'x 1 'y 2)

(define-class <3point> (<point>) (z))

(specialise! initialise <3point>
  (lambda (call-next-method self . args)
    (call-next-method)
    (slot-set! self 'z (get-arg args 'z))))

(define-object pt3 <3point> 'x 4 'y 7 'z 8)

(define-generic move-horiz)

(specialise! move-horiz <point>
  (lambda (call-next-method self delta)
    (call-next-method)
    (slot-set! self 'x (+ (slot-ref self 'x) delta))))

(specialise! move-horiz <3point>
  (lambda (call-next-method self delta)
    (write-object self)
    (newline)
    (call-next-method)
    (display "move-horiz to ")
    (write-object self)
    (newline)))

(define-class <colour> (<class>) (red green blue))

(specialise! initialise <colour>
  (lambda (call-next-method self . args)
    (call-next-method)
    (slot-set! self 'red (get-arg args 'red 0))
    (slot-set! self 'green (get-arg args 'green 0))
    (slot-set! self 'blue (get-arg args 'blue 0))))

(define-class <pixel> (<point> <colour>) ())

(specialise! initialise <pixel>
  (lambda (call-next-method self . args)
    (call-next-method)))

(define-object p <pixel> 'x 5 'y 5 'red 128 'blue 64)