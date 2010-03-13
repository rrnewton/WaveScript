#!r6rs

(library (ws util bos_oop)
  (export 
	    make-object
	    make-generic
	    specialise!
	    initialise
	    <class>
	    is-a?
	    get-arg
	    member-accessor
	    member-mutator
	    write-class
	    write-object
	    class?
	    object?

	    define-class define-object  define-generic  make-class __spec __specreplace

	    ;; These need to be sealed off to prevent breaking the abstraction:
	    class-of slot-ref slot-set!

	    ;; TEMP: For chez
	    i-make-class
	    
            )
  (import (except (rnrs (6)) error) (rnrs mutable-pairs) 
	  (ws common))

; 	 (include "../../depends/bos/bos.scm")
; 	 (include "../../depends/bos/macros.scm")
; ;; This works:	  
; 	 (include "../../depends/bos/utilities.scm")

; ;; This will get an eror based on th file not existing, as expected:
; ;(include "../../depends/bos/utilities.scm_FOOBAR")

; ;; This won't work:
; #; (IFPLT (include "../../depends/bos/utilities.scm")
; 	 (include     "./depends/bos/utilities.scm"))

; ;; This won't work, and in fact doesn't get an error if the name is wrong:
; #;
;   (IFPLT (include "../../depends/bos/utilities.scm_FOOBAR")
; 	 (include     "./depends/bos/utilities.scm_BAZ"))



  (IFPLT (include "../../depends/bos/utilities.scm")
	 (include     "./depends/bos/utilities.scm"))
  (IFPLT (include "../../depends/bos/bos.scm")
	 (include     "./depends/bos/bos.scm"))
  (IFPLT (include "../../depends/bos/macros.scm")
         (include     "./depends/bos/macros.scm"))


  (define-syntax defspecialized
    (lambda (x)
      (syntax-case x ()
        [(_) #'3]
      )))

  ;; A little shorthand:
  (define-syntax __spec
    (syntax-rules ()
      [(_ method class args)
       (define _ignored
	 (specialise! method class 
		      (lambda (call-next . args) (call-next) (void))))]
      [(_ method class args bod ...) 
       (define _ignored
	 (specialise! method class 
		      (lambda (call-next . args) (call-next) (let () bod ...))))]))

  ;; This one DOES NOT call the super method.
  (define-syntax __specreplace
    (syntax-rules ()
      #;
      [(_ method class args)
       (define _ignored
	 (specialise! method class 
		      (lambda (call-next . args)  (void))))]
      [(_ method class args bod ...) 
       (define _ignored
	 (specialise! method class 
		      (lambda (call-next . args)  (let () bod ...))))]))



  (define-class <point> (<class>) (x y))
#|
  (__spec initialise <point> (self . args)
    (slot-set! self 'x (get-arg args 'x))
    (slot-set! self 'y (get-arg args 'y)))

  (define foo (make-object <point> 'x 1 'y 3))
  ;(define-object foo <point>)
 ; (define-object pt <point> 'x 1 )

  (define-generic blah )
  
  (__spec blah <point> (self . args)
                (+ (slot-ref self 'x) (slot-ref self 'y)))

  (display (blah foo))(newline)
|#
  
#|
  (define-class <point> (<class>) (x y))
  (define-class <3point> (<point>) (z))  
  (define-class <colour> (<class>) (red green blue))
  (define-class <pixel> (<point> <colour>) ())

  (define-generic move-horiz)
  (define _ 
    (begin 
      (specialise! initialise <point>
                   (lambda (call-next-method self . args)
                     (call-next-method)
                     (slot-set! self 'x (get-arg args 'x))
                     (slot-set! self 'y (get-arg args 'y))))
      
      (specialise! initialise <3point>
                   (lambda (call-next-method self . args)
                     (call-next-method)
                     (slot-set! self 'z (get-arg args 'z))))
      
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
      (specialise! initialise <colour>
                   (lambda (call-next-method self . args)
                     (call-next-method)
                     (slot-set! self 'red (get-arg args 'red 0))
                     (slot-set! self 'green (get-arg args 'green 0))
                     (slot-set! self 'blue (get-arg args 'blue 0))))
      (specialise! initialise <pixel>
                   (lambda (call-next-method self . args)
                     (call-next-method)))))
    
  (define-object pt <point> 'x 1 'y 2)
  (define-object p <pixel> 'x 5 'y 5 'red 128 'blue 64)
  (define-object pt3 <3point> 'x 4 'y 7 'z 8)
|#

  
) ;; End module

