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


;====================================================================================================
;; HACK -- inlining THREE FILES
;====================================================================================================

#; #; #;
(begin 
; bos.scm is -*- Scheme -*-
;
; Bryan's Object System
;
; (C) 1994 Bryan O'Sullivan <bosullvn@maths.tcd.ie>

; A simple object system for R4RS (with a few tweaks) Scheme, based on
; generics and multiple inheritance.

(define (fold op base list)
  (let loop ((result base)
	     (list list))
    (if (null? list)
	result
	(loop (op (car list) result) (cdr list)))))

; Slot names should come last in the vector which represents a class.

(define *superclasses-offset* 0)
(define *specialised-methods-offset* 1)
(define *name-offset* 2)
(define *slot-names-offset* 3)

;(define pack-generic list)
;(define unpack-generic car)
;(define pack-generic vector)
;(define (unpack-generic x) (vector-ref x 0))
;(define (pack-generic x) (x))
;(define (unpack-generic x) ???)
(define (generic->unique x) (x))

; We keep all superclasses of a class in one list; this makes generic
; function dispatch and calling overridden methods easier.
;
; Since we do things this way, to create the list of superclasses for
; a new class, we just glom its superclass' lists together (with the
; superclasses appearing in the list too, of course).  The upshot of
; this is that superclasses are maintained in `depth-first' order,
; with duplicates removed (otherwise methods should be idempotent for
; safety, which is hard to do).

(define (flatten superclasses)
  (let loop ((new-superclasses '())
	     (superclasses superclasses))
    (if (null? superclasses)
	new-superclasses
	(loop (append new-superclasses
		      (let ((superclass (car superclasses)))
			(if (equal? superclass <class>)
			    (list superclass)
			    (cons superclass
				  (vector-ref superclass
					      *superclasses-offset*)))))
	      (cdr superclasses)))))

(define (remove-from elt list)
  (cond
   ((null? list)
    list)
   ((eq? elt (car list))
    (remove-from elt (cdr list)))
   (else
    (cons (car list) (remove-from elt (cdr list))))))

(define (uniquify list)
  (if (null? list)
      list
      (let ((head (car list)))
	(cons head (uniquify (remove-from head (cdr list)))))))

; Right now, a class consists of a vector with the following elements:
; - its entire list of parents, in depth-first order
; - an a-list of methods specialised on the class
; - the names of all of its slots

(define (i-make-class name superclasses slot-names)
  (let* ((slots (append superclasses (list (list->vector
					    `(dummy-superclasses
					      dummy-specialised-methods
					      dummy-name
					      ,@slot-names)))))
	 (class-vector (make-vector (fold (lambda (v r)
					    (+ (vector-length v)
					       (- r *slot-names-offset*)))
					  *slot-names-offset* slots))))
    (vector-set! class-vector *superclasses-offset* (uniquify (flatten superclasses)))
    (vector-set! class-vector *specialised-methods-offset* '())
    (vector-set! class-vector *name-offset* name)
    (let loop ((slots slots)
	       (slot-offset *slot-names-offset*)
	       (vector-offset *slot-names-offset*))
      (if (null? slots)
	  class-vector
	  (let ((slot (car slots)))
	    (if (< slot-offset (vector-length slot))
		(begin
		  (vector-set! class-vector vector-offset
			       (vector-ref slot slot-offset))
		  (loop slots (+ 1 slot-offset) (+ 1 vector-offset)))
		(loop (cdr slots) *slot-names-offset* vector-offset)))))))

; The root class must contain entries for everything *except* slots.

(define <class> '#(() () "<class>"))

(define *class-offset* 0)
(define *slots-offset* 1)

(define (make-object class . args)
  (let ((object (make-vector (+ *slots-offset* (- (vector-length class)
						 *slot-names-offset*)))))
    (vector-set! object *class-offset* class)
    (apply initialise (cons object args))
    object))

; The generic function interface depends on each class (*not* each
; object) carrying around its own methods.  This could possibly be
; made faster using hash tables instead of a-lists, but I don't know
; whether it would be worth the effort.

(define *unspecific* (if #f #f))

(define (make-generic)
  ;(define unique-tag (cons 0 ()))
  (define unique-tag (gensym "genericmethod"))
  (letrec ((this-function
	    (case-lambda
	      [() unique-tag]
	      [(object . arguments)
	       (let* ((class (vector-ref object *class-offset*))
		      (classes (cons class
				     (vector-ref class
						 *superclasses-offset*)))
		      (have-specialised #f)
		      (my-classes classes))
		 (letrec ((call-next-method
			   (lambda ()
			     (if (null? my-classes)
				 (if have-specialised
				     *unspecific*
				     (error 'make-generic "method not specialised" ))
				 (let* ((class (car my-classes))
					(specialised-methods
					 (vector-ref
					  class *specialised-methods-offset*))
					(this-method
					 (assq unique-tag ;(generic->unique this-function)
					       specialised-methods)))
				   (set! my-classes (cdr my-classes))
				   (if this-method
				       (begin
					 (set! have-specialised #t)
					 (apply (cdr this-method)
						(cons call-next-method
						      (cons object arguments))))
				       (call-next-method)))))))
		   (set! have-specialised #f)
		   (set! my-classes classes)
		   (call-next-method)))])))
    ;; rrn: boxing this so that eq? is well defined:
    this-function))

; Specialise a generic method with respect to a particular class.

(define (specialise! generic class procedure)
  (define tag (generic->unique generic))
  (let* ((specialised-methods (vector-ref class
					  *specialised-methods-offset*))
	 (this-method (assq tag specialised-methods)))
    ;(printf "SPECILIASING specialised: ~s this: ~s\n" specialised-methods this-method)
    (if this-method
	(set-cdr! this-method procedure)
	(vector-set! class *specialised-methods-offset*
		     (cons (cons tag procedure) specialised-methods)))))

; I prefer to do slot accessing dynamically rather than statically; it
; costs a little in terms of runtime and `opacity', but what the hell.

(define (slot-ref object slot-name)
  (let* ((class (vector-ref object *class-offset*))
	 (top (vector-length class)))
    (let loop ((offset *slot-names-offset*))
      (cond
       ((= offset top)
	(error 'slot-ref "no such slot"))
       ((equal? slot-name (vector-ref class offset))
	(vector-ref object (+ (- offset *slot-names-offset*) *slots-offset*)))
       (else
	(loop (+ 1 offset)))))))

(define (slot-set! object slot-name value)
  (let* ((class (vector-ref object *class-offset*))
	 (top (vector-length class)))
    (let loop ((offset *slot-names-offset*))
      (cond
       ((= offset top)
	(error 'slot-set! "no such slot"))
       ((equal? slot-name (vector-ref class offset))
	(vector-set! object (+ (- offset *slot-names-offset*)
			       *slots-offset*) value))
       (else
	(loop (+ 1 offset)))))))

; For the static safety diehards, here are faster and `safer' variants
; of the above, which return closures to do the necessary work.

(define (member-accessor class slot-name)
  (let ((top (vector-length class)))
    (let loop ((offset *slot-names-offset*))
      (cond
       ((= offset top)
	(error 'member-accessor "no such slot"))
       ((equal? slot-name (vector-ref class offset))
	(lambda (object)
	  (vector-ref object (+ (- offset *slot-names-offset*)
				*slots-offset*))))
       (else
	(loop (+ 1 offset)))))))

(define (member-mutator class slot-name)
  (let ((top (vector-length class)))
    (let loop ((offset *slot-names-offset*))
      (cond
       ((= offset top)
	(error 'member-mutator "no such slot"))
       ((equal? slot-name (vector-ref class offset))
	(lambda (object value)
	  (vector-set! object (+ (- offset *slot-names-offset*)
				 *slots-offset*) value)))
       (else
	(loop (+ 1 offset)))))))

; I have no idea whether this may be useful or not (I've never used
; such a thing myself), but it was easy to write and Every Object
; System Should Have One.

(define (is-a? object class)
  (let ((real-class (vector-ref object *class-offset*)))
    (or (eq? real-class class)
	(let loop ((superclasses (vector-ref real-class
					     *superclasses-offset*)))
	  (cond
	   ((null? superclasses) #f)
	   ((eq? (car superclasses) class) #t)
	   (else (loop (cdr superclasses))))))))

(define (class-of object)
  (vector-ref object *class-offset*))

; Bootstrap me, baby.  Specialise the INITIALISE generic for each
; class you define *before* you create objects of that class, or
; you'll be in trouble.

(define initialise (make-generic))
)
(begin
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
)
(begin
; utilities.scm is -*- Scheme -*-
;
; Bryan's Object System
;
; (C) 1994 Bryan O'Sullivan <bosullvn@maths.tcd.ie>

; GET-ARG gives us much the same facilities as Common Lisp's keyword
; argument gumph.  You can use it in the following manner:
;   (define (mumble a b . args)
;     ...
;     (foo (get-arg args 'arg-name default-value))
;     ...)
;   ...
;   (mumble 1 2 'x 4 'y 7)

;; [2008.02.10] RRN applying some minor changes.

(define (get-arg arg-list arg . default)
  (let loop ([arg-list arg-list])
    (cond
     ((null? arg-list)
      (if (null? default)
	  (error 'get-arg "no such arg: ~s" arg)
	  (car default)))
     ((equal? (car arg-list) arg)  (cadr arg-list))
     (else (loop (cddr arg-list))))))

; Print an object nicely to an output port.

(define (write-object object . output-port)
  (let* ((class (vector-ref object *class-offset*))
	 (length (- (vector-length class) *slot-names-offset*)))
    (apply display (cons "#{Object " output-port))
    (apply display (cons (vector-ref class *name-offset*) output-port))
    (apply display (cons ": " output-port))
    (if (> length 0)
	(begin
	  (set! length (- length 1))
	  (do ((offset 0 (+ offset 1)))
	      ((= offset length))
	    (apply write-char (cons #\( output-port))
	    (apply display (cons (vector-ref class (+ offset *slot-names-offset*))
				 output-port))
	    (apply display (cons ": " output-port))
	    (apply display (cons (vector-ref object (+ offset *slots-offset*))
				 output-port))
	    (apply display (cons ") " output-port)))
	  (apply write-char (cons #\( output-port))
	  (apply display (cons (vector-ref class (+ length *slot-names-offset*))
			       output-port))
	  (apply display (cons ": " output-port))
	  (apply display (cons (vector-ref object (+ length *slots-offset*))
			       output-port))
	  (apply write-char (cons #\) output-port)))
	(apply display (cons "(no members)" output-port)))
    (apply write-char (cons #\} output-port))))

(define (write-class class . output-port)
  (apply display (cons "#{Class " output-port))
  (apply display (cons (vector-ref class *name-offset*) output-port))
  (if (not (eq? class <class>))
      (begin
	(apply display (cons ": " output-port))
	(let loop ((superclasses (vector-ref class *superclasses-offset*)))
	  (if (not (null? superclasses))
	      (if (null? (cdr superclasses))
		  (apply display (cons (vector-ref (car superclasses) *name-offset*)
				       output-port))
		  (begin
		    (apply display (cons (vector-ref (car superclasses) *name-offset*)
					 output-port))
		    (apply write-char (cons #\space output-port))
		    (loop (cdr superclasses))))))))
  (apply display (cons ", " output-port))
  (let ((length (vector-length class)))
    (if (= length *slot-names-offset*)
	(apply display (cons "no members" output-port))
	(begin
	  (set! length (- length 1))
	  (do ((offset *slot-names-offset* (+ offset 1)))
	      ((= offset length))
	    (apply display (cons (vector-ref class offset) output-port))
	    (apply write-char (cons #\space output-port)))
	  (apply display (cons (vector-ref class length) output-port)))))
  (apply write-char (cons #\} output-port)))

(define (class? thing)
  (and (vector? thing)
       (>= (vector-length thing) *slot-names-offset*)
       (list? (vector-ref thing *specialised-methods-offset*))
       (list? (vector-ref thing *superclasses-offset*))
       (string? (vector-ref thing *name-offset*))))

(define (object? thing)
  (and (vector? thing)
       (>= (vector-length thing) *slots-offset*)
       (class? (vector-ref thing *class-offset*))))
)
;====================================================================================================
;====================================================================================================


; 	 (plt:include "../../../depends/bos/bos.scm")
; 	 (plt:include "../../../depends/bos/macros.scm")
 ;; This works:	  
; 	 (plt:include "../../../depends/bos/utilities.scm")

; ;; This will get an eror based on th file not existing, as expected:
; ;(include "../../depends/bos/utilities.scm_FOOBAR")

; ;; This won't work:
; #; (IFPLT (include "../../depends/bos/utilities.scm")
; 	 (include     "./depends/bos/utilities.scm"))

; ;; This won't work, and in fact doesn't get an error if the name is wrong:
; #;
;   (IFPLT (include "../../depends/bos/utilities.scm_FOOBAR")
; 	 (include     "./depends/bos/utilities.scm_BAZ"))

#|
  (IFPLT (plt:include "../../depends/bos/utilities.scm")
	 (    include     "./depends/bos/utilities.scm"))
  (IFPLT (plt:include "../../depends/bos/bos.scm")
	 (    include     "./depends/bos/bos.scm"))
  (IFPLT (plt:include "../../depends/bos/macros.scm")
         (    include     "./depends/bos/macros.scm"))
|#


;; With a symlink hack this can work under PLT even with different include conventions.
;; I settled on this hack after painful attempts to try to do the
;; right thing and establish a uniform include.
(include "depends/bos/utilities.scm")
(include "depends/bos/bos.scm")
(include "depends/bos/macros.scm")

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

