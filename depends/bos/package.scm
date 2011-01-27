; package.scm is -*- Scheme -*-
;
; Bryan's Object System
;
; (C) 1994 Bryan O'Sullivan <bosullvn@maths.tcd.ie>

; A pair of wrappers for Scheme 48's module system.

(define-interface bos-interface
  (export define-class
	  define-object
	  define-generic
	  make-class
	  make-object
	  make-generic
	  specialise!
	  initialise
	  <class>
	  class-of
	  is-a?
	  get-arg
	  slot-ref
	  slot-set!
	  member-accessor
	  member-mutator
	  write-class
	  write-object
	  class?
	  object?))

(define-interface bos-safe-interface
  (export define-class
	  define-object
	  define-generic
	  make-class
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
	  object?))

(define-structure bos bos-interface
  (open scheme
	signals)
  (files bos macros utilities))

(define-structure bos-safe bos-safe-interface
  (open scheme
	signals)
  (files bos macros utilities))