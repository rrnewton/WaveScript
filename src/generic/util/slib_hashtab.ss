;; This was ripped from SLIB:

(module slib:hashtab mzscheme
  (require)
  (provide make-hash-table predicate->hash-asso 
	   hash-inquirer hash-associator hash-remover
	   hash-map hash-for-each hash-rehasher)
  (chezprovide)
  (chezimports)


;================================================================================

;;;"alist.scm", alist functions for Scheme.
;;;Copyright (c) 1992, 1993, 2003 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;@code{(require 'alist)}
;;@ftindex alist
;;
;;Alist functions provide utilities for treating a list of key-value pairs
;;as an associative database.  These functions take an equality predicate,
;;@var{pred}, as an argument.  This predicate should be repeatable,
;;symmetric, and transitive.
;;
;;Alist functions can be used with a secondary index method such as hash
;;tables for improved performance.

;;@body
;;Returns an @dfn{association function} (like @code{assq}, @code{assv}, or
;;@code{assoc}) corresponding to @var{pred}.  The returned function
;;returns a key-value pair whose key is @code{pred}-equal to its first
;;argument or @code{#f} if no key in the alist is @var{pred}-equal to the
;;first argument.
(define (predicate->asso pred)
  (cond ((eq? eq? pred) assq)
	((eq? = pred) assv)
	((eq? eqv? pred) assv)
	((eq? char=? pred) assv)
	((eq? equal? pred) assoc)
	((eq? string=? pred) assoc)
	(else (lambda (key alist)
		(let l ((al alist))
		  (cond ((null? al) #f)
			((pred key (caar al)) (car al))
			(else (l (cdr al)))))))))

;;@body
;;Returns a procedure of 2 arguments, @var{alist} and @var{key}, which
;;returns the value associated with @var{key} in @var{alist} or @code{#f} if
;;@var{key} does not appear in @var{alist}.
(define (alist-inquirer pred)
  (let ((assofun (predicate->asso pred)))
    (lambda (alist key)
      (let ((pair (assofun key alist)))
	(and pair (cdr pair))))))

;;@body
;;Returns a procedure of 3 arguments, @var{alist}, @var{key}, and
;;@var{value}, which returns an alist with @var{key} and @var{value}
;;associated.  Any previous value associated with @var{key} will be
;;lost.  This returned procedure may or may not have side effects on its
;;@var{alist} argument.  An example of correct usage is:
;;
;;@lisp
;;(define put (alist-associator string-ci=?))
;;(define alist '())
;;(set! alist (put alist "Foo" 9))
;;@end lisp
(define (alist-associator pred)
  (let ((assofun (predicate->asso pred)))
    (lambda (alist key val)
      (let* ((pair (assofun key alist)))
	(cond (pair (set-cdr! pair val)
		    alist)
	      (else (cons (cons key val) alist)))))))

;;@body
;;Returns a procedure of 2 arguments, @var{alist} and @var{key}, which
;;returns an alist with an association whose @var{key} is key removed.
;;This returned procedure may or may not have side effects on its
;;@var{alist} argument.  An example of correct usage is:
;;
;;@lisp
;;(define rem (alist-remover string-ci=?))
;;(set! alist (rem alist "foo"))
;;@end lisp
(define (alist-remover pred)
  (lambda (alist key)
    (cond ((null? alist) alist)
	  ((pred key (caar alist)) (cdr alist))
	  ((null? (cdr alist)) alist)
	  ((pred key (caadr alist))
	   (set-cdr! alist (cddr alist)) alist)
	  (else
	   (let l ((al (cdr alist)))
	     (cond ((null? (cdr al)) alist)
		   ((pred key (caadr al))
		    (set-cdr! al (cddr al)) alist)
		   (else (l (cdr al)))))))))

;;@body
;;Returns a new association list formed by mapping @var{proc} over the
;;keys and values of @var{alist}.   @var{proc} must be a function of 2
;;arguments which returns the new value part.
(define (alist-map proc alist)
  (map (lambda (pair) (cons (car pair) (proc (car pair) (cdr pair))))
       alist))

;;@body
;;Applies @var{proc} to each pair of keys and values of @var{alist}.
;;@var{proc} must be a function of 2 arguments.  The returned value is
;;unspecified.
(define (alist-for-each proc alist)
  (for-each (lambda (pair) (proc (car pair) (cdr pair))) alist))


;================================================================================


; "hashtab.scm", hash tables for Scheme.
; Copyright (c) 1992, 1993, 2003 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;(require 'hash)
;(require 'alist)

;;@code{(require 'hash-table)}
;;@ftindex hash-table

;;@body
;;Returns a hash function (like @code{hashq}, @code{hashv}, or
;;@code{hash}) corresponding to the equality predicate @var{pred}.
;;@var{pred} should be @code{eq?}, @code{eqv?}, @code{equal?}, @code{=},
;;@code{char=?}, @code{char-ci=?}, @code{string=?}, or
;;@code{string-ci=?}.
(define (predicate->hash pred)
  (cond ((eq? pred eq?) hashq)
	((eq? pred eqv?) hashv)
	((eq? pred equal?) hash)
	((eq? pred =) hashv)
	((eq? pred char=?) hashv)
	((eq? pred char-ci=?) hashv)
	((eq? pred string=?) hash)
	((eq? pred string-ci=?) hash)
	(else (slib:error "unknown predicate for hash" pred))))

;;@noindent
;;A hash table is a vector of association lists.

;;@body
;;Returns a vector of @var{k} empty (association) lists.
(define (make-hash-table k) (make-vector k '()))

;;@noindent
;;Hash table functions provide utilities for an associative database.
;;These functions take an equality predicate, @var{pred}, as an argument.
;;@var{pred} should be @code{eq?}, @code{eqv?}, @code{equal?}, @code{=},
;;@code{char=?}, @code{char-ci=?}, @code{string=?}, or
;;@code{string-ci=?}.

;;@body
;;Returns a hash association function of 2 arguments, @var{key} and
;;@var{hashtab}, corresponding to @var{pred}.  The returned function
;;returns a key-value pair whose key is @var{pred}-equal to its first
;;argument or @code{#f} if no key in @var{hashtab} is @var{pred}-equal to
;;the first argument.
(define (predicate->hash-asso pred)
  (let ((hashfun (predicate->hash pred))
	(asso (predicate->asso pred)))
    (lambda (key hashtab)
      (asso key
	    (vector-ref hashtab (hashfun key (vector-length hashtab)))))))
;;@body
;;Returns a procedure of 2 arguments, @var{hashtab} and @var{key}, which
;;returns the value associated with @var{key} in @var{hashtab} or
;;@code{#f} if @var{key} does not appear in @var{hashtab}.
(define (hash-inquirer pred)
  (let ((hashfun (predicate->hash pred))
	(ainq (alist-inquirer pred)))
    (lambda (hashtab key)
      (ainq (vector-ref hashtab (hashfun key (vector-length hashtab)))
	    key))))
;;@body
;;Returns a procedure of 3 arguments, @var{hashtab}, @var{key}, and
;;@var{value}, which modifies @var{hashtab} so that @var{key} and
;;@var{value} associated.  Any previous value associated with @var{key}
;;will be lost.
(define (hash-associator pred)
  (let ((hashfun (predicate->hash pred))
	(asso (alist-associator pred)))
    (lambda (hashtab key val)
      (let* ((num (hashfun key (vector-length hashtab))))
	(vector-set! hashtab num
		     (asso (vector-ref hashtab num) key val)))
      hashtab)))
;;@body
;;Returns a procedure of 2 arguments, @var{hashtab} and @var{key}, which
;;modifies @var{hashtab} so that the association whose key is @var{key} is
;;removed.
(define (hash-remover pred)
  (let ((hashfun (predicate->hash pred))
	(arem (alist-remover pred)))
    (lambda (hashtab key)
      (let* ((num (hashfun key (vector-length hashtab))))
	(vector-set! hashtab num
		     (arem (vector-ref hashtab num) key)))
      hashtab)))
;;@args proc hash-table
;;Returns a new hash table formed by mapping @var{proc} over the
;;keys and values of @var{hash-table}.  @var{proc} must be a function of 2
;;arguments which returns the new value part.
(define (hash-map proc ht)
  (define nht (make-vector (vector-length ht)))
  (do ((i (+ -1 (vector-length ht)) (+ -1 i)))
      ((negative? i) nht)
    (vector-set!
     nht i
     (alist-map proc (vector-ref ht i)))))
;;@args proc hash-table
;;Applies @var{proc} to each pair of keys and values of @var{hash-table}.
;;@var{proc} must be a function of 2 arguments.  The returned value is
;;unspecified.
(define (hash-for-each proc ht)
  (do ((i (+ -1 (vector-length ht)) (+ -1 i)))
      ((negative? i))
    (alist-for-each proc (vector-ref ht i))))
;;@body
;;@0 accepts a hash table predicate and returns a function of two
;;arguments @var{hashtab} and @var{new-k} which is specialized for
;;that predicate.
;;
;;This function is used for nondestrutively resizing a hash table.
;;@var{hashtab} should be an existing hash-table using @1, @var{new-k}
;;is the size of a new hash table to be returned.  The new hash table
;;will have all of the associations of the old hash table.
(define (hash-rehasher pred)
  (let ((hashfun (predicate->hash pred)))
    (lambda (hashtab newk)
      (let ((newtab (make-hash-table newk)))
	(hash-for-each
	 (lambda (key value)
	   (let ((num (hashfun key newk)))
	     (vector-set! newtab num
			  (cons (cons key value)
				(vector-ref newtab num)))))
	 hashtab)
	newtab))))


) ;; End module.