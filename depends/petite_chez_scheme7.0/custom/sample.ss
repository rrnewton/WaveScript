;;; sample.ss
;;; Copyright (c) 1997 Cadence Research Systems

;;; This is a sample Scheme customization file.

;;; When inserted into the list of Scheme object files (sobj) in Makefile,
;;; either as sample.ss (source) or sample.so (object), the procedures
;;; defined here will be present in the resulting executable image.  The
;;; sample C object file sample.o must be included in the list of C object
;;; files (cobj) in Makefile to provide the required C entry points.


;;; The C-coded "id" (see sample.c) takes an int and returns it.

(define int->int (foreign-procedure "id" (integer-32) integer-32))


;;; Since C characters are passed as C ints, we can pass "id" a character
;;; and get back an int.  This should be equivalent to char->integer.

(define char->int (foreign-procedure "id" (char) integer-32))


;;; After initialization, the procedure value of the parameter scheme-start
;;; is invoked to start up the Scheme session.  The default definition is:
;;;     (scheme-start
;;;       (lambda x
;;;         (for-each load x)
;;;         (new-cafe)))
;;; The version of scheme-start given below causes the filename arguments
;;; to be displayed on the screen before being loaded, then starts up a new
;;; cafe as usual:

(scheme-start
  (lambda x
    (for-each (lambda (x) (printf "Loading ~a ...~%" x) (load x)) x)
    (new-cafe)))
