;;; match.ss 
;;; (broken out for PLT port only)

;;; This program was originally designed and implemented by Dan
;;; Friedman.  It was redesigned and implemented by Erik Hilsdale;
;;; some improvements were suggested by Steve Ganz.  Additional
;;; modifications were made by Kent Dybvig. Original port to PLT Scheme
;;; by Matthew Flatt. As of 20020328, Matt Jadud is maintaining
;;; the PLT Scheme port of this code.

;;; Copyright (c) 1992-2002 Cadence Research Systems
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright notice in full.  This software
;;; is provided AS IS, with NO WARRANTY, EITHER EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO IMPLIED WARRANTIES OF MERCHANTABILITY
;;; OR FITNESS FOR ANY PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
;;; AUTHORS BE LIABLE FOR CONSEQUENTIAL OR INCIDENTAL DAMAGES OF ANY
;;; NATURE WHATSOEVER.

;;; If you are reading this comment, then you do not have
;;; the most recent version of 'match'.


(module iu-exptime mzscheme
  (provide syntax-error
	   let-synvalues*
	   with-values syntax-lambda
	   with-temp with-temps)

  (define (syntax-error e m)
    (raise-syntax-error #f m e))

  (define-syntax let-synvalues*
    (syntax-rules ()
      ((_ () B0 B ...) (begin B0 B ...))
      ((_ (((Formal ...) Exp) Decl ...) B0 B ...)
       (call-with-values (lambda () Exp)
	 (lambda (Formal ...)
	   (with-syntax ((Formal Formal) ...)
	     (let-synvalues* (Decl ...) B0 B ...)))))))

  (define-syntax with-values
      (syntax-rules ()
        ((_ P C) (call-with-values (lambda () P) C))))
    (define-syntax syntax-lambda
      (lambda (x)
        (syntax-case x ()
          ((_ (Pat ...) Body0 Body ...)
           (with-syntax (((X ...) (generate-temporaries #'(Pat ...))))
             #'(lambda (X ...)
                 (with-syntax ((Pat X) ...)
                   Body0 Body ...)))))))
    (define-syntax with-temp
      (syntax-rules ()
        ((_ V Body0 Body ...)
         (with-syntax (((V) (generate-temporaries '(x))))
           Body0 Body ...))))
    (define-syntax with-temps
      (syntax-rules ()
        ((_ (V ...) (Exp ...) Body0 Body ...)
         (with-syntax (((V ...) (generate-temporaries #'(Exp ...))))
           Body0 Body ...))))
)
