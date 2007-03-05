":";exec snow -- "$0" "$@"

;;;; This is a reimplementation of a subset of the functionality
;;;; provided by the IU pattern matching facility (match.ss)
;;;;  -Ryan Newton [2006/2007]

;; [2007.03.04] 
;; This package passes its simple tests under:         
;;   scm, chez, mzscheme, guile, gauche, stklos, bigloo, scheme48, larceny

;; mitscheme -- Runs into some unknown problem with trying to apply 3.
;; chicken -- supports define-syntax, but can't handle the macros in match.r5rs

(package* iu-match/v0.0.1
 (provide:
;  (define* (test-match))
  ;; Should use define-syntax* export here when it's available.
  )

 (maintainer: "Ryan Newton <ryan.newton at alum.mit.edu>")
 (author:     "Ryan Newton <ryan.newton at alum.mit.edu>")
 (homepage:   "http://snow.iro.umontreal.ca")
 (description: "Pattern matching against lists and vectors."
"This package implements a syntactic sugar (match) for deconstructing
 lists and vectors.  For example:

  (match 3 (,x x)) ==> 3
  (match '(1 2) ((,x ,y) (+ x y))) ==> 3
  (match '#(1 2) 
    ((,x ,y ,z)  'err) 
    (#(,x ,y)   (* 100 y))) ==> 200

Thus, (match expression clause*)
Where clause := (pat expr ...) | (pat (guard pred ...) expr ...)

The predicates within the guard may refer to pattern variables so long
as they are not the result of a recursive match (see below).

The grammar for patterns is:

    ,patvar                      ;; matches anything, and binds patvar 
  | literal                      ;; symbol, number, etc, matches with equal? 
  | (pat1 ... patN)              ;; list of n elements 
  | (pat1 ... patN . patN+1)     ;; list of n or more 
  | (pat1 ... patN _...)         ;; patN matches against any number of elements
                                 ;;   all patvars within patN are bound to lists
  | #(pat1 ... patN)             ;; vector of n elements 
  | #(pat1 ... patN _...)        ;; vector of n or more, patN matches remainder
  | ,(patvar)                    ;; recursively match this position, 
                                 ;;   bind result to patvar
  | ,(f -> patvar)               ;; apply function f to this position, 
                                 ;;   bind result to patvar

This pattern grammar uses '_...' for ellipses because syntax-rules on
most scheme implementations will not allow '...' in the literals list.

")

 (keywords: pattern-matching data)
 (license: lgpl/v2.1)
 )

(display "Loading iu-match pattern matcher.")(newline)

; (display "Loading pattern matcher in scheme system \"")
; (display (cond-expand
; 	  (gambit   'gambit)
; 	  (bigloo   'bigloo)
; 	  (scm      'scm)
; 	  (guile    'guile)
; 	  (mzscheme 'mzscheme)
; 	  (gauche   'gauche)
; 	  (chez     'chez)
; 	  (petite   'petite)
; 	  (stklos   'stklos)
; 	  (else 'unknown))) (display #\") (newline)

; (define thecode (read (open-input-file "match.r5rs")))

;------------------------------------------------------------
;; Switch into r5rs mode to get define-syntax.

(cond-expand
 ;; We're already there:
 ((or chez petite mzscheme bigloo stklos scheme48 gauche) (begin))

 ;; Known not to work
 (chicken ;(snow-error "Chicken cannot handle 'highlevel macros'.")
          (begin))
 ;; This should work, but there's a problem when the package is subsequently 'require'd:
 (gambit ;(snow-error "iu-match runs into a problem after loading syntax-case.scm in gambit")
	 (load "~~/syntax-case.scm"))
 
 ;; Otherwise need an incantation:
 (scm  (require 'r5rs))
 (guile (use-syntax (ice-9 syncase)))

 (else (snow-error "Cannot currently support R5RS macros under this implementation.")))


;------------------------------------------------------------
;; Now load the syntax-transformers into the top level.

(load "./match.r5rs")

;------------------------------------------------------------
