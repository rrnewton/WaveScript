":";exec snow -- "$0" "$@"

;;;; This is a reimplementation of a subset of the functionality
;;;; provided by the IU pattern matching facility (match.ss)
;;;;  -Ryan Newton [2006/2007]

;; [2007.03.04] 
;; This package passes its simple tests under:         scm, chez, mzscheme, guile
;; It passes all but the multiple-values test under:   gauche, stklos, bigloo, scheme48, larceny
;; 
;; larceny -- Gets a wrong number of arguments error on the same test as bigloo.

;; chicken -- supports define-syntax, but can't handle the macros in match.r5rs

(package* iu-match/v0.0.1
 (provide:
;  (define* (test-match))
  ;; Should use define-syntax* export here when it's available.
  )

 (maintainer: "Ryan Newton <ryan.newton at alum.mit.edu>")
 (author:     "Ryan Newton <ryan.newton at alum.mit.edu>")
 (homepage:   "http://snow.iro.umontreal.ca")
 (description: "Pattern matching against lists and vectors.")
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
