
;; [2004.06.03]
;; This has been separated from the generic version because chez and
;; plt use their own respective hash tables.

;; Like other chez files this is loaded from the src/ directory, not
;; the src/chez/ directory.

;; Uses slib:
(require 'hash-table)

(define hashtab-get (hash-inquirer eq?))
(define hashtab-set! (hash-associator eq?))

(define (sleep-me sec) 
  (thread-sleep (flonum->fixnum (* sec 1000.0))))

;; Lame directory switching
(cd "generic")
(include "simulator_nought_graphics.ss")
(cd "..")

