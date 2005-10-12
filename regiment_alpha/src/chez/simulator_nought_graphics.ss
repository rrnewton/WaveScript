
;; [2004.06.03]
;; This has been separated from the generic version because chez and
;; plt use their own respective hash tables.

;; Like other chez files this is loaded from the src/ directory, not
;; the src/chez/ directory.

;; Uses slib:
;; [2004.06.28] Moved this to chez/helpers.ss:
;(require 'hash-table)

(define (sleep-me sec) 
  (thread-sleep (flonum->fixnum (* sec 1000.0))))

;; Lame directory switching
(cd "generic")
(include "simulator_nought_graphics.ss")
(cd "..")

