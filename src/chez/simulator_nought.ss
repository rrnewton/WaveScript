
;; [2004.06.03]
;; This has been separated from the generic version because chez and
;; plt use their own respective hash tables.

;; Like other chez files this is loaded from the src/ directory, not
;; the src/chez/ directory.

(define (make-default-hash-table) (make-hash-table 50))

;; Lame directory switching
(cd "generic")
(include "simulator_nought.ss")
(cd "..")


