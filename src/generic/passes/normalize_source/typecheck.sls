#!r6rs

;;;; This is a pass that invokes the typechecker.

(library (ws passes normalize_source typecheck)
  (export retypecheck)
  (import (rnrs) (ws common))

; ----------------------------------------

;; This is simply used between subsequent passes to verify the intermediate programs.
(define-pass retypecheck
    [Program (lambda (prog _)  
	       (annotate-program prog))])

) ; End module
