
;;;; This is a pass that invokes the typechecker.

(module typecheck mzscheme
  (require "../../../plt/common.ss")
  (provide retypecheck)
  (chezimports)

; ----------------------------------------

;; This is simply used between subsequent passes to verify the intermediate programs.
(define-pass retypecheck
    [Program (lambda (prog _)
	       (annotate-program prog))])

) ; End module