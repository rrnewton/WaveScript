
;;;; This is a pass that invokes the typechecker.


;; This is simply used between subsequent passes to verify the intermediate programs.
(define-pass retypecheck
    [Program (lambda (prog _)
	       (annotate-program prog))])

