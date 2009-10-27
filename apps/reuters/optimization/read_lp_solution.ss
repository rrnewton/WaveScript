#! /bin/bash
#|
exec regiment.chez i --script $0 $*
|#

;; [2009.10.23] This script reads the output of lp_solve and retrieves
;; the values of relevant variables to let us know what the solution
;; was.


(define arg1
  ;; A bunch of spurious arguments because of the way this is invoked (above):
  (if (= (length (command-line)) 6)
      (rac (command-line))
      #f))

(define prt
  (if arg1
      (open-input-file arg1)
      (current-input-port)))

(define lines (port->linelists prt))

;; This depends on the convention for variable names that we established in formulate_ilp.ss
(define (assign-var? vr)
  (define str (symbol->string vr))
  (define len (string-length "assign"))
  (if (<= (string-length str) len) 
      #f
      (string=? "assign" (substring str 0 len))))

(define (split-var vr)
  (match (string-split (symbol->string vr) #\_)
    [("assign" ,op , node)
     ;(values (string->symbol op)  (string->symbol node))
     (values op node)
     ]))

(for-each 
    (lambda (line)
      (match line
       [(,var 1) (guard (assign-var? var))
	(define-values (op nd) (split-var var))
	(printf "pin ~a ~a\n" op nd)]
       [,else (void)]))
  lines)

