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
      ;; If we're reading from a file we cut off the prefix first.
      ;; (This allows us to run lp_solve with verbose turned on.)
      (let ((tmp (car (string->lines (system-to-str "tempfile")))))
	;; This can be rather wasteful if the file is, say, 20mb.  Better to do it with awk.
	(system (format "csplit --prefix=~a_ ~a '/Value of objective/' > /dev/null" tmp arg1))
	(open-input-file (string-append tmp "_01")))
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

  ;; How we do this is a hack:  We assume no double-underscore in the variables:
  (define-values (left right)
    (split-before (curry string=? "") 
		  (string-split (symbol->string vr) #\_)))

  
;   (printf "SPLIT UP ~s\n" (string-split (symbol->string vr) #\_))
;   (let-values (((a b) (split-before (curry string=? "") 
; 				    (string-split (symbol->string vr) #\_))))    
;     (printf "MORE SPLIT ~s ~s\n" a b))
  
;   (match (string-split (symbol->string vr) #\_)
;     [("assign" ,op , node)
;      ;(values (string->symbol op)  (string->symbol node))
;      (values op node)
;      ])


  (ASSERT (string=? "assign" (car left)))
  (ASSERT (string=? "" (car right)))
  (values 
   (apply string-append (insert-between "_" (cdr left)))
   (apply string-append (insert-between "_" (cdr right))))
  )

(for-each 
    (lambda (line)
      (match line
       [(,var 1) (guard (assign-var? var))
	(define-values (op nd) (split-var var))
	(printf "pin ~a ~a\n" op nd)]
       [,else (void)]))
  lines)

