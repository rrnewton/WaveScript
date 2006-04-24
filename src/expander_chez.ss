
(define file->slist
  (lambda (filename)
    (let ([p (open-input-file filename)])
      (let loop ([exp (read p)])
        (if (eof-object? exp)
            (begin (close-port p)
                   '())
            (cons exp (loop (read p))))))))



(define exprs (file->slist "main_chez.ss"))

;; Don't use map because it doesn't guarantee order:
(define (map-serial f ls)
  (let loop ((ls ls) (acc '()))
    (if (null? ls) (reverse! acc)
	(loop (cdr ls) (cons (f (car ls)) acc)))))

(define expanded (map-serial expand exprs))

(slist->file expanded "main_chez_EXPANDED.ss")
(display "File written.")(newline)
