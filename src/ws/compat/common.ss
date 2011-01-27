
;; Shared compatibility layer pieces.

(define-syntax fluid-let
    (lambda (x)
      (syntax-case x ()
		   ((_ () e1 e2 ...) #'(let () e1 e2 ...))
		   ((_ ((x v) ...) e1 e2 ...)
		    (for-all identifier? #'(x ...))
		    (with-syntax (((y ...) (generate-temporaries #'(x ...))))
		      #'(let ((y v) ...)
			  (let ((swap (lambda ()
					(let ((t x)) (set! x y) (set! y t)) ...)))
			    (dynamic-wind swap (lambda () e1 e2 ...) swap))))))))

(define merge
  (lambda (pred? l1 l2)
    (cond
     ((null? l1) l2)
     ((null? l2) l1)
     ((pred? (car l2) (car l1))
      (cons (car l2) (merge pred? l1 (cdr l2))))
     (else (cons (car l1) (merge pred? (cdr l1) l2))))))
(define merge! merge)


#;
(define-syntax datum
    (syntax-rules ()
      [(_ t) (syntax->datum (syntax t))]))
  
(define syntax->list
    (lambda (ls)
      (syntax-case ls ()
	[() '()]
	[(x . r) (cons #'x (syntax->list #'r))])))


#;
  (define-syntax identifier-syntax
   (lambda (x)
     (syntax-case x ()
      [(_ e)
       #'(lambda (x)
           (syntax-case x ()
             [id  (identifier? #'id)  #'e]
             [(id x (... ...))  (identifier? #'id)
              #'(e x (... ...))]))])))


  (define (simple-eval xp) (eval xp (environment '(rnrs (6)))))


 (define-syntax IFCHEZ (syntax-rules ()  [(_ a b) b]))

  ;; The default uncaught exception handler prints ugly messages.
  (define (error who msg . args)
    (printf "************\n")
    (printf "Error in ~a: " who)
    (apply printf msg args)(newline)
    (raise (make-error))
    ;(exit -1)
    )