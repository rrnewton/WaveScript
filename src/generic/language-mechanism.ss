
;;; language definition mechanism:
(define define-language
  (lambda (name def)
    (define-top-level-value
      name
      (case-lambda
        [() (eval def)]
        [(exp)
         (match exp
           [print (pretty-print def)]
           [return def]
           ;; Ignores any amount of inserted stuff:
           [(program ,stuff ... ,body)
            (eval `(let () ,def ,body))]
           [,body (eval `(let () ,def ,body))])]))))

(define subtract-bindings
  (lambda (names bindings)
    `(begin
       ,@(filter (lambda (binding)
                   (match binding
                     [(define ,name ,rhs)
                      (guard (memq name names)) #f]
                     [(define-syntax ,name ,rhs)
                      (guard (memq name names)) #f]
                     [,else #t]))
                 (cdr bindings)))))

(define-language 'nil-language '(begin))

(define-language
  'base-language
  '(begin
 
     (define (anchor) '(ANCH world))
     (define (anchor-at l) `(ANCH ,l))
     (define (anchor-where f) `(ANCH fun))

     (define (circle a d) `(CIRC ,a ,d))
     (define (circle-at l d) `(CIRC ,l ,d))
     
    
;     (define (anchor-at 

;     (define (circle rad anch)
;       (match anch
;	      [(anchor ,id ,x ,y)
;	       `(circle ,(gensym)
;TODODOTODOTO
     ))




