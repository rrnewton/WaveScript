
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


;; Define this once for the simulation so that our answers are deterministic.
;; This is a list of <sensereading, xpos, ypos>
(define the-test-field 
  (make-n-list 500 (lambda (ign) (list (#%random 1.0) (#%random 100) (#%random 100)))))

(define-language
  'base-language
  '(begin

      (define (anchor-at pos)
	(find-maximizing (lambda (entry) (posdist (cdr entry) pos)) the-test-field))

      (define (circle-at pos rad)
	(filter (lambda (p) (<= (posdist (cdr p) pos) rad)) the-test-field))
      (define (circle anch rad)
	(filter (lambda (p) (<= (posdist (cdr p) (cdr anch)) rad)) the-test-field))

      (define rfold foldl)
      (define rmap map)
      (define sense car)

      (define world the-test-field)

 
;     (define (anchor) '(ANCH world))
;     (define (anchor-at l) `(ANCH ,l))
;     (define (anchor-where f) `(ANCH fun))

;     (define (circle a d) `(CIRC ,a ,d))
;     (define (circle-at l d) `(CIRC ,l ,d))
         
;     (define (anchor-at 

;     (define (circle rad anch)
;       (match anch
;	      [(anchor ,id ,x ,y)
;	       `(circle ,(gensym)
;TODODOTODOTO
     ))




