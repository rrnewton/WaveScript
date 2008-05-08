
(define tests_slow
  '(
     (letrec ((f (lambda () (+ a b)))
              (g (lambda (y) (set! g (lambda (y) y)) (+ y y)))
              (a 17)
              (b 35)
              (h (cons (lambda () a) (lambda (v) (set! a v)))))
       (let ((x1 (f)) (x2 (g 22)) (x3 ((car h))))
         (let ((x4 (g 22)))
           ((cdr h) 3)
           (let ((x5 (f)) (x6 ((car h))))
             (cons x1 (cons x2 (cons x3 (cons x4 (cons x5 x6)))))))))
     
     (letrec ((f (lambda () (+ a b)))
              (a 17)
              (b 35)
              (h (cons (lambda () a) (lambda () b))))
       (cons (f) (cons a (cons b (cons ((car h)) ((cdr h)))))))
     ;-------
     (let ([t #t] [f #f])
       (let ([bools (cons t f)] [id (lambda (x) (if (not x) f t))])
         (letrec
           ([even (lambda (x) (if (zero? x) (id (car bools)) (odd (- x 1))))]
            [odd (lambda (y) (if (zero? y) (id (cdr bools)) (even (- y 1))))])
           (odd 5))))
     ; stress the register allocator
     (let ((a 17))
       (let ((f (lambda (x)
                  (let ((x1 (+ x 1)) (x2 (+ x 2)))
                    (let ((y1 (* x1 7)) (y2 (* x2 7)))
                      (let ((z1 (- y1 x1)) (z2 (- y2 x2)))
                        (let ((w1 (* z1 a)) (w2 (* z2 a)))
                          (let ([g (lambda (b)
                                     (if (= b a)
                                         (cons x1 (cons y1 (cons z1 '())))
                                         (cons x2 (cons y2 (cons z2 '())))))]
                                [h (lambda (c)
                                     (if (= c x) w1 w2))])
                            (if (if (= (* x x) (+ x x))
                                    #t
                                    (< x 0))
                                (cons (g 17) (g 16))
                                (cons (h x) (h (- x 0))))))))))))
         (cons (f 2) (cons (f -1) (cons (f 3) '())))))
     
     ;; Jie Li
     (letrec ([f (lambda (x y) (if (not x) (g (add1 x) (add1 y)) (h (+ x y))))]
              [g (lambda (u v)
                   (let ([a (+ u v)]
                         [b (* u v)])
                     (letrec ([e (lambda (d)
                                   (letrec ([p (cons a b)]
                                            [q (lambda (m)
                                                 (if (< m u)
                                                     (f m d)
                                                     (h (car p))))])
                                     (q (f a b))))])
                       (e u))))]
              [h (lambda (w) w)])
       (f 4 5))
     
     ;;; Jordan Johnson
     (let ((test (if (not (not 10)) #f 5)))
       (letrec ([num 5]
                [length
                  (lambda (ls)
                    (let ((len (if ((lambda (ck) (begin ck (set! num test) ck))
                                    (null? ls))
                                   (begin num (set! num 0) num)
                                   (begin (length '())
                                          (set! num 5)
                                          (+ 1 (length (cdr ls)))))))
                      (if len len)))])
         (length (cons 5 (cons (if (set! num 50) (length (cons test '())) 1)
                               '())))))
     ;; Jordan Johnson
     (letrec ([quotient (lambda (x y)
                          (if (< x 0)
                              (- 0 (quotient (- 0 x) y))
                              (if (< y 0)
                                  (- 0 (quotient x (- 0 y)))
                                  (letrec ([f (lambda (x a)
                                                (if (< x y)
                                                    a
                                                    (f (- x y) (+ a 1))))])
                                    (f x 0)))))])
       (letrec ([sub-interval 1]
                [sub-and-continue
                  (lambda (n acc k) (k (- n sub-interval) (* n acc)))]
                [strange-fact
                  (lambda (n acc)
                    (if (zero? n)
                        (lambda (proc) (proc acc))
                        (sub-and-continue n acc strange-fact)))])
         (let ([x 20]
               [fact (let ((seed 1)) (lambda (n) (strange-fact n seed)))])
           (let ([give-fact5-answer (fact 5)]
                 [give-fact6-answer (fact 6)]
                 [answer-user (lambda (ans) (quotient ans x))])
             (set! x (give-fact5-answer answer-user))
             (begin (set! x (give-fact6-answer answer-user))
                    x)))))
     ;; Jordan Johnson
     (let ((y '())
           (z 10))
       (let ((test-ls (cons 5 y)))
         (set! y (lambda (f)
                   ((lambda (g) (f (lambda (x) ((g g) x))))
                    (lambda (g) (f (lambda (x) ((g g) x)))))))
         (set! test-ls (cons z test-ls))
         (letrec ((length (lambda (ls)
                            (if (null? ls) 0 (+ 1 (length (cdr ls)))))))
           (let ((len (length test-ls)))
             (eq? (begin
                    (set! length (y (lambda (len)
                                      (lambda (ls)
                                        (if (null? ls)
                                            0
                                            (+ 1 (len (cdr ls))))))))
                    (length test-ls))
                  len)))))
     
     ;; Ryan Newton
     (letrec
       ((dropsearch
          (lambda (cell tree)
            (letrec
              ((create-link
                 (lambda (node f)
                   (lambda (g)
                     (if (not (pair? node))
                         (f g)
                         (if (eq? node cell)
                             #f
                             (f (create-link (car node)
                                             (create-link (cdr node) g))))))))
               (loop
                 (lambda (link)
                   (lambda ()
                     (if link
                         (loop (link (lambda (v) v)))
                         #f)))))
              (loop (create-link tree (lambda (x) x)))
              )))
        
        (racethunks
          (lambda (thunkx thunky)
            (if (if thunkx thunky #f)
                (racethunks (thunkx) (thunky))
                (if thunky
                    #t
                    (if thunkx
                        #f
                        '())))))
        
        (higher?
          (lambda (x y tree)
            (racethunks (dropsearch x tree)
                        (dropsearch y tree))))
        
        (under?
          (lambda (x y tree)
            (racethunks (dropsearch x y)
                        (dropsearch x tree))))
        
        (explore
          (lambda (x y tree)
            (if (not (pair? y))
                #t
                (if (eq? x y)
                    #f    ;This will take out anything that points to itself
                    (let ((result (higher? x y tree)))
                      (if (eq? result #t)
                          (if (explore y (car y) tree)
                              (explore y (cdr y) tree)
                              #f)
                          (if (eq? result #f)
                              (process-vertical-jump x y tree)
                              (if (eq? result '())
                                  (process-horizontal-jump x y tree)
                                  ))))))))
        
        (process-vertical-jump
          (lambda (jumpedfrom jumpedto tree)
            (if
              (under? jumpedfrom jumpedto tree)
              #f
              (fullfinite? jumpedto))))
        
        (process-horizontal-jump
          (lambda (jumpedfrom jumpedto tree)
            (fullfinite? jumpedto)))
        
        (fullfinite?
          (lambda (pair)
            (if (not (pair? pair))
                #t
                (if (explore pair (car pair) pair)
                    (explore pair (cdr pair) pair)
                    #f)))))
       (cons
         (fullfinite? (cons 1 2))
         (cons
           (fullfinite? (let ((x (cons 1 2))) (set-car! x x) x))
           (cons
             (fullfinite? (let ((a (cons 0 0)) (b (cons 0 0)) (c (cons 0 0)))
                            (set-car! a b) (set-cdr! a c) (set-cdr! b c)
                            (set-car! b c) (set-car! c b) (set-cdr! c b) a))
             '()))))
     ))
