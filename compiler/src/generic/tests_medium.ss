
(define tests_medium
  '( (let ([x (lambda () 4)])
       (letrec ([y (lambda () (z))] [z x]) (y)))
     (let ([f (lambda (x) (if x (not x) x))]
           [f2 (lambda (x) (* 10 x))]
           [x 23])
       (add1 (if (f (zero? x)) 1 (* x (f2 (sub1 x))))))
     (letrec ([even
                (lambda (x) (if (zero? x) #t (odd (sub1 x))))]
              [odd (lambda (x) (if (zero? x) #f (even (sub1 x))))])
       (odd 13))
     
     (let ([t #t] [f #f])
       (letrec ([even (lambda (x) (if (zero? x) t (odd (sub1 x))))]
                [odd (lambda (x) (if (zero? x) f (even (sub1 x))))])
         (odd 13)))
     
     (let ([even (lambda (x) x)])
       (even (letrec ([even
                        (lambda (x) (if (zero? x) #t (odd (sub1 x))))]
                      [odd (lambda (x) (if (zero? x) #f (even (sub1 x))))])
               (odd 13))))
     (let ([x 5])
       (letrec ([a
                  (lambda (u v w) (if (zero? u) (b v w) (a (- u 1) v w)))]
                [b
                  (lambda (q r)
                    (let ([p (* q r)])
                      (letrec ([e (lambda (n) (if (zero? n) (c p) (o (- n 1))))]
                               [o
                                 (lambda (n) (if (zero? n) (c x) (e (- n 1))))])
                        (e (* q r)))))]
                [c (lambda (x) (* 5 x))])
         (a 3 2 1)))
     (let ([f (lambda (x) (add1 x))]
           [g (lambda (x) (sub1 x))]
           [t (lambda (x) (add1 x))]
           [j (lambda (x) (add1 x))]
           [i (lambda (x) (add1 x))]
           [h (lambda (x) (add1 x))]
           [x 80])
       (let ([a (f x)] [b (g x)] [c (h (i (j (t x))))])
         (* a (* b (+ c 0)))))
     (let ([v (make-vector 3)])
       (vector-set! v 0 10)
       (vector-set! v 1 20)
       (vector-set! v 2 30)
       (if (vector? v)
           (+ (+ (vector-length v) (vector-ref v 0))
              (+ (vector-ref v 1) (vector-ref v 2)))
           10000))
     ((((((lambda (x)
            (lambda (y)
              (lambda (z)
                (lambda (w)
                  (lambda (u)
                    (+ x (+ y (+ z (+ w u)))))))))
          5) 6) 7) 8) 9)
     ((((lambda (a)
          (lambda (b)
            (set! a (if b 200))
            (lambda (c) (set! c (if 300 400)) (+ a (+ b c)))))
        1000) 2000) 3000)
     (letrec ([f (lambda (x) (if (zero? x) 1 (* x (f (- x 1)))))])
       (let ([q 17])
         (let ([g (lambda (a) (set! q 10) (lambda () (a q)))])
           ((g f)))))
     (letrec ([f (lambda (x) (letrec ([x 3]) 3))])
       (letrec ([g (lambda (x) (letrec ([y 14]) (set! y 7) y))])
         (set! g (cons g 3))
         (letrec ([h (lambda (x) x)] [z 42]) (cons (cdr g) (h z)))))
     
     ;; Breaks Hotdog:
     (letrec ([fib
                (lambda (x)
                  (let ([decrx (lambda () (set! x (- x 1)))])
                    (if (< x 2)
                        1
                        (+ (begin (decrx) (fib x)) (begin (decrx) (fib x))))))])
       (fib 10))
     ;; Hotdog is ok with this tho (sigh):
     #;(letrec
         ((fib (lambda (x)
                 (if (< x 2)
                     1
                     (+ (begin (set! x (- x 1)) (fib x))
                        (begin (set! x (- x 1)) (fib x)))))))
         (fib 10))
     
     ;; GREAT, this one breaks Hotdog also:
     (letrec ([fib
                (lambda (x)
                  (let ([decrx (lambda () (lambda (i) (set! x (- x i))))])
                    (if (< x 2)
                        1
                        (+ (begin ((decrx) 1) (fib x))
                           (begin ((decrx) 1) (fib x))))))])
       (fib 10))
     
     (let ([t #t] [f #f])
       (let ([bools (cons t f)] [id (lambda (x) (if (not x) f t))])
         (letrec
           ([even (lambda (x) (if (id (zero? x)) (car bools) (odd (- x 1))))]
            [odd (lambda (y) (if (zero? y) (id (cdr bools)) (even (- y 1))))])
           (odd 5))))
     ((lambda (x y z)
        (let  ((f (lambda (u v) (begin (set! x u) (+ x v))))
               (g (lambda (r s) (begin (set! y (+ z s)) y))))
          (* (f '1 '2) (g '3 '4))))
      '10 '11 '12)
     ((lambda (x y z)
        (let ((f '#f)
              (g (lambda (r s) (begin (set! y (+ z s)) y))))
          (begin
            (set! f
                  (lambda (u v) (begin (set! v u) (+ x v))))
            (* (f '1 '2) (g '3 '4)))))
      '10 '11 '12)
     (let ([y 3])
       (letrec ([f
                  (lambda (x) (if (zero? x) (g (+ x 1)) (f (- x y))))]
                [g (lambda (x) (h (* x x)))]
                [h (lambda (x) x)])
         (g 39)))
     (letrec ([f (lambda (x) (+ x 1))]
              [g (lambda (y) (f (f y)))])
       (set! f (lambda (x) (- x 1)))
       (+ (f 1) (g 1)))
     (let ([v (make-vector 8)])
       (vector-set! v 0 '())
       (vector-set! v 1 (void))
       (vector-set! v 2 #f)
       (vector-set! v 3 (cons 3 4))
       (vector-set! v 4 (make-vector 3))
       (vector-set! v 5 #t)
       (vector-set! v 6 2)
       (vector-set! v 7 5)
       (vector-ref v (vector-ref v 6)))
     (let ([negative? (lambda (n) (< n 0))])
       (letrec ([fact
                  (lambda (n) (if (zero? n) 1 (* n (fact (- n 1)))))]
                [call-fact
                  (lambda (n)
                    (if (not (negative? n)) (fact n) (- 0 (fact (- 0 n)))))])
         (cons (call-fact 5) (call-fact -5))))
     ; make-vector with non-constant operand and improper alignment
     (let ([x 6])
       (let ([v (make-vector x)])
         (vector-set! v 0 3)
         (vector-set! v 1 (cons (vector-ref v 0) 2))
         (vector-set! v 2 (cons (vector-ref v 1) 2))
         (vector-set! v 3 (cons (vector-ref v 2) 2))
         (vector-set! v 4 (cons (vector-ref v 3) 2))
         (vector-set! v 5 (cons (vector-ref v 4) 2))
         (cons (pair? (vector-ref v 5)) (car (vector-ref v 4)))))
     ; nest some lambdas
     (((((lambda (a)
           (lambda (b)
             (lambda (c)
               (lambda (d)
                 (cons (cons a b) (cons c d))))))
         33) 55) 77) 99)
     
     ;; RRN[2002.03.06]  These tests depend on the uninitialized values
     ;; in a new vector... maybe fix them later, discarding now:
     #;(let ([v (make-vector (add1 37))])
         (vector-set! v 0 (boolean? v))
         (vector-set! v (* 3 11) (vector-length v))
         ((let ([w (cons 33 '())])
            (lambda ()
              (if (not (eq? w (cons 33 '())))
                  (begin (set-cdr! w (vector? v)) w))))))
     
     #;(let ([v (make-vector (add1 37))])
         (vector-set! v 0 (boolean? v))
         (vector-set! v (* 3 11) #t)
         ((let ([w (cons (sub1 34) #f)])
            (lambda ()
              (set-cdr! w v)
              (if (not (eq? w (cons (- (vector-length v) 5) v)))
                  (begin (set-car! w (vector-ref (cdr w) (car w))) w))))))
     
     ; test use of keywords/primitives as variables
     #;(let ([quote (lambda (x) x)]
             [let (lambda (x y) (- y x))]
             [if (lambda (x y z) (cons x z))]
             [cons (lambda (x y) (cons y x))]
             [+ 16])
         (set! + (* 16 2))
         (cons (let ((quote (lambda () 0))) +)
               (if (quote (not #f))
                   720000
                   -1)))
     (let ([a (lambda (x) x)]
           [b (lambda (x y) (- y x))]
           [c (lambda (x y z) (cons x z))]
           [d (lambda (x y) (cons y x))]
           [e 16])
       (set! e (* 16 2))
       (d (b ((a (lambda () 0))) e)
          (c (a (not #f)) 720000 -1)))
     ;; Jie Li
     (let ((a 5))
       (let ((b (cons a 6)))
         (let ((f (lambda(x) (* x a))))
           (begin (if (- (f a) (car b))
                      (begin (set-car! b
                                       (if (not a) (* 2 a) (+ 2 a)))
                             (f a))
                      (if (not (not (< (f a) b)))
                          (f a)))
                  (not 3)
                  (void)
                  (f (car b))))))
     
     (letrec ((f (lambda (x)
                   (+ x (((lambda (y)
                            (lambda (z)
                              (+ y z)))
                          6)7))))
              (g (+ 5 ((lambda (w u) (+ w u)) 8 9))))
       g)
     ;; Ryan Newton
     (letrec
       ((loop
          (lambda ()
            (lambda ()
              (loop)))))
       (loop)
       0)
     (letrec ([f (lambda ()
                   (letrec ([loop
                              (lambda (link)
                                (lambda ()
                                  (link)))])
                     (loop (lambda () 668))))])
       ((f)))
     (if (lambda () 1)
         (let ((a 2))
           (if (if ((lambda (x)
                      (let ((x (set! a (set! a 1))))
                        x)) 1)
                   (if (eq? a (void))
                       #t
                       #f)
                   #f)
               #36rgood        ; dyb: cannot use symbols, so use radix 36
               #36rbad)))      ; syntax to make all letters digits
     
     ; contributed by Ryan Newton
     ))