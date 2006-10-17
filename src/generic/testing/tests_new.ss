;===============================================================================

'(let ([f (case-lambda [(a b) 3]
                       [more 4])])
   (f 1 2 3))


(define tests_new_misc
  '(
     ;(cons 'yay_symbols "And strings")
     "woo"
     #\a
     
     (let ([x "hello"]) (string-set! x 3 #\_)
       (if (string? x)
           (if (char? (string-ref x 4))
               (cons x (string-ref x 1)))))
     (port? (console-output-port))
     
     (list->string (string->list "hello"))
     (char=? (integer->char (char->integer #\a)) #\a)
     (let ([x 2396236462362364932486]
           [y 2396236462362364932400])
       (list (bignum? x)
             (bignum? y)
             (- x y)
             (fixnum? (- x y))))
     (list (< 23946239429468234 912639586213649823)
           (< 324698234962346843 932649864638263264)
           (< 324698234962346843 324698234962346843)
           (< -23946239429468234 -912639586213649823)
           (< -324698234962346843 -932649864638263264)
           (< -324698234962346843 -324698234962346843)
           (< 23946239429468234 -912639586213649823)
           (< -324698234962346843 932649864638263264))
     (- 1111111111111 111111111111111111111)
     
     ;; These cases are probably already tested in the normal tests:
     ;; Check, and delete these if so:
     (list (< 0 0) (<= 1 1) (> 2 2) (>= 3 3))
     
     (list (fx+) (fx+ 3) (fx+ 3 4) (fx+ 1 2 3 4))
     (list       (fx- 3) (fx- 3 4) (fx- 1 2 3 4))
     (list (fx*) (fx* 3) (fx* 3 4) (fx* 1 2 3 4))
     (list       (fx/ 3) (fx/ 4 2) (fx/ 40 3 2 1))
     
     (list (fl+) (fl+ 3.) (fl+ 3. 4.) (fl+ 1. 2. 3. 4.))
     (list       (fl- 3.) (fl- 3. 4.) (fl- 1. 2. 3. 4.))
     (list (fl*) (fl* 3.) (fl* 3. 4.) (fl* 1. 2. 3. 4.))
     (list       (fl/ 3.) (fl/ 4. 2.) (fl/ 40. 3. 2. 1.))
     
     (list (fx= 1 1 1) (fx< 1 2 2 3) (fx<= 1 2 2 3) (fx> 3 2 1) (fx>= 1 2 3))
     (list (fl= 1.5 1.5 1.5) (fl< 1. 2. 2. 3.)
           (fl<= 1. 2. 2. 3.) (fl> 3. 2. 1.) (fl>= 1. 2. 3.))
     
     
     (list (+ 3 4)
           (+ 3 4.5)
           (+ 3 2123456789)  ;; Large fx-integer
           (+ 3 982123456789)  ;; bg-integer
           ;(+ 3 3/4)
           ;(+ 3 4.5+1.5i)
           
           (+ 2123903033 4)
           (+ 2123451234 4.5)
           (+ 2123111885 2123456789)
           ;(+ 2123456789 3/4)
           ;(+ 2123456789 4.5+1.5i)
           
           ;(+ 3/4 4)
           ;(+ 3/4 4.5)
           ;(+ 3/4 2123456789)
           ;(+ 3/4 3/4)
           ;(+ 3/4 4.5+1.5i)
           
           (+ 3.2 4)
           (+ 3.2 4.5)
           (+ 3.2 2123456789)
           ;(+ 3.2 3/4)
           ;(+ 3.2 4.5+1.5i)
           
           ;(+ 3.2+i 4)
           ;(+ 3.2+i 4.5)
           ;(+ 3.2+i 2123456789)
           ;(+ 3.2+i 3/4)
           ;(+ 3.2+i 4.5+1.5i)
           )
     
     (* 3 4)
     (* 3 4.5)
     (* 3 2123456789)  ;; Large fx-integer
     (* 3 982123456789)  ;; bg-integer
     (* 32986259693662 912226239649010101010)
     ;(+ 3 3/4)
     ;(+ 3 4.5+1.5i)
     
     (* 212390300768933 4)
     (* 212345126030664 4.5)
     (* 2123111885 2123456786999)
     ;(+ 2123456789 3/4)
     ;(+ 2123456789 4.5+1.5i)
     
     ;(* 2123903033 4)
     ;(* 2123451234 4.5)
     ;(* 2123111885 2123456789)
     ;(+ 2123456789 3/4)
     ;(+ 2123456789 4.5+1.5i)
     
     
     ;(+ 3/4 4)
     ;(+ 3/4 4.5)
     ;(+ 3/4 2123456789)
     ;(+ 3/4 3/4)
     ;(+ 3/4 4.5+1.5i)
     
     (* 3.2 4)
     (* 3.2 4.5)
     (* 3.2 2123456789)
     ;(+ 3.2 3/4)
     ;(+ 3.2 4.5+1.5i)
     
     ;(+ 3.2+i 4)
     ;(+ 3.2+i 4.5)
     ;(+ 3.2+i 2123456789)
     ;(+ 3.2+i 3/4)
     ;(+ 3.2+i 4.5+1.5i)
     
     (let ([x 2146921649823642984692634])
       (list (- x (- x 3))
             (+ (- x) 3)))
     
     (let ([f (lambda (a b c d e f g) g)])
       (f 1 2 3 4 5 6 7))
     (let ([f (lambda (a b c d e f g) g)]
           [g (lambda (f) (f 1 2 3 4 5 6 7))])
       (g f))
     (let ([f (lambda (a b . c)
                (if (null? c) 100 (car c)))])
       (+ (f 1 2)
          (f 1 2 3)
          (f 1 2 3 4)
          (f 1 2 3 4 5)
          (f 1 2 3 4 5 6)
          (f 1 2 3 4 5 6 7)
          (f 1 2 3 4 5 6 7 8)
          (f 1 2 3 4 5 6 7 8 9)))
     (let ([f (lambda args
                (if (null? args)
                    100
                    (if (null? (cdr args))
                        1000
                        (car (cdr args)))))])
       (+ (f)
          (f 1)
          (f 1 2)
          (f 1 2 3)
          (f 1 2 3 4)
          (f 1 2 3 4 5)
          (f 1 2 3 4 5 6)
          (f 1 2 3 4 5 6 7)
          (f 1 2 3 4 5 6 7 8)))
     
     (let ((f (lambda () '(1 2 3)))) (eq? (f) (f)))
     
     (let ([f (lambda args_19
                (if (#%null? args_19)
                    (#%+)
                    (letrec
                      ([loop
                         (lambda (n ls)
                           (if (#%null? ls) n
                               (loop (#%+ n (#%car ls)) (#%cdr ls))))])
                      (loop (#%+ (#%car args_19)) (#%cdr args_19)))))])
       (f 1 2 3))
     
     ;; These two are syntax-expanded in pass00
     ;; I'm removing them because syntax expansion is no longer
     ;; included in the testing suite.
     ;(let loop () (define x 3) x)
     #;(list (if (and) (or) (not (or)))
             (or #f #f #f #t) (and #t #t #f)
             (or #f #f (and (and #t) (or #f #t))))
     ))

;===============================================================================

(define tests_derived-prims
  '(
     (let ([y 0]) (if (> y 0) 123 (if (>= y 0) 456 789)))
     (+ 1 2 3 4 5 6 7 8)
     (vector 1 2 3 4 5 6 7 8)
     (list 1 2 3 4 5 6 7 8)
     ))
  
  ;===============================================================================
  
(define tests_callcc
  '(
     (call/cc (lambda (k) 34))
     (call/cc (lambda (name) (name 34)))
     (add1 (call/cc (lambda (k) 33)))
     (add1 (call/cc (lambda (k) (k 33))))
     (add1 (call/cc (lambda (k) (k (k 33)))))
     (add1 (call/cc (lambda (k) (k 33) 97)))
     (let ([f (lambda (cont) (add1 (cont 99)))]) (call/cc f))
     (* 3 (call/cc
            (lambda (exit)
              (letrec ([loop (lambda (n)
                               (if (zero? n)
                                   (exit 5)
                                   (+ 100
                                      (loop (sub1 n)))))])
                (loop 4)))))
     ))

;===============================================================================