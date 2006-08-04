
(module scheme_fft mzscheme

  (require  "constants.ss"
	    "../plt/helpers.ss")
  
  ; dft-vector
  (provide dft-list testfft-bigger testfft-more)
  
  (chezimports )

  ;; This is the list-based recursive DFT from the chez scheme users guide:
  (define (dft-list x)
    (define (w-powers n)
      (let ((pi (* (acos 0.0) 2)))
	(let ((delta (/ (* -2.0i pi) n)))
	  (let f ((n n) (x 0.0))
	    (if (= n 0)
		'()
		(cons (exp x) (f (- n 2) (+ x delta))))))))
    (define (evens w)
      (if (null? w)
	  '()
	  (cons (car w) (evens (cddr w)))))
    (define (interlace x y)
      (if (null? x)
	  '()
	  (cons (car x) (cons (car y) (interlace (cdr x) (cdr y))))))
    (define (split ls)
      (let split ((fast ls) (slow ls))
	(if (null? fast)
	    (values '() slow)
	    (call-with-values
		(lambda () (split (cddr fast) (cdr slow)))
	      (lambda (front back)
		(values (cons (car slow) front) back))))))
    (define (butterfly x w)
      (call-with-values
	  (lambda () (split x))
	(lambda (front back)
	  (values
	   (map + front back)
	   (map * (map - front back) w)))))
    (define (rfft x w)
      (if (null? (cddr x))
	  (let ((x0 (car x)) (x1 (cadr x)))
	    (list (+ x0 x1) (- x0 x1)))
	  (call-with-values
	      (lambda () (butterfly x w))
	    (lambda (front back)
	      (let ((w (evens w)))
		(interlace (rfft front w) (rfft back w)))))))
    (rfft x (w-powers (length x))))

;;;=========================================================================
;; This vector version isn't useful, it's probably trash:

;; Refactored to use vectors rather than lists.
;; It doesn't help because it's still a recursive algorithm.
;; (Could look into in-place versions.)
(define (dft-vector x)
  (define (w-powers n)
    (let* ([pi (* (acos 0.0) 2)]
           [delta (/ (* -2.0i pi) n)]
           [half (fxsrl n 1)]
           [out (make-vector half)])      
      (do ([n 0 (fx+ n 1)] [x 0.0 (+ x delta)])   ((fx= n half) out)
        (vector-set! out n (exp x)))))
   (define (evens v)
    (let* ([half (fxsrl (vector-length v) 1)]
           [out (make-vector half)])
      (do ((i 0 (fx+ i 1)))  ((fx= i half) out)
        (vector-set! out i (vector-ref v (fxsll i 1))))))
   (define (interlace x y)
     (let* ([double (fx+ (vector-length x) (vector-length y))]
            [out (make-vector double)])
       (do ((i 0 (fx+ i 1)))  ((fx= i (vector-length x)) out)
         (vector-set! out (fxsll i 1)         (vector-ref x i))
         (vector-set! out (fx+ 1 (fxsll i 1)) (vector-ref y i))
         )))
   (define (subvector v ind l)
     (let ([out (make-vector l)])
       (do ((i 0 (fx+ i 1)))   ((fx= i l) out)
         (vector-set! out i (vector-ref v (fx+ ind i))))))
   (define (split v)
     (let ([half (fxsrl (vector-length v) 1)])
       (values (subvector v 0 half) 
               (subvector v half half))))
   (define (vmap2 f v1 v2)
     (let ([out (make-vector (vector-length v1))])
       (do ((i 0 (fx+ 1 i))) ((fx= i (vector-length v1)) out)
         (vector-set! out i (f (vector-ref v1 i) (vector-ref v2 i))))))
   (define (vmap3 f v1 v2 v3)
     (let ([out (make-vector (vector-length v1))])
       (do ((i 0 (fx+ 1 i))) ((fx= i (vector-length v1)) out)
         (vector-set! out i (f (vector-ref v1 i)
                               (vector-ref v2 i)
                               (vector-ref v3 i))))))
  (define (butterfly x w)
    (let-values ([(front back) (split x)])
      (values (vmap2 + front back)
              (vmap3 (lambda (f b w) (* (- f b) w))
                     front back w))))
  (define (rfft x w)
    (if (fx= (vector-length x) 2)
        (let ((x0 (vector-ref x 0)) (x1 (vector-ref x 1)))
          (vector (+ x0 x1) (- x0 x1)))
        (let-values ([(front back) (butterfly x w)])
          (let ([w (evens w)])
            (interlace (rfft front w) (rfft back w))))))
  (rfft x (w-powers (vector-length x))))


;; Compare list vs. vector implementation:
(define (testfft-bigger)
  (let loop ((n 2))
    (let* ([ls (iota n)]
           [vec (list->vector ls)])
      ;(collect 4)
      (printf "\n\n\n============================================\n")
      (printf "IOTA ~a\n" n)
      (printf "   w/Lists:\n")
      (collect)
      (time (dft-list ls))
      (printf "   w/Vectors:\n")
      (collect)
      (let ([v2 0] [v3 0])
	(time (set! v3 (dft-vector vec)))
;	(collect 4)
;	(time (set! v2 (dft2 vec)))
;	(if (not (equal? v2 v3))
;	    (error 'testfft-bigger "results were not equal!"))
	)
      (loop (* n 2))
  )))
(define (testfft-more)
  (let loop ((n 2))
    (let* ([ls (iota 1024)]
           [vec (list->vector ls)])
      (printf "\n\n\n============================================\n")
      (printf "REPEATING ~a\n" n)
      (printf "   w/Lists:\n")
;      (collect 4)
      ;; Make it pay for converting too:  It's still better!
      (time (rep n (dft-list (vector->list vec))))
      (printf "   w/Vectors:\n")
;      (collect 4)
      (time (rep n (dft-vector vec)))
      (loop (* n 2)))))
  
) ;; End module

;(require scheme_fft)
;(testfft-bigger)