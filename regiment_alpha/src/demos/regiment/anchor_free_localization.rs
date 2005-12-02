
;;;; This demo implements the Anchor Free Localization algorithm.

(parameters 
  [simalpha-realtime-mode #t]

  [simalpha-channel-model 'lossless]
  [simalpha-placement-type 'gridlike]
  [simalpha-failure-model  'none]
  ;[simalpha-sense-function sense-noisy-rising]
  ;[simalpha-sense-function sense-random-1to100]
  [simalpha-sense-function sense-dist-from-origin]
  [sim-timeout 2000])


;; Main program:

(define anchor-helper		       
  (lambda (funs)
    (anchor-optimizing
     (lambda (x y)
       (letrec ((compare
		 (lambda (ls x y)
		   (if (null? ls) 0
		       (letrec ([f (car ls)])
			 (if (f x y) 1
			     (if (f y x) -1
				 (compare (cdr ls) x y))))))))
	 (compare funs x y)))
     (rmap node->anchor world))))
(define between
  (lambda (a b)
    (lambda (x y)
      (> (abs (- (anchor-dist a x) (anchor-dist b x)))
	 (abs (- (anchor-dist a y) (anchor-dist a y)))))))
(define away-from 
  (lambda (a) 
    (lambda (x y) (> (anchor-dist a x) (anchor-dist a y)))))
(define away-from-both
  (lambda (a b)
    (lambda (x y)
      (> (abs (- (anchor-dist a x) (anchor-dist b x)))
	 (abs (- (anchor-dist a y) (anchor-dist b y)))))))

(define n0 (anchor))
(define n1 (anchor-helper (list (away-from n0))))
(define n2 (anchor-helper (list (away-from n1))))
(define n3 (anchor-helper (list (between n1 n2) (away-from-both n1 n2))))
(define n4 (anchor-helper (list (between n1 n2) (away-from n3))))
(define n5 (anchor-helper (list (between n1 n2) (between n3 n4))))

