
(require (lib "27.ss" "srfi")
         "simulator_nought.ss")

(define make-n-list
  (lambda (n func)
    (letrec ((loop (lambda (n acc)
                     (if (zero? n)
                         acc
                         (loop (sub1 n) (cons (func n) acc))))))
      (loop n '())))) 

(define-structure (simnode id sensor pos))
 
(define the-test-field 
  (make-n-list 500 
	       (lambda (ign) 
		 (make-simnode (random 10000) 
                          (random-real) ;(prim_random 1.0) 
                          (list (random 100) (random 100))))))
     
(define world (map cons the-test-field the-test-field))
(define radius 10.0)

(define (entry? r)
  (and (pair? r)
       (simnode? (cdr r))))
(define (region? r)
  (and (list? r)
       (andmap entry? r)))
(define (nestedregion? r)
  (cond
    [(entry? r) 0]
    [(null? r) 1]
    [else (and (list? r)
               (let ((ls (map nestedregion? r)))
;                 (disp "entering let" ls)
                 (and (andmap number? ls)                     
                      (if (not (null? (cdr ls)))
                          (and (apply = ls) (add1 (car ls)))
                          (add1 (car ls))))))]))
                
(define (neighbors? x y)
  (<= (posdist (simnode-pos (cdr x))
               (simnode-pos (cdr y))) radius))
(define (neighbors n)
  (filter (lambda (p) (neighbors? n p)) world))

;; Node format: (id reading x y)
;(define (node->pos n) (cddr n))
;(define (node->id n) (car n))
;(define (node->reading n) (cadr n))

(define (anchor-at x y)
  (find-maximizing (lambda (entry) (posdist (simnode-pos (cdr entry)) (list x y)))
                   world))

(define (circle-at x y  rad)
  (filter (lambda (p) (<= (posdist (simnode-pos (cdr p)) (list x y)) rad))
          world))

      (define (circle anch rad)
	(filter (lambda (p) (<= (posdist (simnode-pos (cdr p)) (simnode-pos (cdr anch))) rad))
		world))

      (define nodeid simnode-id)
      (define sense  simnode-sensor)
      (define (rfold f s r) 
        (let ((depth (nestedregion? r)))
          (case depth
            [(1) (foldl f s (map car r))]
            [(0 #f) (error 'rfold "input not a region: ~n~a" r)]
            [else (foldl f s r)])))
      
      (define (rmap f r)                
        (let ((depth (nestedregion? r)))
          (case depth
            [(1) (map (lambda (x) (cons (f (car x)) (cdr x))) r)]             
            [(0 #f) (error 'rmap "input not a region: ~n~a" r)]
            [else (map f r)])))
      
      (define (rfilter f r)
        (let ((depth (nestedregion? r)))
          (case depth
            [(1) (filter (lambda (x) (f (car x))) r)]
            [(0 #f) (error 'rfilter "input not a region: ~n~a" r)]
            [else (filter f r)])))
        
      (define (cluster r) (clump neighbors? r))

      
(define (t) 
     (rmap (lambda (r) (rfold + 0 r))
	  (cluster (rfilter even? (rmap nodeid world)))))

(define (t1)
  (nestedregion? (rac (cluster world))))

(define (t2)
  (rfold + 0 (rmap nodeid world)))