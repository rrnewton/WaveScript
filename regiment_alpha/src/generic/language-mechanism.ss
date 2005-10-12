
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

(define-structure (baselang-simnode id sensor pos))

;; Define this once for the simulation so that our answers are deterministic.
;; This is a list of <sensereading, xpos, ypos> 
(define the-test-field 
  (make-n-list 100
	       (lambda (ign) 
		 (make-baselang-simnode (reg:random-int 10000) 
                          (random-real) ;(prim_random 1.0) 
                          (list (reg:random-int 100) (reg:random-int 100))))))
     


;; I ended up deciding on a weird format for regions in this
;; simulator.  They are ((v . node) ...) but then regions of regions
;; are just lists, and don't repeat the ( . node) portion.

(define-language
  'base-language
  '(begin
     
     (define world (map cons the-test-field the-test-field))
     (define radius 27.0)

;     (define-structure (baselang-simnode id sensor pos))
     
     (define (entry? r)
       (and (pair? r)
	    (baselang-simnode? (cdr r))))

     (define (region? r)
       (and (list? r)
	    (andmap entry? r)))

     (define (quickdepth r)
       (if (entry? r) 0
	   (add1 (quickdepth (car r)))))

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
       (<= (posdist (baselang-simnode-pos (cdr x))
		    (baselang-simnode-pos (cdr y))) radius))
     (define (neighbors n)
       (filter (lambda (p) (neighbors? n p)) world))

     ;; Node format: (id reading x y)
					;(define (node->pos n) (cddr n))
					;(define (node->id n) (car n))
					;(define (node->reading n) (cadr n))

     (define (anchor-at x y)
       (find-maximizing (lambda (entry) (posdist (baselang-simnode-pos (cdr entry)) 
						 (list x y)))
			world))

     (define (circle-at x y  rad)
       (filter (lambda (p) (<= (posdist (baselang-simnode-pos (cdr p))
					(list x y)) rad))
	       world))

     (define (circle anch rad)
       (filter (lambda (p) (<= (posdist (baselang-simnode-pos (cdr p)) 
					(baselang-simnode-pos (cdr anch))) rad))
	       world))

     (define nodeid baselang-simnode-id)
     (define sense  baselang-simnode-sensor)
     (define (rfold f s r) 
       ;(let ((depth (nestedregion? r)))
       (let ((depth (quickdepth r)))
	 (case depth
	   [(1) (foldl f s (map car r))]
	   [(0 #f) (error 'rfold "input not a region: ~n~a" r)]
	   [else (foldl f s r)])))
     
     (define (rmap f r)                
       ;(let ((depth (nestedregion? r)))
       (let ((depth (quickdepth r)))
	 (case depth
	   [(1) (map (lambda (x) (cons (f (car x)) (cdr x))) r)]             
	   [(0 #f) (error 'rmap "input not a region: ~n~a" r)]
	   [else (map f r)])))
     
     (define (rfilter f r)
       ;(let ((depth (nestedregion? r)))
       (let ((depth (quickdepth r)))
	 (case depth
	   [(1) (filter (lambda (x) (f (car x))) r)]
	   [(0 #f) (error 'rfilter "input not a region: ~n~a" r)]
	   [else (filter f r)])))
     
     (define (cluster r) (clump neighbors? r))

      ;; Since this is frozen in time events have little meaning:
     (define (rwhen-any f r)
       (if (ormap f (map car r))
	   #t #f))
     (define (swhen-any f s) (f s))

      ;; And until has no meaning at all.
      (define (until e s1 s2) s1)


     ))



      
'(define-language
  'base-language
  '(begin
;     (define ignored (begin (disp "Loading base language!") (flush-output-port)))
     
     (define world (map cons the-test-field the-test-field))
     (define radius 10.0)
     (define (neighbors? x y)
       (<= (posdist (node->pos (cdr x)) (node->pos (cdr y))) radius))
     (define (neighbors n)
       (filter (lambda (p) (neighbors? n p)) world))

     ;; Node format: (id reading x y)
     (define (node->pos n) (cddr n))
     (define (node->id n) (car n))
     (define (node->reading n) (cadr n))

     (define (anchor-at x y)
       (find-maximizing (lambda (entry) (posdist (node->pos (cdr entry)) (list x y)))
			world))

      (define (circle-at x y  rad)
	(filter (lambda (p) (<= (posdist (node->pos (cdr p)) (list x y)) rad))
		world))

      (define (circle anch rad)
	(filter (lambda (p) (<= (posdist (node->pos (cdr p)) (node->pos (cdr anch))) rad))
		world))

      (define nodeid node->id)
      (define sense node->reading)
      (define (rfold f s r) (rfold f s (map car r)))
      (define (rmap f r) (map (lambda (x) (cons (f (car x)) (cdr x))) r))
      (define (rfilter f r) (filter (lambda (x) (f (car x))) r))
      (define (cluster r) (clump neighbors? r))

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




