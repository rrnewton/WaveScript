#!r6rs

(library (depends matpak)   
  (export ws-invert-matrix)
  (import (rnrs) (ws common))

;; This matrix code is lifted from the web.
;  Found at: http://www.cap-lore.com/MathPhys/Field/
(define matpak (lambda (zero zer? one fa fs fm fi)                  
  (letrec ((tf (lambda (x zp) 
                 (if (null? x) (zp (list one))
                     (if (zer? (caar x)) (let ((z (tf (cdr x) zp)))
                                           (cons (car z) (cons (car x) (cdr z)))) x))))
           (tr (trnxx zero zer?))
           (sm (lambda (s a)(map (lambda (w) (fm s w)) a)))
           (fms (lambda (a s b) 
                  (if (null? a) (sm (fs zero s) b) 
                      (if (null? b) a
                          (cons (fs (car a) (fm s (car b))) (fms (cdr a) s (cdr b)))))))
           (deter (lambda (a) 
                    (letrec ((p #f)
                             (tf (lambda (x) (if (null? x) (list one)
                                                 (if (zer? (caar x)) 
                                                     (let ((z (tf (cdr x))))
                                                       (set! p (not p))
                                                       (cons (car z) (cons (car x) (cdr z)))) x)))))
                      (let inx ((d one)(a a))
                        (if (null? a) (if p (fs zero d) d) 
                            (let* ((A (tf a))
                                   (i (fi (caar A)))
                                   (b (map (lambda (z) (fm z i)) (cdar A))))
                              (inx (fm (caar A) d) (map (lambda (x w) (fms x w b))
                                                        (map cdr (cdr A)) (map car (cdr A))))))))))
           (inv (lambda (a nxp) 
                  (let ol ((ut (let inx 
                                 ((a (let pad ((x a)(e (list one)))
                                       (if (null? x) '() 
                                           (cons (let ap ((z (car x)) (ln a))
                                                   (if (null? ln) e 
                                                       (if (null? z) (cons zero (ap z (cdr ln)))
                                                           (cons (car z)(ap (cdr z)(cdr ln))))))
                                                 (pad (cdr x) (cons zero e))))))
                                  (np nxp))
                                 (if (null? a) '() 
                                     (let* ((A (tf a np))
                                            (i (fi (caar A)))
                                            (b (map (lambda (z) (fm z i)) (cdar A))))
                                       (cons b (inx (map (lambda (x w) (fms x w b))
                                                         (map cdr (cdr A)) (map car (cdr A)))
                                                    (lambda (w) (np (cons (fs zero (ip w b)) w))))))))))
                    (if (null? ut) '() (cons
                                        (let eg ((top (car ut))(bod (cdr ut)))
                                          (if (null? bod) top
                                              (eg (fms (cdr top) (car top) (car bod))(cdr bod))))
                                        (ol (cdr ut)))))))
           (ip (lambda (x y)(if (or (null? x) (null? y)) zero (fa (fm (car x)(car y))
                                                                  (ip (cdr x)(cdr y))))))
           (mp (lambda (a b)(let ((b (tr b)))
                              (map (lambda(x) (map (lambda (y) (ip x y)) b)) a)))))    
    (values mp inv ip tr deter))))


(define (trnxx zer zer?) (lambda (x) (if (null? x) '()
  (let ((z ((trnxx zer zer?) (cdr x)))) (let m ((u (car x))(v z))
    (if (null? u) (map (lambda (q) (if (null? q) '() (cons zer q))) v)
     (if (null? v) (let y ((q u)) (if (null? q) '() (let ((p (y (cdr q))))
         (if (and (null? p) (zer? (car q))) '()
           (cons (if (zer? (car q)) '() (list (car q))) p)))))
       (cons (cons (car u)(car v)) (m (cdr u)(cdr v))))))))))

(define our-exit (let* ((s (cons 0 0))
   (z (call-with-current-continuation (lambda (e) (cons s e)))))
      (if (and (pair? z) (eq? (car z) s)) (cdr z) our-exit)))

(define mer (lambda (x) (let ((r (call-with-current-continuation
  (lambda(d) (cons #f (lambda(y) (d (cons #t y))))))))
   (if (car r) (begin (write (list "MatrixException:" x (cdr r))) (newline) (our-exit 0))
   (cdr r)))))

(define cc (mer "Singular:"))
  
(define-values (mp inv ip tr deter) (matpak 0 zero? 1 + - * (lambda (x)(/ 1 x))))

(define (ws-invert-matrix mat)
  (list->vector
   (map list->vector
        (inv (map vector->list (vector->list mat)) cc))))
 
) ;; End module

;(require matpak)

#;
(call-with-values 
 (lambda ()        ; Here we supply matpak with the field goodies the rationals.
       (matpak 0 zero? 1 + - * (lambda (x)(/ 1 x))))
 (lambda (matm matinv ip tr det)
   
         ; A test matrix inversion with rational arithmetic.
         (let* ((a '((0 2 4 5) (3 4 5 -2) (7 6 5 3) (4 6 5 7)))
                (b (matinv a cc)))
           (list b (matm a b)(matinv b cc)))))

#;
(ws-invert-matrix 
 (ws-invert-matrix
  #(#(0 2 4 5) 
    #(3 4 5 -2)
    #(7 6 5 3)
    #(4 6 5 7))))
 