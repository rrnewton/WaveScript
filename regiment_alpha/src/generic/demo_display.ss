

;; 2004.03.28
;; Starting this file.

;; This is what Paul Graham would call "exploratory programming".

;; I want a simulator to serve the role of doctor scheme's "Expander"
;; language.  That is, to show conceptually what is meant 

;;======================================================================


;; This file must be used from a context where this makes senes:
;(init-graphics)  ;; This should open windows, etc.

;; Uhh sholud just depend on the one in helpers...
#;(define iota
    (lambda (n . fun*)
      (let ([f (if (null? fun*)
                   (lambda (i) i)
                   (if (procedure? (car fun*))
                       (car fun*)
                       (error 'iota
                              "the optional argument to iota~a ~s"
                              " should be a funciton:" (car fun*))))])
        (letrec ((loop (lambda (n acc)
                         (if (zero? n)
                     
			     (cons (f 0) acc)
                             (loop (sub1 n) (cons (f n) acc))))))
          (if (zero? n)
              '()
              (loop (sub1 n) '())))))) 

;; This maybe shouldn't use the regiment random num generator: 
(define processors_temp
  (map (lambda (_) (list (reg:random-int window-width) (reg:random-int window-height))) 
       (iota 100)))


(define regions
  '(  (circle (100 100) 50)
      (anchor (300 400))
     ))


;; Ok, here we go 
(define (test-this . args)
  (init-graphics)
  (draw-procs processors_temp)
  (close-graphics))
  
#;(define (test)
  (let loop ((acc 0))
    (if (= acc 1000) (void)
	(begin 
	  (draw-procs processors_temp)
	  (paint-buffer)
	  (thread-sleep 100)
	  (loop (add1 acc))))))
	  

